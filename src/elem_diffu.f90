module elem_diffu

      use mod_nvtx
      use mod_constants
      use mod_veclen

      ! TODO: Create unit tests for all subroutines

      contains

              subroutine mass_diffusion(nelem,npoin,connec,Ngp,dNgp,He,gpvol,rho,mu_e,Rmass)

                      ! TODO: Add stab. viscosity

                      implicit none

                      integer(4), intent(in)  :: nelem, npoin
                      integer(4), intent(in)  :: connec(nelem,nnode)
                      real(8),    intent(in)  :: Ngp(ngaus,nnode), dNgp(ndime,nnode,ngaus)
                      real(8),    intent(in)  :: He(ndime,ndime,ngaus,nelem)
                      real(8),    intent(in)  :: gpvol(1,ngaus,nelem)
                      real(8),    intent(in)  :: rho(npoin), mu_e(nelem,ngaus)
                      real(8),    intent(out) :: Rmass(npoin)
                      integer(4)              :: ielem, igaus, inode, idime, jdime
                      real(8)                 :: Re(nnode), nu_e
                      real(8)                 :: tmp1, gpcar(ndime,nnode), tmp2

                      call nvtxStartRange("Mass diffusion")
                      !$acc kernels
                      Rmass(:) = 0.0d0
                      !$acc end kernels
                      !$acc parallel loop gang  private(gpcar,Re) vector_length(vecLength)
                      do ielem = 1,nelem
                         !$acc loop vector
                         do inode = 1,nnode
                            Re(inode) = 0.0d0
                         end do
                         !$acc loop seq
                         do igaus = 1,ngaus
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop vector
                               do inode = 1,nnode
                                  gpcar(idime,inode) = dot_product(He(idime,:,igaus,ielem),dNgp(:,inode,igaus))
                               end do
                            end do
                           !$acc loop seq
                           do idime = 1,ndime
                              tmp1 = 0.0d0
                              tmp2 = 0.0d0
                              !$acc loop vector reduction(+:tmp1,tmp2)
                              do inode = 1,nnode
                                 tmp1 = tmp1+gpcar(idime,inode)*rho(connec(ielem,inode))
                                 tmp2 = tmp2+Ngp(igaus,inode)*rho(connec(ielem,inode)) 
                              end do
                              nu_e = c_rho*mu_e(ielem,igaus)/tmp2
                              !$acc loop vector
                              do inode = 1,nnode
                                 Re(inode) = Re(inode)+nu_e*gpvol(1,igaus,ielem)* &
                                             gpcar(idime,inode)*tmp1
                              end do
                           end do
                         end do
                         !$acc loop vector
                         do inode = 1,nnode
                            !$acc atomic update
                            Rmass(connec(ielem,inode)) = Rmass(connec(ielem,inode))+Re(inode)
                            !$acc end atomic
                         end do
                      end do
                      !$acc end parallel loop
                      call nvtxEndRange

              end subroutine mass_diffusion

              subroutine mom_diffusion(nelem,npoin,connec,Ngp,dNgp,He,gpvol,u,mu_fluid,mu_e,mu_sgs,Rmom)

                      ! TODO: Add. stab. viscosity

                      implicit none

                      integer(4), intent(in)  :: nelem, npoin
                      integer(4), intent(in)  :: connec(nelem,nnode)
                      real(8),    intent(in)  :: Ngp(ngaus,nnode), dNgp(ndime,nnode,ngaus)
                      real(8),    intent(in)  :: He(ndime,ndime,ngaus,nelem)
                      real(8),    intent(in)  :: gpvol(1,ngaus,nelem)
                      real(8),    intent(in)  :: u(npoin,ndime),mu_e(nelem,ngaus),mu_sgs(nelem,ngaus)
                      real(8),    intent(in)  :: mu_fluid(npoin)
                      real(8),    intent(out) :: Rmom(npoin,ndime)
                      integer(4)              :: ielem, igaus, idime, jdime, inode, jnode
                      real(8)                 :: Re(nnode,ndime), twoThirds, gpcar(ndime,nnode), tau(ndime,ndime)
                      real(8)                 :: divU, gradU(ndime,ndime), mu_fgp, aux

                      twoThirds = 2.0d0/3.0d0
                      call nvtxStartRange("Momentum diffusion")
                      !$acc kernels
                      Rmom(:,:) = 0.0d0
                      !$acc end kernels
                      !$acc parallel loop gang  private(Re,gpcar,tau,gradU) vector_length(vecLength)
                      do ielem = 1,nelem
                         !$acc loop vector collapse(2)
                         do inode = 1,nnode
                            do idime = 1,ndime
                               Re(inode,idime) = 0.0d0
                            end do
                         end do
                         !
                         ! Gradient structure:
                         !
                         !         | u1,1 u1,2 u1,3 |
                         ! u_i,j = | u2,1 u2,2 u2,3 |
                         !         | u3,1 u3,2 u3,3 |
                         !
                         !$acc loop seq
                         do igaus = 1,ngaus
                            !
                            ! Compute gpcar
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop vector
                               do inode = 1,nnode
                                  gpcar(idime,inode) = dot_product(He(idime,:,igaus,ielem),dNgp(:,inode,igaus))
                               end do
                            end do
                            !
                            ! Compute combined viscosity at Gauss point
                            !
                            mu_fgp = 0.0d0
                            !$acc loop vector reduction(+:mu_fgp)
                            do inode = 1,nnode
                               mu_fgp = mu_fgp+(Ngp(igaus,inode)*mu_fluid(connec(ielem,inode)))
                            end do
                            mu_fgp = mu_fgp+mu_e(ielem,igaus)+mu_sgs(ielem,igaus)
                            !
                            ! Compute grad(u)
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop seq
                               do jdime = 1,ndime
                                  aux = 0.0d0
                                  !$acc loop vector reduction(+:aux)
                                  do inode = 1,nnode
                                     aux = aux+gpcar(jdime,inode)*u(connec(ielem,inode),idime)
                                  end do
                                  gradU(idime,jdime) = aux
                               end do
                            end do
                            !
                            ! Compute div(u)
                            !
                            divU = 0.0d0
                            ! divU = gradU(1,1)+gradU(2,2)+gradU(3,3)
                            !$acc loop seq
                            do idime = 1,ndime
                               divU = divU+gradU(idime,idime)
                            end do

                            ! TODO: Remove this
                            !divU = 0.0d0
                            !!$acc loop vector collapse(2) reduction(+:divU)
                            !do idime = 1,ndime
                            !   do inode = 1,nnode
                            !      divU = divU+gpcar(idime,inode)*u(connec(ielem,inode),idime)
                            !   end do
                            !end do

                            !
                            ! Finish computing tau_ij = u_i,j + u_j,i - (2/3)*div(u)*d_ij
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop seq
                               do jdime = 1,ndime
                                  tau(idime,jdime) = gradU(idime,jdime)+gradU(jdime,idime)
                               end do
                            end do
                            !$acc loop seq
                            do idime = 1,ndime
                               tau(idime,idime) = tau(idime,idime)-twoThirds*divU
                            end do
                            !
                            ! Compute div(tau) at the Gauss point
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop seq
                               do jdime = 1,ndime
                                  !$acc loop vector
                                  do inode = 1,nnode
                                     Re(inode,idime) = Re(inode,idime)+gpvol(1,igaus,ielem)* &
                                        gpcar(jdime,inode)*mu_fgp*tau(idime,jdime)
                                  end do
                               end do
                            end do
                         end do
                         !
                         ! Assembly
                         !
                         !$acc loop vector collapse(2)
                         do inode = 1,nnode
                            do idime = 1,ndime
                               !$acc atomic update
                               Rmom(connec(ielem,inode),idime) = Rmom(connec(ielem,inode),idime)+1.0d0*Re(inode,idime)
                               !$acc end atomic
                            end do
                         end do
                      end do
                      !$acc end parallel loop
                      call nvtxEndRange

              end subroutine mom_diffusion

              subroutine ener_diffusion(nelem,npoin,connec,Ngp,dNgp,He,gpvol,u,Tem,mu_fluid,mu_e,mu_sgs,Rener)

                      implicit none

                      integer(4), intent(in)  :: nelem, npoin
                      integer(4), intent(in)  :: connec(nelem,nnode)
                      real(8),    intent(in)  :: Ngp(ngaus,nnode), dNgp(ndime,nnode,ngaus)
                      real(8),    intent(in)  :: He(ndime,ndime,ngaus,nelem)
                      real(8),    intent(in)  :: gpvol(1,ngaus,nelem)
                      real(8),    intent(in)  :: u(npoin,ndime), Tem(npoin), mu_e(nelem,ngaus), mu_sgs(nelem,ngaus)
                      real(8),    intent(in)  :: mu_fluid(npoin)
                      real(8),    intent(out) :: Rener(npoin)
                      integer(4)              :: ielem, igaus, inode, idime, jdime
                      real(8)                 :: Re(nnode), kappa_e, mu_fgp, aux, divU, tauU(ndime), twoThirds
                      real(8)                 :: gpcar(ndime,nnode), gradU(ndime,ndime), gradT(ndime)

                      call nvtxStartRange("Energy diffusion")
                      twoThirds = 2.0d0/3.0d0
                      !$acc kernels
                      Rener(:) = 0.0d0
                      !$acc end kernels
                      !$acc parallel loop gang  private(Re,gpcar,gradU,gradT,tauU)  vector_length(vecLength)
                      do ielem = 1,nelem
                         !$acc loop vector
                         do inode = 1,nnode
                            Re(inode) = 0.0d0
                         end do
                         !$acc loop seq
                         do igaus = 1,ngaus
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop vector
                               do inode = 1,nnode
                                  gpcar(idime,inode) = dot_product(He(idime,:,igaus,ielem),dNgp(:,inode,igaus))
                               end do
                            end do
                            !
                            ! Compute viscosity and conductivity at Gauss point
                            !
                            mu_fgp = 0.0d0
                            !$acc loop vector reduction(+:mu_fgp)
                            do inode = 1,nnode
                               mu_fgp = mu_fgp+(Ngp(igaus,inode)*mu_fluid(connec(ielem,inode)))
                            end do
                            kappa_e =mu_fgp*1004.0d0/0.71d0+c_ener*mu_e(ielem,igaus)/0.4d0 + mu_sgs(ielem,igaus)/0.9d0
                            mu_fgp = mu_fgp+mu_e(ielem,igaus)
                            !
                            ! Compute grad(U)
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop seq
                               do jdime = 1,ndime
                                  aux = 0.0d0
                                  !$acc loop vector reduction(+:aux)
                                  do inode = 1,nnode
                                     aux = aux+gpcar(jdime,inode)*u(connec(ielem,inode),idime)
                                  end do
                                  gradU(idime,jdime) = aux
                               end do
                            end do
                            !
                            ! Compute div(u)
                            divU = 0.0d0
                            !$acc loop seq
                            do idime = 1,ndime
                               divU = divU+gradU(idime,idime)
                            end do

                            ! TODO: Remove this lines
                            !divU = 0.0d0
                            !!$acc loop vector collapse(2) reduction(+:divU)
                            !do idime = 1,ndime
                            !   do inode = 1,nnode
                            !      divU = divU+gpcar(idime,inode)*u(connec(ielem,inode),idime)
                            !   end do
                            !end do

                            !
                            ! Compute tau*u
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               tauU(idime) = 0.0d0
                               !$acc loop seq
                               do jdime = 1,ndime
                                  aux = 0.0d0
                                  !$acc loop vector reduction(+:aux)
                                  do inode = 1,nnode
                                     aux = aux+Ngp(igaus,inode)*u(connec(ielem,inode),jdime)
                                  end do
                                  tauU(idime) = tauU(idime) + &
                                     gradU(idime,jdime)*aux + gradU(jdime,idime)*aux
                               end do
                               aux = 0.0d0
                               !$acc loop vector reduction(+:aux)
                               do inode = 1,nnode
                                  aux = aux+Ngp(igaus,inode)*u(connec(ielem,inode),idime)
                               end do
                               tauU(idime) = tauU(idime)-twoThirds*divU*aux
                            end do
                            !
                            ! Compute grad(T)
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               aux = 0.0d0
                               !$acc loop vector reduction(+:aux)
                               do inode = 1,nnode
                                  aux = aux+gpcar(idime,inode)*Tem(connec(ielem,inode))
                               end do
                               gradT(idime) = aux
                            end do
                            !
                            ! Gaussian integ
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop vector
                               do inode = 1,nnode
                                  Re(inode) = Re(inode)+gpvol(1,igaus,ielem)*gpcar(idime,inode)* &
                                     (mu_fgp*tauU(idime)+kappa_e*gradT(idime))
                               end do
                            end do
                         end do
                         !$acc loop vector
                         do inode = 1,nnode
                            !$acc atomic update
                            Rener(connec(ielem,inode)) = Rener(connec(ielem,inode))+1.0d0*Re(inode)
                            !$acc end atomic
                         end do
                      end do
                      !$acc end parallel loop
                      call nvtxEndRange

              end subroutine ener_diffusion

              subroutine full_diffusion(nelem,npoin,connec,Ngp,dNgp,He,gpvol,Cp,Pr,rho,u,Tem,mu_fluid,mu_e,mu_sgs,Ml,Rmass,Rmom,Rener)
                      implicit none

                      integer(4), intent(in)  :: nelem, npoin
                      integer(4), intent(in)  :: connec(nelem,nnode)
                      real(8),    intent(in)  :: Ngp(ngaus,nnode), dNgp(ndime,nnode,ngaus)
                      real(8),    intent(in)  :: He(ndime,ndime,ngaus,nelem)
                      real(8),    intent(in)  :: gpvol(1,ngaus,nelem)
                      real(8),    intent(in)  :: Cp,Pr,rho(npoin),u(npoin,ndime), Tem(npoin), mu_e(nelem,ngaus), mu_sgs(nelem,ngaus),Ml(npoin)
                      real(8),    intent(in)  :: mu_fluid(npoin)
                      real(8),    intent(out) :: Rmass(npoin)
                      real(8),    intent(out) :: Rmom(npoin,ndime)
                      real(8),    intent(out) :: Rener(npoin)
                      integer(4)              :: ielem, igaus, inode, idime, jdime
                      real(8)                 :: Re_mass(nnode),Re_mom(nnode,ndime),Re_ener(nnode)
                      real(8)                 :: kappa_e, mu_fgp, mu_egp,aux, divU, tauU(ndime), twoThirds,nu_e,tau(ndime,ndime)
                      real(8)                 :: gpcar(ndime,nnode), gradU(ndime,ndime), gradT(ndime),tmp1,ugp(ndime),vol,arho
                      real(8)                 :: ul(nnode,ndime), rhol(nnode),Teml(nnode),mufluidl(nnode), aux2

                      call nvtxStartRange("Full diffusion")
                      twoThirds = 2.0d0/3.0d0
                      !$acc kernels
                      Rmass(:) = 0.0d0
                      Rmom(:,:) = 0.0d0
                      Rener(:) = 0.0d0
                      !$acc end kernels

                      !$acc parallel loop gang  private(ul,Teml,rhol,mufluidl,Re_mass,Re_mom,Re_ener) !!vector_length(vecLength)
                      do ielem = 1,nelem
                         !$acc loop vector
                         do inode = 1,nnode
                            Re_mass(inode) = 0.0d0
                            Re_ener(inode) = 0.0d0
                            rhol(inode) = rho(connec(ielem,inode))
                            Teml(inode) = Tem(connec(ielem,inode))
                            mufluidl(inode) = mu_fluid(connec(ielem,inode))
                         end do
                         !$acc loop vector collapse(2)
                         do inode = 1,nnode
                            do idime = 1,ndime
                               Re_mom(inode,idime) = 0.0d0
                               ul(inode,idime) = u(connec(ielem,inode),idime)
                            end do
                         end do
                         !$acc loop worker private(gpcar,tau,gradU,gradT,tauU,ugp)
                         do igaus = 1,ngaus
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop vector
                               do inode = 1,nnode
                                  aux =  dot_product(He(idime,:,igaus,ielem),dNgp(:,inode,igaus))
                                  gpcar(idime,inode) = aux
                               end do
                               ugp(idime) = ul(igaus,idime)
                            end do
                            nu_e = c_rho*mu_e(ielem,igaus)/rhol(igaus)
                            mu_fgp = mufluidl(igaus)+rhol(igaus)*mu_sgs(ielem,igaus)
                            mu_egp = mu_e(ielem,igaus)
                            kappa_e =mufluidl(igaus)*Cp/Pr+c_ener*mu_e(ielem,igaus)/0.4d0 + rhol(igaus)*mu_sgs(ielem,igaus)/0.9d0
                            !
                            ! Compute grad(u)
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop seq
                               do jdime = 1,ndime
                                  aux = 0.0d0
                                  !$acc loop vector reduction(+:aux)
                                  do inode = 1,nnode
                                     aux = aux+gpcar(jdime,inode)*ul(inode,idime)
                                  end do
                                  gradU(idime,jdime) = aux
                               end do
                            end do
                            !
                            ! Compute div(u)
                            !
                            divU = gradU(1,1)+gradU(2,2)+gradU(3,3)
                            !
                            ! Finish computing tau_ij = u_i,j + u_j,i - (2/3)*div(u)*d_ij
                            ! Compute tau*u
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               tauU(idime) = 0.0d0
                               !$acc loop seq
                               do jdime = 1,ndime
                                  tauU(idime) = tauU(idime) + &
                                     (mu_fgp+mu_egp)*(gradU(idime,jdime)+ gradU(jdime,idime))*ugp(jdime)
                                  tau(idime,jdime) = (mu_fgp+mu_egp)*(gradU(idime,jdime)+gradU(jdime,idime))
                               end do
                               tauU(idime) = tauU(idime)-(mu_fgp)*twoThirds*divU*ugp(idime)
                               tau(idime,idime) = tau(idime,idime)-(mu_fgp)*twoThirds*divU
                            end do

                            ! Dif rho
                            ! Dif T
                            ! Compute div(tau) at the Gauss point
                            !$acc loop seq
                            do idime = 1,ndime
                               tmp1 = 0.0d0
                               aux = 0.0d0
                               !$acc loop vector reduction(+:tmp1,aux)
                               do inode = 1,nnode
                                  tmp1 = tmp1+gpcar(idime,inode)*rhol(inode)
                                  aux = aux+gpcar(idime,inode)*Teml(inode)
                               end do
                               !$acc loop vector
                               do inode = 1,nnode
                                  !$acc atomic update
                                  Re_mass(inode) = Re_mass(inode)+nu_e*gpvol(1,igaus,ielem)* &
                                     gpcar(idime,inode)*tmp1
                                  !$acc end atomic
                                  !$acc atomic update
                                  Re_ener(inode) = Re_ener(inode)+gpvol(1,igaus,ielem)*gpcar(idime,inode)* &
                                     (tauU(idime)+kappa_e*aux) 
                                  !$acc end atomic
                               end do
                               !$acc loop vector collapse(2)
                               do jdime = 1,ndime
                                  do inode = 1,nnode
                                     !$acc atomic update
                                     Re_mom(inode,idime) = Re_mom(inode,idime)+gpvol(1,igaus,ielem)* &
                                        gpcar(jdime,inode)*tau(idime,jdime)
                                     !$acc end atomic
                                  end do
                               end do
                            end do
                         end do
                         !$acc loop vector
                         do inode = 1,nnode
                            !$acc atomic update
                            Rmass(connec(ielem,inode)) = Rmass(connec(ielem,inode))+Re_mass(inode)
                            !$acc end atomic
                            !$acc atomic update
                            Rener(connec(ielem,inode)) = Rener(connec(ielem,inode))+Re_ener(inode)
                            !$acc end atomic
                         end do
                         !$acc loop vector collapse(2)
                         do inode = 1,nnode
                            do idime = 1,ndime
                               !$acc atomic update
                               Rmom(connec(ielem,inode),idime) = Rmom(connec(ielem,inode),idime)+Re_mom(inode,idime)
                               !$acc end atomic
                            end do
                         end do
                      end do
                      !$acc end parallel loop
                      call nvtxEndRange

              end subroutine full_diffusion

              subroutine full_diffusion_ijk(nelem,npoin,connec,Ngp,dNgp,He,gpvol,dlxigp_ip,xgp,atoIJK,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,Cp,Pr,rho,u,Tem,mu_fluid,mu_e,mu_sgs,Ml,Rmass,Rmom,Rener)
                      implicit none

                      integer(4), intent(in)  :: nelem, npoin
                      integer(4), intent(in)  :: connec(nelem,nnode)
                      real(8),    intent(in)  :: Ngp(ngaus,nnode), dNgp(ndime,nnode,ngaus)
                      real(8),    intent(in)  :: He(ndime,ndime,ngaus,nelem),xgp(ngaus,ndime),dlxigp_ip(ngaus,ndime,porder+1)
                      real(8),    intent(in)  :: gpvol(1,ngaus,nelem)
                      integer(4), intent(in)  :: atoIJK(nnode),invAtoIJK(porder+1,porder+1,porder+1),gmshAtoI(nnode), gmshAtoJ(nnode), gmshAtoK(nnode)
                      real(8),    intent(in)  :: Cp,Pr,rho(npoin),u(npoin,ndime), Tem(npoin), mu_e(nelem,ngaus), mu_sgs(nelem,ngaus),Ml(npoin)
                      real(8),    intent(in)  :: mu_fluid(npoin)
                      real(8),    intent(out) :: Rmass(npoin)
                      real(8),    intent(out) :: Rmom(npoin,ndime)
                      real(8),    intent(out) :: Rener(npoin)
                      integer(4)              :: ielem, igaus, inode, idime, jdime, isoI, isoJ, isoK,kdime,ii
                      real(8)                 :: kappa_e, mu_fgp, mu_egp,divU, tauU(ndime), twoThirds,nu_e,tau(ndime,ndime)
                      real(8)                 :: gradU(ndime,ndime), gradT(ndime),tmp1,vol,arho
                      real(8)                 :: gradIsoRho(ndime),gradIsoT(ndime),gradIsoU(ndime,ndime)
                      real(8)                 :: gradRho(ndime), gradIsoDm(ndime,ndime,ndime),divDm(ndime),divDr,divDe
                      real(8)                 :: ul(nnode,ndime), rhol(nnode),Teml(nnode),mufluidl(nnode)
                      real(8)                 :: tauXl(nnode,ndime), tauYl(nnode,ndime), tauZl(nnode,ndime)
                      real(8)                 :: gradTl(nnode,ndime),gradRhol(nnode,ndime),tauUl(nnode,ndime)
                      real(8)                 :: gradIsoDe(ndime,ndime),gradIsoDr(ndime,ndime)

                      call nvtxStartRange("Full diffusion")
                      twoThirds = 2.0d0/3.0d0
                      !$acc kernels
                      Rmass(:) = 0.0d0
                      Rmom(:,:) = 0.0d0
                      Rener(:) = 0.0d0
                      !$acc end kernels

                      !$acc parallel loop gang  private(ul,Teml,rhol,mufluidl,gradRhol,gradTl,tauUl,tauXl,tauYl,tauZl)
                      do ielem = 1,nelem
                         !$acc loop vector
                         do inode = 1,nnode
                            rhol(inode) = rho(connec(ielem,inode))
                            Teml(inode) = Tem(connec(ielem,inode))
                            mufluidl(inode) = mu_fluid(connec(ielem,inode))
                         end do
                         !$acc loop vector collapse(2)
                         do inode = 1,nnode
                            do idime = 1,ndime
                               ul(inode,idime) = u(connec(ielem,inode),idime)
                            end do
                         end do
                         tauXl(:,:) = 0.0d0
                         tauYl(:,:) = 0.0d0
                         tauZl(:,:) = 0.0d0
                         gradTl(:,:) = 0.0d0
                         gradRhol(:,:) = 0.0d0
                         tauUl(:,:) = 0.0d0

                         !$acc loop vector private(tau,gradU,gradT,tauU,gradIsoRho,gradIsoT,gradIsoU,gradRho,divU)
                         do igaus = 1,ngaus
                            nu_e = c_rho*mu_e(ielem,igaus)/rhol(igaus)
                            mu_fgp = mufluidl(igaus)+rhol(igaus)*mu_sgs(ielem,igaus)
                            mu_egp = mu_e(ielem,igaus)
                            kappa_e =mufluidl(igaus)*Cp/Pr+c_ener*mu_e(ielem,igaus)/0.4d0 + rhol(igaus)*mu_sgs(ielem,igaus)/0.9d0

                            isoI = gmshAtoI(igaus) 
                            isoJ = gmshAtoJ(igaus) 
                            isoK = gmshAtoK(igaus) 

                            gradIsoRho(:) = 0.0d0
                            gradIsoT(:) = 0.0d0
                            gradIsoU(:,:) = 0.0d0
                            !$acc loop seq
                            do ii=1,porder+1
                               gradIsoRho(1) = gradIsoRho(1) + dlxigp_ip(igaus,1,ii)*rhol(invAtoIJK(ii,isoJ,isoK))
                               gradIsoRho(2) = gradIsoRho(2) + dlxigp_ip(igaus,2,ii)*rhol(invAtoIJK(isoI,ii,isoK))
                               gradIsoRho(3) = gradIsoRho(3) + dlxigp_ip(igaus,3,ii)*rhol(invAtoIJK(isoI,isoJ,ii))

                               gradIsoT(1) = gradIsoT(1) + dlxigp_ip(igaus,1,ii)*Teml(invAtoIJK(ii,isoJ,isoK))
                               gradIsoT(2) = gradIsoT(2) + dlxigp_ip(igaus,2,ii)*Teml(invAtoIJK(isoI,ii,isoK))
                               gradIsoT(3) = gradIsoT(3) + dlxigp_ip(igaus,3,ii)*Teml(invAtoIJK(isoI,isoJ,ii))

                               !$acc loop seq
                               do idime=1,ndime
                                  gradIsoU(idime,1) = gradIsoU(idime,1) + dlxigp_ip(igaus,1,ii)*ul(invAtoIJK(ii,isoJ,isoK),idime)
                                  gradIsoU(idime,2) = gradIsoU(idime,2) + dlxigp_ip(igaus,2,ii)*ul(invAtoIJK(isoI,ii,isoK),idime)
                                  gradIsoU(idime,3) = gradIsoU(idime,3) + dlxigp_ip(igaus,3,ii)*ul(invAtoIJK(isoI,isoJ,ii),idime)
                               end do
                            end do

                            gradRho(:) = 0.0d0
                            gradT(:) = 0.0d0
                            gradU(:,:) = 0.0d0
                            !$acc loop seq
                            do idime=1, ndime
                               !$acc loop seq
                               do jdime=1, ndime
                                  gradRho(idime) = gradRho(idime) + He(idime,jdime,igaus,ielem) * gradIsoRho(jdime)
                                  gradT(idime)   = gradT(idime)   + He(idime,jdime,igaus,ielem) * gradIsoT(jdime)
                                  !$acc loop seq
                                  do kdime=1,ndime
                                     gradU(idime,jdime) = gradU(idime,jdime) + He(jdime,kdime,igaus,ielem) * gradIsoU(idime,kdime)
                                  end do
                               end do
                            end do

                            divU = gradU(1,1)+gradU(2,2)+gradU(3,3)

                            tauU(:) = 0.0d0
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop seq
                               do jdime = 1,ndime
                                  tauU(idime) = tauU(idime) + &
                                     (mu_fgp+mu_egp)*(gradU(idime,jdime)+ gradU(jdime,idime))*ul(igaus,jdime)
                                  tau(idime,jdime) = (mu_fgp+mu_egp)*(gradU(idime,jdime)+gradU(jdime,idime))
                               end do
                               tauU(idime) = tauU(idime)-(mu_fgp)*twoThirds*divU*ul(igaus,idime)
                               tau(idime,idime) = tau(idime,idime)-(mu_fgp)*twoThirds*divU
                            end do

                            !$acc loop seq
                            do idime = 1,ndime
                               tauXl(igaus,idime) =  tau(1,idime)
                               tauYl(igaus,idime) =  tau(2,idime)
                               tauZl(igaus,idime) =  tau(3,idime)
                               gradTl(igaus,idime) =  gradT(idime)
                               gradRhol(igaus,idime) =  gradRho(idime)
                               tauUl(igaus,idime) =  tauU(idime)
                            end do
                         end do

                         !$acc loop vector private(gradIsoDr,gradIsoDe,gradIsoDm,divDm,divDr,divDe) 
                         do igaus = 1,ngaus
                            nu_e = c_rho*mu_e(ielem,igaus)/rhol(igaus)
                            mu_fgp = mufluidl(igaus)+rhol(igaus)*mu_sgs(ielem,igaus)
                            mu_egp = mu_e(ielem,igaus)
                            kappa_e =mufluidl(igaus)*Cp/Pr+c_ener*mu_e(ielem,igaus)/0.4d0 + rhol(igaus)*mu_sgs(ielem,igaus)/0.9d0

                            isoI = gmshAtoI(igaus) 
                            isoJ = gmshAtoJ(igaus) 
                            isoK = gmshAtoK(igaus) 

                            gradIsoDm(:,:,:) = 0.0d0
                            gradIsoDe(:,:) = 0.0d0
                            gradIsoDr(:,:) = 0.0d0
                            !$acc loop seq
                            do ii=1,porder+1
                               !$acc loop seq
                               do idime=1,ndime
                                  gradIsoDr(idime,1) = gradIsoDr(idime,1) + dlxigp_ip(igaus,1,ii)*gradRhol(invAtoIJK(ii,isoJ,isoK),idime)
                                  gradIsoDr(idime,2) = gradIsoDr(idime,2) + dlxigp_ip(igaus,2,ii)*gradRhol(invAtoIJK(isoI,ii,isoK),idime)
                                  gradIsoDr(idime,3) = gradIsoDr(idime,3) + dlxigp_ip(igaus,3,ii)*gradRhol(invAtoIJK(isoI,isoJ,ii),idime)

                                  gradIsoDe(idime,1) = gradIsoDe(idime,1) + dlxigp_ip(igaus,1,ii)*(tauUl(invAtoIJK(ii,isoJ,isoK),idime)&
                                                                                                  +kappa_e*gradTl(invAtoIJK(ii,isoJ,isoK),idime))
                                  gradIsoDe(idime,2) = gradIsoDe(idime,2) + dlxigp_ip(igaus,2,ii)*(tauUl(invAtoIJK(isoI,ii,isoK),idime)&
                                                                                                  +kappa_e*gradTl(invAtoIJK(isoI,ii,isoK),idime))
                                  gradIsoDe(idime,3) = gradIsoDe(idime,3) + dlxigp_ip(igaus,3,ii)*(tauUl(invAtoIJK(isoI,isoJ,ii),idime)&
                                                                                                  +kappa_e*gradTl(invAtoIJK(isoI,isoJ,ii),idime))
                                 
                                  gradIsoDm(1,idime,1) = gradIsoDm(1,idime,1) + dlxigp_ip(igaus,1,ii)*tauXl(invAtoIJK(ii,isoJ,isoK),idime)
                                  gradIsoDm(1,idime,2) = gradIsoDm(1,idime,2) + dlxigp_ip(igaus,2,ii)*tauXl(invAtoIJK(isoI,ii,isoK),idime)
                                  gradIsoDm(1,idime,3) = gradIsoDm(1,idime,3) + dlxigp_ip(igaus,3,ii)*tauXl(invAtoIJK(isoI,isoJ,ii),idime)

                                  gradIsoDm(2,idime,1) = gradIsoDm(2,idime,1) + dlxigp_ip(igaus,1,ii)*tauYl(invAtoIJK(ii,isoJ,isoK),idime)
                                  gradIsoDm(2,idime,2) = gradIsoDm(2,idime,2) + dlxigp_ip(igaus,2,ii)*tauYl(invAtoIJK(isoI,ii,isoK),idime)
                                  gradIsoDm(2,idime,3) = gradIsoDm(2,idime,3) + dlxigp_ip(igaus,3,ii)*tauYl(invAtoIJK(isoI,isoJ,ii),idime)

                                  gradIsoDm(3,idime,1) = gradIsoDm(3,idime,1) + dlxigp_ip(igaus,1,ii)*tauZl(invAtoIJK(ii,isoJ,isoK),idime)
                                  gradIsoDm(3,idime,2) = gradIsoDm(3,idime,2) + dlxigp_ip(igaus,2,ii)*tauZl(invAtoIJK(isoI,ii,isoK),idime)
                                  gradIsoDm(3,idime,3) = gradIsoDm(3,idime,3) + dlxigp_ip(igaus,3,ii)*tauZl(invAtoIJK(isoI,isoJ,ii),idime)
                               end do
                            end do

                            divDe = 0.0d0
                            divDr = 0.0d0
                            !$acc loop seq
                            do idime=1, ndime
                               divDm(idime) = 0.0d0
                               !$acc loop seq
                               do jdime=1, ndime
                                  divDr = divDr + He(idime,jdime,igaus,ielem) * gradIsoDr(idime,jdime)
                                  divDe = divDe + He(idime,jdime,igaus,ielem) * gradIsoDe(idime,jdime)
                                  !$acc loop seq
                                  do kdime=1,ndime
                                     divDm(idime) = divDm(idime) + He(jdime,kdime,igaus,ielem)*gradIsoDm(idime,jdime,kdime)
                                  end do
                               end do
                            end do
                            !$acc atomic update
                            Rmass(connec(ielem,igaus)) = Rmass(connec(ielem,igaus))+nu_e*gpvol(1,igaus,ielem)*divDr
                            !$acc end atomic
                            !$acc atomic update
                            Rener(connec(ielem,igaus)) = Rener(connec(ielem,igaus))+gpvol(1,igaus,ielem)*divDe
                            !$acc end atomic
                            do idime = 1,ndime
                               !$acc atomic update
                               Rmom(connec(ielem,igaus),idime) = Rmom(connec(ielem,igaus),idime)+gpvol(1,igaus,ielem)*divDm(idime)
                               !$acc end atomic
                            end do
                         end do
                      end do
                      !$acc end parallel loop
                      call nvtxEndRange
              end subroutine full_diffusion_ijk

end module elem_diffu

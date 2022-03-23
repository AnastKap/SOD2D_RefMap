module elem_convec

      use mod_nvtx
      use mod_constants

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Computes convective term for Euler/NS equation system, as well             !
      ! as for any generic scalar transport that might occur. Based                !
      ! on Ljunkvist matrix-free implementation (assembles only rhs vector).       !
      ! This module can be passed to CUDA in order to do fine-grained parallelism. !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      contains

              subroutine mass_convec(nelem,npoin,connec,Ngp,dNgp,He,gpvol,q,Rmass)

                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                      ! Subroutine to compute R = div(rho*u) using a standard Continuous  !
                      ! Galerkin formulation. In the above, rho is the scalar sensity and !
                      ! u is the vector of velocities in all dimensions.                  !
                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                      implicit none

                      integer(4), intent(in)  :: nelem, npoin
                      integer(4), intent(in)  :: connec(nelem,nnode)
                      real(8),    intent(in)  :: Ngp(ngaus,nnode), dNgp(ndime,nnode,ngaus)
                      real(8),    intent(in)  :: He(ndime,ndime,ngaus,nelem)
                      real(8),    intent(in)  :: gpvol(1,ngaus,nelem)
                      real(8),    intent(in)  :: q(npoin,ndime)
                      real(8),    intent(out) :: Rmass(npoin)
                      integer(4)              :: ielem, igaus, inode, idime, jdime
                      real(8)                 :: Re(nnode)
                      real(8)                 :: tmp1, gpcar(ndime,nnode)

                      call nvtxStartRange("Mass Convection")
                      !$acc kernels
                      Rmass(:) = 0.0d0
                      !$acc end kernels
                      !$acc parallel loop gang private(Re,gpcar) vector_length(32)
                      do ielem = 1,nelem
                         !$acc loop vector
                         do inode = 1,nnode
                            Re(inode) = 0.0d0
                         end do
                         !
                         ! Quadrature
                         !
                         !$acc loop seq
                         do igaus = 1,ngaus
                            tmp1 = 0.0d0
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop vector
                               do inode = 1,nnode
                                  gpcar(idime,inode) = dot_product(He(idime,:,igaus,ielem),dNgp(:,inode,igaus))
                               end do
                            end do
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop vector reduction(+:tmp1)
                               do inode = 1,nnode
                                  tmp1 = tmp1+(gpcar(idime,inode)*q(connec(ielem,inode),idime))
                               end do
                            end do
                            !$acc loop vector
                            do inode = 1,nnode
                               Re(inode) = Re(inode)+gpvol(1,igaus,ielem)* &
                                           Ngp(igaus,inode)*tmp1
                            end do
                         end do
                         !
                         ! Assembly
                         !
                         !$acc loop vector
                         do inode = 1,nnode
                            !$acc atomic update
                            Rmass(connec(ielem,inode)) = Rmass(connec(ielem,inode))+Re(inode)
                            !$acc end atomic
                         end do
                      end do
                      !$acc end parallel loop
                      call nvtxEndRange

              end subroutine mass_convec

              subroutine mom_convec(nelem,npoin,connec,Ngp,dNgp,He,gpvol,u,q,pr,Rmom)

                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                      ! Subroutine to compute R = div(q*u) using a standard Continuous   !
                      ! Galerkin formulation. In the above, q is the momentum vector and !
                      ! u is the vector of velocities in all dimensions. The product     !
                      ! inside div() is an outer tensor product between the 2 vectors.   !
                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                      implicit none

                      integer(4), intent(in)  :: nelem, npoin
                      integer(4), intent(in)  :: connec(nelem,nnode)
                      real(8),    intent(in)  :: Ngp(ngaus,nnode), dNgp(ndime,nnode,ngaus)
                      real(8),    intent(in)  :: He(ndime,ndime,ngaus,nelem)
                      real(8),    intent(in)  :: gpvol(1,ngaus,nelem)
                      real(8),    intent(in)  :: q(npoin,ndime), u(npoin,ndime), pr(npoin)
                      real(8),    intent(out) :: Rmom(npoin,ndime)
                      integer(4)              :: ielem, igaus, idime, jdime, inode
                      real(8)                 :: Re(nnode,ndime), aux, divU,gradQ(ndime, ndime)
                      real(8)                 :: tmp1(ndime), tmp2(ndime), gpcar(ndime,nnode)

                      call nvtxStartRange("Momentum convection")
                      !$acc kernels
                      Rmom(:,:) = 0.0d0
                      !$acc end kernels
                      !$acc parallel loop gang private(Re,gpcar,tmp1,tmp2,gradQ,divU) vector_length(32)
                      do ielem = 1,nelem
                         !$acc loop vector collapse(2)
                         do inode = 1,nnode
                            do idime = 1,ndime
                               Re(inode,idime) = 0.0d0
                            end do
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
                               aux = 0.0d0
                               !$acc loop seq
                               do jdime = 1,ndime
                                  !$acc loop vector reduction(+:aux)
                                  do inode = 1,nnode
                                     aux = aux+gpcar(jdime,inode)*(q(connec(ielem,inode),idime)*u(connec(ielem,inode),jdime))
                                  end do
                               end do
                               tmp1(idime) = aux
                            end do
                            !$acc loop seq
                            do idime = 1,ndime
                               aux = 0.0d0
                               !$acc loop vector reduction(+:aux)
                               do inode = 1,nnode
                                  aux = aux+gpcar(idime,inode)*pr(connec(ielem,inode))
                               end do
                               tmp2(idime) = aux
                            end do
                            !$acc loop vector
                            do inode = 1,nnode
                               Re(inode,1) = Re(inode,1)+gpvol(1,igaus,ielem)*Ngp(igaus,inode)*(tmp1(1)+tmp2(1))
                               Re(inode,2) = Re(inode,2)+gpvol(1,igaus,ielem)*Ngp(igaus,inode)*(tmp1(2)+tmp2(2))
                               Re(inode,3) = Re(inode,3)+gpvol(1,igaus,ielem)*Ngp(igaus,inode)*(tmp1(3)+tmp2(3))
                            end do
                         end do
                         !
                         ! Final assembly
                         !
                         !$acc loop vector collapse(2)
                         do idime = 1,ndime
                            do inode = 1,nnode
                              !$acc atomic update
                              Rmom(connec(ielem,inode),idime) = Rmom(connec(ielem,inode),idime)+Re(inode,idime)
                              !$acc end atomic
                            end do
                         end do
                      end do
                      !$acc end parallel loop
                      call nvtxEndRange

              end subroutine mom_convec

              subroutine mom_convec_emac(nelem,npoin,connec,Ngp,dNgp,He,gpvol,Aemac,Femac,pr,Rmom)

                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                      ! Subroutine to compute R = EMAC(A) using a standard Continuous    !
                      ! Galerkin model. A_i is q_i/sqrt(rho), and the EMAC term is       !
                      ! defined as:                                                      !
                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                      implicit none

                      integer(4), intent(in)  :: nelem,npoin
                      integer(4), intent(in)  :: connec(nelem,nnode)
                      real(8),    intent(in)  :: Ngp(ngaus,nnode), dNgp(ndime,nnode,ngaus)
                      real(8),    intent(in)  :: He(ndime,ndime,ngaus,nelem)
                      real(8),    intent(in)  :: gpvol(1,ngaus,nelem)
                      real(8),    intent(in)  :: Aemac(npoin,ndime), Femac(npoin), pr(npoin)
                      real(8),    intent(out) :: Rmom(npoin,ndime)
                      integer(4)              :: ielem, igaus, idime, jdime, inode,jnode, kdime
                      real(8)                 :: Re(nnode,ndime), tmp(ndime)
                      real(8)                 :: gpcar(ndime,nnode), gradA(ndime,ndime), divA, aux

                      call nvtxStartRange("EMAC Momentum convection")

                      !
                      ! Start global RHS
                      !
                      !$acc kernels
                      Rmom(:,:) = 0.0d0
                      !$acc end kernels
                      
                      !
                      ! Start elemental ops
                      !
                      !$acc parallel loop gang private(Re,gpcar,gradA,tmp) vector_length(32)
                      do ielem = 1,nelem
                         !
                         ! Initialize element vector to 0
                         !
                         !$acc loop vector collapse(2)
                         do idime = 1,ndime
                            do inode = 1,nnode
                               Re(inode,idime) = 0.0d0
                            end do
                         end do
                         !
                         ! Loop over Gauss points
                         !
                         !$acc loop seq
                         do igaus = 1,ngaus
                            !
                            ! Create GPCAR(ndime,nnode) for each element at each Gauss point
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop vector
                               do inode = 1,nnode
                                  gpcar(idime,inode) = dot_product(He(idime,:,igaus,ielem),dNgp(:,inode,igaus))
                               end do
                            end do
                            !
                            ! grad(A)
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               !$acc loop seq
                               do jdime = 1,ndime
                                  aux = 0.0d0
                                  !$acc loop vector reduction(+:aux)
                                  do inode = 1,nnode
                                     aux = aux+gpcar(jdime,inode)*Aemac(connec(ielem,inode),idime)
                                  end do
                                  gradA(idime,jdime) = aux
                               end do
                            end do
                            !
                            ! div(A)
                            !
                            divA = 0.0d0
                            !$acc loop vector collapse(2) reduction(+:divA)
                            do idime = 1,ndime
                               do inode = 1,nnode
                                  divA = divA+gpcar(idime,inode)*Aemac(connec(ielem,inode),idime)
                               end do
                            end do
                            !
                            ! grad(A)*A + gradT(A)*A
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               tmp(idime) = 0.0d0
                               !$acc loop seq
                               do jdime = 1,ndime
                                  aux = 0.0d0
                                  !$acc loop vector reduction(+:aux)
                                  do inode = 1,nnode
                                     aux = aux+Ngp(igaus,inode)*Aemac(connec(ielem,inode),jdime)
                                  end do
                                  tmp(idime) = gradA(idime,jdime)*aux + gradA(jdime,idime)*aux
                               end do
                            end do
                            !
                            ! Add div(A)*A
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               aux = 0.0d0
                               !$acc loop vector reduction(+:aux)
                               do inode = 1,nnode
                                  aux = aux+Ngp(igaus,inode)*Aemac(connec(ielem,inode),idime)
                               end do
                               tmp(idime) = tmp(idime)+divA*aux
                            end do
                            !
                            ! Subtract -0.5*grad(F), where F = (A.A)
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               aux = 0.0d0
                               !$acc loop vector reduction(+:aux)
                               do inode = 1,nnode
                                  aux = aux+gpcar(idime,inode)*Femac(connec(ielem,inode))
                               end do
                               tmp(idime) = tmp(idime)-0.5d0*aux
                            end do
                            !
                            ! Addd pressure
                            !
                            !$acc loop seq
                            do idime = 1,ndime
                               aux = 0.0d0
                               !$acc loop vector reduction(+:aux)
                               do inode = 1,nnode
                                  aux = aux+gpcar(idime,inode)*pr(connec(ielem,inode))
                               end do
                               tmp(idime) = tmp(idime)+aux
                            end do
                            !$acc loop vector
                            do inode = 1,nnode
                               !aux = 0.0d0
                               !!$acc loop vector reduction(+:aux)
                               !do jnode = 1,nnode
                               !   aux = aux+Ngp(igaus,jnode)*pr(connec(ielem,jnode))
                               !end do
                               Re(inode,1) = Re(inode,1)+gpvol(1,igaus,ielem)*(Ngp(igaus,inode)*tmp(1))
                               Re(inode,2) = Re(inode,2)+gpvol(1,igaus,ielem)*(Ngp(igaus,inode)*tmp(2))
                               Re(inode,3) = Re(inode,3)+gpvol(1,igaus,ielem)*(Ngp(igaus,inode)*tmp(3))
                               !Re(inode,1) = Re(inode,1)+gpvol(1,igaus,ielem)*(Ngp(igaus,inode)*tmp(1)-aux*gpcar(1,inode))
                               !Re(inode,2) = Re(inode,2)+gpvol(1,igaus,ielem)*(Ngp(igaus,inode)*tmp(2)-aux*gpcar(2,inode))
                               !Re(inode,3) = Re(inode,3)+gpvol(1,igaus,ielem)*(Ngp(igaus,inode)*tmp(3)-aux*gpcar(3,inode))
                            end do
                         end do
                         !
                         ! Final assembly
                         !
                         !$acc loop vector collapse(2)
                         do idime = 1,ndime
                            do inode = 1,nnode
                              !$acc atomic update
                              Rmom(connec(ielem,inode),idime) = Rmom(connec(ielem,inode),idime)+Re(inode,idime)
                              !$acc end atomic
                            end do
                         end do
                      end do
                      !$acc end parallel loop
                      call nvtxEndRange

              end subroutine mom_convec_emac

              subroutine ener_convec(nelem,npoin,connec,Ngp,dNgp,He,gpvol,u,pr,E,Rener)

                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                      ! Subroutine to compute R = div(u*(E+p)) using a standard Continuous !
                      ! Galerkin formulation. In the above, E is the scalar total energy,  !
                      ! p is the scalar pressure and u is the vector of velocities in all  !
                      ! dimensions.                                                        !
                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                      implicit none

                      integer(4), intent(in)  :: nelem, npoin
                      integer(4), intent(in)  :: connec(nelem,nnode)
                      real(8),    intent(in)  :: Ngp(ngaus,nnode), dNgp(ndime,nnode,ngaus)
                      real(8),    intent(in)  :: He(ndime,ndime,ngaus,nelem)
                      real(8),    intent(in)  :: gpvol(1,ngaus,nelem)
                      real(8),    intent(in)  :: u(npoin,ndime), pr(npoin), E(npoin)
                      real(8),    intent(out) :: Rener(npoin)
                      integer(4)              :: ielem, igaus, inode, idime, ipoin, jdime
                      real(8)                 :: Re(nnode), aux
                      real(8)                 :: tmp1, gpcar(ndime,nnode)

                      call nvtxStartRange("Energy Convection")
                      !$acc kernels
                      Rener(:) = 0.0d0
                      !$acc end kernels
                      !$acc parallel loop gang private(Re,gpcar) vector_length(32)
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
                            tmp1 = 0.0d0
                            !$acc loop vector collapse(2) reduction(+:tmp1)
                            do idime = 1,ndime
                               do inode = 1,nnode
                                  tmp1 = tmp1+(gpcar(idime,inode)* &
                                     (u(connec(ielem,inode),idime)*(E(connec(ielem,inode))+pr(connec(ielem,inode)))))
                               end do
                            end do
                            !$acc loop vector
                            do inode = 1,nnode
                               Re(inode) = Re(inode)+gpvol(1,igaus,ielem)*Ngp(igaus,inode)*tmp1
                            end do
                         end do
                         !
                         ! Assembly
                         !
                         !$acc loop vector
                         do inode = 1,nnode
                            !$acc atomic update
                            Rener(connec(ielem,inode)) = Rener(connec(ielem,inode))+Re(inode)
                            !$acc end atomic
                         end do
                      end do
                      !$acc end parallel loop
                      call nvtxEndRange

              end subroutine ener_convec

              subroutine generic_scalar_convec(nelem,npoin,connec,Ngp, &
                                               dNgp,He,gpvol,q,Rconvec,alpha)

                      implicit none

                      integer(4), intent(in)  :: nelem, npoin
                      integer(4), intent(in)  :: connec(nelem,nnode)
                      real(8),    intent(in)  :: Ngp(ngaus,nnode), dNgp(ndime,nnode,ngaus)
                      real(8),    intent(in)  :: He(ndime,ndime,ngaus,nelem)
                      real(8),    intent(in)  :: gpvol(1,ngaus,nelem)
                      real(8),    intent(in)  :: q(npoin,ndime)
                      real(8),    intent(in)  :: alpha(npoin)
                      real(8),    intent(out) :: Rconvec(npoin)
                      integer(4)              :: ielem, igaus, inode, idime, jdime
                      real(8)                 :: tmp1, tmp2, Re(nnode), gpcar(ndime,nnode)

                      call nvtxStartRange("Generic Convection")
                      !$acc kernels
                      Rconvec(:) = 0.0d0
                      !$acc end kernels
                      !$acc parallel loop gang private(Re,gpcar) vector_length(32)
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
                            tmp1 = 0.0d0
                            !$acc loop vector reduction(+:tmp1)
                            do inode = 1,nnode
                               tmp1 = tmp1+(Ngp(igaus,inode)*alpha(connec(ielem,inode)))
                            end do
                            tmp2 = 0.0d0
                            !$acc loop vector collapse(2) reduction(+:tmp2)
                            do idime = 1,ndime
                               do inode = 1,nnode
                                  tmp2 = tmp2+(gpcar(idime,inode)*q(connec(ielem,inode),idime))
                               end do
                            end do
                            !$acc loop vector
                            do inode = 1,nnode
                               Re(inode) = Re(inode)+gpvol(1,igaus,ielem)* &
                                           Ngp(igaus,inode)*tmp1*tmp2
                            end do
                         end do
                         !
                         ! Assembly
                         !
                         !$acc loop vector
                         do inode = 1,nnode
                            !$acc atomic update
                            Rconvec(connec(ielem,inode)) = Rconvec(connec(ielem,inode))+Re(inode)
                            !$acc end atomic
                         end do
                      end do
                      !$acc end parallel loop
                      call nvtxEndRange

              end subroutine generic_scalar_convec

end module elem_convec

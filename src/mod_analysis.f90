module mod_analysis

   use mod_constants
   use mod_veclen
   use mod_nvtx

   contains

      subroutine volAvg_EK(nelem,npoin,connec,gpvol,Ngp,rho0,rho,u,EK)

         implicit none

         integer(4), intent(in)  :: nelem, npoin, connec(nelem,nnode)
         real(rp),    intent(in)  :: gpvol(1,ngaus,nelem), Ngp(ngaus,nnode)
         real(rp),    intent(in)  :: rho0, rho(npoin), u(npoin,ndime)
         real(rp),    intent(out) :: EK
         integer(4)              :: ielem, igaus, inode
         real(rp)                 :: R1

         call nvtxStartRange("Compute EK")
         EK = 0.0_rp
         !$acc parallel loop gang reduction(+:EK) vector_length(vecLength)
         do ielem = 1,nelem
            R1 = 0.0_rp
            !$acc loop vector collapse(2) reduction(+:R1)
            do igaus = 1,ngaus
               do inode = 1,nnode
                  R1 = R1 + gpvol(1,igaus,ielem)*Ngp(igaus,inode)*0.5_rp*rho(connec(ielem,inode))* &
                     dot_product(u(connec(ielem,inode),:),u(connec(ielem,inode),:))
               end do
            end do
            EK = EK+R1
         end do
         !$acc end parallel loop
         EK = EK/(rho0*((2.0_rp*3.14159_rp)**3.0_rp))
         call nvtxEndRange

      end subroutine volAvg_EK

      subroutine visc_dissipationRate(nelem,npoin,connec,leviCivi,rho0,mu_fluid,mu_e,u,volT,gpvol,He,dNgp,eps_S,eps_D,eps_T)

         implicit none

         integer(4), intent(in)  :: nelem, npoin, connec(nelem,nnode)
         real(rp),    intent(in)  :: leviCivi(3,3,3), rho0, mu_fluid(npoin), u(npoin,ndime), volT
         real(rp),    intent(in)  :: gpvol(1,ngaus,nelem), He(ndime,ndime,ngaus,nelem), dNgp(ndime,ngaus,nelem),mu_e(nelem,ngaus)
         real(rp),    intent(out) :: eps_S, eps_D, eps_T
         integer(4)              :: ielem, igaus, inode, idime, jdime, kdime
         real(rp)                 :: R1, R2, div2U, curl2U, alpha, aux,aux2
         real(rp)                 :: gpcar(ndime,nnode), gradU(ndime,ndime)

         if (flag_spectralElem .ne. 1) then
            write(1,*) "--| THIS ONLY WORKS WITH SPECTRAL ELEMENTS FOR NOW!"
            error stop
         end if

         eps_S = 0.0_rp
         eps_D = 0.0_rp
         eps_T = 0.0_rp

         call nvtxStartRange("Compute visc_dissipationRate")
         !$acc parallel loop gang private(gradU,gpcar) reduction(+:eps_S,eps_D) vector_length(vecLength)
         do ielem = 1,nelem
            R1 = 0.0_rp
            R2 = 0.0_rp
            !$acc loop seq
            do igaus = 1,ngaus
               !
               ! Compute gpcar
               !
               !$acc loop vector collapse(2)
               do idime = 1,ndime
                  do inode = 1,nnode
                     gpcar(idime,inode) = dot_product(He(idime,:,igaus,ielem),dNgp(:,inode,igaus))
                  end do
               end do
               !
               ! Compute gradient of u
               !
               !$acc loop seq
               do jdime = 1,ndime
                  !$acc loop seq
                  do kdime = 1,ndime
                     aux = 0.0_rp
                     !$acc loop vector reduction(+:aux)
                     do inode = 1,nnode
                        aux = aux + gpcar(jdime,inode)*u(connec(ielem,inode),kdime)
                     end do
                     gradU(jdime,kdime) = aux
                  end do
               end do
               !
               ! Compute divergence of u
               !
               div2U = gradU(1,1)+gradU(2,2)+gradU(3,3)
               !
               ! Squared divergence of u
               !
               div2U = div2U**2
               !
               !
               ! Compute dot vproduct of curl u and curl u
               !
               curl2U = 0.0_rp
               !!$acc loop vector collapse(3) reduction(+:curl2U)
               !$acc loop seq
               do idime = 1,ndime
                  aux2= 0.0_rp
                  !$acc loop seq
                  do jdime = 1,ndime
                     !$acc loop seq
                     do kdime = 1,ndime
                        aux2 = aux2+(leviCivi(idime,jdime,kdime)*gradU(jdime,kdime))
                     end do
                  end do
                  curl2U = curl2U + aux2*aux2
               end do
               !
               ! Compute enstrophy
               !
               R1 = R1+gpvol(1,igaus,ielem)*(mu_fluid(connec(ielem,igaus))+mu_e(ielem,igaus))*curl2U
               !
               ! Compute dilational component
               !
               R2 = R2+gpvol(1,igaus,ielem)*mu_fluid(connec(ielem,igaus))*div2U
            end do
            eps_S = eps_S+R1
            eps_D = eps_D+R2
         end do
         !!$acc end parallel loop
         call nvtxEndRange

         alpha = 1.0_rp/(rho0*volT)
         eps_S = eps_S*alpha
         eps_D = (4.0_rp/3.0_rp)*eps_D*alpha
         eps_T = eps_S+eps_D

      end subroutine visc_dissipationRate

      subroutine maxMach(npoin,npoin_w,lpoin_w,machno,maxmachno)

         implicit none

         integer(4), intent(in)  :: npoin
         integer(4), intent(in)  :: npoin_w
         integer(4), intent(in)  :: lpoin_w(npoin)
         real(rp),    intent(in)  :: machno(npoin)
         real(rp),    intent(out) :: maxmachno
         integer(4)              :: ipoin

         maxmachno = 0.0_rp
         !$acc parallel loop reduction(max:maxmachno)
         do ipoin = 1,npoin_w
            maxmachno = max(maxmachno,machno(lpoin_w(ipoin)))
         end do
         !$acc end parallel loop

      end subroutine maxMach

      subroutine write_EK(time,EK,eps_S,eps_D,eps_T,maxmachno)

         implicit none

         real(rp), intent(in) :: time, EK, eps_S, eps_D, eps_T, maxmachno

         write(666,10) time, EK, eps_S, eps_D, eps_T, maxmachno
10       format(6(F12.8,2X))

      end subroutine write_EK

end module mod_analysis

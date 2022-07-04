module mod_aver

	use mod_constants

	contains

	subroutine  favre_average(nelem,npoin,npoin_w,lpoin_w,connec,dt,rho,u,pr, &
									  mu_fluid,mu_e,mu_sgs,acutim,acurho,acupre,acuvel,acuve2,acumueff)

		implicit none

		integer(4), intent(in)                             :: nelem, npoin, npoin_w, lpoin_w(npoin_w), connec(nelem,nnode)
		real(rp),    intent(in)                             :: dt
		real(rp),    intent(in),    dimension(npoin)        :: rho, pr, mu_fluid
		real(rp),    intent(in),    dimension(npoin,ndime)  :: u
		real(rp),    intent(in),    dimension(nelem,ngaus)  :: mu_e, mu_sgs
		real(rp),    intent(inout)                          :: acutim
		real(rp),    intent(inout), dimension(npoin)        :: acurho, acupre, acumueff
		real(rp),    intent(inout), dimension(npoin,ndime)  :: acuvel, acuve2
		integer(4)                                         :: ipoin, idime, ielem, inode
		real(rp)                                            :: envit(npoin), mut(npoin)

		! Compute accumulated time
		acutim = acutim+dt
		! Compute accumulated tally for density times current dt and other variables times density times current dt
		!$acc parallel loop
		do ipoin = 1,npoin_w
			!$acc atomic update
			acurho(lpoin_w(ipoin)) = acurho(lpoin_w(ipoin))+rho(lpoin_w(ipoin))*dt
			!$acc end atomic
		end do
		!$acc end parallel loop
		!$acc parallel loop
		do ipoin = 1,npoin_w
			!$acc atomic update
			acupre(lpoin_w(ipoin)) = acupre(lpoin_w(ipoin))+pr(lpoin_w(ipoin))*dt
			!$acc end atomic
		end do
		!$acc end parallel loop
		!$acc parallel loop collapse(2)
		do ipoin = 1,npoin_w
			do idime = 1,ndime
				!$acc atomic update
				acuvel(lpoin_w(ipoin),idime) = acuvel(lpoin_w(ipoin),idime)+rho(lpoin_w(ipoin))*u(lpoin_w(ipoin),idime)*dt
				!$acc end atomic
			end do
		end do
		!$acc end parallel loop
		!$acc parallel loop collapse(2)
		do ipoin = 1,npoin_w
			do idime = 1,ndime
				!$acc atomic update
				acuve2(lpoin_w(ipoin),idime) = acuve2(lpoin_w(ipoin),idime)+ &
					rho(lpoin_w(ipoin))*u(lpoin_w(ipoin),idime)*u(lpoin_w(ipoin),idime)*dt
				!$acc end atomic
			end do
		end do
		!$acc end parallel loop

		! Compute accumulated tally for effective viscosity times current dt
		!$acc parallel loop collapse(2)
		do ielem = 1,nelem
         do inode = 1, nnode
            envit(connec(ielem,inode)) =  mu_e(ielem,inode)
            mut(connec(ielem,inode))   =  mu_sgs(ielem,inode)
         end do
      end do
		!$acc end parallel loop
		!$acc parallel loop
		do ipoin = 1,npoin_w
			!$acc atomic update
			acumueff(lpoin_w(ipoin)) = acumueff(lpoin_w(ipoin))+ &
				(mu_fluid(lpoin_w(ipoin))+envit(lpoin_w(ipoin))+mut(lpoin_w(ipoin)))*dt
			!$acc end atomic
		end do
		!$acc end parallel loop

	end subroutine favre_average

end module mod_aver
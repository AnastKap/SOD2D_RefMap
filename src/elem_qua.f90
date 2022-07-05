module elem_qua

        use mod_constants
        use mod_maths

        contains

                subroutine qua04(s,t,N,dN) ! QUA04 element

                        implicit none

                        real(rp), intent(in)  :: s, t
                        real(rp), intent(out) :: N(4), dN(2,4)

                        N(1) = (1.0_rp-s)*(1.0_rp-t)
                        N(2) = (1.0_rp+s)*(1.0_rp-t)
                        N(3) = (1.0_rp+s)*(1.0_rp+t)
                        N(4) = (1.0_rp-s)*(1.0_rp+t)
                        N = 0.25_rp*N

                        dN(1,1) = -1.0_rp+t
                        dN(2,1) = -1.0_rp+s
                        dN(1,2) =  1.0_rp-t
                        dN(2,2) = -1.0_rp-s
                        dN(1,3) =  1.0_rp+t
                        dN(2,3) =  1.0_rp+s
                        dN(1,4) = -1.0_rp-t
                        dN(2,4) =  1.0_rp-s
                        dN = 0.25_rp*dN

                end subroutine qua04

                subroutine qua09(s,t,N,dN) ! QUA09 element

                        ! TODO: IMPLEMENT PROPERLY!!!!!

                        implicit none

                        real(rp), intent(in)  :: s, t
                        real(rp), intent(out) :: N(9), dN(2,9)

                        N(1) = (1.0_rp-s)*(1.0_rp-t)
                        N(2) = (1.0_rp+s)*(1.0_rp-t)
                        N(3) = (1.0_rp+s)*(1.0_rp+t)
                        N(4) = (1.0_rp-s)*(1.0_rp+t)
                        N = 0.25_rp*N

                        dN(1,1) = -1.0_rp+t
                        dN(2,1) = -1.0_rp+s
                        dN(1,2) =  1.0_rp-t
                        dN(2,2) = -1.0_rp-s
                        dN(1,3) =  1.0_rp+t
                        dN(2,3) =  1.0_rp+s
                        dN(1,4) = -1.0_rp-t
                        dN(2,4) =  1.0_rp-s
                        dN = 0.25_rp*dN

                end subroutine qua09

		!> @brief Computes the shape functions and their derivatives for QUA_16
		!> @param[in] xi The isoparametric coordinate in xi-direction
		!> @param[in] eta The isoparametric coordinate in eta-direction
		!> @param[out] N The shape functions and their derivatives
		!> @param[out] atoIJ Node a to IJ relationship
		subroutine qua16(xi,eta,atoIJ,N,dN) ! QUA16 element
			implicit none
			integer(4),           intent(out) :: atoIJ(16)
			real(rp),              intent(in)  :: xi, eta
			real(rp),    optional, intent(out) :: N(16), dN(2,16)
			real(rp)                           :: xi_grid(porder+1)
			atoIJ = [1,4,12,11,2,3,7,8,5,10,13,16,6,9,14,15]
			if (present(N) .and. present(dN)) then
				call chebyshev_roots(xi_grid)
				call DoubleTensorProduct(xi_grid,xi,eta,atoIJ,N,dN)
			end if
		end subroutine qua16

                subroutine quad_edges(ielem,nelem,npoin,connec,coord,ncorner,nedge,dist)

                        implicit none

                        integer(4), intent(in)            :: ielem, nelem, npoin
                        integer(4), intent(in)            :: connec(nelem,nnode)
                        real(rp),    intent(in)            :: coord(npoin,ndime)
                        integer(4), intent(out)           :: ncorner, nedge
                        real(rp),    intent(out)           :: dist(4,ndime)
                        integer(4)                        :: ind(nnode)
                        real(rp)                           :: xp(4,ndime)

                        ind = connec(ielem,:)
                        ncorner = 4
                        nedge = 4

                        xp(1:4,1:ndime) = coord(ind(1:4),1:ndime) ! Corner coordinates
                        dist(1,:) = xp(2,:)-xp(1,:)
                        dist(2,:) = xp(3,:)-xp(2,:)
                        dist(3,:) = xp(4,:)-xp(3,:)
                        dist(4,:) = xp(1,:)-xp(4,:)

                end subroutine quad_edges

end module

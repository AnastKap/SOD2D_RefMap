module quadrature_rules

        use mod_constants
        use mod_maths

        contains

                subroutine gll_qua(xgp,wgp)

                        implicit none

                        real(rp),    intent(out) :: xgp(ngaus,ndime), wgp(ngaus)
                        real(rp)                 :: q

                        if (ngaus == 1) then
                           xgp(1,1) = 0.0_rp
                           xgp(1,2) = 0.0_rp
                           wgp(1) = 4.0_rp
                        else if (ngaus == 4) then
                           q = 1.0_rp/sqrt(3.0_rp)
                           xgp(1,1) = -q
                           xgp(1,2) = -q
                           xgp(2,1) =  q
                           xgp(2,2) = -q
                           xgp(3,1) =  q
                           xgp(3,2) =  q
                           xgp(4,1) = -q
                           xgp(4,2) =  q
                           wgp(1) = 1.0_rp
                           wgp(2) = 1.0_rp
                           wgp(3) = 1.0_rp
                           wgp(4) = 1.0_rp
                        else if (ngaus == 9) then
                           write(*,*) 'NOT CODED YET!'
                           xgp = 0.0_rp
                           wgp = 0.0_rp
                        end if

                end subroutine gll_qua

                subroutine gll_hex(xgp,wgp)

                        implicit none

                        real(rp),    intent(out) :: xgp(ngaus,ndime), wgp(ngaus)
                        real(rp)                 :: q, sl, s0, sr, ws, w0

                        if (ngaus == 1) then
                           xgp(1,1) = 0.0_rp
                           xgp(1,2) = 0.0_rp
                           xgp(1,3) = 0.0_rp
                           wgp(1) = 4.0_rp
                        else if (ngaus == 8) then
                           q = 1.0_rp/sqrt(3.0_rp)
                           xgp(1,1) = -q ! xi
                           xgp(1,2) = -q ! eta
                           xgp(1,3) = -q ! zeta
                           xgp(2,1) = -q
                           xgp(2,2) = -q
                           xgp(2,3) =  q
                           xgp(3,1) =  q
                           xgp(3,2) = -q
                           xgp(3,3) =  q
                           xgp(4,1) =  q
                           xgp(4,2) = -q
                           xgp(4,3) = -q
                           xgp(5,1) = -q ! xi
                           xgp(5,2) =  q ! eta
                           xgp(5,3) = -q ! zeta
                           xgp(6,1) = -q
                           xgp(6,2) =  q
                           xgp(6,3) =  q
                           xgp(7,1) =  q
                           xgp(7,2) =  q
                           xgp(7,3) =  q
                           xgp(8,1) =  q
                           xgp(8,2) =  q
                           xgp(8,3) = -q
                           wgp(1) = 1.0_rp
                           wgp(2) = 1.0_rp
                           wgp(3) = 1.0_rp
                           wgp(4) = 1.0_rp
                           wgp(5) = 1.0_rp
                           wgp(6) = 1.0_rp
                           wgp(7) = 1.0_rp
                           wgp(8) = 1.0_rp
                        else if (ngaus == 27) then
                           sl = -sqrt(0.6_rp)
                           s0 = 0.0_rp
                           sr = sqrt(0.6_rp)
                           xgp(1,1:3) = [sl,sl,sl]
                           xgp(2,1:3) = [sl,sl,s0]
                           xgp(3,1:3) = [sl,sl,sr]
                           xgp(4,1:3) = [s0,sl,sl]
                           xgp(5,1:3) = [s0,sl,s0]
                           xgp(6,1:3) = [s0,sl,sr]
                           xgp(7,1:3) = [sr,sl,sl]
                           xgp(8,1:3) = [sr,sl,s0]
                           xgp(9,1:3) = [sr,sl,sr]
                           xgp(10,1:3) = [sl,s0,sl]
                           xgp(11,1:3) = [sl,s0,s0]
                           xgp(12,1:3) = [sl,s0,sr]
                           xgp(13,1:3) = [s0,s0,sl]
                           xgp(14,1:3) = [s0,s0,s0]
                           xgp(15,1:3) = [s0,s0,sr]
                           xgp(16,1:3) = [sr,s0,sl]
                           xgp(17,1:3) = [sr,s0,s0]
                           xgp(18,1:3) = [sr,s0,sr]
                           xgp(19,1:3) = [sl,sr,sl]
                           xgp(20,1:3) = [sl,sr,s0]
                           xgp(21,1:3) = [sl,sr,sr]
                           xgp(22,1:3) = [s0,sr,sl]
                           xgp(23,1:3) = [s0,sr,s0]
                           xgp(24,1:3) = [s0,sr,sr]
                           xgp(25,1:3) = [sr,sr,sl]
                           xgp(26,1:3) = [sr,sr,s0]
                           xgp(27,1:3) = [sr,sr,sr]

                           ws = 5.0_rp/9.0_rp
                           w0 = 8.0_rp/9.0_rp
                           wgp(1) = ws*ws*ws
                           wgp(2) = ws*ws*w0
                           wgp(3) = ws*ws*ws
                           wgp(4) = w0*ws*ws
                           wgp(5) = w0*ws*w0
                           wgp(6) = w0*ws*ws
                           wgp(7) = ws*ws*ws
                           wgp(8) = ws*ws*w0
                           wgp(9) = ws*ws*ws
                           wgp(10) = ws*w0*ws
                           wgp(11) = ws*w0*w0
                           wgp(12) = ws*w0*ws
                           wgp(13) = w0*w0*ws
                           wgp(14) = w0*w0*w0
                           wgp(15) = w0*w0*ws
                           wgp(16) = ws*w0*ws
                           wgp(17) = ws*w0*w0
                           wgp(18) = ws*w0*ws
                           wgp(19) = ws*ws*ws
                           wgp(20) = ws*ws*w0
                           wgp(21) = ws*ws*ws
                           wgp(22) = w0*ws*ws
                           wgp(23) = w0*ws*w0
                           wgp(24) = w0*ws*ws
                           wgp(25) = ws*ws*ws
                           wgp(26) = ws*ws*w0
                           wgp(27) = ws*ws*ws
                        else if (ngaus == 64) then
                           write(*,*) 'NOT CODED YET!'
                           xgp = 0.0_rp
                           wgp = 0.0_rp
                        end if

                end subroutine gll_hex

		!> @brief Computes the integral of a function over a QUA_XX
		!> @details Closed rule quadrature for a function f(x) over a
		!> QUA_X element type. The integral is computed using the
		!> given grid and weights associated with a particular element order.
		!> Notice that the order of the nodes follows that of the element itself.
		!> For boundary elements only!
		!> @param[in] atoIJ Node a to IJ rellationship
		!> @param[out] xgp Quadrature points
		!> @param[out] wgp Quadrature weights
		subroutine chebyshev_qua(atoIJ,xgp,wgp)
			implicit none
			integer(4), intent(in)  :: atoIJ(npbou)
			real(rp),    intent(out) :: xgp(npbou,ndime-1), wgp(npbou)
			integer(4)              :: inode, i, j, lorder(porder+1)
			real(rp)                 :: xi(porder+1),w1d(porder+1)
			call chebyshev_roots(xi)
			lorder(1) = 1
			lorder(2) = porder+1
			do i = 3,porder+1
				lorder(i) = i-1
			end do
			if (porder == 3) then
				!w1d(1:4) = [1.0_rp/9.0_rp, 8.0_rp/9.0_rp, 8.0_rp/9.0_rp, 1.0_rp/9.0_rp]
				w1d(1:4) = [1.0_rp/6.0_rp, 5.0_rp/6.0_rp, 5.0_rp/6.0_rp, 1.0_rp/6.0_rp]
			else
				write(1,*) "--| WEIGHTING FUNCTION NOT CODED YET"
				STOP(1)
			end if
			inode = 0
			do i = 1,porder+1
				do j = 1,porder+1
					inode = inode + 1
					xgp(atoIJ(inode),1:2) = [xi(lorder(i)), xi(lorder(j))]
					wgp(atoIJ(inode)) = w1d(lorder(i))*w1d(lorder(j))
				end do
			end do
		end subroutine chebyshev_qua

      !> @brief Computes the integral of a function over a HEX_XX
		!> @details Closed rule quadrature for a function f(x) over a
		!> HEX_XX element type. The integral is computed using the
		!> given grid and weights associated with a particular element order.
		!> Notice that the order of the nodes follows that of the element itself.
		!> @param[in] atoIJK Node a to IJK rellationship
		!> @param[out] xgp Quadrature points
		!> @param[out] wgp Quadrature weights
                subroutine chebyshev_hex(atoIJK,xgp,wgp)

                   use mod_maths

                   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                   ! Closed rule quadrature that uses the     !
                   ! Chebyshev grid+enddpoints as abcissas.   !
                   ! Weights are obtained by evaluating       !
                   ! w_j = int(l^n_i(xi_j),-1,1).             !
                   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                   
                   implicit none

                   integer(4), intent(in)  :: atoIJK(ngaus)
                   real(rp),    intent(out) :: xgp(ngaus,ndime), wgp(ngaus)
                   integer(4)              :: inode, i, j, k, lorder(porder+1)
                   real(rp)                 :: xi(porder+1),w1d(porder+1),w0,w1,w2,w3

                   call chebyshev_roots(xi)
                   lorder(1) = 1
                   lorder(2) = porder+1
                   do i = 3,porder+1
                      lorder(i) = i-1
                   end do


                   if (porder == 3) then
                      !w1d(1:4) = [1.0_rp/9.0_rp, 8.0_rp/9.0_rp, 8.0_rp/9.0_rp, 1.0_rp/9.0_rp]
                      w1d(1:4) = [1.0_rp/6.0_rp, 5.0_rp/6.0_rp, 5.0_rp/6.0_rp, 1.0_rp/6.0_rp]
                   else
                      STOP(1)
                   end if

                   inode = 0
                   do k = 1,porder+1
                      do i = 1,porder+1
                         do j = 1,porder+1
                            inode = inode + 1
                            xgp(atoIJK(inode),1:3) = [xi(lorder(i)), xi(lorder(j)), xi(lorder(k))]
                            wgp(atoIJK(inode)) = w1d(lorder(i))*w1d(lorder(j))*w1d(lorder(k))
                         end do
                      end do
                   end do

                   ! TODO: remove when confirmed above method
                  !if (ngaus == 64) then
                  !   xgp( 1,1:3) = [xi(1) , xi(1) , xi(1)]
                  !   xgp( 2,1:3) = [xi(4) , xi(1) , xi(1)]
                  !   xgp( 3,1:3) = [xi(4) , xi(4) , xi(1)]
                  !   xgp( 4,1:3) = [xi(1) , xi(4) , xi(1)]
                  !   xgp( 5,1:3) = [xi(1) , xi(1) , xi(4)]
                  !   xgp( 6,1:3) = [xi(4) , xi(1) , xi(4)]
                  !   xgp( 7,1:3) = [xi(4) , xi(4) , xi(4)]
                  !   xgp( 8,1:3) = [xi(1) , xi(4) , xi(4)]
                  !   xgp( 9,1:3) = [xi(2) , xi(1) , xi(1)]
                  !   xgp(10,1:3) = [xi(3) , xi(1) , xi(1)]
                  !   xgp(11,1:3) = [xi(1) , xi(2) , xi(1)]
                  !   xgp(12,1:3) = [xi(1) , xi(3) , xi(1)]
                  !   xgp(13,1:3) = [xi(1) , xi(1) , xi(2)]
                  !   xgp(14,1:3) = [xi(1) , xi(1) , xi(3)]
                  !   xgp(15,1:3) = [xi(4) , xi(2) , xi(1)]
                  !   xgp(16,1:3) = [xi(4) , xi(3) , xi(1)]
                  !   xgp(17,1:3) = [xi(4) , xi(1) , xi(2)]
                  !   xgp(18,1:3) = [xi(4) , xi(1) , xi(3)]
                  !   xgp(19,1:3) = [xi(3) , xi(4) , xi(1)]
                  !   xgp(20,1:3) = [xi(2) , xi(4) , xi(1)]
                  !   xgp(21,1:3) = [xi(4) , xi(4) , xi(2)]
                  !   xgp(22,1:3) = [xi(4) , xi(4) , xi(3)]
                  !   xgp(23,1:3) = [xi(1) , xi(4) , xi(2)]
                  !   xgp(24,1:3) = [xi(1) , xi(4) , xi(3)]
                  !   xgp(25,1:3) = [xi(2) , xi(1) , xi(4)]
                  !   xgp(26,1:3) = [xi(3) , xi(1) , xi(4)]
                  !   xgp(27,1:3) = [xi(1) , xi(2) , xi(4)]
                  !   xgp(28,1:3) = [xi(1) , xi(3) , xi(4)]
                  !   xgp(29,1:3) = [xi(4) , xi(2) , xi(4)]
                  !   xgp(30,1:3) = [xi(4) , xi(3) , xi(4)]
                  !   xgp(31,1:3) = [xi(3) , xi(4) , xi(4)]
                  !   xgp(32,1:3) = [xi(2) , xi(4) , xi(4)]
                  !   xgp(33,1:3) = [xi(2) , xi(2) , xi(1)]
                  !   xgp(34,1:3) = [xi(2) , xi(3) , xi(1)]
                  !   xgp(35,1:3) = [xi(3) , xi(3) , xi(1)]
                  !   xgp(36,1:3) = [xi(3) , xi(2) , xi(1)]
                  !   xgp(37,1:3) = [xi(2) , xi(1) , xi(2)]
                  !   xgp(38,1:3) = [xi(3) , xi(1) , xi(2)]
                  !   xgp(39,1:3) = [xi(3) , xi(1) , xi(3)]
                  !   xgp(40,1:3) = [xi(2) , xi(1) , xi(3)]
                  !   xgp(41,1:3) = [xi(1) , xi(2) , xi(2)]
                  !   xgp(42,1:3) = [xi(1) , xi(2) , xi(3)]
                  !   xgp(43,1:3) = [xi(1) , xi(3) , xi(3)]
                  !   xgp(44,1:3) = [xi(1) , xi(3) , xi(2)]
                  !   xgp(45,1:3) = [xi(4) , xi(2) , xi(2)]
                  !   xgp(46,1:3) = [xi(4) , xi(3) , xi(2)]
                  !   xgp(47,1:3) = [xi(4) , xi(3) , xi(3)]
                  !   xgp(48,1:3) = [xi(4) , xi(2) , xi(3)]
                  !   xgp(49,1:3) = [xi(3) , xi(4) , xi(2)]
                  !   xgp(50,1:3) = [xi(2) , xi(4) , xi(2)]
                  !   xgp(51,1:3) = [xi(2) , xi(4) , xi(3)]
                  !   xgp(52,1:3) = [xi(3) , xi(4) , xi(3)]
                  !   xgp(53,1:3) = [xi(2) , xi(2) , xi(4)]
                  !   xgp(54,1:3) = [xi(3) , xi(2) , xi(4)]
                  !   xgp(55,1:3) = [xi(3) , xi(3) , xi(4)]
                  !   xgp(56,1:3) = [xi(2) , xi(3) , xi(4)]
                  !   xgp(57,1:3) = [xi(2) , xi(2) , xi(2)]
                  !   xgp(58,1:3) = [xi(3) , xi(2) , xi(2)]
                  !   xgp(59,1:3) = [xi(3) , xi(3) , xi(2)]
                  !   xgp(60,1:3) = [xi(2) , xi(3) , xi(2)]
                  !   xgp(61,1:3) = [xi(2) , xi(2) , xi(3)]
                  !   xgp(62,1:3) = [xi(3) , xi(2) , xi(3)]
                  !   xgp(63,1:3) = [xi(3) , xi(3) , xi(3)]
                  !   xgp(64,1:3) = [xi(2) , xi(3) , xi(3)]

                  !    w0 = 1.0_rp/9.0_rp
                  !    w1 = 8.0_rp/9.0_rp
                  !    w2 = 8.0_rp/9.0_rp
                  !    w3 = 1.0_rp/9.0_rp

                  !    wgp( 1) = w0*w0*w0
                  !    wgp( 2) = w3*w0*w0
                  !    wgp( 3) = w3*w3*w0
                  !    wgp( 4) = w0*w3*w0
                  !    wgp( 5) = w0*w0*w3
                  !    wgp( 6) = w3*w0*w3
                  !    wgp( 7) = w3*w3*w3
                  !    wgp( 8) = w0*w3*w3
                  !    wgp( 9) = w1*w0*w0
                  !    wgp(10) = w2*w0*w0
                  !    wgp(11) = w1*w1*w0
                  !    wgp(12) = w1*w2*w0
                  !    wgp(13) = w0*w0*w1
                  !    wgp(14) = w0*w0*w2
                  !    wgp(15) = w3*w1*w0
                  !    wgp(16) = w3*w2*w0
                  !    wgp(17) = w3*w0*w1
                  !    wgp(18) = w3*w0*w2
                  !    wgp(19) = w2*w3*w0
                  !    wgp(20) = w1*w3*w0
                  !    wgp(21) = w3*w3*w1
                  !    wgp(22) = w3*w3*w2
                  !    wgp(23) = w0*w3*w1
                  !    wgp(24) = w0*w3*w2
                  !    wgp(25) = w1*w0*w3
                  !    wgp(26) = w2*w0*w3
                  !    wgp(27) = w0*w1*w3
                  !    wgp(28) = w0*w2*w3
                  !    wgp(29) = w3*w1*w3
                  !    wgp(30) = w3*w2*w3
                  !    wgp(31) = w2*w3*w3
                  !    wgp(32) = w1*w3*w3
                  !    wgp(33) = w1*w1*w0
                  !    wgp(34) = w1*w2*w0
                  !    wgp(35) = w2*w2*w0
                  !    wgp(36) = w2*w1*w0
                  !    wgp(37) = w1*w0*w1
                  !    wgp(38) = w2*w0*w1
                  !    wgp(39) = w2*w0*w2
                  !    wgp(40) = w1*w0*w2
                  !    wgp(41) = w0*w1*w1
                  !    wgp(42) = w0*w1*w2
                  !    wgp(43) = w0*w2*w2
                  !    wgp(44) = w0*w2*w1
                  !    wgp(45) = w3*w1*w1
                  !    wgp(46) = w3*w2*w1
                  !    wgp(47) = w3*w2*w2
                  !    wgp(48) = w3*w1*w2
                  !    wgp(49) = w2*w3*w1
                  !    wgp(50) = w1*w3*w1
                  !    wgp(51) = w1*w3*w2
                  !    wgp(52) = w2*w3*w2
                  !    wgp(53) = w1*w1*w3
                  !    wgp(54) = w2*w1*w3
                  !    wgp(55) = w2*w2*w3
                  !    wgp(56) = w1*w2*w3
                  !    wgp(57) = w1*w1*w1
                  !    wgp(58) = w2*w1*w1
                  !    wgp(59) = w2*w2*w1
                  !    wgp(60) = w1*w2*w1
                  !    wgp(61) = w1*w1*w2
                  !    wgp(62) = w2*w1*w2
                  !    wgp(63) = w2*w2*w2
                  !    wgp(64) = w1*w2*w2
                  !end if

                end subroutine chebyshev_hex

end module quadrature_rules
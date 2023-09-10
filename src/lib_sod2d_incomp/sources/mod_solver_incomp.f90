module mod_solver_incomp

      use mod_numerical_params
      use mod_comms
      use mod_mpi
      use mod_nvtx
      use mod_time_ops
      use mod_bc_routines
      use mod_operators
      use elem_diffu_incomp


      implicit none

	   real(rp)  , allocatable, dimension(:) :: x, r0, p0, qn, v, b,z0,z1,M,x0
      real(rp)  , allocatable, dimension(:,:) :: x_u, r0_u, p0_u, qn_u, v_u, b_u,z0_u,z1_u,M_u
	   logical  :: flag_cg_mem_alloc_pres=.true.
      logical  :: flag_cg_mem_alloc_veloc=.true.


      contains

            subroutine conjGrad_veloc_incomp(igtime,save_logFile_next,dt,nelem,npoin,npoin_w,connec,lpoin_w,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,dlxigp_ip,He,gpvol,Ngp,Ml,mu_fluid,mu_e,mu_sgs,Rp0,R)

           implicit none

           integer(4),           intent(in)    :: igtime,save_logFile_next
           integer(4), intent(in)    :: nelem, npoin, npoin_w, connec(nelem,nnode), lpoin_w(npoin_w)
           real(rp)   , intent(in)    :: gpvol(1,ngaus,nelem), Ngp(ngaus,nnode),dt
           real(rp),   intent(in)    :: dlxigp_ip(ngaus,ndime,porder+1),He(ndime,ndime,ngaus,nelem),Ml(npoin),Rp0(npoin,ndime)
           integer(4), intent(in)  :: invAtoIJK(porder+1,porder+1,porder+1), gmshAtoI(nnode), gmshAtoJ(nnode), gmshAtoK(nnode)
            real(rp),             intent(inout) :: mu_fluid(npoin)
            real(rp),             intent(inout) :: mu_e(nelem,ngaus)
            real(rp),             intent(inout) :: mu_sgs(nelem,ngaus)
           real(rp)   , intent(inout) :: R(npoin,ndime)
           integer(4)                :: ipoin, iter,ialpha,idime
           real(rp)                   :: alphaCG, betaCG,Q1(2)
           real(8)                     :: auxT1,auxT2,auxQ(2),auxQ1,auxQ2,auxB,alpha(5),alpha2(5),aux_alpha,T1
          
           call nvtxStartRange("CG solver scalar")
          if (flag_cg_mem_alloc_veloc .eqv. .true.) then
				allocate(x_u(npoin,ndime), r0_u(npoin,ndime), p0_u(npoin,ndime), qn_u(npoin,ndime), v_u(npoin,ndime), b_u(npoin,ndime),z0_u(npoin,ndime),z1_u(npoin,ndime),M_u(npoin,ndime))
            !$acc enter data create(x_u(:,:), r0_u(:,:), p0_u(:,:), qn_u(:,:), v_u(:,:), b_u(:,:),z0_u(:,:),z1_u(:,:),M_u(:,:))
				flag_cg_mem_alloc_veloc = .false.
			 end if

           !
           ! Initialize solver
           !
            !$acc parallel loop
            do ipoin = 1,npoin
               !$acc loop seq
               do idime = 1,ndime           
                  r0_u(ipoin,idime) = 0.0_rp
                  p0_u(ipoin,idime) = 0.0_rp
                  qn_u(ipoin,idime) = 0.0_rp
                  v_u(ipoin,idime) = 0.0_rp
                  b_u(ipoin,idime) = 0.0_rp
                  z0_u(ipoin,idime) = 0.0_rp
                  z1_u(ipoin,idime) = 0.0_rp
                  M_u(ipoin,idime) = Ml(ipoin)
               end do
            end do 
            !$acc end parallel loop

            !$acc parallel loop
            do ipoin = 1,npoin_w
               !$acc loop seq
               do idime = 1,ndime   
                  b_u(lpoin_w(ipoin),idime) = R(lpoin_w(ipoin),idime)
                  x_u(lpoin_w(ipoin),idime) = Rp0(lpoin_w(ipoin),idime)
               end do
            end do
            !$acc end parallel loop
               
            ! Real solver form here

            call full_diffusion_ijk_incomp(nelem,npoin,connec,Ngp,He,gpvol,dlxigp_ip,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,x_u,mu_fluid,mu_e,mu_sgs,Ml,qn_u)
            if(mpi_size.ge.2) then
               do idime = 1,ndime
                  call mpi_halo_atomic_update_real(qn_u(:,idime))
               end do            
            end if
            
            !$acc parallel loop
            do ipoin = 1,npoin_w
               !$acc loop seq
               do idime = 1,ndime  
                  qn_u(lpoin_w(ipoin),idime) = x_u(lpoin_w(ipoin),idime)*Ml(lpoin_w(ipoin))+qn_u(lpoin_w(ipoin),idime)*0.5_rp*dt
                  r0_u(lpoin_w(ipoin),idime) = b_u(lpoin_w(ipoin),idime)-qn_u(lpoin_w(ipoin),idime) ! b-A*x0
                  z0_u(lpoin_w(ipoin),idime) = r0_u(lpoin_w(ipoin),idime)/M_u(lpoin_w(ipoin),idime)
                  p0_u(lpoin_w(ipoin),idime) = z0_u(lpoin_w(ipoin),idime)
              end do
            end do
            !$acc end parallel loop

            auxT1 = 0.0d0
            !$acc parallel loop reduction(+:auxT1)
            do ipoin = 1,npoin
               !$acc loop seq
              do idime = 1,ndime 
               auxT1 = auxT1+real(r0_u(ipoin,idime)*r0_u(ipoin,idime),8)
              end do
            end do

            call MPI_Allreduce(auxT1,auxT2,1,mpi_datatype_real8,MPI_SUM,MPI_COMM_WORLD,mpi_err)

            auxB = sqrt(auxT2)
 

           !
           ! Start iterations
           !
           do iter = 1,maxIter
              call nvtxStartRange("Iteration")
              call full_diffusion_ijk_incomp(nelem,npoin,connec,Ngp,He,gpvol,dlxigp_ip,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,p0_u,mu_fluid,mu_e,mu_sgs,Ml,qn_u)
              if(mpi_size.ge.2) then
               do idime = 1,ndime
                  call mpi_halo_atomic_update_real(qn_u(:,idime))
                  end do              
               end if
             !$acc parallel loop
             do ipoin = 1,npoin_w
               !$acc loop seq
               do idime = 1,ndime  
                  qn_u(lpoin_w(ipoin),idime) = p0_u(lpoin_w(ipoin),idime)*Ml(lpoin_w(ipoin))+qn_u(lpoin_w(ipoin),idime)*0.5_rp*dt
              end do
             end do
             !$acc end parallel loop
            
              auxQ1 = 0.0d0
              auxQ2 = 0.0d0
              !$acc parallel loop reduction(+:auxQ1,auxQ2) 
              do ipoin = 1,npoin_w
                  !$acc loop seq
                  do idime = 1,ndime 
                   auxQ1 = auxQ1+real(r0_u(lpoin_w(ipoin),idime)*z0_u(lpoin_w(ipoin),idime),8) ! <s_k-1,r_k-1>
                   auxQ2 = auxQ2+real(p0_u(lpoin_w(ipoin),idime)*qn_u(lpoin_w(ipoin),idime),8) ! <s_k-1,A*s_k-1>
                 end do
              end do
              !$acc end parallel loop
              auxQ(1) = auxQ1
              auxQ(2) = auxQ2
              call MPI_Allreduce(auxQ,Q1,2,mpi_datatype_real8,MPI_SUM,MPI_COMM_WORLD,mpi_err)
              alphaCG = real(Q1(1)/Q1(2),rp)
              !$acc parallel loop
              do ipoin = 1,npoin_w
                 !$acc loop seq
                  do idime = 1,ndime 
                     x_u(lpoin_w(ipoin),idime) = x_u(lpoin_w(ipoin),idime)+alphaCG*p0_u(lpoin_w(ipoin),idime) ! x_k = x_k-1 + alpha*s_k-1
                 end do
              end do
              !$acc end parallel loop
              !$acc parallel loop
              do ipoin = 1,npoin_w
                  !$acc loop seq
                  do idime = 1,ndime 
                     r0_u(lpoin_w(ipoin),idime) = r0_u(lpoin_w(ipoin),idime)-alphaCG*qn_u(lpoin_w(ipoin),idime) ! b-A*p0
                     z1_u(lpoin_w(ipoin),idime) = z0_u(lpoin_w(ipoin),idime) 
                     z0_u(lpoin_w(ipoin),idime) = r0_u(lpoin_w(ipoin),idime)/M_u(lpoin_w(ipoin),idime) 
                  end do
              end do
              !$acc end parallel loop
              auxT1 = 0.0d0
              !$acc parallel loop reduction(+:auxT1)
              do ipoin = 1,npoin
                  !$acc loop seq
                 do idime = 1,ndime 
                  auxT1 = auxT1+real(r0_u(ipoin,idime)*r0_u(ipoin,idime),8)
                 end do
              end do

               call MPI_Allreduce(auxT1,auxT2,1,mpi_datatype_real8,MPI_SUM,MPI_COMM_WORLD,mpi_err)

               T1 = auxT2
              !
              ! Stop cond
              !
              if (sqrt(T1) .lt. (tol*auxB)) then
                 call nvtxEndRange
                 exit
              end if
              !
              ! Update p
              !
              auxT1 = 0.0d0
              !$acc parallel loop reduction(+:auxT1)
              do ipoin = 1,npoin
                 !$acc loop seq
                  do idime = 1,ndime 
                     auxT1 = auxT1+real(r0_u(ipoin,idime)*(z0_u(ipoin,idime)-z1_u(ipoin,idime)),8) ! <r_k,A*s_k-1>
                  end do
              end do
              !$acc end parallel loop
              call MPI_Allreduce(auxT1,auxT2,1,mpi_datatype_real8,MPI_SUM,MPI_COMM_WORLD,mpi_err)
              betaCG = real(auxT2/Q1(1),rp)
              !$acc parallel loop
              do ipoin = 1,npoin_w
                  !$acc loop seq
                  do idime = 1,ndime 
                     p0_u(lpoin_w(ipoin),idime) = z0_u(lpoin_w(ipoin),idime)+betaCG*p0_u(lpoin_w(ipoin),idime) ! s_k = r_k+beta*s_k-1
                  end do
              end do
              !$acc end parallel loop
              call nvtxEndRange
           end do
           if (iter == maxIter) then
               if(igtime==save_logFile_next.and.mpi_rank.eq.0) write(111,*) "--|[veloc] CG, iters: ",iter," tol ",sqrt(T1)/auxB
           else
               if(igtime==save_logFile_next.and.mpi_rank.eq.0) write(111,*) "--|[veloc] CG, iters: ",iter," tol ",sqrt(T1)/auxB
           endif
            
            !$acc kernels
            R(:,:) = x_u(:,:)
            !$acc end kernels

           call nvtxEndRange

        end subroutine conjGrad_veloc_incomp   

        subroutine conjGrad_pressure_incomp(igtime,save_logFile_next,nelem,npoin,npoin_w,connec,lpoin_w,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,dlxigp_ip,He,gpvol,Ngp,Ml,Rp0,R)

           implicit none

           integer(4),           intent(in)    :: igtime,save_logFile_next
           integer(4), intent(in)    :: nelem, npoin, npoin_w, connec(nelem,nnode), lpoin_w(npoin_w)
           real(rp)   , intent(in)    :: gpvol(1,ngaus,nelem), Ngp(ngaus,nnode)
           real(rp),   intent(in)    :: dlxigp_ip(ngaus,ndime,porder+1),He(ndime,ndime,ngaus,nelem),Ml(npoin),Rp0(npoin)
           integer(4), intent(in)  :: invAtoIJK(porder+1,porder+1,porder+1), gmshAtoI(nnode), gmshAtoJ(nnode), gmshAtoK(nnode)
           real(rp)   , intent(inout) :: R(npoin)
           integer(4)                :: ipoin, iter,ialpha
           real(rp)                   :: T1, alphaCG, betaCG,Q1(2)
           real(8)                     :: auxT1,auxT2,auxQ(2),auxQ1,auxQ2,auxB,alpha(5),alpha2(5),aux_alpha
          
           call nvtxStartRange("CG solver scalar")
          if (flag_cg_mem_alloc_pres .eqv. .true.) then
				allocate(x(npoin), r0(npoin), p0(npoin), qn(npoin), v(npoin), b(npoin),z0(npoin),z1(npoin),M(npoin),x0(npoin))
            !$acc enter data create(x(:), r0(:), p0(:), qn(:), v(:), b(:),z0(:),z1(:),M(:),x0(:))
				flag_cg_mem_alloc_pres = .false.
			 end if

           !
           ! Initialize solver
           !
           !$acc kernels
           x(:) = 0.0_rp
           r0(:) = 0.0_rp
           p0(:) = 0.0_rp
           qn(:) = 0.0_rp
           v(:) = 0.0_rp
           b(:) = 0.0_rp
           z0(:) = 0.0_rp
           z1(:) = 0.0_rp
           M(:) = Ml(:)
           !$acc end kernels
            call eval_laplacian_mult(nelem,npoin,npoin_w,connec,lpoin_w,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,dlxigp_ip,He,gpvol,Rp0,qn)! A*x0
            !$acc parallel loop
            do ipoin = 1,npoin_w
               x0(lpoin_w(ipoin)) = Rp0(lpoin_w(ipoin))
               b(lpoin_w(ipoin)) = R(lpoin_w(ipoin)) - qn(lpoin_w(ipoin))
               x(lpoin_w(ipoin)) = 0.0_rp
            end do
            !$acc end parallel loop
               
            ! Real solver form here

            if((mpi_rank.eq.0) .and. (flag_fs_fix_pressure .eqv. .true.)) then
               b(lpoin_w(1)) = 0.0_rp
               x(lpoin_w(1)) = 0.0_rp
            end if
            call eval_laplacian_mult(nelem,npoin,npoin_w,connec,lpoin_w,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,dlxigp_ip,He,gpvol,x,qn)! A*x0
           !$acc parallel loop
           do ipoin = 1,npoin_w
              r0(lpoin_w(ipoin)) = b(lpoin_w(ipoin))-qn(lpoin_w(ipoin)) ! b-A*x0
              z0(lpoin_w(ipoin)) = r0(lpoin_w(ipoin))/M(lpoin_w(ipoin))
              p0(lpoin_w(ipoin)) = z0(lpoin_w(ipoin))
           end do
            !$acc end parallel loop


            !auxT1 = 0.0d0
            !!$acc parallel loop reduction(+:auxT1)
            !do ipoin = 1,npoin
            !   auxT1 = auxT1+real(b(ipoin)*b(ipoin),8)
            !end do
            !!$acc end parallel loop
            !call MPI_Allreduce(auxT1,auxB,1,mpi_datatype_real8,MPI_SUM,MPI_COMM_WORLD,mpi_err)
            auxB = 1.0_rp !sqrt(auxB)


           !
           ! Start iterations
           !
           do iter = 1,maxIter
              call nvtxStartRange("Iteration")
              call eval_laplacian_mult(nelem,npoin,npoin_w,connec,lpoin_w,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,dlxigp_ip,He,gpvol,p0,qn) ! A*s_k-1
              auxQ1 = 0.0d0
              auxQ2 = 0.0d0
              !$acc parallel loop reduction(+:auxQ1,auxQ2) 
              do ipoin = 1,npoin_w
                 auxQ1 = auxQ1+real(r0(lpoin_w(ipoin))*z0(lpoin_w(ipoin)),8) ! <s_k-1,r_k-1>
                 auxQ2 = auxQ2+real(p0(lpoin_w(ipoin))*qn(lpoin_w(ipoin)),8) ! <s_k-1,A*s_k-1>
              end do
              !$acc end parallel loop
              auxQ(1) = auxQ1
              auxQ(2) = auxQ2
              call MPI_Allreduce(auxQ,Q1,2,mpi_datatype_real8,MPI_SUM,MPI_COMM_WORLD,mpi_err)
              alphaCG = real(Q1(1)/Q1(2),rp)
              !$acc parallel loop
              do ipoin = 1,npoin_w
                 x(lpoin_w(ipoin)) = x(lpoin_w(ipoin))+alphaCG*p0(lpoin_w(ipoin)) ! x_k = x_k-1 + alpha*s_k-1
              end do
              !$acc end parallel loop
              !$acc parallel loop
              do ipoin = 1,npoin_w
                 r0(lpoin_w(ipoin)) = r0(lpoin_w(ipoin))-alphaCG*qn(lpoin_w(ipoin)) ! b-A*p0
                 z1(lpoin_w(ipoin)) = z0(lpoin_w(ipoin)) 
                 z0(lpoin_w(ipoin)) = r0(lpoin_w(ipoin))/M(lpoin_w(ipoin)) 
              end do
              !$acc end parallel loop
              auxT1 = 0.0d0
              !$acc parallel loop reduction(+:auxT1)
              do ipoin = 1,npoin
                 auxT1 = auxT1+real(r0(ipoin)*r0(ipoin),8)
              end do

               call MPI_Allreduce(auxT1,auxT2,1,mpi_datatype_real8,MPI_SUM,MPI_COMM_WORLD,mpi_err)

               T1 = real(auxT2,rp)
              !
              ! Stop cond
              !
              if (sqrt(T1) .lt. (tol*auxB)) then
                 call nvtxEndRange
                 exit
              end if
              !
              ! Update p
              !
              auxT1 = 0.0d0
              !$acc parallel loop reduction(+:auxT1)
              do ipoin = 1,npoin
                 auxT1 = auxT1+real(r0(ipoin)*(z0(ipoin)-z1(ipoin)),8) ! <r_k,A*s_k-1>
              end do
              !$acc end parallel loop
              call MPI_Allreduce(auxT1,auxT2,1,mpi_datatype_real8,MPI_SUM,MPI_COMM_WORLD,mpi_err)
              betaCG = real(auxT2/Q1(1),rp)
              !$acc parallel loop
              do ipoin = 1,npoin_w
                 p0(lpoin_w(ipoin)) = z0(lpoin_w(ipoin))+betaCG*p0(lpoin_w(ipoin)) ! s_k = r_k+beta*s_k-1
              end do
              !$acc end parallel loop
              call nvtxEndRange
              !if(mpi_rank.eq.0) write(111,*) "--|[in] CG, iters: ",iter," tol ",sqrt(T1)
           end do
           if (iter == maxIter) then
              !if(mpi_rank.eq.0) write(111,*) "--| TOO MANY ITERATIONS!"
              !call nvtxEndRange
              !stop 1
               if(igtime==save_logFile_next.and.mpi_rank.eq.0) write(111,*) "--|[pres] CG, iters: ",iter," tol ",sqrt(T1)/auxB
           else
               if(igtime==save_logFile_next.and.mpi_rank.eq.0) write(111,*) "--|[pres] CG, iters: ",iter," tol ",sqrt(T1)/auxB
           endif
            
            !$acc kernels
            R(:) = x0(:)+x(:)
            !$acc end kernels

           call nvtxEndRange

        end subroutine conjGrad_pressure_incomp   
end module mod_solver_incomp

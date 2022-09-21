module mod_comms_performance
   use mod_mpi
   use mod_mpi_mesh
   use mod_comms
   use mod_hdf5
   use inicond_reader
#ifndef NOACC
   use openacc
   use mod_nvtx
#endif

#define _ONLYBUFFERS_ 1
#define _SENDRECV_ 1
#define _ISENDIRECV_ 1
#define _PUTFENCEFLAGOFF_ 1
#define _PUTFENCEFLAGON_ 1
#define _PUTPSCWON_ 0
#define _PUTPSCWOFF_ 0
#define _PUTLOCKBON_ 1
#define _PUTLOCKBOFF_ 1
#define _GETFENCEFLAGOFF_ 1
#define _GETFENCEFLAGON_ 1
#define _GETPSCWON_ 0
#define _GETPSCWOFF_ 0
#define _GETLOCKBON_ 1
#define _GETLOCKBOFF_ 1

   implicit none

   real(8), dimension(:), allocatable :: res_dfield
   real(4), dimension(:), allocatable :: res_ffield
   integer(4), dimension(:), allocatable :: res_ifield
   real(4), dimension(:), allocatable :: xVec,yVec,aux_ffield

contains

    subroutine create_dummy_1Dmesh(numNodesSrl,numNodesB_1r)
      implicit none
      integer,intent(in) :: numNodesSrl,numNodesB_1r
      integer :: i,j,iRank,iNodeL,iNodeGSrl,iNodeGPar,iNS,iNE
      integer :: numNodesBound_1rank,numNodesBoundRank,numNodesRankSrl
      integer, dimension(0:mpi_size-1) :: iNodeStartSrl, iNodeEndSrl
      integer, dimension(0:mpi_size-1) :: iNodeStartPar, iNodeEndPar
      integer, allocatable :: boundaryNodes(:),vecSharedBN_full(:),nodesInRank(:)

        !1.fer la meva propia 'malla dummy'
        totalNumNodesSrl = numNodesSrl
        numNodesBound_1rank = numNodesB_1r
        numNodesBoundRank = numNodesBound_1rank*2

        call get_serialNodePartitioning(numNodesRankSrl,iNodeStartSrl,iNodeEndSrl)

        !write(*,*) 'rank[',mpi_rank,'] rankSrl ',numNodesRankSrl
        !do iRank=0,mpi_size-1
            !write(*,*) ' ##rank ', iRank , ' iNS ', iNodeStartSrl(iRank), ' iNE ', iNodeEndSrl(iRank)
        !end do

        !2. crear artificialment els nodes boundaries d'aquesta malla
        numNodesRankPar = numNodesRankSrl + numNodesBound_1rank
        allocate(boundaryNodes(numNodesBoundRank))
        boundaryNodes(:)=-1

        !write(*,*) 'rank[',mpi_rank,'] rankPar ',numNodesRankPar

        i=1
        iNS=iNodeStartSrl(mpi_rank)
        iNE=iNodeStartSrl(mpi_rank)+numNodesBound_1rank-1
        do iNodeGSrl=iNS,iNE
            !write(*,*) 'rank[',mpi_rank,'] iNodeGSrl ',iNodeGSrl,' i ',i
            boundaryNodes(i)=iNodeGSrl
            i=i+1
        end do

        if(mpi_rank.ne.(mpi_size-1)) then
            iNS=iNodeEndSrl(mpi_rank)+1
            iNE=iNodeEndSrl(mpi_rank)+numNodesBound_1rank
        else
            iNS=iNodeStartSrl(0)
            iNE=iNodeStartSrl(0)+numNodesBound_1rank-1
        end if
        do iNodeGSrl=iNS,iNE
            !write(*,*) 'rank[',mpi_rank,'] iNodeGSrl ',iNodeGSrl,' i ',i
            boundaryNodes(i)=iNodeGSrl
            i=i+1
        end do

        !3. crear llista amb id node global d'aquest rank
        allocate(nodesInRank(numNodesRankPar))
        nodesInRank(:)=-1

        i=1
        iNS=iNodeStartSrl(mpi_rank)
        iNE=iNodeStartSrl(mpi_rank)+numNodesRankSrl-1
        do iNodeGSrl=iNS,iNE
            !write(*,*) 'rank[',mpi_rank,'] iNodeGSrl ',iNodeGSrl,' i ',i
            nodesInRank(i)=iNodeGSrl
            i=i+1
        end do
        iNS=numNodesBoundRank-numNodesBound_1rank+1
        iNE=numNodesBoundRank
        do j=iNS,iNE
            iNodeGSrl=boundaryNodes(j)
            nodesInRank(i)=iNodeGSrl
            i=i+1
        end do

        !write(*,*) '#rank ', mpi_rank, ' nodesInRank ', nodesInRank(:)

        !3. fer el particionament dels nodes
        call define_parallelNodePartitioning(iNodeStartPar,iNodeEndPar)

        write(*,*) '#rank ', mpi_rank, ' nodesInRank ', numNodesRankPar, ' iNodeS ', rankNodeStart, ' iNodeE ', rankNodeEnd
        !write(*,*) 'rank[',mpi_rank,'] -> start ',iNodeStartPar(:),' end ', iNodeEndPar(:), 'tNNP ', totalNumNodesPar

        call define_mpi_boundaries_inPar(boundaryNodes,vecSharedBN_full)

        !4. fer un pseudo-reordering

        allocate(globalIdSrl(numNodesRankPar))
        allocate(globalIdPar(numNodesRankPar))

        do iNodeL=1,numNodesRankPar
               !iNodeL = iPos
               !isNodeAdded(iNodeGSrl)  = iNodeL
            iNodeGSrl = nodesInRank(iNodeL)
            iNodeGPar = iNodeL + iNodeStartPar(mpi_rank) - 1

            globalIdSrl(iNodeL) = iNodeGsrl
            globalIdPar(iNodeL) = iNodeGPar

        end do

        !5. generar esquema de comunicacio
        call generate_mpi_comm_scheme(vecSharedBN_full)

        deallocate(boundaryNodes)
        deallocate(vecSharedBN_full)

   end subroutine create_dummy_1Dmesh

   subroutine init_comms_performance(useIntInComms,useFloatInComms,useDoubleInComms)
      implicit none
      logical, intent(in) :: useIntInComms,useFloatInComms,useDoubleInComms

      if(useIntInComms) then
         allocate(res_ifield(numNodesRankPar))
      end if
      if(useFloatInComms) then
         allocate(res_ffield(numNodesRankPar))
      end if
      if(useDoubleInComms) then
         allocate(res_dfield(numNodesRankPar))
      end if

      call init_comms(useIntInComms,useFloatInComms,useDoubleInComms)
   end subroutine init_comms_performance

   subroutine debug_comms_float()
      implicit none

      res_ffield(:) = 10.0_4
      call mpi_halo_atomic_update_float(res_ffield) !using default method

      call save_vtkhdf_flotFieldFile(res_ffield)

   end subroutine debug_comms_float

   subroutine test_comms_performance_float(numIters)
      implicit none
      integer,intent(in) :: numIters
      real(8) :: start_time,end_time,elapsed_time_r,elapsed_time
      real(8) :: array_timers(20)
      real(4) :: refValue2check
      integer :: numRanksNodeCnt(numNodesRankPar)
      integer :: iter,iTimer,iter2check
      logical :: isOk

      iter2check = 5
      refValue2check = 1.0_4
   
      call evalNumRanksNodeCnt(numRanksNodeCnt)

      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      iTimer = 0
#if _ONLYBUFFERS_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_onlyBuffers(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_onlyBuffers(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'OnlyBuffers float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _ONLYBUFFERS_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_onlyBuffers(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_onlyBuffers(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'OnlyBuffers2 float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _SENDRECV_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_sendRcv(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_sendRcv(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'SendRecv float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _ISENDIRECV_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_iSendiRcv(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_iSendiRcv(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'ISendIRecv float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _PUTFENCEFLAGOFF_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call setFenceFlags(.false.)
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_put_fence(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_put_fence(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'PUT(Fence-flagsOFF) float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _PUTFENCEFLAGON_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call setFenceFlags(.true.)
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_put_fence(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_put_fence(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'PUT(Fence-flagsON) float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _PUTPSCWON_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call setPSCWAssertNoCheckFlags(.true.)
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_put_pscw(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_put_pscw(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'PUT(PSCW-NoCheckON) float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _PUTPSCWOFF_
      !$acc end kernels
      call setPSCWAssertNoCheckFlags(.false.)
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_put_pscw(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_put_pscw(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'PUT(PSCW-NoCheckOFF) float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _PUTLOCKBON_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call setLockBarrier(.true.)
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_put_lock(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_put_lock(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'PUT(Lock-BarrierON) float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _PUTLOCKBOFF_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call setLockBarrier(.false.)
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_put_lock(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_put_lock(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'PUT(Lock-BarrierOFF) float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _GETFENCEFLAGOFF_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call setFenceFlags(.false.)
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_get_fence(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_get_fence(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'GET(Fence-flagsOFF) float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _GETFENCEFLAGON_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call setFenceFlags(.true.)
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_get_fence(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_get_fence(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'GET(Fence-flagsON) float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _GETPSCWON_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call setPSCWAssertNoCheckFlags(.true.)
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_get_pscw(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_get_pscw(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'GET(PSCW-NoCheckON) float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _GETPSCWOFF_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call setPSCWAssertNoCheckFlags(.false.)
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_get_pscw(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_get_pscw(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'GET(PSCW-NoCheckOFF) float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _GETLOCKBON_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call setLockBarrier(.true.)
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_get_lock(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_get_lock(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'GET(Lock-BarrierON) float time:',elapsed_time,'isOk',isOk
      !---------------------
#endif
#if _GETLOCKBOFF_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call setLockBarrier(.false.)
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_get_lock(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_get_lock(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'GET(Lock-BarrierOFF) float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
#if _ONLYBUFFERS_
      !---------------------------------------------------------------
      !$acc kernels
          res_ffield(:) = 0.0_4
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      start_time = MPI_Wtime()
      do iter=1,numIters
         call mpi_halo_atomic_update_float_onlyBuffers(res_ffield)
      end do
      end_time = MPI_Wtime()
      elapsed_time_r = end_time - start_time
      call MPI_Allreduce(elapsed_time_r,elapsed_time,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD,mpi_err)
      iTimer=iTimer+1
      array_timers(iTimer) = elapsed_time;
      !---- CHECK IF WORKS OK -----!
      !$acc kernels
          res_ffield(:) = refValue2check
      !$acc end kernels
      call MPI_Barrier(MPI_COMM_WORLD,mpi_err)
      do iter=1,iter2check
         call mpi_halo_atomic_update_float_onlyBuffers(res_ffield)
         call normalize_floatField_in_sharedNodes(numRanksNodeCnt,res_ffield)
      end do
      call check_results_mpi_halo_atomic_update_float(refValue2check,res_ffield,isOk)
      !----------------------------!
      if(mpi_rank.eq.0) write(*,*) 'OnlyBuffers float time:',elapsed_time,'isOk',isOk
      !---------------------------------------------------------------
#endif
   end subroutine

   subroutine check_results_mpi_halo_atomic_update_float(refValue,floatField,isOk)
      implicit none
      real(4),intent(in) :: refValue
      real(4),intent(in) :: floatField(:)
      logical,intent(inout) :: isOk
      integer :: i,iNodeL
      real(8) :: value,refValueHi,refValueLo,tol

      tol = 1.0e-6
      refValueHi = real(refValue,8) + tol!real(2**numIters,8) + tol
      refValueLo = real(refValue,8) - tol!real(2**numIters,8) - tol

      isOk = .true.

      do i=1,numNodesToComm
         iNodeL = matrixCommScheme(i,1)
         value = real(floatField(iNodeL),8)
         if((value>refValueHi).or.(value<refValueLo)) then
            !write(*,*) '[',mpi_rank,'] Wrong value! iNodeL',iNodeL,' value ',value
            isOk = .false.
            exit
         end if
      end do

   end subroutine

   subroutine evalNumRanksNodeCnt(numRanksNodeCnt)
      implicit none
      integer,intent(inout) :: numRanksNodeCnt(numNodesRankPar)
      integer :: i,iNodeL

      numRanksNodeCnt(:)=1

      do i= 1,numNodesToComm
         iNodeL = matrixCommScheme(i,1)
         numRanksNodeCnt(iNodeL) = numRanksNodeCnt(iNodeL) + 1
      end do 
   end subroutine evalNumRanksNodeCnt

   subroutine normalize_floatField_in_sharedNodes(numRanksNodeCnt,floatField)
      implicit none
      integer,intent(in) :: numRanksNodeCnt(numNodesRankPar)
      real(rp), intent(inout) :: floatField(numNodesRankPar)
      integer :: i,iNodeL

      do iNodeL = 1,numNodesRankPar
         floatField(iNodeL) = floatField(iNodeL) / real(numRanksNodeCnt(iNodeL),rp)
      end do
   end subroutine normalize_floatField_in_sharedNodes

   subroutine saxpy(n,y,alpha,x)
      !saxpy: y <- y+alpha*x
      implicit none
      integer, intent(in) :: n
      real(4), dimension(n), intent(inout) :: y
      real(4), intent(in) :: alpha
      real(4), dimension(n), intent(in) :: x
      integer :: i

      !$acc kernels
      do i=1,n
          y(i) = y(i) + alpha*x(i)
      end do
      !$acc end kernels
   end subroutine saxpy

   subroutine sgemv(n,y,alpha,beta,A,x)
      !sgemv: y <- beta*y+alpha*[A]*x
      implicit none
      integer, intent(in) :: n
      real(4), dimension(n), intent(inout) :: y
      real(4), intent(in) :: alpha,beta
      real(4), dimension(n,n), intent(in) :: A
      real(4), dimension(n), intent(in) :: x
      integer :: i,j

      !test order 
#if 1
      !$acc kernels
      do j=1,n !cols
        y(j) = 0 
        do i=1,n !rows
           y(i) = beta*y(i) + alpha*A(i,j)*x(j) 
        end do 
      end do
      !$acc end kernels
#else
      !$acc kernels
      do i=1,n !rows
         y(j) = 0 
         do j=1,n !cols
            y(i) = beta*y(i) + alpha*A(i,j)*x(j) 
         end do
      end do
      !$acc end kernels
#endif
   end subroutine sgemv

   subroutine do_saxpy_loop(numIters)
      implicit none
      integer, intent(in) :: numIters
      integer :: iter
      real(4) :: alpha

      res_ffield(:) = 1.
      aux_ffield(:) = 1.
      alpha = 0.

      write(*,*) 'rank',mpi_rank,' calling SAXPY LOOP!'


      do iter=1,numIters

         !call nvtxStartRange("saxpy op")
         call saxpy(numNodesRankPar,res_ffield,alpha,aux_ffield)
         !call nvtxEndRange

         !call nvtxStartRange("comms")
         call update_and_comm_floatField(res_ffield)
         !call nvtxEndRange

      end do

   end subroutine do_saxpy_loop

   subroutine do_comms(numIters)
       implicit none
       integer, intent(in) :: numIters
       integer :: iter
       real(8) :: wc_start,wc_end

       res_dfield(:) = 1.d0
       res_ffield(:) = 1.

#if 1

        call MPI_Barrier(MPI_COMM_WORLD, mpi_err)


        do iter=1,numIters
            res_dfield(:) = 0.1
            call MPI_Barrier(MPI_COMM_WORLD, mpi_err)
            call update_and_comm_doubleField(res_dfield)
            !write(*,*) 'res_dfield[',mpi_rank,'] ', res_dfield(:)
            call MPI_Barrier(MPI_COMM_WORLD, mpi_err)
        end do

        call MPI_Barrier(MPI_COMM_WORLD, mpi_err)
        !call print_dtimers()

#endif
#if 1
        call MPI_Barrier(MPI_COMM_WORLD, mpi_err)

        do iter=1,numIters
            res_ffield(:) = 0.1
            call MPI_Barrier(MPI_COMM_WORLD, mpi_err)
            call update_and_comm_floatField(res_ffield)
            call MPI_Barrier(MPI_COMM_WORLD, mpi_err)
        end do

        call MPI_Barrier(MPI_COMM_WORLD, mpi_err)
        !write(*,*) 'res_ffield [',mpi_rank,'] ', res_ffield(:)
        !call print_ftimers()
#endif

#if 0
        call MPI_Barrier(MPI_COMM_WORLD, mpi_err)
        call init_shared_mem_window()

        do iter=1,numIters
            res_ffield(:) = 0.1
            call MPI_Barrier(MPI_COMM_WORLD, mpi_err)
            call update_and_comm_shared_mem_floatField(res_ffield)

            call MPI_Barrier(MPI_COMM_WORLD, mpi_err)
        end do

        call MPI_Barrier(MPI_COMM_WORLD, mpi_err)
        !write(*,*) 'res_sm[',mpi_rank,'] ', res_ffield(:)
        call print_smtimers()


        call close_shared_mem_windows()
#endif

    end subroutine do_comms

    subroutine do_comms_sharedMem()
        implicit none
        integer :: iter,numIters

        call init_shared_mem_window()

        numIters = 10000

        do iter=1,numIters
            call MPI_Barrier(MPI_COMM_WORLD, mpi_err)
            call update_and_comm_shared_mem_floatField(res_ffield)
            call MPI_Barrier(MPI_COMM_WORLD, mpi_err)
        end do

        !call print_smtimers()


        call close_shared_mem_windows()

    end subroutine do_comms_sharedMem

   subroutine test_mpi_cudaware(n,numIters)
      implicit none
      integer,intent(in) :: n,numIters
      integer :: i,iter
      real(4), allocatable :: x(:),y(:)
      integer :: tag_send,tag_recv,ngb_rank

      allocate(x(n))
      allocate(y(n))

      tag_send = 0
      tag_recv = tag_send
      ngb_rank = merge(1, 0, mpi_rank.eq.0)


      !call nvtxStartRange("full_loop")
      !$acc data create(x(:)) copyout(y(:))
      do iter=1,numIters

         !call nvtxStartRange("loop_data")
         !$acc parallel loop
         do i=1,n
            x(i) = mpi_rank + 0.5
            y(i) = 1.5
         end do

         !$acc parallel loop
         do i=1,n
            y(i) = 2.0*x(i)**2.+y(i)**2. - 2.0*x(i)**2.+y(i)**2. + (mpi_rank+1)*1.
         end do
         !call nvtxEndRange

         !call nvtxStartRange("data_transfer")
         !$acc host_data use_device (y,x)
         call MPI_Sendrecv(y, n, MPI_FLOAT, ngb_rank, tag_send, &
                           x, n, MPI_FLOAT, ngb_rank, tag_recv, &
                           MPI_COMM_WORLD, MPI_STATUS_IGNORE, mpi_err)
         !$acc end host_data
         !call nvtxEndRange

      end do
      !call nvtxEndRange
      !$acc end data

      deallocate(x)
      deallocate(y)

   end subroutine test_mpi_cudaware


   subroutine do_crazy_mpi_test(N,numIters)
      implicit none
      integer,intent(in) :: N,numIters
      integer :: iter

      call alloc_vecs(N)

      !call nvtxStartRange("crazy_loop")
      do iter=1,numIters

         !call nvtxStartRange("loop_data")
         call do_crazy_loops(N)
         !call nvtxEndRange

         !call nvtxStartRange("data_transfer")
         call do_crazy_comms(N)
         !call nvtxEndRange

      end do
      !call nvtxEndRange

      call dealloc_vecs()

   end subroutine

   subroutine do_crazy_loops(N)
      implicit none
      integer, intent(in) :: N
      integer :: i

      !$acc parallel loop present(xVec(:),yVec(:))
      do i=1,N
         xVec(i) = mpi_rank + 0.5
         yVec(i) = 1.5
      end do

      !$acc parallel loop present(xVec(:),yVec(:))
      do i=1,N
         yVec(i) = 2.0*xVec(i)**2.+yVec(i)**2. - 2.0*xVec(i)**2.+yVec(i)**2. + (mpi_rank+1)*1.
      end do
   end subroutine do_crazy_loops

   subroutine do_crazy_comms(N)
      implicit none
      integer, intent(in) :: N
      integer :: tag_send,tag_recv,ngb_rank

      tag_send = 0
      tag_recv = tag_send
      ngb_rank = merge(1, 0, mpi_rank.eq.0)

      !$acc host_data use_device (yVec,xVec)
      call MPI_Sendrecv(yVec, N, MPI_FLOAT, ngb_rank, tag_send, &
                        xVec, N, MPI_FLOAT, ngb_rank, tag_recv, &
                        MPI_COMM_WORLD, MPI_STATUS_IGNORE, mpi_err)
      !$acc end host_data
   end subroutine do_crazy_comms

   subroutine alloc_vecs(N)
      implicit none
      integer, intent(in) :: N

      allocate(xVec(N))
      allocate(yVec(N))
      !$acc enter data create(xVec(:))
      !$acc enter data create(yVec(:))
   end subroutine alloc_vecs

   subroutine dealloc_vecs()
      implicit none

      !$acc exit data delete(xVec)
      !$acc exit data delete(yVec)
      deallocate(xVec)
      deallocate(yVec)

   end subroutine dealloc_vecs

end module mod_comms_performance

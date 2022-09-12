module elem_source

   use mod_constants
   use mod_nvtx
   use mod_veclen
   use mod_mpi
   use mod_mpi_mesh
   use mod_hdf5
   use mod_comms

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Computes source term integration                                           !
   ! Added to the rhs                                                           !
   ! This module can be passed to CUDA in order to do fine-grained parallelism. !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

   ! integrates a constant source term (s[ndime]) for each cartessian
   ! direction in the momentum equations 
   subroutine mom_source_const_vect(nelem,npoin,connec,Ngp,dNgp,He,gpvol,u,s,Rmom)


      implicit none

      integer(4), intent(in)    :: nelem, npoin
      integer(4), intent(in)    :: connec(nelem,nnode)
      real(rp),    intent(in)    :: Ngp(ngaus,nnode), dNgp(ndime,nnode,ngaus)
      real(rp),    intent(in)    :: He(ndime,ndime,ngaus,nelem)
      real(rp),    intent(in)    :: gpvol(1,ngaus,nelem)
      real(rp),    intent(in)    :: s(ndime), u(npoin,ndime)
      real(rp),    intent(inout) :: Rmom(npoin,ndime)
      integer(4)                :: ielem, igaus, idime, inode
      real(rp)                   :: Re(nnode,ndime)

      call nvtxStartRange("Momentum source term")

      !oriol: I will assue that you will call
      !this subroutine at least having convection so Rmom is
      !already initialized

      if (flag_SpectralElem == 0) then
         !$acc parallel loop gang private(Re) vector_length(vecLength)
         do ielem = 1,nelem
            !$acc loop vector collapse(2)
            do inode = 1,nnode
               do idime = 1,ndime
                  Re(inode,idime) = 0.0_rp
               end do
            end do
            !$acc loop seq
            do igaus = 1,ngaus
               !$acc loop vector
               do inode = 1,nnode
                  Re(inode,1) = Re(inode,1)+gpvol(1,igaus,ielem)*s(1)
                  Re(inode,2) = Re(inode,2)+gpvol(1,igaus,ielem)*s(2)
                  Re(inode,3) = Re(inode,3)+gpvol(1,igaus,ielem)*s(3)
               end do
            end do
            !
            ! Final assembly
            !
            !$acc loop vector collapse(2)
            do idime = 1,ndime
               do inode = 1,nnode
                  !$acc atomic update
                  Rmom(connec(ielem,inode),idime) = Rmom(connec(ielem,inode),idime)-Re(inode,idime)
                  !$acc end atomic
               end do
            end do
         end do
         !$acc end parallel loop
      else
         !$acc parallel loop gang private(Re) vector_length(vecLength)
         do ielem = 1,nelem
            !$acc loop vector collapse(2)
            do idime = 1,ndime
               do inode = 1,nnode
                  !$acc atomic update
                  Rmom(connec(ielem,inode),idime) = Rmom(connec(ielem,inode),idime)-gpvol(1,inode,ielem)*s(idime)
                  !$acc end atomic
               end do
            end do
         end do
         !$acc end parallel loop
      end if
      call nvtxEndRange

#if 0
      if(mpi_size.ge.2) then
         call nvtxStartRange("MPI_comms_source")
         call update_and_comm_floatField(Rmom(:,1))
         call update_and_comm_floatField(Rmom(:,2))
         call update_and_comm_floatField(Rmom(:,3))
         call nvtxEndRange
      end if
#endif
   end subroutine mom_source_const_vect

end module elem_source
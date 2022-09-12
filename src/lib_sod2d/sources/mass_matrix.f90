module mass_matrix

   use mod_constants
   use mod_nvtx
   use mod_veclen
   use mod_mpi
   use mod_mpi_mesh
   use mod_hdf5
   use mod_comms

      contains

              subroutine consistent_mass(nelem,npoin,connec,nzdom,rdom,cdom,gpvol,Ngp,Mc,weight)

                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                      ! Forms Mc as a sparse CSR matrix, utilizing nzdom, rdom and cdom to          !
                      ! compress the elemental matrices into the full sparse assembly structure.    !
                      ! Mc is defined by A{int(Na*Nb*det(Je))}, where A{} is the assembly operator. !
                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                      implicit none

                      integer(4), intent(in)           :: nelem, npoin, nzdom
                      integer(4), intent(in)           :: connec(nelem,nnode), rdom(npoin+1), cdom(nzdom)
                      real(rp),    intent(in)           :: gpvol(1,ngaus,nelem), Ngp(ngaus,nnode)
                      real(rp),    intent(in), optional :: weight(npoin)
                      real(rp),    intent(out)          :: Mc(nzdom)
                      integer(4)                       :: ielem, igaus, inode, jnode, lnode(nnode), izdom, ipoin, jpoin
                      integer(4)                       :: jzdom, rowb, rowe
                      real(rp)                          :: Me(nnode,nnode), el_w(nnode), tmp

                      !
                      ! Initialize Mc to zeros
                      !
                      call nvtxStartRange("Mass Matrix")
                      Mc = 0.0_rp

                      !
                      ! Loop over all elements to form Mc_e(nnode,nnode)
                      !
                      do ielem = 1,nelem
                         lnode(1:nnode) = connec(ielem,1:nnode) ! get elemental indices
                         if(present(weight))then
                            el_w(1:nnode) = weight(lnode)
                         else
                            el_w(:) = 1.0_rp
                         end if

                         !
                         ! Form Mc_e with Gaussian quadrature (open)
                         !
                         call nvtxStartRange("Elemental Matrix")
                         Me = 0.0_rp
                         do igaus = 1,ngaus ! Loop over Gauss points
                            tmp = dot_product(Ngp(igaus,:),el_w(:))
                            do inode = 1,nnode ! Loop over element nodes (row)
                               do jnode = 1,nnode ! Loop over element nodex (column)
                                  Me(inode,jnode) = Me(inode,jnode) + &
                                     gpvol(1,igaus,ielem)*(tmp)* &
                                     Ngp(igaus,inode)*Ngp(igaus,jnode) ! Gaussian quad.
                               end do
                            end do
                         end do
                         call nvtxEndRange
                         !
                         ! Assemble Mc_e to CSR Mc
                         !
                         call nvtxStartRange("Assembly")
                         do inode = 1,nnode
                            ipoin = lnode(inode) ! Global node/Mc row index
                            rowb = rdom(ipoin)+1 ! Start of izdom for cdom
                            rowe = rdom(ipoin+1) ! end of izdom for cdom
                            do jnode = 1,nnode
                               jpoin = lnode(jnode) ! Nodes associated with ipoin on ielem
                               !
                               ! Loop over section of cdom to find out izdom
                               jzdom = rowb
                               do while ((cdom(jzdom) .ne. jpoin) .and. (jzdom .le. rowe))
                                  jzdom = jzdom+1
                               end do
                               Mc(jzdom) = Mc(jzdom) + Me(inode,jnode)
                            end do
                         end do
                         call nvtxEndRange
                      end do
                      call nvtxEndRange

              end subroutine consistent_mass

              subroutine lumped_mass(nelem,npoin,connec,gpvol,Ngp,Ml)

                      implicit none

                      integer(4), intent(in)           :: nelem, npoin
                      integer(4), intent(in)           :: connec(nelem,nnode)
                      real(rp),    intent(in)           :: gpvol(1,ngaus,nelem), Ngp(ngaus,nnode)
                      real(rp),    intent(out)          :: Ml(npoin)
                      integer(4)                       :: ielem, igaus, inode, jnode
                      real(rp)                          :: Me(nnode), alpha, aux1, aux2, el_w(nnode), T,d

                      !$acc kernels
                      Ml(:) = 0.0_rp
                      !$acc end kernels
#if 1
                      !$acc parallel loop gang private(Me) vector_length(vecLength)
                      do ielem = 1,nelem
                         Me = 0.0_rp
                         aux1 = 0.0_rp
                         aux2 = 0.0_rp
                         !
                         ! tr[Mc] and int(detJe)
                         !
                         !$acc loop seq
                         do igaus = 1,ngaus
                            !$acc loop vector reduction(+:aux1)
                            do inode = 1,nnode
                               aux1 = aux1 + gpvol(1,igaus,ielem)*Ngp(igaus,inode)*Ngp(igaus,inode) ! tr[Mc]
                            end do
                            aux2 = aux2 + gpvol(1,igaus,ielem) ![int(detJe)]
                         end do
                         !
                         ! Me
                         !
                         !$acc loop seq
                         do igaus = 1,ngaus
                            !$acc loop vector
                            do inode = 1,nnode
                               Me(inode) = Me(inode)+gpvol(1,igaus,ielem)*(Ngp(igaus,inode)**2)
                            end do
                         end do
                         !$acc loop vector
                         do inode = 1,nnode
                            !$acc atomic update
                            Ml(connec(ielem,inode)) = Ml(connec(ielem,inode))+((aux2/aux1)*Me(inode))
                            !$acc end atomic
                         end do
                      end do
                      !$acc end parallel loop
#else
                      !$acc parallel loop gang private(Me) vector_length(vecLength)
                      do ielem = 1,nelem
                         !$acc loop vector
                         do inode = 1,nnode
                            Me(inode) = 0.0_rp
                         end do
                         !$acc loop seq
                         do igaus = 1,ngaus
                            !$acc loop vector
                            do inode = 1,nnode
                               Me(inode) = Me(inode) &
                                   +gpvol(1,igaus,ielem)&
                                   *Ngp(igaus,inode)
                            end do
                         end do

                         !$acc loop vector
                         do inode = 1,nnode
                            !$acc atomic update
                            Ml(connec(ielem,inode)) = Ml(connec(ielem,inode))+Me(inode)
                            !$acc end atomic
                         end do
                      end do
                      !$acc end parallel loop
#endif
              end subroutine lumped_mass

              subroutine lumped_mass_spectral(nelem,npoin,connec,gpvol,Ml)

                 implicit none

                 integer(4), intent(in)  :: nelem,npoin, connec(nelem,nnode)
                 real(rp),    intent(in)  :: gpvol(1,ngaus,nelem)
                 real(rp),    intent(out) :: Ml(npoin)
                 integer(4)              :: ielem, inode

                 !$acc kernels
                 Ml(:) = 0.0_rp
                 !$acc end kernels
                 !$acc parallel loop gang vector_length(vecLength)
                 do ielem = 1,nelem
                    !$acc loop vector
                    do inode = 1,nnode
                       !$acc atomic update
                       Ml(connec(ielem,inode)) = Ml(connec(ielem,inode))+gpvol(1,inode,ielem)
                       !$acc end atomic
                    end do
                 end do
                 !$acc end parallel loop

                 if(mpi_size.ge.2) then
                  call nvtxStartRange("MPI_comms_mass")
#if 1
                  call sendRcv_floatField(Ml)
#endif 
#if 0
                  call sendRcv_floatField_devel(Ml)
#endif 
#if 0
                  call sendRcv_floatField_noGPU(Ml)
#endif 
#if 0                  
                  call update_and_comm_floatField(Ml)
#endif
                  call nvtxEndRange
                 end if

              end subroutine lumped_mass_spectral

              subroutine cmass_times_vector(nelem,npoin,connec,gpvol,Ngp,v,Rmc)

                      implicit none

                      integer(4), intent(in)  :: nelem, npoin
                      integer(4), intent(in)  :: connec(nelem,nnode)
                      real(rp),    intent(in)  :: gpvol(1,ngaus,nelem), Ngp(ngaus,nnode), v(npoin)
                      real(rp),    intent(out) :: Rmc(npoin)
                      integer(4)              :: ielem, igaus, inode
                      real(rp)                 :: Re(nnode), tmp2

                      !
                      ! Initialize Mc to zeros
                      !
                      call nvtxStartRange("Cmass times vector")
                      !$acc kernels
                      Rmc(:) = 0.0_rp
                      !$acc end kernels

                      !$acc parallel loop gang private(Re) vector_length(vecLength)
                      do ielem = 1,nelem
                         !$acc loop vector
                         do inode = 1,nnode
                            Re(inode) = 0.0_rp
                         end do
                         !
                         ! Form Re with Gaussian quadrature (open)
                         !
                         !$acc loop seq
                         do igaus = 1,ngaus ! Loop over Gauss points
                            tmp2 = 0.0_rp
                            !$acc loop vector reduction(+:tmp2)
                            do inode = 1,nnode
                               tmp2 = tmp2+Ngp(igaus,inode)*v(connec(ielem,inode)) 
                            end do
                            !$acc loop vector
                            do inode = 1,nnode ! Loop over element nodes (row)
                               Re(inode) = Re(inode)+gpvol(1,igaus,ielem)* &
                                           Ngp(igaus,inode)*tmp2
                            end do
                         end do
                         !$acc loop vector
                         do inode = 1,nnode
                            !$acc atomic update
                            Rmc(connec(ielem,inode)) = Rmc(connec(ielem,inode))+Re(inode)
                            !$acc end atomic
                         end do
                      end do
                      !$acc end parallel loop
                      call nvtxEndRange

              end subroutine cmass_times_vector

              subroutine wcmass_times_vector(nelem,npoin,connec,gpvol,Ngp,v,Rmc,weight)

                      implicit none

                      integer(4), intent(in)  :: nelem, npoin
                      integer(4), intent(in)  :: connec(nelem,nnode)
                      real(rp),    intent(in)  :: gpvol(1,ngaus,nelem), Ngp(ngaus,nnode), v(npoin)
                      real(rp),    intent(in)  :: weight(npoin)
                      real(rp),    intent(out) :: Rmc(npoin)
                      integer(4)              :: ielem, igaus, inode
                      real(rp)                 :: Re(nnode), tmp1, tmp2

                      !
                      ! Initialize Mc to zeros
                      !
                      call nvtxStartRange("Cmass times vector")
                      !$acc kernels
                      Rmc(:) = 0.0_rp
                      !$acc end kernels

                      !
                      ! Loop over all elements to form Mc_e(nnode,nnode)
                      !
                      !$acc parallel loop gang private(Re) vector_length(vecLength)
                      do ielem = 1,nelem
                         !$acc loop vector
                         do inode = 1,nnode
                            Re(inode) = 0.0_rp
                         end do
                         !
                         ! Form Re with Gaussian quadrature (open)
                         !
                         !$acc loop seq
                         do igaus = 1,ngaus ! Loop over Gauss points
                            tmp1 = 0.0_rp
                            tmp2 = 0.0_rp
                            !$acc loop vector reduction(+:tmp1,tmp2)
                            do inode = 1,nnode
                               tmp1 = tmp1+Ngp(igaus,inode)*weight(connec(ielem,inode)) 
                               tmp2 = tmp2+Ngp(igaus,inode)*v(connec(ielem,inode)) 
                            end do
                            !$acc loop vector
                            do inode = 1,nnode 
                               Re(inode) = Re(inode) + &
                                  gpvol(1,igaus,ielem)*tmp1* &
                                  Ngp(igaus,inode)*tmp2
                            end do
                         end do
                         !$acc loop vector
                         do inode = 1,nnode
                            !$acc atomic update
                            Rmc(connec(ielem,inode)) = Rmc(connec(ielem,inode))+Re(inode)
                            !$acc end atomic
                         end do
                      end do
                      !$acc end parallel loop
                      call nvtxEndRange

              end subroutine wcmass_times_vector

end module mass_matrix
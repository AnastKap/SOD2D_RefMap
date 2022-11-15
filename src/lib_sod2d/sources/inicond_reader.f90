module inicond_reader

      use mod_constants
      use mod_utils
      use mod_maths
      use mod_mpi
      use mod_comms

      contains

              subroutine read_veloc_from_file_Srl(npoin,file_path,u)
                      implicit none

                      character(500), intent(in)  :: file_path
                      integer(4)    , intent(in)  :: npoin
                      real(rp)       , intent(out) :: u(npoin,ndime)
                      character(500)              :: file_type, file_name
                      integer(4)                  :: ipoin, ind
                      real(8)                       :: vx,vy,vz

                      write(file_type,*) ".alya"
                      write(file_name,*) "VELOC"
                      if(mpi_rank.eq.0) write(*,*) "--| READING FILE VELOC.alya..."
                      open(99,file=trim(adjustl(file_path))//trim(adjustl(file_name))//trim(adjustl(file_type)),status="old")
                      do ipoin = 1,npoin
                        if (ndime == 2) then
                           read(99,*) ind, u(ipoin,1), u(ipoin,2)
                        else if (ndime == 3) then
                           if(rp == 4) then
                              read(99,*) ind, vx, vy, vz
                              u(ipoin,1)=real(vx,rp)
                              u(ipoin,2)=real(vy,rp) 
                              u(ipoin,3)=real(vz,rp)
                           else
                              read(99,*) ind, u(ipoin,1), u(ipoin,2), u(ipoin,3)
                           end if
                        end if
                      end do
                      close(99)

              end subroutine read_veloc_from_file_Srl

              subroutine read_densi_from_file_Srl(npoin,file_path,rho)

                      implicit none

                      character(500), intent(in)  :: file_path
                      integer(4)    , intent(in)  :: npoin
                      real(rp)       , intent(out) :: rho(npoin)
                      character(500)              :: file_type, file_name
                      integer(4)                  :: ipoin, ind
                      real(8)                       :: x

                      write(file_type,*) ".alya"
                      write(file_name,*) "DENSI"
                      if(mpi_rank.eq.0) write(*,*) "--| READING FILE DENSI.alya..."
                      open(99,file=trim(adjustl(file_path))//trim(adjustl(file_name))//trim(adjustl(file_type)),status="old")
                      do ipoin = 1,npoin
                         if(rp == 4) then
                            read(99,*) ind, x
                            rho(ipoin)=real(x,rp)
                         else
                            read(99,*) ind, rho(ipoin)
                         end if
                      end do
                      close(99)

              end subroutine read_densi_from_file_Srl

              subroutine read_press_from_file_Srl(npoin,file_path,pr)

                      implicit none

                      character(500), intent(in)  :: file_path
                      integer(4)    , intent(in)  :: npoin
                      real(rp)       , intent(out) :: pr(npoin)
                      character(500)              :: file_type, file_name
                      integer(4)                  :: ipoin, ind
                      real(8)                       :: x

                      write(file_type,*) ".alya"
                      write(file_name,*) "PRESS"
                      if(mpi_rank.eq.0) write(*,*) "--| READING FILE PRESS.alya..."
                      open(99,file=trim(adjustl(file_path))//trim(adjustl(file_name))//trim(adjustl(file_type)),status="old")
                      do ipoin = 1,npoin
                         if(rp == 4) then
                            read(99,*) ind, x
                            pr(ipoin)=real(x,rp)
                         else
                            read(99,*) ind, pr(ipoin)
                         end if
                      end do
                      close(99)

              end subroutine read_press_from_file_Srl

              subroutine read_temper_from_file_Srl(npoin,file_path,Tem)

                      implicit none

                      character(500), intent(in)  :: file_path
                      integer(4)    , intent(in)  :: npoin
                      real(rp)       , intent(out) :: Tem(npoin)
                      character(500)              :: file_type, file_name
                      integer(4)                  :: ipoin, ind
                      real(8)                       :: x

                      write(file_type,*) ".alya"
                      write(file_name,*) "TEMPE"
                      if(mpi_rank.eq.0) write(*,*) "--| READING FILE TEMPE.alya..."
                      open(99,file=trim(adjustl(file_path))//trim(adjustl(file_name))//trim(adjustl(file_type)),status="old")
                      do ipoin = 1,npoin
                         if(rp == 4) then
                            read(99,*) ind, x
                            Tem(ipoin)=real(x,rp)
                         else
                            read(99,*) ind, Tem(ipoin)
                         end if
                      end do
                      close(99)

              end subroutine read_temper_from_file_Srl

   subroutine order_matrix_globalIdSrl(numNodesRankPar,globalIdSrl,matGidSrlOrdered)
      implicit none
      integer, intent(in)  :: numNodesRankPar
      integer, intent(in)  :: globalIdSrl(numNodesRankPar)
      integer, intent(out) :: matGidSrlOrdered(numNodesRankPar,2)
      integer :: iNodeL

      do iNodeL=1,numNodesRankPar
         matGidSrlOrdered(iNodeL,1) = iNodeL
         matGidSrlOrdered(iNodeL,2) = globalIdSrl(iNodeL)
      end do

      call quicksort_matrix_int(matGidSrlOrdered,2)

   end subroutine order_matrix_globalIdSrl

   subroutine avg_randomField_in_sharedNodes_Par(numNodesRankPar,floatField)
      implicit none
      integer, intent(in) :: numNodesRankPar
      real(rp), intent(inout) :: floatField(numNodesRankPar)
      integer :: numRanksNodeCnt(numNodesRankPar)
      integer :: i,iNodeL

      numRanksNodeCnt(:)=1

      do i= 1,numNodesToComm
         iNodeL = matrixCommScheme(i,1)
         numRanksNodeCnt(iNodeL) = numRanksNodeCnt(iNodeL) + 1
      end do 

      call mpi_halo_atomic_update_float(floatField)

      do iNodeL = 1,numNodesRankPar
         floatField(iNodeL) = floatField(iNodeL) / real(numRanksNodeCnt(iNodeL),rp)
      end do
   end subroutine avg_randomField_in_sharedNodes_Par

   subroutine read_densi_from_file_Par(numElemsInRank,numNodesRankPar,totalNumNodesSrl,file_path,rho,connecParOrig,Ngp_l,matGidSrlOrdered)
      implicit none
      character(500), intent(in) :: file_path
      integer(4), intent(in)     :: numElemsInRank,numNodesRankPar,totalNumNodesSrl
      real(rp), intent(out)      :: rho(numNodesRankPar)
      integer(4), intent(in)     :: connecParOrig(numElemsInRank,nnode)
      real(rp), intent(in)       :: Ngp_l(ngaus,nnode)
      integer, intent(in)        :: matGidSrlOrdered(numNodesRankPar,2)
      character(500)             :: file_type, file_name
      integer(4)                 :: iLine,iNodeL,iElem,iNodeGSrl,igp,idime,auxCnt,readInd
      real(8)                    :: readValue
      real(rp)                   :: aux_2(numNodesRankPar)

      write(file_type,*) ".alya"
      write(file_name,*) "DENSI"
      if(mpi_rank.eq.0) write(*,*) "--| Reading file DENSI.alya in parallel..."

      open(99,file=trim(adjustl(file_path))//trim(adjustl(file_name))//trim(adjustl(file_type)),status="old")
      
      auxCnt = 1
      do iLine = 1,totalNumNodesSrl
         read(99,*) readInd, readValue
         if(iLine.eq.matGidSrlOrdered(auxCnt,2)) then
            iNodeL = matGidSrlOrdered(auxCnt,1)
            auxCnt=auxCnt+1
            if(rp.eq.4) then
               rho(iNodeL) = real(readValue,rp)
            else
               rho(iNodeL) = readValue
            end if
         end if         
      end do

      close(99)

      if(mpi_rank.eq.0) write(*,*) "--| Interpolating density from file coords to new mesh coords..."
      aux_2(:) = rho(:)
      do iElem = 1,numElemsInRank
         do igp = (2**ndime)+1,nnode
            call var_interpolate(aux_2(connecParOrig(iElem,:)),Ngp_l(igp,:),rho(connecParOrig(iElem,igp)))
         end do
      end do

   end subroutine read_densi_from_file_Par

   subroutine read_veloc_from_file_Par(numElemsInRank,numNodesRankPar,totalNumNodesSrl,file_path,u,connecParOrig,Ngp_l,matGidSrlOrdered)
      implicit none
      character(500), intent(in) :: file_path
      integer(4), intent(in)     :: numElemsInRank,numNodesRankPar,totalNumNodesSrl
      real(rp), intent(out)      :: u(numNodesRankPar,ndime)
      integer(4), intent(in)     :: connecParOrig(numElemsInRank,nnode)
      real(rp), intent(in)       :: Ngp_l(ngaus,nnode)
      integer, intent(in)        :: matGidSrlOrdered(numNodesRankPar,2)
      character(500)             :: file_type, file_name
      integer(4)                 :: iLine,iNodeL,iElem,iNodeGSrl,igp,idime,auxCnt,readInd
      real(8)                    :: readValue_ux,readValue_uy,readValue_uz
      real(rp)                   :: aux_1(numNodesRankPar,ndime)

      write(file_type,*) ".alya"
      write(file_name,*) "VELOC"
      if(mpi_rank.eq.0) write(*,*) "--| Reading file VELOC.alya in parallel..."

      open(99,file=trim(adjustl(file_path))//trim(adjustl(file_name))//trim(adjustl(file_type)),status="old")
      
      auxCnt = 1
      do iLine = 1,totalNumNodesSrl
         read(99,*) readInd, readValue_ux, readValue_uy, readValue_uz
         if(iLine.eq.matGidSrlOrdered(auxCnt,2)) then
            iNodeL = matGidSrlOrdered(auxCnt,1)
            auxCnt=auxCnt+1
            if(rp.eq.4) then
               u(iNodeL,1)=real(readValue_ux,rp)
               u(iNodeL,2)=real(readValue_uy,rp)
               u(iNodeL,3)=real(readValue_uz,rp)
            else
               u(iNodeL,1)=readValue_ux
               u(iNodeL,2)=readValue_uy
               u(iNodeL,3)=readValue_uz
            end if
         end if         
      end do
      
      close(99)

      if(mpi_rank.eq.0) write(*,*) "--| Interpolating velocity from file coords to new mesh coords..."
      aux_1(:,:) = u(:,:)
      do iElem = 1,numElemsInRank
         do igp = (2**ndime)+1,nnode
            do idime = 1,ndime
               call var_interpolate(aux_1(connecParOrig(iElem,:),idime),Ngp_l(igp,:),u(connecParOrig(iElem,igp),idime))
            end do
         end do
      end do

   end subroutine read_veloc_from_file_Par

   subroutine read_press_from_file_Par(numElemsInRank,numNodesRankPar,totalNumNodesSrl,file_path,pr,connecParOrig,Ngp_l,matGidSrlOrdered)
      implicit none
      character(500), intent(in) :: file_path
      integer(4), intent(in)     :: numElemsInRank,numNodesRankPar,totalNumNodesSrl
      real(rp), intent(out)      :: pr(numNodesRankPar)
      integer(4), intent(in)     :: connecParOrig(numElemsInRank,nnode)
      real(rp), intent(in)       :: Ngp_l(ngaus,nnode)
      integer, intent(in)        :: matGidSrlOrdered(numNodesRankPar,2)
      character(500)             :: file_type, file_name
      integer(4)                 :: iLine,iNodeL,iElem,iNodeGSrl,igp,idime,auxCnt,readInd
      real(8)                    :: readValue
      real(rp)                   :: aux_2(numNodesRankPar)

      write(file_type,*) ".alya"
      write(file_name,*) "PRESS"
      if(mpi_rank.eq.0) write(*,*) "--| Reading file PRESS.alya in parallel..."

      open(99,file=trim(adjustl(file_path))//trim(adjustl(file_name))//trim(adjustl(file_type)),status="old")
      
      auxCnt = 1
      do iLine = 1,totalNumNodesSrl
         read(99,*) readInd, readValue
         if(iLine.eq.matGidSrlOrdered(auxCnt,2)) then
            iNodeL = matGidSrlOrdered(auxCnt,1)
            auxCnt=auxCnt+1
            if(rp.eq.4) then
               pr(iNodeL) = real(readValue,rp)
            else
               pr(iNodeL) = readValue
            end if
         end if         
      end do
      
      close(99)

      if(mpi_rank.eq.0) write(*,*) "--| Interpolating pressure from file coords to new mesh coords..."
      aux_2(:) = pr(:)
      do iElem = 1,numElemsInRank
         do igp = (2**ndime)+1,nnode
            call var_interpolate(aux_2(connecParOrig(iElem,:)),Ngp_l(igp,:),pr(connecParOrig(iElem,igp)))
         end do
      end do

   end subroutine read_press_from_file_Par

   subroutine read_temper_from_file_Par(numElemsInRank,numNodesRankPar,totalNumNodesSrl,file_path,temp,connecParOrig,Ngp_l,matGidSrlOrdered)
      implicit none
      character(500), intent(in) :: file_path
      integer(4), intent(in)     :: numElemsInRank,numNodesRankPar,totalNumNodesSrl
      real(rp), intent(out)      :: temp(numNodesRankPar)
      integer(4), intent(in)     :: connecParOrig(numElemsInRank,nnode)
      real(rp), intent(in)       :: Ngp_l(ngaus,nnode)
      integer, intent(in)        :: matGidSrlOrdered(numNodesRankPar,2)
      character(500)             :: file_type, file_name
      integer(4)                 :: iLine,iNodeL,iElem,iNodeGSrl,igp,idime,auxCnt,readInd
      real(8)                    :: readValue
      real(rp)                   :: aux_2(numNodesRankPar)

      write(file_type,*) ".alya"
      write(file_name,*) "TEMPE"
      if(mpi_rank.eq.0) write(*,*) "--| Reading file TEMPE.alya in parallel..."

      open(99,file=trim(adjustl(file_path))//trim(adjustl(file_name))//trim(adjustl(file_type)),status="old")
      
      auxCnt = 1
      do iLine = 1,totalNumNodesSrl
         read(99,*) readInd, readValue
         if(iLine.eq.matGidSrlOrdered(auxCnt,2)) then
            iNodeL = matGidSrlOrdered(auxCnt,1)
            auxCnt=auxCnt+1
            if(rp.eq.4) then
               temp(iNodeL) = real(readValue,rp)
            else
               temp(iNodeL) = readValue
            end if
         end if         
      end do
      
      close(99)

      if(mpi_rank.eq.0) write(*,*) "--| Interpolating temperature from file coords to new mesh coords..."
      aux_2(:) = temp(:)
      do iElem = 1,numElemsInRank
         do igp = (2**ndime)+1,nnode
            call var_interpolate(aux_2(connecParOrig(iElem,:)),Ngp_l(igp,:),temp(connecParOrig(iElem,igp)))
         end do
      end do

   end subroutine read_temper_from_file_Par

end module inicond_reader

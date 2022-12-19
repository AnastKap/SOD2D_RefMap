
program tool_meshConversorPar
    use mod_mpi
    use mod_read_inputFile
    use mod_meshConversorTool
    !use mod_hdf5

    implicit none

    character(999) :: input_file
    character(512) :: gmsh_filePath,gmsh_fileName
    character(512) :: mesh_h5_filePath,mesh_h5_fileName
    character(256) :: parameter2read
    integer :: lineCnt

    integer :: num_partitions
    logical :: isPeriodic=.false.
    logical :: useIntInComms=.true.,useFloatInComms=.true.,useDoubleInComms=.false.

!------------------------------------------------------------------------------------------------------

    call init_mpi()

    if(mpi_rank.eq.0) then
        write(*,*) '|-- WELCOME TO THE AWESOME MESH CONVERSION PARALLEL TOOL ! ;)'
        write(*,*) '|-- From GMSH to Sod2D format'
    end if

    !------------------------------------------------------------------------------
    ! Reading input file
    if(command_argument_count() .eq. 1) then
        call get_command_argument(1, input_file)
        if(mpi_rank.eq.0) write(*,*) 'Input file: ',trim(adjustl(input_file))
    else
        if(mpi_rank.eq.0) write(*,*) 'You must call this amazing tool with an input file!!!'
        call MPI_Abort(MPI_COMM_WORLD,-1,mpi_err)
    endif
    !------------------------------------------------------------------------------
    !------------------------------------------------------------------------------
    ! Reading the parameters
    call open_inputFile(input_file)
    lineCnt = 1

    !1. gmsh_filePath--------------------------------------------------------------
    parameter2read = 'gmsh_filePath'
    call read_inputFile_string(lineCnt,parameter2read,gmsh_filePath)

    !2. gmsh_fileName--------------------------------------------------------------
    parameter2read = 'gmsh_fileName'
    call read_inputFile_string(lineCnt,parameter2read,gmsh_fileName)

    !3. mesh_h5_filePath--------------------------------------------------------------
    parameter2read = 'mesh_h5_filePath'
    call read_inputFile_string(lineCnt,parameter2read,mesh_h5_filePath)

    !4. mesh_h5_fileName--------------------------------------------------------------
    parameter2read = 'mesh_h5_fileName'
    call read_inputFile_string(lineCnt,parameter2read,mesh_h5_fileName)

    !6. num_partitions--------------------------------------------------------------------------
    parameter2read = 'num_partitions'
    call read_inputFile_integer(lineCnt,parameter2read,num_partitions)

    close(99)
    if(mpi_rank.eq.0) write(*,*) '## End of Reading input file: ',trim(adjustl(input_file))

!---------------------------------------------------------------------------------------------------------

    !call init_hdf5_interface()
    !call set_hdf5_meshFile_name(mesh_h5_filePath,mesh_h5_fileName)

    !vars to be read by chunks




    !-- read the alya mesh fesh files in GMSH/ALYA FORMAT
    !-- & do the partitioning
    call read_gmsh_files_and_do_partitioning_in_parallel(gmsh_filePath,gmsh_fileName,isPeriodic,num_partitions)

#if 0

    !----- init comms
    call init_comms(useIntInComms,useFloatInComms,useDoubleInComms)

    !----- for boundaries
    if(isMeshBoundaries) then
       call splitBoundary_inPar()
       call generate_boundary_mpi_comm_scheme()
       !----- init comms boundaries
       call init_comms_bnd(useIntInComms,useFloatInComms,useDoubleInComms)
    end if
    !----- Deallocate alya/gmsh arrays
    call deallocate_read_alya_mesh_arrays()
    !----- Create HDF5 File
    call create_hdf5_meshFile()
#endif

    !call end_hdf5_interface()
!---------------------------------------------------------------------------------------------------------

    call end_mpi()

end program tool_meshConversorPar

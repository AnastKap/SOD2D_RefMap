#define average 1

program tool_hdf5_to_cgns
    use mod_mpi
    use mod_mpi_mesh
    use mod_hdf5
    use mod_cgns_mesh
    implicit none

    character(512) :: mesh_h5_file_path,mesh_h5_file_name
    character(512) :: cgns_file_path,cgns_file_name
    character(512) :: results_h5_file_path,results_h5_file_name
    character(128) :: res_string
    character(999) :: full_fileName
    !logical :: periodic,loadMesh

    integer :: first_step,last_step,nstep,iStep
    real(rp) :: time
#if(average)
    real(rp), allocatable :: avrho(:),avpre(:),avmueff(:)
    real(rp), allocatable :: avvel(:,:),avve2(:,:)

#else
    real(rp), allocatable :: rho(:),pr(:),E(:),eta(:),csound(:),machno(:),divU(:),Qcrit(:)
    real(rp), allocatable :: envit(:),mut(:),mu_fluid(:)
    real(rp), allocatable :: u(:,:),gradRho(:,:),curlU(:,:)
#endif

!------------------------------------------------------------------------------------------------------

    write(mesh_h5_file_path,*) ""
    write(mesh_h5_file_name,*) "channel_sem"

    write(cgns_file_path,*) ""
    write(cgns_file_name,*) "channel_sem"

#if(average)
    write(results_h5_file_path,*) ""
    write(results_h5_file_name,*) "resultsFile"

    write(res_string,*) "resultsFile_AVG"
#else
    write(results_h5_file_path,*) ""
    write(results_h5_file_name,*) "resultsFile"

    write(res_string,*) "resultsFile"
#endif

    first_step = 2760001
    last_step  = 2760001
    nstep      = 1

!------------------------------------------------------------------------------------------------------
    call init_mpi()

    if(mpi_rank.eq.0) write(*,*) '## CONVERSION TOOL HDF5 -> CGNS ##'

    call init_hdf5_interface(mesh_h5_file_path,mesh_h5_file_name,results_h5_file_path,results_h5_file_name)

    if(mpi_rank.eq.0) write(*,*) '## LOADING HDF5 MESH FILE... ##'
    call load_hdf5_meshfile(mesh_h5_file_path,mesh_h5_file_name)

#if(average)
    allocate(avrho(numNodesRankPar))
    allocate(avpre(numNodesRankPar))
    allocate(avmueff(numNodesRankPar))
    allocate(avvel(numNodesRankPar,ndime))
    allocate(avve2(numNodesRankPar,ndime))
#else
    allocate(rho(numNodesRankPar))
    allocate(pr(numNodesRankPar))
    allocate(E(numNodesRankPar))
    allocate(eta(numNodesRankPar))
    allocate(csound(numNodesRankPar))
    allocate(machno(numNodesRankPar))
    allocate(divU(numNodesRankPar))
    allocate(Qcrit(numNodesRankPar))
    allocate(envit(numNodesRankPar))
    allocate(mut(numNodesRankPar))
    allocate(mu_fluid(numNodesRankPar))
    allocate(u(numNodesRankPar,ndime))
    allocate(gradRho(numNodesRankPar,ndime))
    allocate(curlU(numNodesRankPar,ndime))
#endif

    call init_CGNSmesh_arrays()

    !here the loop!
    do iStep=first_step,last_step,nstep
        if(mpi_rank.eq.0) write(*,*) '## Doing iStep',iStep   

        call set_CGNS_full_fileName(full_fileName,cgns_file_path,cgns_file_name,res_string,iStep)

        if(mpi_rank.eq.0) write(*,*) '## LOADING HDF5 RESULTS FILE... ##'

#if(average)
        call load_hdf5_avgResultsFile(iStep,avvel,avve2,avrho,avpre,avmueff)
#else
        call load_hdf5_resultsFile_allArrays(iStep,time,rho,u,pr,E,eta,csound,machno,gradRho,curlU,divU,Qcrit,mu_fluid,envit,mut)
#endif

        if(mpi_rank.eq.0) write(*,*) '## CREATING CGNS FILE... ##'
        call create_CGNSmesh_par(full_fileName)
#if(average)
        call add_write_floatField_CGNSmesh_vertexSolution('avrho',avrho)
        call add_write_floatField_CGNSmesh_vertexSolution('avpre',avpre)
        call add_write_floatField_CGNSmesh_vertexSolution('avmueff',avmueff)
        call add_write_floatField_CGNSmesh_vertexSolution('avvelX',avvel(:,1))
        call add_write_floatField_CGNSmesh_vertexSolution('avvelY',avvel(:,2))
        call add_write_floatField_CGNSmesh_vertexSolution('avvelZ',avvel(:,3))
        call add_write_floatField_CGNSmesh_vertexSolution('avve2X',avve2(:,1))
        call add_write_floatField_CGNSmesh_vertexSolution('avve2Y',avve2(:,2))
        call add_write_floatField_CGNSmesh_vertexSolution('avve2Z',avve2(:,3))
#else
        call add_write_floatField_CGNSmesh_vertexSolution('rho',rho)
        call add_write_floatField_CGNSmesh_vertexSolution('VelocityX',u(:,1))
        call add_write_floatField_CGNSmesh_vertexSolution('VelocityY',u(:,2))
        call add_write_floatField_CGNSmesh_vertexSolution('VelocityZ',u(:,3))
        call add_write_floatField_CGNSmesh_vertexSolution('E',E)
        call add_write_floatField_CGNSmesh_vertexSolution('pr',pr)
        call add_write_floatField_CGNSmesh_vertexSolution('eta',eta)
        call add_write_floatField_CGNSmesh_vertexSolution('csound',csound)
        call add_write_floatField_CGNSmesh_vertexSolution('machno',machno)
        call add_write_floatField_CGNSmesh_vertexSolution('gradRhoX',gradRho(:,1))
        call add_write_floatField_CGNSmesh_vertexSolution('gradRhoY',gradRho(:,2))
        call add_write_floatField_CGNSmesh_vertexSolution('gradRhoZ',gradRho(:,3))
        call add_write_floatField_CGNSmesh_vertexSolution('curlUX',curlU(:,1))
        call add_write_floatField_CGNSmesh_vertexSolution('curlUY',curlU(:,2))
        call add_write_floatField_CGNSmesh_vertexSolution('curlUZ',curlU(:,3))
        call add_write_floatField_CGNSmesh_vertexSolution('divU',divU)
        call add_write_floatField_CGNSmesh_vertexSolution('Qcrit',Qcrit)
        call add_write_floatField_CGNSmesh_vertexSolution('mu_fluid',mu_fluid)
        call add_write_floatField_CGNSmesh_vertexSolution('envit',envit)
        call add_write_floatField_CGNSmesh_vertexSolution('mut',mut)
#endif

        call close_CGNSmesh_par()
    end do

    call end_CGNSmesh_arrays()

    call end_hdf5_interface()

    call end_mpi()

end program tool_hdf5_to_cgns

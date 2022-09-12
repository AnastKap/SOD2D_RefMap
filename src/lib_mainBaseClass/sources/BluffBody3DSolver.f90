module BluffBody3DSolver_mod
   use mod_arrays
   use mod_nvtx
#ifndef NOACC
   use cudafor
#endif
   use mod_veclen

   use elem_qua
   use elem_hex
   use jacobian_oper
   use quadrature_rules
   use mesh_reader
   use inicond_reader
   use mass_matrix
   use mod_geom
   use mod_output
   use mod_period
   use time_integ
   use mod_analysis
   use mod_constants
   use mod_time_ops
   use mod_fluid_viscosity
   use mod_postpro
   use mod_aver
   use mod_mpi
   use mod_mpi_mesh
   use mod_hdf5
   use CFDSolver3DWithBoundaries_mod
   implicit none
   private

   type, public, extends(CFDSolver3DWithBoundaries) :: BluffBody3DSolver

      real(rp) , public  :: vo, M, delta, rho0, Re, to, po

   contains
      procedure, public :: initializeParameters  => BluffBody3DSolver_initializeParameters
      procedure, public :: evalInitialConditions => BluffBody3DSolver_evalInitialConditions
   end type BluffBody3DSolver
contains

   subroutine BluffBody3DSolver_initializeParameters(this)
      class(BluffBody3DSolver), intent(inout) :: this
      real(rp) :: mul, mur

      write(this%gmsh_file_path,*) "./mesh/"
      write(this%gmsh_file_name,*) "auto" 

      write(this%mesh_h5_file_path,*) ""
      write(this%mesh_h5_file_name,*) "auto"

      this%isPeriodic = .false.
      this%loadMesh = .false.

      this%nstep = 2000
      this%cfl_conv = 0.85_rp
      this%cfl_diff = 0.85_rp
      this%nsave  = 1  ! First step to save, TODO: input
      this%nsave2 = 1   ! First step to save, TODO: input
      this%nsaveAVG = 1
      this%nleap = 250 ! Saving interval, TODO: input
      this%tleap = 0.5_rp ! Saving interval, TODO: input
      this%nleap2 = 5  ! Saving interval, TODO: input
      this%nleapAVG = 2000

      this%Cp = 1004.0_rp
      this%Prt = 0.71_rp
      this%vo = 1.0_rp
      this%M  = 0.2_rp
      this%delta  = 1.0_rp
      this%rho0   = 1.0_rp
      this%gamma_gas = 1.40_rp
      this%Re     =  2900.0_rp
      !this%Re     =  2900000.0_rp

      mul    = (this%rho0*this%delta*this%vo)/this%Re
      this%Rgas = this%Cp*(this%gamma_gas-1.0_rp)/this%gamma_gas
      this%to = this%vo*this%vo/(this%gamma_gas*this%Rgas*this%M*this%M)
      this%po = this%rho0*this%Rgas*this%to
      mur = 0.000001458_rp*(this%to**1.50_rp)/(this%to+110.40_rp)
      flag_mu_factor = mul/mur

      nscbc_u_inf = this%vo
      nscbc_p_inf = this%po
      nscbc_rho_inf = this%rho0
      nscbc_gamma_inf = this%gamma_gas
      nscbc_c_inf = sqrt(this%gamma_gas*this%po/this%rho0)

   end subroutine BluffBody3DSolver_initializeParameters

   subroutine BluffBody3DSolver_evalInitialConditions(this)
      class(BluffBody3DSolver), intent(inout) :: this
      integer(rp) :: matGidSrlOrdered(numNodesRankPar,2)
      integer(4) :: iNodeL
      logical :: readFiles

      readFiles = .false.

      if(readFiles) then
         this%interpInitialResults = .true.
         call order_matrix_globalIdSrl(numNodesRankPar,globalIdSrl,matGidSrlOrdered)
         call read_veloc_from_file_Par(numNodesRankPar,totalNumNodesSrl,this%gmsh_file_path,u(:,:,2),matGidSrlOrdered)
      else
         !$acc parallel loop
         do iNodeL = 1,numNodesRankPar
              u(iNodeL,1,2) = this%vo
              u(iNodeL,2,2) = 0.0_rp
              u(iNodeL,3,2) = 0.0_rp
         end do
         !$acc end parallel loop
      end if

      !$acc parallel loop
      do iNodeL = 1,numNodesRankPar
         pr(iNodeL,2) = this%po
         rho(iNodeL,2) = this%po/this%Rgas/this%to
         e_int(iNodeL,2) = pr(iNodeL,2)/(rho(iNodeL,2)*(this%gamma_gas-1.0_rp))
         Tem(iNodeL,2) = pr(iNodeL,2)/(rho(iNodeL,2)*this%Rgas)
         E(iNodeL,2) = rho(iNodeL,2)*(0.5_rp*dot_product(u(iNodeL,:,2),u(iNodeL,:,2))+e_int(iNodeL,2))
         q(iNodeL,1:ndime,2) = rho(iNodeL,2)*u(iNodeL,1:ndime,2)
         csound(iNodeL) = sqrt(this%gamma_gas*pr(iNodeL,2)/rho(iNodeL,2))
      end do
      !$acc end parallel loop

      !$acc parallel loop
      do iNodeL = 1,numNodesRankPar
         machno(iNodeL) = dot_product(u(iNodeL,:,2),u(iNodeL,:,2))/csound(iNodeL)
      end do
      !$acc end parallel loop

      !$acc kernels
      mu_e(:,:) = 0.0_rp ! Element syabilization viscosity
      mu_sgs(:,:) = 0.0_rp
      kres(:) = 0.0_rp
      etot(:) = 0.0_rp
      ax1(:) = 0.0_rp
      ax2(:) = 0.0_rp
      ax3(:) = 0.0_rp
      au(:,:) = 0.0_rp
      !$acc end kernels
      call nvtxEndRange

      ! set out of the buffer zone
      ! remember that the mu_factor field has to we filled at least with the
      ! flag_mu_factor

      !$acc parallel loop
      do iNodeL = 1,numNodesRankPar
         mu_factor(iNodeL) = flag_mu_factor
        if(coordPar(iNodeL,1)<-4.0_rp) then
           mu_factor(iNodeL) = flag_mu_factor*100.0_rp
        end if
        if(coordPar(iNodeL,1)>5.0_rp) then
           mu_factor(iNodeL) = flag_mu_factor*100.0_rp
        end if
      end do
      !$acc end parallel loop
   end subroutine BluffBody3DSolver_evalInitialConditions

end module BluffBody3DSolver_mod
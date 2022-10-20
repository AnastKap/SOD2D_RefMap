module time_integ

   use mod_nvtx
   use elem_convec
   use elem_diffu
   use elem_source
   use mod_solver
   use mod_entropy_viscosity
   use mod_constants
   use mod_fluid_viscosity
   use mod_sgs_viscosity
   use mod_sgs_ilsa_viscosity
   use mod_bc_routines
   use mod_wall_model

      contains

         subroutine rk_4_main(noBoundaries,flag_predic,flag_emac,nelem,nboun,npoin,npoin_w,point2elem,lnbn,lnbn_nodes,dlxigp_ip,xgp,atoIJK,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,&
                         ppow,connec,Ngp,dNgp,He,Ml,gpvol,dt,helem,helem_l,Rgas,gamma_gas,Cp,Prt, &
                         rho,u,u_wall,rho_wall,mu_wall,q,pr,E,Tem,csound,machno,e_int,eta,mu_e,mu_sgs,kres,etot,au,ax1,ax2,ax3,lpoin_w,mu_fluid,mu_factor, &
                         ndof,nbnodes,ldof,lbnodes,bound,bou_codes,bou_codes_nodes,numBoundCodes,wgp_b, bounorm,bouCodes2WallModel,coord,normalsAtNodes,source_term) ! Optional arg

            implicit none

            logical,              intent(in)   :: noBoundaries
            integer(4),           intent(in)    :: flag_predic, flag_emac
            integer(4),           intent(in)    :: nelem, nboun, npoin
            integer(4),           intent(in)    :: connec(nelem,nnode), npoin_w, lpoin_w(npoin_w),point2elem(npoin),lnbn(nboun,npbou),lnbn_nodes(npoin)
            integer(4),           intent(in)    :: atoIJK(nnode),invAtoIJK(porder+1,porder+1,porder+1),gmshAtoI(nnode), gmshAtoJ(nnode), gmshAtoK(nnode)
            integer(4),           intent(in)    :: ppow
            real(rp),              intent(in)    :: Ngp(ngaus,nnode), dNgp(ndime,nnode,ngaus),dlxigp_ip(ngaus,ndime,porder+1)
            real(rp),              intent(in)    :: He(ndime,ndime,ngaus,nelem),xgp(ngaus,ndime)
            real(rp),              intent(in)    :: gpvol(1,ngaus,nelem)
            real(rp),              intent(in)    :: dt, helem(nelem) !helem_l(npoin) TO REVIEW I THINK IS BUG!
            real(rp),              intent(in)    :: helem_l(nelem,nnode)
            real(rp),              intent(in)    :: Ml(npoin)
            real(rp),              intent(in)    :: mu_factor(npoin)
            real(rp),              intent(in)    :: Rgas, gamma_gas, Cp, Prt
            real(rp),              intent(inout) :: rho(npoin,2)
            real(rp),              intent(inout) :: u(npoin,ndime,2)
            real(rp),              intent(inout) :: u_wall(npoin,ndime),rho_wall(npoin),mu_wall(npoin)
            real(rp),              intent(inout) :: q(npoin,ndime,2)
            real(rp),              intent(inout) :: pr(npoin,2)
            real(rp),              intent(inout) :: E(npoin,2)
            real(rp),              intent(inout) :: Tem(npoin,2)
            real(rp),              intent(inout) :: e_int(npoin,2)
            real(rp),              intent(inout) :: eta(npoin,2)
            real(rp),              intent(inout) :: mu_fluid(npoin)
            real(rp),              intent(inout)   :: csound(npoin)
            real(rp),              intent(inout)   :: machno(npoin)
            real(rp),              intent(inout)   :: mu_e(nelem,ngaus)
            real(rp),              intent(inout)   :: mu_sgs(nelem,ngaus)
            real(rp),              intent(inout)   :: kres(npoin)
            real(rp),              intent(inout)   :: etot(npoin)
            real(rp),              intent(inout)   :: au(npoin,ndime)
            real(rp),              intent(inout)   :: ax1(npoin)
            real(rp),              intent(inout)   :: ax2(npoin)
            real(rp),              intent(inout)   :: ax3(npoin)
            integer(4), optional, intent(in)    :: ndof, nbnodes, ldof(*), lbnodes(*)
            integer(4), optional, intent(in)    :: bound(nboun,npbou), bou_codes(nboun), bou_codes_nodes(npoin),numBoundCodes,bouCodes2WallModel(*)
            real(rp), optional, intent(in)    :: wgp_b(npbou), bounorm(nboun,ndime*npbou),coord(npoin,ndime),normalsAtNodes(npoin,ndime)
            real(rp),    optional, intent(in)    :: source_term(ndime)
            integer(4)                          :: pos
            integer(4)                          :: istep, ipoin, idime,icode
            real(rp),    dimension(npoin)        :: Reta, Rrho
            real(rp),    dimension(4)            :: a_i, b_i, c_i
            real(rp),    dimension(npoin,ndime)  :: aux_u, aux_q,aux_u_wall
            real(rp),    dimension(npoin)        :: aux_rho, aux_pr, aux_E, aux_Tem, aux_e_int,aux_eta
            real(rp),    dimension(npoin)        :: Rmass, Rener, Rmass_sum, Rener_sum, alpha,Reta_sum
            real(rp),    dimension(npoin,ndime)  :: Rmom, Rmom_sum, f_eta
            real(rp)                             :: Rdiff_mass(npoin), Rdiff_mom(npoin,ndime), Rdiff_ener(npoin),Rdiff_mom_wall(npoin,ndime)
            real(rp)                             :: Aemac(npoin,ndime), Femac(npoin), umag

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! New version of RK4 using loops                 !
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !
            ! Choose between updating prediction or correction
            !
            pos = 2 ! Set correction as default value
            if (flag_predic == 1) then
               pos = 1 ! Change to prediction update
            end if
            !
            ! Butcher tableau
            !
            call nvtxStartRange("Create tableau")
            if (flag_rk_order == 1) then
               a_i = [0.0_rp, 0.0_rp, 0.0_rp, 0.0_rp]
               c_i = [0.0_rp, 0.0_rp, 0.0_rp, 0.0_rp]
               b_i = [1.0_rp, 0.0_rp, 0.0_rp, 0.0_rp]
            else if (flag_rk_order == 2) then
               a_i = [0.0_rp, 1.0_rp, 0.0_rp, 0.0_rp]
               c_i = [0.0_rp, 1.0_rp, 0.0_rp, 0.0_rp]
               b_i = [0.5_rp, 0.5_rp, 0.0_rp, 0.0_rp]
            else if (flag_rk_order == 3) then
               write(1,*) "--| NOT CODED FOR RK3 YET!"
               STOP(1)
            else if (flag_rk_order == 4) then
               a_i = [0.0_rp, 0.5_rp, 0.5_rp, 1.0_rp]
               c_i = [0.0_rp, 0.5_rp, 0.5_rp, 1.0_rp]
               b_i = [1.0_rp/6.0_rp, 1.0_rp/3.0_rp, 1.0_rp/3.0_rp, 1.0_rp/6.0_rp]
            else
               write(1,*) "--| NOT CODED FOR RK > 4 YET!"
               STOP(1)
            end if
            call nvtxEndRange
            !
            ! Initialize variables to zero
            !
            call nvtxStartRange("Initialize variables")
            !$acc kernels
            aux_rho(1:npoin) = 0.0_rp
            aux_u(1:npoin,1:ndime) = 0.0_rp
            aux_q(1:npoin,1:ndime) = 0.0_rp
            aux_pr(1:npoin) = 0.0_rp
            aux_E(1:npoin) = 0.0_rp
            aux_Tem(1:npoin) = 0.0_rp
            aux_e_int(1:npoin) = 0.0_rp
            aux_eta(1:npoin) = 0.0_rp
            Rdiff_mass(1:npoin) = 0.0_rp
            Rdiff_mom(1:npoin,1:ndime) = 0.0_rp
            Rdiff_ener(1:npoin) = 0.0_rp
            Rmass(1:npoin) = 0.0_rp
            Rmom(1:npoin,1:ndime) = 0.0_rp
            Rener(1:npoin) = 0.0_rp
            Reta(1:npoin) = 0.0_rp
            Rmass_sum(1:npoin) = 0.0_rp
            Rener_sum(1:npoin) = 0.0_rp
            Reta_sum(1:npoin) = 0.0_rp
            Rmom_sum(1:npoin,1:ndime) = 0.0_rp
            !$acc end kernels

            call nvtxEndRange
            !
            ! Loop over all RK steps
            !
            call nvtxStartRange("Loop over RK steps")

            do istep = 1,flag_rk_order
               !
               ! Compute variable at substep (y_i = y_n+dt*A_ij*R_j)
               !
               call nvtxStartRange("Update aux_*")
               !$acc parallel loop
               do ipoin = 1,npoin
                  aux_rho(ipoin) = rho(ipoin,pos) - dt*a_i(istep)*Rmass(ipoin)
                  aux_E(ipoin)   = E(ipoin,pos)   - dt*a_i(istep)*Rener(ipoin)
                  !$acc loop seq
                  do idime = 1,ndime
                     aux_q(ipoin,idime) = q(ipoin,idime,pos) - dt*a_i(istep)*Rmom(ipoin,idime)
                  end do
               end do
               !$acc end parallel loop
               call nvtxEndRange
               !
               ! Update velocity and equations of state
               !
               call nvtxStartRange("Update u and EOS")
               !$acc parallel loop
               do ipoin = 1,npoin_w
                  !$acc loop seq
                  do idime = 1,ndime
                     aux_u(lpoin_w(ipoin),idime) = aux_q(lpoin_w(ipoin),idime)/aux_rho(lpoin_w(ipoin))
                  end do
                  aux_e_int(lpoin_w(ipoin)) = (aux_E(lpoin_w(ipoin))/aux_rho(lpoin_w(ipoin)))- &
                     0.5_rp*dot_product(aux_u(lpoin_w(ipoin),:),aux_u(lpoin_w(ipoin),:))
                  aux_pr(lpoin_w(ipoin)) = aux_rho(lpoin_w(ipoin))*(gamma_gas-1.0_rp)*aux_e_int(lpoin_w(ipoin))
                  aux_Tem(lpoin_w(ipoin)) = aux_pr(lpoin_w(ipoin))/(aux_rho(lpoin_w(ipoin))*Rgas)
               end do
               !$acc end parallel loop

               call nvtxEndRange

               !
               ! Compute viscosities and diffusion
               !
               !
               ! Update viscosity if Sutherland's law is active
               !
               if (flag_real_diff == 1 .and. flag_diff_suth == 1) then
                  call nvtxStartRange("MU_SUT")
                  call sutherland_viscosity(npoin,aux_Tem,mu_factor,mu_fluid)
                  call nvtxEndRange
               end if
               !
               ! Compute diffusion terms with values at current substep
               !
               call nvtxStartRange("DIFFUSIONS")
               call full_diffusion_ijk(nelem,npoin,connec,Ngp,dNgp,He,gpvol,dlxigp_ip,xgp,atoIJK,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,Cp,Prt,aux_rho,aux_u,aux_Tem,mu_fluid,mu_e,mu_sgs,Ml,Rdiff_mass,Rdiff_mom,Rdiff_ener)
               call nvtxEndRange
               !
               ! Call source term if applicable
               !
               if(present(source_term)) then
                  call mom_source_const_vect(nelem,npoin,connec,Ngp,dNgp,He,gpvol,aux_u,source_term,Rdiff_mom)
               end if
               !
               ! Evaluate wall models
     
               if((flag_activate_wall_model==1) .and. (nboun .ne. 0)) then
                  call evalWallModel(bouCodes2WallModel,nelem,npoin,nboun,numBoundCodes,connec,bound,point2elem,atoIJK, bou_codes, &
                     bounorm,normalsAtNodes,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,wgp_b,coord,dlxigp_ip,He,gpvol, mu_fluid,aux_rho(:),aux_u(:,:),Rdiff_mom)
               end if
               !
               !
               ! Compute convective terms
               !
               call full_convec_ijk(nelem,npoin,connec,Ngp,dNgp,He,gpvol,dlxigp_ip,xgp,atoIJK,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,aux_u,aux_q,aux_rho,aux_pr,aux_E,Rmass,Rmom,Rener)

               ! entropy advection
               !
               ! Add convection and diffusion terms (Rdiff_* is zero during prediction)
               !
               call nvtxStartRange("Add convection and diffusion")
               !$acc kernels
               Rmass(:) = Rmass(:) + Rdiff_mass(:)
               Rener(:) = Rener(:) + Rdiff_ener(:)
               Rmom(:,:) = Rmom(:,:) + Rdiff_mom(:,:)
               !$acc end kernels
               call nvtxEndRange

               !TESTING NEW LOCATION FOR MPICOMMS
               if(mpi_size.ge.2) then
                  call nvtxStartRange("MPI_comms_tI")

                  call mpi_halo_atomic_update_float(Rmass)
                  call mpi_halo_atomic_update_float(Rener)
                  do idime = 1,ndime
                     call mpi_halo_atomic_update_float(Rmom(:,idime))
                  end do

                  call nvtxEndRange
               end if

               !
               ! Call lumped mass matrix solver
               !
               call nvtxStartRange("Call solver")
               call lumped_solver_scal(npoin,npoin_w,lpoin_w,Ml,Rmass)
               call lumped_solver_scal(npoin,npoin_w,lpoin_w,Ml,Rener)
               call lumped_solver_vect(npoin,npoin_w,lpoin_w,Ml,Rmom)
               call nvtxEndRange
               !
               ! Accumulate the residuals
               !
               call nvtxStartRange("Accumulate residuals")
               !$acc parallel loop
               do ipoin = 1,npoin
                  Rmass_sum(ipoin) = Rmass_sum(ipoin) + b_i(istep)*Rmass(ipoin)
                  Rener_sum(ipoin) = Rener_sum(ipoin) + b_i(istep)*Rener(ipoin)
                  !$acc loop seq
                  do idime = 1,ndime
                     Rmom_sum(ipoin,idime) = Rmom_sum(ipoin,idime) + b_i(istep)*Rmom(ipoin,idime)
                  end do
               end do
               !$acc end parallel loop
               call nvtxEndRange
            end do
            call nvtxEndRange
            !
            ! RK update to variables
            !
            call nvtxStartRange("RK_UPDATE")
            !$acc parallel loop
            do ipoin = 1,npoin
               rho(ipoin,pos) = rho(ipoin,pos)-dt*Rmass_sum(ipoin)
               E(ipoin,pos) = E(ipoin,pos)-dt*Rener_sum(ipoin)
               !$acc loop seq
               do idime = 1,ndime
                  q(ipoin,idime,pos) = q(ipoin,idime,pos)-dt*Rmom_sum(ipoin,idime)
               end do
            end do
            !$acc end parallel loop
            call nvtxEndRange

            !
            ! Apply bcs after update
            !
            if (noBoundaries .eqv. .false.) then
               call nvtxStartRange("BCS_AFTER_UPDATE")
               call temporary_bc_routine_dirichlet_prim(npoin,nboun,bou_codes,bou_codes_nodes,bound,nbnodes,lbnodes,lnbn,lnbn_nodes,normalsAtNodes,rho(:,pos),q(:,:,pos),u(:,:,pos),pr(:,pos),E(:,pos))
               call nvtxEndRange
            end if

            !
            ! Update velocity and equations of state
            !
            call nvtxStartRange("Update u and EOS")
            
            !$acc parallel loop
            do ipoin = 1,npoin_w
               umag = 0.0_rp
               !$acc loop seq
               do idime = 1,ndime
                  u(lpoin_w(ipoin),idime,pos) = q(lpoin_w(ipoin),idime,pos)/rho(lpoin_w(ipoin),pos)
                  umag = umag + u(lpoin_w(ipoin),idime,pos)**2
               end do
               umag = sqrt(umag)
               e_int(lpoin_w(ipoin),pos) = (E(lpoin_w(ipoin),pos)/rho(lpoin_w(ipoin),pos))- &
                  0.5_rp*dot_product(u(lpoin_w(ipoin),:,pos),u(lpoin_w(ipoin),:,pos))
               pr(lpoin_w(ipoin),pos) = rho(lpoin_w(ipoin),pos)*(gamma_gas-1.0_rp)*e_int(lpoin_w(ipoin),pos)
               csound(lpoin_w(ipoin)) = sqrt(gamma_gas*pr(lpoin_w(ipoin),pos)/rho(lpoin_w(ipoin),pos))
               machno(lpoin_w(ipoin)) = umag/csound(lpoin_w(ipoin))
               Tem(lpoin_w(ipoin),pos) = pr(lpoin_w(ipoin),pos)/(rho(lpoin_w(ipoin),pos)*Rgas)
               eta(lpoin_w(ipoin),pos) = (rho(lpoin_w(ipoin),pos)/(gamma_gas-1.0_rp))* &
                  log(pr(lpoin_w(ipoin),pos)/(rho(lpoin_w(ipoin),pos)**gamma_gas))
               !$acc loop seq
               do idime = 1,ndime
                  f_eta(lpoin_w(ipoin),idime) = u(lpoin_w(ipoin),idime,pos)*eta(lpoin_w(ipoin),pos)
               end do
            end do
            !$acc end parallel loop

            call generic_scalar_convec_ijk(nelem,npoin,connec,Ngp,dNgp,He, &
               gpvol,dlxigp_ip,xgp,atoIJK,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,f_eta,eta(:,pos),u(:,:,pos),Reta,alpha)


            if(mpi_size.ge.2) then
               call nvtxStartRange("MPI_comms_tI")
               call mpi_halo_atomic_update_float(Reta)
               call nvtxEndRange
            end if

            call lumped_solver_scal(npoin,npoin_w,lpoin_w,Ml,Reta)

            !$acc parallel loop
            do ipoin = 1,npoin_w
               Reta(lpoin_w(ipoin)) = -Reta(lpoin_w(ipoin))-(eta(lpoin_w(ipoin),2)-eta(lpoin_w(ipoin),1))/dt
            end do
            !$acc end parallel loop

            call nvtxEndRange
            !
            ! If using Sutherland viscosity model:
            !
            if (flag_real_diff == 1 .and. flag_diff_suth == 1) then
               call nvtxStartRange("Sutherland viscosity")
               call sutherland_viscosity(npoin,Tem(:,pos),mu_factor,mu_fluid)
               call nvtxEndRange
            end if

            !
            ! Compute entropy viscosity
            !
            call smart_visc_spectral(nelem,npoin,npoin_w,connec,lpoin_w,Reta,Rrho,Ngp, &
               gamma_gas,rho(:,pos),u(:,:,pos),Tem(:,pos),eta(:,pos),helem_l,helem,Ml,mu_e)
            !
            ! Compute subgrid viscosity if active
            !
            if(flag_les == 1) then
               call nvtxStartRange("MU_SGS")
               if(flag_les_ilsa == 1) then
                  call sgs_ilsa_visc(nelem,npoin,npoin_w,lpoin_w,connec,Ngp,dNgp,He,dlxigp_ip,atoIJK,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,dt,rho(:,pos),u(:,:,pos),mu_sgs,mu_fluid,mu_e,kres,etot,au,ax1,ax2,ax3) 
               else
                  call sgs_visc(nelem,npoin,connec,Ngp,dNgp,He,gpvol,dlxigp_ip,atoIJK,invAtoIJK,gmshAtoI,gmshAtoJ,gmshAtoK,rho(:,pos),u(:,:,pos),Ml,mu_sgs)
               end if
               call nvtxEndRange
            end if

         end subroutine rk_4_main

      end module time_integ

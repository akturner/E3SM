! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.io/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_seaice_driver
!
!> \brief   Driver for sea ice column package
!> \author  Adrian K. Turner
!> \date    30th November 2022
!> \details This module controls the use the sea ice column package
!>          directly from MPAS-Ocean
!
!-----------------------------------------------------------------------

module ocn_seaice_column

  use mpas_derived_types
  use mpas_pool_routines
  use mpas_log
  use mpas_timekeeping

  implicit none
  private

  public :: &
       ocn_seaice_setup_packages, &
       ocn_seaice_init, &
       ocn_seaice_run, &
       ocn_seaice_finalize

!***********************************************************************

contains

!***********************************************************************
!
!  function ocn_seaice_setup_packages
!
!> \brief
!> \author  Adrian K. Turner
!> \date    9th January 2023
!> \details
!
!-----------------------------------------------------------------------

  subroutine ocn_seaice_setup_packages(configPool, packagePool, ierr)!{{{

    type (mpas_pool_type), intent(inout) :: configPool
    type (mpas_pool_type), intent(inout) :: packagePool
    integer, intent(out) :: ierr

#include "setup_packages_column_physics.inc"

  end subroutine ocn_seaice_setup_packages

!***********************************************************************
!
!  function ocn_seaice_init
!
!> \brief   Initialize the sea ice column package
!> \author  Adrian K. Turner
!> \date    30th November 2022
!> \details Perform operations that initialize the sea ice column
!>          package when run directly from MPAS-Ocean
!
!-----------------------------------------------------------------------

  subroutine ocn_seaice_init(domain)!{{{

    use seaice_forcing, only: &
         seaice_prepare_atmospheric_variables, &
         seaice_prepare_oceanic_variables

    use seaice_column, only: &
         seaice_init_column_physics_package_parameters, &
         seaice_init_column_physics_package_variables

    use seaice_initialize, only: &
         seaice_initialize_coupler_fields

    use seaice_mesh, only: &
         seaice_init_boundary

    type (domain_type), intent(inout) :: &
         domain

    type (MPAS_timeInterval_type) :: &
         timeStep

    type (MPAS_time_type) :: &
         currentTime

    real(kind=RKIND) :: &
         dt

    integer :: &
         ierr

    call mpas_log_write("Initialize column sea ice")

    timeStep = mpas_get_clock_timestep(domain % clock, ierr=ierr)
    call mpas_get_timeInterval(timeStep, dt=dt)

    currentTime = MPAS_get_clock_time(domain % clock, MPAS_NOW, ierr=ierr)

    ! set up boundary arrays
    call mpas_log_write(" Set up sea ice boundary arrays...")
    call seaice_init_boundary(domain)

    ! init the basic column physics package
    call mpas_log_write(" Initialize column parameters...")
    call seaice_init_column_physics_package_parameters(domain)

    ! init coupler fluxes
    call mpas_log_write(" Initialize coupler fields...")
    call seaice_initialize_coupler_fields(domain)

    ! column physics initialization
    call mpas_log_write(" Initialize column variables...")
    call seaice_init_column_physics_package_variables(domain, domain % clock, dt)

    ! set up sea ice forcing
    call mpas_log_write(" Initialize sea ice forcing...")
    call seaice_prepare_atmospheric_variables(domain, currentTime)
    call seaice_prepare_oceanic_variables(domain, .true.)

  end subroutine ocn_seaice_init!}}}

!***********************************************************************
!
!  function ocn_seaice_run
!
!> \brief   Run the sea ice column package
!> \author  Adrian K. Turner
!> \date    30th November 2022
!> \details Perform operations that run the sea ice column package
!>          directly from MPAS-Ocean
!
!-----------------------------------------------------------------------

  subroutine ocn_seaice_run(domain)!{{{

    use seaice_column, only: &
         seaice_column_reinitialize_diagnostics_thermodynamics, &
         seaice_column_reinitialize_diagnostics_bgc, &
         seaice_column_predynamics_time_integration, &
         seaice_column_postdynamics_time_integration

    type (domain_type), intent(inout) :: &
         domain

    type (MPAS_timeInterval_type) :: &
         timeStep

    real(kind=RKIND) :: &
         dt

    integer :: &
         ierr

    call mpas_log_write("Run column sea ice")

    timeStep = mpas_get_clock_timestep(domain % clock, ierr=ierr)
    call mpas_get_timeInterval(timeStep, dt=dt)

    ! couple ocean to sea ice
    call ocn_seaice_couple_ocn_to_ice(domain)

    ! reinitialize diagnostics
    call seaice_column_reinitialize_diagnostics_thermodynamics(domain)
    call seaice_column_reinitialize_diagnostics_bgc(domain)

    ! pre dynamics column physics
    call seaice_column_predynamics_time_integration(domain, domain % clock, dt)

    ! shortwave
    call seaice_column_postdynamics_time_integration(domain, domain % clock, dt)

    ! ocean surface fluxes
    call ocn_seaice_ocn_fluxes(domain)

    ! couple sea ice to ocean
    call ocn_seaice_couple_ice_to_ocn(domain)

  end subroutine ocn_seaice_run!}}}

!***********************************************************************
!
!  function ocn_seaice_ocn_fluxes
!
!> \brief   Set ocean fluxes
!> \author  Adrian K. Turner
!> \date    23rd May 2022
!> \details Calculate ocean bulk surface fluxes from atmospheric forcing
!
!-----------------------------------------------------------------------

  subroutine ocn_seaice_ocn_fluxes(domain)!{{{

    use seaice_constants, only: &
         seaiceFreshWaterFreezingPoint, &
         seaiceLatentHeatSublimation, &
         seaiceStefanBoltzmann

    use ice_colpkg, only: &
         colpkg_atm_boundary

    type (domain_type), intent(inout) :: &
         domain

    type (block_type), pointer :: &
         block_ptr

    type (mpas_pool_type), pointer :: &
         meshPool, &
         forcingPool, &
         statePool, &
         tracersPool, &
         atmosCouplingPool, &
         atmosForcingPool, &
         oceanAtmospherePool, &
         oceanCouplingPool

    ! ocean fields
    real(kind=RKIND), dimension(:), pointer :: &
         shortWaveHeatFlux, &
         windStressZonal, &
         windStressMeridional

    ! forcing/coupling fields
    real(kind=RKIND), dimension(:), pointer :: &
         airPotentialTemperature, &
         uAirVelocity, &
         vAirVelocity, &
         windSpeed, &
         airLevelHeight, &
         airSpecificHumidity, &
         airDensity, &
         airStressOceanU, &
         airStressOceanV, &
         atmosReferenceTemperature2mOcean, &
         atmosReferenceHumidity2mOcean, &
         albedoVisibleDirectOcean, &
         albedoVisibleDiffuseOcean, &
         albedoIRDirectOcean, &
         albedoIRDiffuseOcean, &
         shortwaveVisibleDirectDown, &
         shortwaveVisibleDiffuseDown, &
         shortwaveIRDirectDown, &
         shortwaveIRDiffuseDown, &
         seaSurfaceTemperature, &
         longWaveHeatFluxUp, &
         longWaveHeatFluxDown, &
         sensibleHeatFlux, &
         latentHeatFlux, &
         rainFlux, &
         snowFlux, &
         longwaveDown, &
         rainfallRate, &
         snowfallRate, &
         evaporationFlux

    real (kind=RKIND), dimension(:,:,:), pointer :: &
         activeTracers

    integer, dimension(:), pointer :: &
         minLevelCell

    integer :: &
         iCell

    integer, pointer :: &
         nCellsSolve, &
         index_temperature

    real(kind=RKIND) :: &
         seaSurfaceTemperatureK, &
         latentTransferCoefficient, &
         sensibleTransferCoefficient, &
         potentialTemperatureDifference, &
         specificHumidityDifference, &
         airDragCoefficient, &
         airOceanDragCoefficientRatio

    real(kind=RKIND), parameter :: &
         emissivity = 1.0_RKIND

    block_ptr => domain % blocklist
    do while(associated(block_ptr))

       call mpas_pool_get_subpool(block_ptr % structs, 'forcing', forcingPool)
       call mpas_pool_get_subpool(block_ptr % structs, 'mesh', meshPool)
       call mpas_pool_get_subpool(block_ptr % structs, 'state', statePool)
       call mpas_pool_get_subpool(statePool, 'tracers', tracersPool)
       call mpas_pool_get_subpool(block_ptr % structs, 'atmos_coupling', atmosCouplingPool)
       call mpas_pool_get_subpool(block_ptr % structs, 'atmos_forcing', atmosForcingPool)
       call mpas_pool_get_subpool(block_ptr % structs, 'ocean_coupling', oceanCouplingPool)
       call mpas_pool_get_subpool(block_ptr % structs, 'ocean_atmosphere', oceanAtmospherePool)

       call MPAS_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)
       call mpas_pool_get_array(meshPool, 'minLevelCell', minLevelCell)

       call mpas_pool_get_array(forcingPool, 'shortWaveHeatFlux', shortWaveHeatFlux)
       call mpas_pool_get_array(forcingPool, 'longWaveHeatFluxUp', longWaveHeatFluxUp)
       call mpas_pool_get_array(forcingPool, 'longWaveHeatFluxDown', longWaveHeatFluxDown)
       call mpas_pool_get_array(forcingPool, 'sensibleHeatFlux', sensibleHeatFlux)
       call mpas_pool_get_array(forcingPool, 'latentHeatFlux', latentHeatFlux)
       call mpas_pool_get_array(forcingPool, 'rainFlux', rainFlux)
       call mpas_pool_get_array(forcingPool, 'snowFlux', snowFlux)
       call mpas_pool_get_array(forcingPool, 'evaporationFlux', evaporationFlux)

       call mpas_pool_get_array(forcingPool, 'windStressZonal', windStressZonal)
       call mpas_pool_get_array(forcingPool, 'windStressMeridional', windStressMeridional)

       call mpas_pool_get_dimension(tracersPool, 'index_temperature', index_temperature)
       call mpas_pool_get_array(tracersPool, 'activeTracers', activeTracers, 1)

       call MPAS_pool_get_array(atmosCouplingPool, "airPotentialTemperature", airPotentialTemperature)
       call MPAS_pool_get_array(atmosCouplingPool, "uAirVelocity", uAirVelocity)
       call MPAS_pool_get_array(atmosCouplingPool, "vAirVelocity", vAirVelocity)
       call MPAS_pool_get_array(atmosCouplingPool, "airLevelHeight", airLevelHeight)
       call MPAS_pool_get_array(atmosCouplingPool, "airSpecificHumidity", airSpecificHumidity)
       call MPAS_pool_get_array(atmosCouplingPool, "airDensity", airDensity)
       call MPAS_pool_get_array(atmosCouplingPool, "shortwaveVisibleDirectDown", shortwaveVisibleDirectDown)
       call MPAS_pool_get_array(atmosCouplingPool, "shortwaveVisibleDiffuseDown", shortwaveVisibleDiffuseDown)
       call MPAS_pool_get_array(atmosCouplingPool, "shortwaveIRDirectDown", shortwaveIRDirectDown)
       call MPAS_pool_get_array(atmosCouplingPool, "shortwaveIRDiffuseDown", shortwaveIRDiffuseDown)
       call MPAS_pool_get_array(atmosCouplingPool, "longwaveDown", longwaveDown)
       call MPAS_pool_get_array(atmosCouplingPool, "rainfallRate", rainfallRate)
       call MPAS_pool_get_array(atmosCouplingPool, "snowfallRate", snowfallRate)

       call MPAS_pool_get_array(atmosForcingPool, "windSpeed", windSpeed)

       call MPAS_pool_get_array(oceanCouplingPool, "seaSurfaceTemperature", seaSurfaceTemperature)

       call MPAS_pool_get_array(oceanAtmospherePool, "airStressOceanU", airStressOceanU)
       call MPAS_pool_get_array(oceanAtmospherePool, "airStressOceanV", airStressOceanV)
       call MPAS_pool_get_array(oceanAtmospherePool, "atmosReferenceTemperature2mOcean", atmosReferenceTemperature2mOcean)
       call MPAS_pool_get_array(oceanAtmospherePool, "atmosReferenceHumidity2mOcean", atmosReferenceHumidity2mOcean)
       call MPAS_pool_get_array(oceanAtmospherePool, "albedoVisibleDirectOcean", albedoVisibleDirectOcean)
       call MPAS_pool_get_array(oceanAtmospherePool, "albedoVisibleDiffuseOcean", albedoVisibleDiffuseOcean)
       call MPAS_pool_get_array(oceanAtmospherePool, "albedoIRDirectOcean", albedoIRDirectOcean)
       call MPAS_pool_get_array(oceanAtmospherePool, "albedoIRDiffuseOcean", albedoIRDiffuseOcean)

       do iCell = 1, nCellsSolve

          seaSurfaceTemperature(iCell) = activeTracers(index_temperature, minLevelCell(iCell), iCell)

          seaSurfaceTemperatureK = seaSurfaceTemperature(iCell) + seaiceFreshWaterFreezingPoint

          call colpkg_atm_boundary(&
               'ocn', &
               seaSurfaceTemperature(iCell), &
               airPotentialTemperature(iCell), &
               uAirVelocity(iCell), &
               vAirVelocity(iCell), &
               windSpeed(iCell), &
               airLevelHeight(iCell), &
               airSpecificHumidity(iCell), &
               airDensity(iCell), &
               airStressOceanU(iCell), &
               airStressOceanV(iCell), &
               atmosReferenceTemperature2mOcean(iCell), &
               atmosReferenceHumidity2mOcean(iCell), &
               potentialTemperatureDifference, &
               specificHumidityDifference, &
               latentTransferCoefficient, &
               sensibleTransferCoefficient, &
               airDragCoefficient, &
               airOceanDragCoefficientRatio)

          shortWaveHeatFlux(iCell) = &
               (1.0_RKIND - albedoVisibleDirectOcean (iCell)) * shortwaveVisibleDirectDown (iCell) + &
               (1.0_RKIND - albedoIRDirectOcean      (iCell)) * shortwaveIRDirectDown      (iCell) + &
               (1.0_RKIND - albedoVisibleDiffuseOcean(iCell)) * shortwaveVisibleDiffuseDown(iCell) + &
               (1.0_RKIND - albedoIRDiffuseOcean     (iCell)) * shortwaveIRDiffuseDown     (iCell)

          longWaveHeatFluxUp(iCell) = -emissivity * seaiceStefanBoltzmann * seaSurfaceTemperatureK**4
          sensibleHeatFlux  (iCell) = sensibleTransferCoefficient * potentialTemperatureDifference
          latentHeatFlux    (iCell) = latentTransferCoefficient   * specificHumidityDifference

          longWaveHeatFluxDown(iCell) = longwaveDown(iCell)

          rainFlux(iCell) = rainfallRate(iCell)
          snowFlux(iCell) = snowfallRate(iCell)

          evaporationFlux(iCell) = latentHeatFlux(iCell) / seaiceLatentHeatSublimation

       enddo ! iCell

       block_ptr => block_ptr % next
    enddo

  end subroutine ocn_seaice_ocn_fluxes

!***********************************************************************
!
!  function ocn_seaice_couple_ice_to_ocn
!
!> \brief   Send sea ice coupling fields to ocean
!> \author  Adrian K. Turner
!> \date    23rd May 2022
!> \details Coupling based on E3SM
!
!-----------------------------------------------------------------------

  subroutine ocn_seaice_couple_ice_to_ocn(domain)!{{{

    use ice_constants_colpkg, only: &
         p001, &
         ice_ref_salinity

    use seaice_mesh, only: &
         seaice_latlon_vector_rotation_backward

    type (domain_type), intent(inout) :: &
         domain

    type (block_type), pointer :: &
         block_ptr

    type (mpas_pool_type), pointer :: &
         configs, &
         meshPool, &
         tracersAggregatePool, &
         velocitySolverPool, &
         oceanFluxesPool, &
         oceanCouplingPool, &
         forcingPool

    integer, pointer :: &
         nCellsSolve

    integer :: &
         i

    real(kind=RKIND), pointer :: &
         sphere_radius

    real (kind=RKIND), dimension(:), pointer :: &
         latCell, &
         lonCell, &
         xCell, &
         yCell, &
         zCell

    logical, pointer :: &
         config_rotate_cartesian_grid

    ! sea ice fields
    real (kind=RKIND), dimension(:), pointer :: &
         iceAreaCell,  &
         oceanHeatFlux, &
         oceanShortwaveFlux, &
         oceanFreshWaterFlux, &
         oceanSaltFlux,  &
         oceanStressCellU, &
         oceanStressCellV, &
         frazilMassAdjust

    ! ocean fields
    type (field1DReal), pointer :: &
         !iceFractionField, &
         seaIceHeatFluxField, &
         shortWaveHeatFluxField, &
         seaIceFreshWaterFluxField, &
         seaIceSalinityFluxField, &
         windStressZonalField, &
         windStressMeridionalField

    real (kind=RKIND), dimension(:), pointer :: &
         !iceFraction, &
         seaIceHeatFlux, &
         shortWaveHeatFlux, &
         seaIceFreshWaterFlux, &
         seaIceSalinityFlux, &
         windStressZonal, &
         windStressMeridional

    real(kind=RKIND) :: &
         ailohi, &
         tauxo, &
         tauyo

    block_ptr => domain % blocklist
    do while(associated(block_ptr))

       configs => block_ptr % configs
       call MPAS_pool_get_config(configs, "config_rotate_cartesian_grid", config_rotate_cartesian_grid)

       call MPAS_pool_get_subpool(block_ptr % structs, "mesh", meshPool)

       call MPAS_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)

       call MPAS_pool_get_config(meshPool, "sphere_radius", sphere_radius)
       call MPAS_pool_get_array(meshPool, "latCell", latCell)
       call MPAS_pool_get_array(meshPool, "lonCell", lonCell)
       call MPAS_pool_get_array(meshPool, "xCell", xCell)
       call MPAS_pool_get_array(meshPool, "yCell", yCell)
       call MPAS_pool_get_array(meshPool, "zCell", zCell)

       ! sea ice fields
       call MPAS_pool_get_subpool(block_ptr % structs, "tracers_aggregate", tracersAggregatePool)
       call MPAS_pool_get_subpool(block_ptr % structs, "velocity_solver", velocitySolverPool)
       call MPAS_pool_get_subpool(block_ptr % structs, "ocean_fluxes", oceanFluxesPool)
       call MPAS_pool_get_subpool(block_ptr % structs, "ocean_coupling", oceanCouplingPool)

       call MPAS_pool_get_array(oceanCouplingPool, 'frazilMassAdjust', frazilMassAdjust)

       call MPAS_pool_get_array(tracersAggregatePool, 'iceAreaCell', iceAreaCell)

       call MPAS_pool_get_array(velocitySolverPool, 'oceanStressCellU', oceanStressCellU)
       call MPAS_pool_get_array(velocitySolverPool, 'oceanStressCellV', oceanStressCellV)

       call MPAS_pool_get_array(oceanFluxesPool, 'oceanHeatFlux', oceanHeatFlux)
       call MPAS_pool_get_array(oceanFluxesPool, 'oceanShortwaveFlux', oceanShortwaveFlux)
       call MPAS_pool_get_array(oceanFluxesPool, 'oceanFreshWaterFlux', oceanFreshWaterFlux)
       call MPAS_pool_get_array(oceanFluxesPool, 'oceanSaltFlux', oceanSaltFlux)

       ! ocean fields
       call mpas_pool_get_subpool(block_ptr % structs, 'forcing', forcingPool)

       !call mpas_pool_get_field(forcingPool, 'iceFraction', iceFractionField)

       call mpas_pool_get_field(forcingPool, 'seaIceHeatFlux', seaIceHeatFluxField)
       call mpas_pool_get_field(forcingPool, 'shortWaveHeatFlux', shortWaveHeatFluxField)
       call mpas_pool_get_field(forcingPool, 'seaIceFreshWaterFlux', seaIceFreshWaterFluxField)
       call mpas_pool_get_field(forcingPool, 'seaIceSalinityFlux', seaIceSalinityFluxField)

       call mpas_pool_get_field(forcingPool, 'windStressZonal', windStressZonalField)
       call mpas_pool_get_field(forcingPool, 'windStressMeridional', windStressMeridionalField)

       !iceFraction = iceFractionField % array
       seaIceHeatFlux => seaIceHeatFluxField % array
       shortWaveHeatFlux => shortWaveHeatFluxField % array
       seaIceFreshWaterFlux => seaIceFreshWaterFluxField % array
       seaIceSalinityFlux => seaIceSalinityFluxField % array

       windStressZonal => windStressZonalField % array
       windStressMeridional => windStressMeridionalField % array

       do i = 1, nCellsSolve

          ailohi = min(iceAreaCell(i), 1.0_RKIND)

          !if ( iceFractionField % isActive ) then
          !   iceFraction(i) = ailohi
          !end if
          if ( seaIceHeatFluxField % isActive ) then
             seaIceHeatFlux(i) = oceanHeatFlux(i)
          endif
          if ( shortWaveHeatFluxField % isActive ) then
             shortWaveHeatFlux(i) = shortWaveHeatFlux(i)  * (1.0_RKIND - ailohi) + &
                                    oceanShortwaveFlux(i) * ailohi
          endif
          if ( seaIceFreshWaterFluxField % isActive ) then
             if (ailohi > 0.0_RKIND) then
                seaIceFreshWaterFlux(i) = oceanFreshWaterFlux(i) + frazilMassAdjust(i)/ailohi
             else
                seaIceFreshWaterFlux(i) = 0.0_RKIND
             endif
          endif
          if ( seaIceSalinityFluxField % isActive ) then
             if (ailohi > 0.0_RKIND) then
                seaIceSalinityFlux(i) = oceanSaltFlux(i) + ice_ref_salinity*p001*frazilMassAdjust(i)/ailohi
             else
                seaIceSalinityFlux(i) = 0.0_RKIND
             endif
          endif

          ! ice/ocean stress (on POP T-grid:  convert to lat-lon)
          call seaice_latlon_vector_rotation_backward(&
               tauxo,                &
               tauyo,                &
               -oceanStressCellU(i), &
               -oceanStressCellV(i), &
               latCell(i),           &
               lonCell(i),           &
               xCell(i),             &
               yCell(i),             &
               zCell(i),             &
               sphere_radius,        &
               config_rotate_cartesian_grid)

          if ( windStressZonalField % isActive ) then
             windStressZonal(i) = windStressZonal(i) * (1.0_RKIND - ailohi) + &
                                  tauxo              * ailohi
          endif
          if ( windStressMeridionalField % isActive ) then
             windStressMeridional(i) = windStressMeridional(i) * (1.0_RKIND - ailohi) + &
                                       tauyo                   * ailohi
          endif

       enddo ! i

       block_ptr => block_ptr % next
    enddo

    call mpas_pool_get_subpool(domain % blocklist % structs, 'forcing', forcingPool)

    !if ( iceFractionField % isActive ) then
    !   call mpas_dmpar_exch_halo_field(iceFractionField)
    !end if

    if ( seaIceHeatFluxField % isActive ) then
       call mpas_dmpar_exch_halo_field(seaIceHeatFluxField)
    end if
    if ( shortWaveHeatFluxField % isActive ) then
       call mpas_dmpar_exch_halo_field(shortWaveHeatFluxField)
    end if
    if ( seaIceFreshWaterFluxField % isActive ) then
       call mpas_dmpar_exch_halo_field(seaIceFreshWaterFluxField)
    end if
    if ( seaIceSalinityFluxField % isActive ) then
       call mpas_dmpar_exch_halo_field(seaIceSalinityFluxField)
    end if

    if ( windStressMeridionalField % isActive ) then
       call mpas_dmpar_exch_halo_field(windStressMeridionalField)
    end if
    if ( windStressZonalField % isActive ) then
       call mpas_dmpar_exch_halo_field(windStressZonalField)
    end if

  end subroutine ocn_seaice_couple_ice_to_ocn

!***********************************************************************
!
!  function ocn_seaice_couple_ocn_to_ice
!
!> \brief   Send ocean coupling fields to sea ice
!> \author  Adrian K. Turner
!> \date    23rd May 2022
!> \details Coupling based on E3SM
!
!-----------------------------------------------------------------------

  subroutine ocn_seaice_couple_ocn_to_ice(domain)!{{{

    use ice_colpkg, only: &
         colpkg_sea_freezing_temperature

    use ocn_equation_of_state, only: &
         ocn_freezing_temperature

    use ocn_constants, only: &
         rho_sw, &
         cp_sw, &
         T0_Kelvin

    use ocn_config, only: &
         config_frazil_heat_of_fusion, &
         config_dt

    use ocn_diagnostics_variables, only: &
         gradSSHZonal, &
         gradSSHMeridional, &
         indexSurfaceVelocityZonal, &
         indexSurfaceVelocityMeridional

    type (domain_type), intent(inout) :: &
         domain

    type (block_type), pointer :: &
         block_ptr

    type (mpas_pool_type), pointer :: &
         configs, &
         meshPool, &
         oceanCoupling, &
         statePool, &
         forcingPool, &
         diagnosticsPool, &
         tracersPool

    integer, pointer :: &
         nCellsSolve

    integer :: &
         i

    real (kind=RKIND) :: &
         frazilMassFlux, &
         frazilMassFluxRev, &
         surfaceFreezingTemp

    ! coupler fields
    real(kind=RKIND) :: &
         Fioo_q, &
         Fioo_frazil

    ! sea ice fields
    type (field1DReal), pointer ::         &
         seaSurfaceTemperatureField,       &
         seaSurfaceSalinityField,          &
         seaFreezingTemperatureField,      &
         freezingMeltingPotentialField,    &
         frazilMassAdjustField,            &
         uOceanVelocityField,              &
         vOceanVelocityField,              &
         seaSurfaceTiltUField,             &
         seaSurfaceTiltVField

    real (kind=RKIND), dimension(:), pointer :: &
         seaSurfaceTemperature,       &
         seaSurfaceSalinity,          &
         seaFreezingTemperature,      &
         freezingMeltingPotential,    &
         frazilMassAdjust,            &
         uOceanVelocity,              &
         vOceanVelocity,              &
         seaSurfaceTiltU,             &
         seaSurfaceTiltV

    ! ocean fields
    integer, pointer :: &
         index_temperature, &
         index_salinity, &
         index_avgZonalSurfaceVelocity, &
         index_avgMeridionalSurfaceVelocity

    real (kind=RKIND), dimension(:), pointer :: &
         seaIceEnergy, &
         accumulatedFrazilIceMass, &
         frazilSurfacePressure, &
         filteredSSHGradientZonal, &
         filteredSSHGradientMeridional

    real (kind=RKIND), dimension(:,:), pointer :: &
         avgTracersSurfaceValue, &
         avgSurfaceVelocity, &
         layerThickness, &
         surfaceVelocity

    real (kind=RKIND), dimension(:,:,:), pointer :: &
         activeTracers

    integer, dimension(:), pointer :: &
         landIceMask, &
         minLevelCell

    type (MPAS_timeInterval_type) :: &
         timeStep

    integer :: &
         ierr

    real(kind=RKIND) :: &
         ocn_cpl_dt

    ! configs
    character(len=strKIND), pointer ::   &
         config_ocean_surface_type, &
         config_thermodynamics_type

    logical, pointer :: &
         frazilIceActive

    logical :: &
         keepFrazil

    timeStep = mpas_get_clock_timestep(domain % clock, ierr=ierr)
    call mpas_get_timeInterval(timeStep, dt=ocn_cpl_dt)

    ! get configure options
    call mpas_pool_get_package(domain % packages, 'frazilIceActive', frazilIceActive)

    block_ptr => domain % blocklist
    do while(associated(block_ptr))

       configs => block_ptr % configs
       call mpas_pool_get_config(configs, "config_ocean_surface_type", config_ocean_surface_type)
       call mpas_pool_get_config(configs, "config_thermodynamics_type", config_thermodynamics_type)

       call mpas_pool_get_subpool(block_ptr % structs, 'mesh', meshPool)

       call mpas_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)
       call mpas_pool_get_array(meshPool, 'minLevelCell', minLevelCell)

       ! sea ice fields
       call mpas_pool_get_subpool(block_ptr % structs, 'ocean_coupling', oceanCoupling)

       call mpas_pool_get_array(oceanCoupling, 'seaSurfaceTemperature', seaSurfaceTemperature)
       call mpas_pool_get_array(oceanCoupling, 'seaSurfaceSalinity', seaSurfaceSalinity)
       call mpas_pool_get_array(oceanCoupling, 'seaFreezingTemperature', seaFreezingTemperature)
       call mpas_pool_get_array(oceanCoupling, 'freezingMeltingPotential', freezingMeltingPotential)
       call mpas_pool_get_array(oceanCoupling, 'frazilMassAdjust', frazilMassAdjust)
       call mpas_pool_get_array(oceanCoupling, 'uOceanVelocity', uOceanVelocity)
       call mpas_pool_get_array(oceanCoupling, 'vOceanVelocity', vOceanVelocity)
       call mpas_pool_get_array(oceanCoupling, 'seaSurfaceTiltU', seaSurfaceTiltU)
       call mpas_pool_get_array(oceanCoupling, 'seaSurfaceTiltV', seaSurfaceTiltV)

       ! ocean fields
       call mpas_pool_get_subpool(block_ptr % structs, 'state', statePool)
       call mpas_pool_get_array(statePool, 'layerThickness', layerThickness, 1)

       call mpas_pool_get_subpool(statePool, 'tracers', tracersPool)
       call mpas_pool_get_dimension(tracersPool, 'index_temperature', index_temperature)
       call mpas_pool_get_dimension(tracersPool, 'index_salinity', index_salinity)
       call mpas_pool_get_array(tracersPool, 'activeTracers', activeTracers, 1)

       call mpas_pool_get_subpool(block_ptr % structs, 'diagnostics', diagnosticsPool)
       call mpas_pool_get_array(diagnosticsPool, "gradSSHZonal", gradSSHZonal)
       call mpas_pool_get_array(diagnosticsPool, "gradSSHMeridional", gradSSHMeridional)
       call mpas_pool_get_array(diagnosticsPool, "surfaceVelocity", surfaceVelocity)

       call mpas_pool_get_subpool(block_ptr % structs, 'forcing', forcingPool)
       call mpas_pool_get_array(forcingPool, 'landIceMask', landIceMask)
       if ( frazilIceActive ) then
          call mpas_pool_get_array(forcingPool, 'seaIceEnergy', seaIceEnergy)
          call mpas_pool_get_array(forcingPool, 'frazilSurfacePressure', frazilSurfacePressure)
          call mpas_pool_get_array(statePool, 'accumulatedFrazilIceMass', accumulatedFrazilIceMass, 1)
       end if

       do i = 1, nCellsSolve

          seaSurfaceTemperature(i) = activeTracers(index_temperature, minLevelCell(i), i)
          seaSurfaceSalinity(i)    = activeTracers(index_salinity, minLevelCell(i), i)

          seaFreezingTemperature(i) = colpkg_sea_freezing_temperature(seaSurfaceSalinity(i))

          uOceanVelocity(i)        = surfaceVelocity(indexSurfaceVelocityZonal, i)
          vOceanVelocity(i)        = surfaceVelocity(indexSurfaceVelocityMeridional, i)
          seaSurfaceTiltU(i)       = gradSSHZonal(i)
          seaSurfaceTiltV(i)       = gradSSHMeridional(i)

          ! ocean frazil
          !call mpas_log_write("frazilIceActive: $l", logicArgs=(/frazilIceActive/))
          if ( frazilIceActive ) then
             ! negative when frazil ice can be melted
             keepFrazil = .true.
             if ( associated(landIceMask) ) then
                if ( landIceMask(i) == 1 ) then
                   keepFrazil = .false.
                end if
             end if

             if ( keepFrazil ) then

                ! Calculate energy associated with frazil mass transfer to sea ice if frazil has accumulated
                if ( accumulatedFrazilIceMass(i) > 0.0_RKIND ) then
                   call mpas_log_write("accumulatedFrazilIceMass(i) > 0.0_RKIND")

                   seaIceEnergy(i) = accumulatedFrazilIceMass(i) * config_frazil_heat_of_fusion

                   ! Otherwise calculate the melt potential where avgTracersSurfaceValue represents only the
                   ! top layer of the ocean
                else

                   surfaceFreezingTemp = ocn_freezing_temperature(salinity=activeTracers(index_salinity, minLevelCell(i), i), &
                        pressure=0.0_RKIND,  inLandIceCavity=.false.)

                   seaIceEnergy(i) = min(rho_sw*cp_sw*layerThickness(1, i)*( surfaceFreezingTemp + T0_Kelvin &
                                   - activeTracers(index_temperature, minLevelCell(i), i) ), 0.0_RKIND )

                end if

                Fioo_q  = seaIceEnergy(i) / ocn_cpl_dt
                Fioo_frazil = accumulatedFrazilIceMass(i) / ocn_cpl_dt

             else

                Fioo_q  = 0.0_RKIND
                Fioo_frazil = 0.0_RKIND

             end if

             ! Reset SeaIce Energy and Accumulated Frazil Ice
             seaIceEnergy(i) = 0.0_RKIND
             accumulatedFrazilIceMass(i) = 0.0_RKIND
             frazilSurfacePressure(i) = 0.0_RKIND
          end if

          ! sea ice frazil
          if (trim(config_ocean_surface_type) == "free") then ! free surface (MPAS-O)

             ! freezingMeltingPotential(i) is the ocean energy associated with frazil formation
             ! when it is positive and frazilMassFlux is positive. Conversely, freezingMeltingPotential(i)
             ! is negative when there is the melting potential in which case frazilMassFlux is zero.

             freezingMeltingPotential(i) = Fioo_q

             frazilMassFlux              = Fioo_frazil

             ! Now determine the sea ice mass associated with the frazil heat flux given when
             ! freezingMeltingPotential(i) is positive. This produces a revised mass flux, given
             ! in frazilMassFluxRev for the given sea surface salinity. The resulting difference
             ! is assigned to frazilMassAdjust(i) which is exported to the ocean in the subsequent
             ! coupling step as a freshwater and salt flux. This step is required to balance mass
             ! and heat with the ocean.

             call frazil_mass(freezingMeltingPotential(i), frazilMassFluxRev, seaSurfaceSalinity(i), &
                  config_thermodynamics_type)

             frazilMassAdjust(i) = frazilMassFlux-frazilMassFluxRev

             if (Fioo_frazil > 0.0_RKIND) then
                call MPAS_log_write("Frazil: $r $r", realArgs=(/freezingMeltingPotential(i),frazilMassAdjust(i)/))
             endif

          else ! non-free surface (SOM)

             freezingMeltingPotential(i) = Fioo_q

          endif

       enddo ! i

       block_ptr => block_ptr % next
    end do

  end subroutine ocn_seaice_couple_ocn_to_ice

!***********************************************************************
!BOP
!
! !IROUTINE: frazil_mass
!
! !INTERFACE
  subroutine frazil_mass(&
       freezingPotential, &
       frazilMassFlux, &
       seaSurfaceSalinity, &
       config_thermodynamics_type)

    use seaice_constants, only: &
         seaiceDensityIce, &
         seaiceLatentHeatMelting
!
! !DESCRIPTION:
! Calculate frazil mass based on on the sea surface salinity, and frazil heat flux
! from the ocean, otherwise referred to as to as the freeze-melt potential. When
! freezingPotential is positive, it gives the heat flux, according to the ocean model
! associated with the frazil mass passed from the ocean.  This function calculates
! the frazil mass based on the freezingPotential according to sea ice model thermodynamics,
!
! !USES:
      use ice_mushy_physics, only:  &
         liquidus_temperature_mush, &
         enthalpy_mush

      use ice_colpkg_shared, only: &
         dSin0_frazil,             &
         phi_init

! !INPUT PARAMETERS:
      real (kind=RKIND),      intent(in)   :: freezingPotential
      real (kind=RKIND),      intent(in)   :: seaSurfaceSalinity
      character(len=strKIND), intent(in)   :: config_thermodynamics_type

! !OUTPUT PARAMETERS:
      real (kind=RKIND),      intent(out)  :: frazilMassFlux

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------
   real(kind=RKIND) :: &
      Si0new,          &
      Ti,              &
      qi0new,          &
      vi0new


   if (freezingPotential > 0.0_RKIND) then

      if (trim(config_thermodynamics_type) == "mushy") then  ! mushy
         if (seaSurfaceSalinity > 2.0_RKIND * dSin0_frazil) then
             Si0new = seaSurfaceSalinity - dSin0_frazil
         else
             Si0new = seaSurfaceSalinity**2 / (4.0_RKIND*dSin0_frazil)
         endif
         Ti = liquidus_temperature_mush(Si0new/phi_init)
         qi0new = enthalpy_mush(Ti, Si0new)
      else
         qi0new = -seaiceDensityIce*seaiceLatentHeatMelting
      endif    ! ktherm

      frazilMassFlux = -freezingPotential*seaiceDensityIce/qi0new

   else

      frazilMassFlux = 0.0_RKIND

   endif

! REVISION HISTORY:
! Revised Andrew Roberts May 2021
!-----------------------------------------------------------------------
!EOC

 end subroutine frazil_mass!}}}

!***********************************************************************
!
!  function ocn_seaice_finalize
!
!> \brief   Finalize sea ice column model
!> \author  Adrian K. Turner
!> \date    3rd January 2022
!> \details Finalize the sea ice column model by setting non-activated
!>          column pointers back to null
!
!-----------------------------------------------------------------------

  subroutine ocn_seaice_finalize(domain)!{{{

    use seaice_column, only: &
         seaice_column_finalize

    type (domain_type), intent(inout) :: &
         domain

    call mpas_log_write("Finalize column sea ice")

    call seaice_column_finalize(domain)

  end subroutine ocn_seaice_finalize

!-----------------------------------------------------------------------

end module ocn_seaice_column

! vim: foldmethod=marker

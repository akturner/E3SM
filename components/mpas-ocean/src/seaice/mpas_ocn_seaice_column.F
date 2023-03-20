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

    ! reinitialize diagnostics
    call seaice_column_reinitialize_diagnostics_thermodynamics(domain)
    call seaice_column_reinitialize_diagnostics_bgc(domain)

    ! pre dynamics column physics
    call seaice_column_predynamics_time_integration(domain, domain % clock, dt)

    ! shortwave
    call seaice_column_postdynamics_time_integration(domain, domain % clock, dt)

  end subroutine ocn_seaice_run!}}}

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

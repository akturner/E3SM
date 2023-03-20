!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  seaice_advection
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 19th March 2015
!> \details
!>
!
!-----------------------------------------------------------------------

module seaice_advection

  use mpas_derived_types
  use mpas_pool_routines
  use mpas_timer

  implicit none

  private
  save

  public :: &
       seaice_init_advection, &
       seaice_run_advection, &
       seaice_reinitialize_diagnostics_advection

contains

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  seaice_init_advection
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 19th March 2015
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine seaice_init_advection(domain)

    use seaice_advection_upwind, only: &
         seaice_init_advection_upwind

    use seaice_advection_incremental_remap, only: &
         seaice_init_advection_incremental_remap

    type (domain_type), intent(inout) :: &
         domain !< Input/Output:

    logical, pointer :: &
         config_use_advection

    character(len=strKIND), pointer :: &
         config_advection_type

    call MPAS_pool_get_config(domain % blocklist % configs, "config_use_advection", config_use_advection)
    call MPAS_pool_get_config(domain % blocklist % configs, "config_advection_type", config_advection_type)

    if (config_use_advection) then

       if (trim(config_advection_type) == "upwind") then

          call seaice_init_advection_upwind(domain)

       else if (trim(config_advection_type) == "incremental_remap") then

          call seaice_init_advection_incremental_remap(domain)

       endif

    endif

  end subroutine seaice_init_advection

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  seaice_run_advection
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 19th March 2015
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine seaice_run_advection(domain, clock)

    use seaice_advection_upwind, only: &
         seaice_run_advection_upwind

    use seaice_advection_incremental_remap, only: &
         seaice_run_advection_incremental_remap

    type (domain_type), intent(inout) :: &
         domain !< Input/Output:

    type (MPAS_Clock_type), intent(in) :: &
         clock !< Input:

    logical, pointer :: &
         config_use_advection

    character(len=strKIND), pointer :: &
         config_advection_type

    call MPAS_pool_get_config(domain % blocklist % configs, "config_use_advection", config_use_advection)
    call MPAS_pool_get_config(domain % blocklist % configs, "config_advection_type", config_advection_type)

    if (config_use_advection) then

       if (trim(config_advection_type) == "upwind") then

          call mpas_timer_start("advection upwind")
          call seaice_run_advection_upwind(domain, clock)
          call mpas_timer_stop("advection upwind")

       else if (trim(config_advection_type) == "incremental_remap") then

          !TODO WHL - Convert ice/snow volume to thickness here
          !           (Later, change volume to thickness throughout code?)

          call mpas_timer_start("advection incr remap")
          call seaice_run_advection_incremental_remap(domain, clock)
          call mpas_timer_stop("advection incr remap")

          !TODO WHL - Convert ice/snow thickness to volume here

       endif

    endif

  end subroutine seaice_run_advection

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  seaice_reinitialize_diagnostics_advection
!
!> \brief Reinitialize advection diagnostics
!> \author Adrian K. Turner, LANL
!> \date 27th September 2015
!> \details
!>  Reinitialize dynamics diagnostics
!
!-----------------------------------------------------------------------

  subroutine seaice_reinitialize_diagnostics_advection(domain)

    type(domain_type) :: domain

    type(block_type), pointer :: blockPtr

    type(MPAS_pool_type), pointer :: &
         diagnosticsPool, &
         tracersAggregatePool

    ! diagnostic tendencies
    real(kind=RKIND), dimension(:), pointer :: &
         iceAreaTendencyTransport, &
         iceVolumeTendencyTransport, &
         iceAgeTendencyTransport, &
         iceAreaCell, &
         iceVolumeCell, &
         iceAgeCell

    logical, pointer :: &
         config_use_ice_age

    blockPtr => domain % blocklist
    do while (associated(blockPtr))

       ! tendancies
       call MPAS_pool_get_config(blockPtr % configs, "config_use_ice_age", config_use_ice_age)

       call MPAS_pool_get_subpool(blockPtr % structs, "advection_diagnostics", diagnosticsPool)
       call MPAS_pool_get_subpool(blockPtr % structs, "tracers_aggregate", tracersAggregatePool)

       call MPAS_pool_get_array(diagnosticsPool, "iceAreaTendencyTransport", iceAreaTendencyTransport)
       call MPAS_pool_get_array(diagnosticsPool, "iceVolumeTendencyTransport", iceVolumeTendencyTransport)
       call MPAS_pool_get_array(diagnosticsPool, "iceAgeTendencyTransport", iceAgeTendencyTransport)

       call MPAS_pool_get_array(tracersAggregatePool, "iceAreaCell", iceAreaCell)
       call MPAS_pool_get_array(tracersAggregatePool, "iceVolumeCell", iceVolumeCell)
       call MPAS_pool_get_array(tracersAggregatePool, "iceAgeCell", iceAgeCell)

       ! transport tendencies
       iceAreaTendencyTransport   = iceAreaCell
       iceVolumeTendencyTransport = iceVolumeCell
       if (config_use_ice_age) then
          iceAgeTendencyTransport = iceAgeCell
       else
          iceAgeTendencyTransport = 0.0_RKIND
       endif

       blockPtr => blockPtr % next
    end do

  end subroutine seaice_reinitialize_diagnostics_advection

!-----------------------------------------------------------------------

end module seaice_advection

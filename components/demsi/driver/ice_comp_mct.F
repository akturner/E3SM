module ice_comp_mct

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!BOP
! !MODULE: ice_comp_mct
! !INTERFACE:

! !DESCRIPTION:
!  This is the main driver for the Discrete Element Model for Sea Ice model (DEMSI).
!
! !USES:
  ! CPL modules
   use mct_mod
   use esmf
   use seq_flds_mod
   use seq_cdata_mod
   use seq_infodata_mod
   use seq_timemgr_mod
   use seq_comm_mct,      only : seq_comm_suffix, seq_comm_inst, seq_comm_name, info_taskmap_comp
   use shr_file_mod
   use shr_cal_mod,       only : shr_cal_date2ymd
   use shr_sys_mod
   use shr_taskmap_mod,   only : shr_taskmap_write
   use shr_pio_mod
   use perf_mod
   use shr_strdata_mod,   only : shr_strdata_type


!
! !PUBLIC MEMBER FUNCTIONS:
  implicit none
  public :: ice_init_mct
  public :: ice_run_mct
  public :: ice_final_mct
  SAVE
  private                              ! By default make data private

!
! ! PUBLIC DATA:
!
! !REVISION HISTORY:
! Author: Adrian K. Turner
!
!EOP
! !PRIVATE MODULE FUNCTIONS:

  integer, parameter :: &
       StrKIND = 64

  type(seq_infodata_type), pointer :: &
       infodata

  character(len=StrKIND) :: &
       runtype

!
! !PRIVATE MODULE VARIABLES


!=======================================================================

contains

!***********************************************************************
!BOP
!
! !IROUTINE: ice_init_mct
!
! !INTERFACE:
  subroutine ice_init_mct( EClock, cdata_i, x2i_i, i2x_i, NLFilename )!{{{
!
! !DESCRIPTION:
! Initialize MPASSI
!
! !INPUT/OUTPUT PARAMETERS:

    use iso_c_binding, only: &
         c_char, c_int, c_null_char, c_ptr, c_null_ptr, c_loc

    type(ESMF_Clock), intent(inout) :: EClock
    type(seq_cdata), intent(inout) :: cdata_i
    type(mct_aVect), intent(inout) :: x2i_i, i2x_i
    character(len=*), optional, intent(in) :: NLFilename ! Namelist filename
!
! !REVISION HISTORY:
! Author: Jon Wolfe
!EOP
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

    integer :: ICEID !< task number
    integer :: mpicom_i

    type(mct_gsMap), pointer :: gsMap_i

    type(mct_gGrid), pointer :: dom_i

    type(c_ptr) :: argv(2)

    character(len=9, kind=c_char) :: &
         configFilename = c_char_'demsi_in'//c_null_char

    character(len=46, kind=c_char) :: &
         configDoRestart = c_char_'ConfigGroup:simulationTiming,Config:doRestart'//c_null_char

    character(len=StrKIND) :: &
         starttype

!-----------------------------------------------------------------------
!
!   interfaces
!
!-----------------------------------------------------------------------

    interface
       subroutine demsi_start_model(narg, argv, mpicom) bind(C)
         use iso_c_binding, only: c_char, c_int
         implicit none
         integer(kind=c_int) :: narg
         type(c_ptr) :: argv
         integer(kind=c_int) :: mpicom
       end subroutine demsi_start_model
    end interface

    interface
       subroutine demsi_initialize_model(narg, argv, inE3SM) bind(C)
         implicit none
         integer(kind=c_int) :: narg
         type(c_ptr) :: argv
         integer(kind=c_int) :: inE3SM
       end subroutine demsi_initialize_model
    end interface

    interface
       subroutine demsi_modify_config_int(elementLocation, newValue) bind(C)
         use iso_c_binding, only: c_char, c_int
         implicit none
         character(kind=c_char), dimension(*) :: elementLocation
         integer(kind=c_int) :: newValue
       end subroutine demsi_modify_config_int
    end interface

    interface
       subroutine demsi_modify_config_double(elementLocation, newValue) bind(C)
         use iso_c_binding, only: c_char, c_double
         implicit none
         character(kind=c_char), dimension(*) :: elementLocation
         real(kind=c_double) :: newValue
       end subroutine demsi_modify_config_double
    end interface

    interface
       subroutine demsi_modify_config_string(elementLocation, newValue) bind(C)
         use iso_c_binding, only: c_char
         implicit none
         character(kind=c_char), dimension(*) :: elementLocation
         character(kind=c_char), dimension(*) :: newValue
       end subroutine demsi_modify_config_string
    end interface

    interface
       subroutine demsi_modify_config_bool(elementLocation, newValue) bind(C)
         use iso_c_binding, only: c_char, c_int
         implicit none
         character(kind=c_char), dimension(*) :: elementLocation
         integer(kind=c_int) :: newValue
       end subroutine demsi_modify_config_bool
    end interface

    interface
       subroutine demsi_abort(message) bind(C)
         use iso_c_binding, only: c_char
         implicit none
         character(kind=c_char), dimension(*) :: message
       end subroutine demsi_abort
    end interface

    integer(kind=c_int), parameter :: &
         TRUE_C  = 1_c_int, &
         FALSE_C = 0_c_int

!-----------------------------------------------------------------------
!
!   Set cdata pointers
!
!-----------------------------------------------------------------------

    call seq_cdata_setptrs(cdata_i, ID=ICEID, mpicom=mpicom_i, &
         gsMap=gsMap_i, dom=dom_i, infodata=infodata)

!-----------------------------------------------------------------------
!
!   Start demsi model
!
!-----------------------------------------------------------------------

    argv(1) = c_loc(configFilename)
    argv(2) = c_null_ptr
    call demsi_startup_model(1, argv, mpicom_i)

!-----------------------------------------------------------------------
!
!   Override configs
!
!-----------------------------------------------------------------------

    call seq_infodata_GetData(infodata, start_type=starttype)
    if (     trim(starttype) == trim(seq_infodata_start_type_start)) then
       runtype = "initial"
    else if (trim(starttype) == trim(seq_infodata_start_type_cont) ) then
       runtype = "continue"
    else if (trim(starttype) == trim(seq_infodata_start_type_brnch)) then
       runtype = "branch"
    else
       call demsi_abort('ice_comp_mct ERROR: infodata provided unknown starttype: '//trim(starttype)//c_null_char)
    end if

    if (runtype == "initial") then ! Start up run

       ! modify restart flag
       call demsi_modify_config_bool(configDoRestart, FALSE_C)

    else if (runtype == "continue" .or. &
             runtype == "branch") then ! Restart run or branch run

       ! modify restart flag
       call demsi_modify_config_bool(configDoRestart, TRUE_C)

    end if

!-----------------------------------------------------------------------
!
!   Initialize LAMMPS
!
!-----------------------------------------------------------------------

    call demsi_initialize_model(1, argv, TRUE_C)

!-----------------------------------------------------------------------
!EOC

  end subroutine ice_init_mct!}}}

!***********************************************************************
!BOP
!
! !IROUTINE: ice_run_mct
!
! !INTERFACE:
  subroutine ice_run_mct( EClock, cdata_i, x2i_i, i2x_i)!{{{

!
! !DESCRIPTION:
! Run MPASSI for one coupling interval
!
! !INPUT/OUTPUT PARAMETERS:
      type(ESMF_Clock)            , intent(inout) :: EClock
      type(seq_cdata)             , intent(inout) :: cdata_i
      type(mct_aVect)             , intent(inout) :: x2i_i
      type(mct_aVect)             , intent(inout) :: i2x_i

!! !REVISION HISTORY:
!! Author: Adrian K. Turner
!!EOP
!!-----------------------------------------------------------------------
!!
!!  local variables
!!
!!-----------------------------------------------------------------------

      interface
         subroutine demsi_time_step() bind(C)
           implicit none
         end subroutine demsi_time_step
      end interface

      call demsi_time_step()

!-----------------------------------------------------------------------
!EOC

    end subroutine ice_run_mct!}}}

!***********************************************************************
!BOP
!
! !IROUTINE: ice_final_mct
!
! !INTERFACE:
  subroutine ice_final_mct( EClock, cdata_i, x2i_i, i2x_i)!{{{

!
! !DESCRIPTION:
! Finalize MPASSI
!
! !USES:
! !ARGUMENTS:
    type(ESMF_Clock)            , intent(inout) :: EClock
    type(seq_cdata)             , intent(inout) :: cdata_i
    type(mct_aVect)             , intent(inout) :: x2i_i
    type(mct_aVect)             , intent(inout) :: i2x_i
!
! !REVISION HISTORY:
! Author: Adrian K. Turner
!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

    interface
       subroutine demsi_finalize() bind(C)
         implicit none
       end subroutine demsi_finalize
    end interface

    call demsi_finalize()

  end subroutine ice_final_mct!}}}

end module ice_comp_mct

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

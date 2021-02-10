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

    interface
       subroutine demsi_initialize_e3sm() bind(C)
         implicit none
       end subroutine demsi_initialize_e3sm
    end interface

    call demsi_initialize_e3sm()

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
         subroutine demsi_run() BIND(C)
           implicit none
         end subroutine demsi_run
      end interface

      call demsi_run()

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
       subroutine demsi_finalize() BIND(C)
         implicit none
       end subroutine demsi_finalize
    end interface

    call demsi_finalize()

  end subroutine ice_final_mct!}}}

end module ice_comp_mct

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

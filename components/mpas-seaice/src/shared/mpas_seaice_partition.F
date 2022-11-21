!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  seaice_partition
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 
!> \details
!>
!
!-----------------------------------------------------------------------

module seaice_partition

  use mpas_derived_types
  use mpas_pool_routines
  use mpas_log, only: mpas_log_write

  implicit none

  private
  save

  public :: &
       seaice_init_block_iterator, &
       seaice_block_iterator

  logical, pointer :: &
       useEvenBlocksOnly

contains

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  seaice_init_block_iterator
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine seaice_init_block_iterator(domain)

    type(domain_type), intent(inout) :: &
         domain

    type(block_type), pointer :: &
         blockPtr

    type(MPAS_pool_type), pointer :: &
         diagnosticsPool

    integer, dimension(:), pointer :: &
         cellActive, &
         cellEvenness

    integer, pointer :: &
         nCellsSolve

    integer :: &
         iCell

    call mpas_log_write(" Initializing sea ice partitions...")

    call mpas_pool_get_config(domain % configs, "config_use_even_blocks_only", useEvenBlocksOnly)

    if (useEvenBlocksOnly) then

       call MPAS_log_write("config_explicit_proc_decomp: $l", logicArgs=(/domain % dminfo % explicitDecomp/))

       blockPtr => domain % blocklist
       do while (seaice_block_iterator(blockPtr))

          call MPAS_log_write("Proc/Block ID: $i, $i", intArgs=(/domain % dminfo % my_proc_id, blockPtr % blockID/))

          call MPAS_pool_get_dimension(blockPtr % dimensions, "nCells", nCellsSolve)

          call MPAS_pool_get_subpool(blockPtr % structs, "diagnostics", diagnosticsPool)

          call MPAS_pool_get_array(diagnosticsPool, "cellActive", cellActive)
          call MPAS_pool_get_array(diagnosticsPool, "cellEvenness", cellEvenness)

          do iCell = 1, nCellsSolve

             cellActive(iCell) = 1
             cellEvenness(iCell) = modulo(blockPtr % blockID,2)

          enddo ! iCell

          blockPtr => blockPtr % next
       enddo

    endif

  end subroutine seaice_init_block_iterator

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  seaice_block_iterator
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 
!> \details
!>
!
!-----------------------------------------------------------------------

  function seaice_block_iterator(blockPtr) result(blockStatus)

    type(block_type), pointer :: &
         blockPtr

    logical :: &
         blockStatus

    if (.not. associated(blockPtr)) then

       blockStatus = .false.

    else

       blockStatus = .true.

       if (useEvenBlocksOnly .and. modulo(blockPtr % blockID, 2) == 1) then
          blockPtr => blockPtr % next

          if (.not. associated(blockPtr)) then
             blockStatus = .false.
          endif

       endif

    endif

  end function seaice_block_iterator

!-----------------------------------------------------------------------

end module seaice_partition

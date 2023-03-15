!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  seaice_particles
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date
!> \details
!>
!
!-----------------------------------------------------------------------

module seaice_particles

  use mpas_derived_types
  use mpas_pool_routines
  use mpas_timekeeping
  use mpas_io_units
  use mpas_log, only: mpas_log_write
  use mpas_dmpar
  use mpas_stream_manager
  use mpi

  implicit none

  private
  save

  public :: &
       seaice_particles_init, &
       seaice_particles_run

  logical :: madeChanges = .false.

contains

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  seaice_particles_init
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine seaice_particles_init(domain)

    type(domain_type), intent(inout) :: &
         domain !< Input/Output:

    type(block_type), pointer :: &
         blockPtr

    type(MPAS_pool_type), pointer :: &
         particlesPool

    integer, pointer :: &
         nParticles

    integer :: &
         nParticlesProc, &
         nParticlesTotal, &
         iParticle

    integer, dimension(:), pointer :: &
         particleID

    nParticlesProc = 0

    blockPtr => domain % blocklist
    do while (associated(blockPtr))

       call MPAS_pool_get_dimension(blockPtr % dimensions, "nParticles", nParticles)

       call mpas_log_write("nParticles: $i", intArgs=(/nParticles/))

       call MPAS_pool_get_subpool(blockPtr % structs, "particles", particlesPool)

       call MPAS_pool_get_array(particlesPool, "particleID", particleID)

       do iParticle = 1, nParticles

          call mpas_log_write("particleID: $i $i", intArgs=(/iParticle, particleID(iParticle)/))

       enddo ! iParticle

       nParticlesProc = nParticlesProc + nParticles

       blockPtr => blockPtr % next
    enddo

    call mpas_dmpar_sum_int(domain % dminfo, nParticlesProc, nParticlesTotal)

    call mpas_log_write("nParticlesTotal: $i", intArgs=(/nParticlesTotal/))

  end subroutine seaice_particles_init

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  seaice_particles_run
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine seaice_particles_run(domain)

    type(domain_type), intent(inout) :: &
         domain !< Input/Output:

    type(block_type), pointer :: &
         blockPtr

    type(MPAS_pool_type), pointer :: &
         particlesPool

    type(field1DInteger), pointer :: &
         particleIDField

    integer, pointer :: &
         nParticles

    integer :: &
         iParticle, &
         nParticlesNew

    type(MPAS_Time_Type) :: &
         currTime, &
         changeTime

    real(kind=RKIND) :: &
         u

    currTime = mpas_get_clock_time(domain % clock, MPAS_NOW)
    call mpas_set_time(changeTime, dateTimeString="2000-01-01_13:00:00")

    if (.not. madeChanges .and. currTime .gt. changeTime) then

       call mpas_log_write("seaice_particles_run: Making changes")

       madeChanges = .true.

       ! changes in particle model
       blockPtr => domain % blocklist
       do while (associated(blockPtr))

          call random_number(u)
          nParticlesNew = floor(20*u)

          ! update nParticle dimension
          call MPAS_pool_get_dimension(blockPtr % dimensions, "nParticles", nParticles)
          nParticles = nParticlesNew

          ! reset particleID field
          call MPAS_pool_get_subpool(blockPtr % structs, "particles", particlesPool)
          call MPAS_pool_get_field(particlesPool, "particleID", particleIDField)
          particleIDField % dimSizes(1) = nParticles

          deallocate(particleIDField % array)
          allocate(particleIDField % array(nParticles))

          do iParticle = 1, nParticles
             particleIDField % array(iParticle) = iParticle + 10000 * domain % dminfo % my_proc_id
          enddo ! iParticle

          blockPtr => blockPtr % next
       enddo

       ! changes needed in streams
       call seaice_particles_modify_streams(domain)

    endif

  end subroutine seaice_particles_run

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  seaice_particles_modify_streams
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine seaice_particles_modify_streams(domain)

    type(domain_type), intent(inout) :: &
         domain !< Input/Output:

    type(block_type), pointer :: &
         blockPtr

    integer, pointer :: &
         nParticles

    integer :: &
         nParticlesThisProc, &
         nParticlesAllPrevProcs, &
         nParticlesThisProcPrev, &
         nParticlesTotal, &
         iParticle, &
         iProc, &
         ierr, &
         directionProperty

    logical :: &
         activeProperty

    integer, dimension(:), allocatable :: &
         nParticlesAllProcs

    type(field1DInteger), pointer :: &
         nParticlesOwnedIndicesField

    character(len=strKIND) :: &
         streamID

    type(field_list_type), pointer :: &
         fieldList

    ! changes needed for streams
    nParticlesThisProc = 0
    blockPtr => domain % blocklist
    do while (associated(blockPtr))

       call MPAS_pool_get_dimension(blockPtr % dimensions, "nParticles", nParticles)
       nParticlesThisProc = nParticlesThisProc + nParticles

       blockPtr => blockPtr % next
    enddo

    allocate(nParticlesAllProcs(domain % dminfo % nprocs))
    call MPI_ALLGATHER(nParticlesThisProc, 1, MPI_INTEGER, nParticlesAllProcs, 1, MPI_INTEGER, domain % dminfo % comm, ierr)

    nParticlesAllPrevProcs = 0
    do iProc = 1, domain % dminfo % my_proc_id
       nParticlesAllPrevProcs = nParticlesAllPrevProcs + nParticlesAllProcs(iProc)
    enddo ! iProc

    ! modify nParticlesOwnedIndices
    nParticlesThisProcPrev = 0
    blockPtr => domain % blocklist
    do while (associated(blockPtr))

       ! reset nParticlesOwnedIndices field
       call mpas_pool_get_field(blockPtr % allFields, "nParticlesOwnedIndices", nParticlesOwnedIndicesField)

       deallocate(nParticlesOwnedIndicesField % array)
       allocate(nParticlesOwnedIndicesField % array(nParticles))

       do iParticle = 1, nParticles
          nParticlesOwnedIndicesField % array(iParticle) = iParticle + nParticlesThisProcPrev + nParticlesAllPrevProcs
       enddo ! iParticle

       nParticlesThisProcPrev = nParticlesThisProcPrev + nParticles

       blockPtr => blockPtr % next
    enddo

    nParticlesTotal = 0
    do iProc = 1, domain % dminfo % nprocs
       nParticlesTotal = nParticlesTotal + nParticlesAllProcs(iProc)
    enddo ! iProc

    ! update totalDimSize in streams
    call MPAS_stream_mgr_begin_iteration(domain % streamManager, ierr=ierr)
    do while (MPAS_stream_mgr_get_next_stream(domain % streamManager, streamID, directionProperty, activeProperty))

       if (activeProperty .and. directionProperty == MPAS_STREAM_OUTPUT .and. trim(streamID) == "particlesOut") then

          fieldList => domain % streamManager % currentStream % stream % fieldList
          do while (associated(fieldList))

             fieldList % totalDimSize = nParticlesTotal

             fieldList => fieldList % next
          enddo

       endif ! active stream

    enddo ! stream iteration

    call mpas_log_write("Modify Particle Number")
    call mpas_log_write("nParticles:      $i",intArgs=(/nParticlesThisProc/))
    call mpas_log_write("nParticlesTotal: $i",intArgs=(/nParticlesTotal/))

  end subroutine seaice_particles_modify_streams

!-----------------------------------------------------------------------

end module seaice_particles

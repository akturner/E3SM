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
  use mpas_log

  implicit none
  private

  public :: &
       ocn_init_seaice

!***********************************************************************

contains

!***********************************************************************
!
!  function ocn_init_seaice
!
!> \brief   Initialize the sea ice column package
!> \author  Adrian K. Turner
!> \date    30th November 2022
!> \details Perform operations that initialize the sea ice column
!>          package when run directly from MPAS-Ocean
!
!-----------------------------------------------------------------------

  subroutine ocn_init_seaice(domain)!{{{

    use seaice_column, only: &
         seaice_init_column_physics_package_parameters

    type (domain_type), intent(inout) :: domain

    call mpas_log_write("RUNNING SEA ICE!")

    call seaice_init_column_physics_package_parameters(domain)

  end subroutine ocn_init_seaice!}}}

end module ocn_seaice_column

! vim: foldmethod=marker

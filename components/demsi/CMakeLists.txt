if (EXISTS "${BUILDCONF}/demsiconf")

  include(GNUInstallDirs)

  # Source CIME-generated Macros
  include(${CASEROOT}/Macros.cmake)
  # Load machine/compiler specific settings
  set(COMPILER_SPECIFIC_DEPENDS ${CASEROOT}/Depends.${COMPILER}.cmake)
  set(MACHINE_SPECIFIC_DEPENDS ${CASEROOT}/Depends.${MACH}.cmake)
  set(PLATFORM_SPECIFIC_DEPENDS ${CASEROOT}/Depends.${MACH}.${COMPILER}.cmake)
  set(TRY_TO_LOAD ${COMPILER_SPECIFIC_DEPENDS} ${MACHINE_SPECIFIC_DEPENDS} ${PLATFORM_SPECIFIC_DEPENDS})
  foreach(ITEM IN LISTS TRY_TO_LOAD)
    if (EXISTS ${ITEM})
      include(${ITEM})
    endif()
  endforeach()

  if (NETCDF_PATH)
    set(netCDF_INCLUDE_DIR "${NETCDF_PATH}/include")
    find_library(netCDF_LIBRARY NAMES netcdf PATHS "${NETCDF_PATH}/lib")
  else()
    if (NETCDF_C_PATH)
      set(netCDF_INCLUDE_DIR "${NETCDF_C_PATH}/include")
      find_library(netCDF_LIBRARY NAMES netcdf PATHS "${NETCDF_C_PATH}/lib")
    else()
      message(FATAL_ERROR "DEMSI Netcdf library not present")
    endif()
  endif()

  set(DEMSI_BUILD_EXECUTABLE false)
  set(DEMSI_BUILD_LOC "${PROJECT_BINARY_DIR}/demsi/DEMSI/")
  set(DEMSI_LIBRARY_SUFFIX ${CMAKE_SHARED_LIBRARY_SUFFIX})

  add_subdirectory(DEMSI/model)
  add_subdirectory(driver)

endif()

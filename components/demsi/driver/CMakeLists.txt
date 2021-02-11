set( CMAKE_VERBOSE_MAKEFILE on )

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

if (USE_ESMF_LIB)
  set(ESMFDIR "esmf")
else()
  set(ESMFDIR "noesmf")
endif()

set(INCLUDES "${INSTALL_SHAREDPATH}/include" "${INSTALL_SHAREDPATH}/${COMP_INTERFACE}/${ESMFDIR}/${NINST_VALUE}/csm_share")

add_library(ice
    ${CMAKE_BINARY_DIR}/demsi/driver/ice_comp_mct.f90)

target_include_directories(ice PRIVATE ${INCLUDES})

add_custom_command(
  OUTPUT ${CMAKE_BINARY_DIR}/demsi/driver/ice_comp_mct.f90
  COMMAND cpp -P -traditional ${CMAKE_CURRENT_SOURCE_DIR}/ice_comp_mct.F > ${CMAKE_BINARY_DIR}/demsi/driver/ice_comp_mct.f90
  DEPENDS ice_comp_mct.F)

add_dependencies(ice demsilib)
target_include_directories(ice PUBLIC "${EXEROOT}/cmake-bld/demsi/DEMSI/model/src/")
target_link_libraries(ice PUBLIC "${EXEROOT}/cmake-bld/demsi/DEMSI/model/build/src-install/libdemsilib.so")

include_directories(${CMAKE_BINARY_DIR}/demsi/driver/)
link_directories(${CMAKE_BINARY_DIR}/demsi/driver/)

# Define a global property to check if yamlcpp has already been built
define_property(GLOBAL
                PROPERTY EKAT_YAMLCPP_BUILT
                BRIEF_DOCS "Wheter yaml-cpp subdir has already been processed"
                FULL_DOCS "This property is used by cmake to ensure that yamlcpp
                           submodule directory is only processed once (with add_subdirectory).")

get_property(IS_EKAT_YAMLCPP_BUILT GLOBAL PROPERTY EKAT_YAMLCPP_BUILT SET)

# Process the libyaml subdirectory
if (NOT IS_YAMLCPP_ALREADY_BUILT)

  if (NOT YAMLCPP_SOURCE_DIR)
    message (FATAL_ERROR "Error! Please, specify path to yaml-cpp in YAMLCPP_SOURCE_DIR.\n")
  elseif (NOT EXISTS ${YAMLCPP_SOURCE_DIR})
    message (FATAL_ERROR "Error! Please specify a valid source folder for yamlcpp.\n"
                         "       Provided path: ${YAMLCPP_SOURCE_DIR}")
  endif()
  set (YAMLCPP_BINARY_DIR ${CMAKE_BINARY_DIR}/externals/yaml-cpp)

  # We don't want testing
  set (BUILD_TESTING FALSE)
  add_subdirectory (${YAMLCPP_SOURCE_DIR} ${YAMLCPP_BINARY_DIR})

  set (YAMLCPP_INCLUDE_DIRS ${YAMLCPP_SOURCE_DIR}/include)
  set (YAMLCPP_LIBRARIES yaml-cpp)

  # Make sure it is processed only once
  set_property(GLOBAL PROPERTY EKAT_YAMLCPP_BUILT TRUE)
endif()

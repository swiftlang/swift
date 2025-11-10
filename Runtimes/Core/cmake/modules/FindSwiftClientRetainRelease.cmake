#[=======================================================================[.rst:
FindSwiftClientRetainRelease
------------

Find SwiftClientRetainRelease, deferring to the associated SwiftClientRetainReleaseConfig.cmake when requested.
This is meant to find the Cxx overlays to be linked by the Supplemental libraries.

Imported Targets
^^^^^^^^^^^^^^^^

The following :prop_tgt:`IMPORTED` TARGETS may be defined:

 ``swiftRetainReleaseClient``

#]=======================================================================]

include_guard(GLOBAL)

if(SwiftClientRetainRelease_DIR)
  if(${CMAKE_FIND_PACKAGE_NAME}_FIND_REQUIRED)
    list(APPEND args REQUIRED)
  endif()
  if(${CMAKE_FIND_PACKAGE_NAME}_FIND_QUIETLY)
    list(APPEND args QUIET)
  endif()
  find_package(SwiftClientRetainRelease CONFIG ${args})
  return()
endif()

include(FindPackageHandleStandardArgs)
include(PlatformInfo)

if(APPLE)
  # ClientRetainRelease is not installed in the SDKs, but in the
  # toolchain next to the compiler
  set(target_info_command "${CMAKE_Swift_COMPILER}" -print-target-info)
  if(CMAKE_Swift_COMPILER_TARGET)
    list(APPEND target_info_command -target ${CMAKE_Swift_COMPILER_TARGET})
  endif()
  execute_process(COMMAND ${target_info_command} OUTPUT_VARIABLE target_info_json)
  message(CONFIGURE_LOG "Swift target info: ${target_info_command}\n"
  "${target_info_json}")

  string(JSON runtime_library_import_paths_json GET "${target_info_json}" "paths" "runtimeLibraryImportPaths")
  message(CONFIGURE_LOG "runtime_library_import_paths_json ${runtime_library_import_paths_json}")

  string(JSON number_of_runtime_library_import_paths LENGTH "${runtime_library_import_paths_json}")
  math(EXPR index_of_last_runtime_library_import_path "${number_of_runtime_library_import_paths} - 1")
  foreach(index RANGE 0 ${index_of_last_runtime_library_import_path})
    string(JSON runtime_library_import_path GET "${runtime_library_import_paths_json}" ${index})

    list(APPEND SwiftClientRetainRelease_LIBRARY_HINTS
      "${runtime_library_import_path}")
  endforeach()

  list(APPEND swiftRetainReleaseClient_NAMES libswiftClientRetainRelease.a)
else()
  message(FATAL_ERROR "ClientRetainRelease is only available for Apple platforms at the moment.\n")
endif()

find_library(swiftRetainReleaseClient_LIBRARY
  NAMES
    ${swiftRetainReleaseClient_NAMES}
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${SwiftClientRetainRelease_LIBRARY_HINTS})

add_library(swiftRetainReleaseClient STATIC IMPORTED GLOBAL)
set_target_properties(swiftRetainReleaseClient PROPERTIES
  IMPORTED_LOCATION "${swiftRetainReleaseClient_LIBRARY}")

find_package_handle_standard_args(SwiftClientRetainRelease DEFAULT_MSG
  swiftRetainReleaseClient_LIBRARY)

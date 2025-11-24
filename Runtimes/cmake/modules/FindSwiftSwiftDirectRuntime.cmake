#[=======================================================================[.rst:
FindSwiftSwiftDirectRuntime
------------

Find swiftSwiftDirectRuntime, deferring to the associated SwiftSwiftDirectRuntimeConfig.cmake when requested.
This is meant to be linked in swiftCore.

Imported Targets
^^^^^^^^^^^^^^^^

The following :prop_tgt:`IMPORTED` TARGETS may be defined:

 ``swiftSwiftDirectRuntime``

#]=======================================================================]

include_guard(GLOBAL)

if(SwiftSwiftDirectRuntime_DIR)
  if(${CMAKE_FIND_PACKAGE_NAME}_FIND_REQUIRED)
    list(APPEND args REQUIRED)
  endif()
  if(${CMAKE_FIND_PACKAGE_NAME}_FIND_QUIETLY)
    list(APPEND args QUIET)
  endif()
  find_package(SwiftSwiftDirectRuntime CONFIG ${args})
  return()
endif()

include(FindPackageHandleStandardArgs)
include(PlatformInfo)

if(APPLE)
  # SwiftDirectRuntime is not installed in the SDKs, but in the
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

    list(APPEND swiftSwiftDirectRuntime_LIBRARY_HINTS
      "${runtime_library_import_path}")
  endforeach()

  list(APPEND swiftSwiftDirectRuntime_NAMES libswiftSwiftDirectRuntime.a)
else()
  message(WARNING "SwiftDirectRuntime is only available for Apple platforms at the moment.\n")
  return()
endif()

find_library(swiftSwiftDirectRuntime_LIBRARY
  NAMES
    ${swiftSwiftDirectRuntime_NAMES}
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${swiftSwiftDirectRuntime_LIBRARY_HINTS})

add_library(swiftSwiftDirectRuntime STATIC IMPORTED GLOBAL)
set_target_properties(swiftSwiftDirectRuntime PROPERTIES
  IMPORTED_LOCATION "${swiftSwiftDirectRuntime_LIBRARY}")

find_package_handle_standard_args(SwiftSwiftDirectRuntime DEFAULT_MSG
  swiftSwiftDirectRuntime_LIBRARY)

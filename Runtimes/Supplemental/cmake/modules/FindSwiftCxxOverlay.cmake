#[=======================================================================[.rst:
FindSwiftCxxOverlay
------------

Find SwiftCxxOverlay, deferring to the associated SwiftCxxOverlayConfig.cmake when requested.
This is meant to find the Cxx overlays to be linked by the Supplemental libraries.

Imported Targets
^^^^^^^^^^^^^^^^

The following :prop_tgt:`IMPORTED` TARGETS may be defined:

 ``swiftCxx``
 ``swiftCxxStdlib``
 ``cxxshims`` (except on Apple platforms)
 ``libstdcxx`` (if libstdc++ is present)

Hint Variables
^^^^^^^^^^^^^^

 ``SDKROOT`` (environment variable)
   Set the path to the Swift SDK Root.
   This only affects Windows and Android builds.

 ``Swift_SDKROOT``
   Set the path to the Swift SDK installation.
   This affects Linux, Android, and Windows builds.
   Apple builds always use the overlay provided by the SDK.

#]=======================================================================]

include_guard(GLOBAL)

if(SwiftCxxOverlay_DIR)
  if(${CMAKE_FIND_PACKAGE_NAME}_FIND_REQUIRED)
    list(APPEND args REQUIRED)
  endif()
  if(${CMAKE_FIND_PACKAGE_NAME}_FIND_QUIETLY)
    list(APPEND args QUIET)
  endif()
  find_package(SwiftCxxOverlay CONFIG ${args})
  return()
endif()

include(FindPackageHandleStandardArgs)
include(PlatformInfo)
include(CheckSymbolExists)

if(APPLE)
  # Cxx Interop is not installed in the SDKs, but in the
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

    list(APPEND SwiftCxxOverlay_INCLUDE_DIR_HINTS
      "${runtime_library_import_path}")
    list(APPEND SwiftCxxOverlay_LIBRARY_HINTS
      "${runtime_library_import_path}")
  endforeach()

  list(APPEND swiftCxx_NAMES libswiftCxx.a)
  list(APPEND swiftCxxStdlib_NAMES libswiftCxxStdlib.a)
elseif(LINUX)
  list(APPEND SwiftCxxOverlay_INCLUDE_DIR_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/linux/")
  list(APPEND SwiftCxxOverlay_LIBRARY_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/linux")
  list(APPEND swiftCxx_NAMES libswiftCxx.a)
  list(APPEND swiftCxxStdlib_NAMES libswiftCxxStdlib.a)
elseif(WIN32)
  list(APPEND SwiftCxxOverlay_INCLUDE_DIR_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/windows"
    "$ENV{SDKROOT}/usr/lib/swift/windows")
  list(APPEND SwiftCxxOverlay_LIBRARY_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}"
    "${Swift_SDKROOT}/usr/lib/swift"
    "$ENV{SDKROOT}/usr/lib/swift/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}"
    "$ENV{SDKROOT}/usr/lib/swift")

    list(APPEND swiftCxx_NAMES libswiftCxx.lib)
    list(APPEND swiftCxxStdlib_NAMES libswiftCxxStdlib.lib)
elseif(ANDROID)
  list(APPEND SwiftCxxOverlay_INCLUDE_DIR_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/android"
    "$ENV{SDKROOT}/usr/lib/swift/android")
  list(APPEND SwiftCxxOverlay_LIBRARY_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
    "${Swift_SDKROOT}/usr/lib/swift"
    "$ENV{SDKROOT}/usr/lib/swift/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
    "$ENV{SDKROOT}/usr/lib/swift")
  list(APPEND swiftCxx_NAMES libswiftCxx.a)
  list(APPEND swiftCxxStdlib_NAMES libswiftCxxStdlib.a)
else()
  message(FATAL_ERROR "FindSwiftCxxOverlay.cmake module search not implemented for targeted platform\n"
  " Build the Overlays for your platform and set the appropriate `SwiftCxxOverlay_DIR` variable to"
  " the directory containing SwiftCxxOverlayConfig.cmake\n")
endif()

set(swiftCxx_MODULE_NAME "Cxx.swiftmodule")
set(swiftCxxStdlib_MODULE_NAME "CxxStdlib.swiftmodule")
if(NOT APPLE)
  set(cxxshims_MODULE_NAME libcxxshim.modulemap)
endif()
check_symbol_exists(__GLIBCXX__ "version" HAVE___GLIBCXX__)
if(HAVE___GLIBCXX__)
  set(libstdcxx_MODULE_NAME libstdcxx.modulemap)
endif()

find_path(swiftCxx_INCLUDE_DIR
  ${swiftCxx_MODULE_NAME}
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${SwiftCxxOverlay_INCLUDE_DIR_HINTS})
find_library(swiftCxx_LIBRARY
  NAMES
    ${swiftCxx_NAMES}
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${SwiftCxxOverlay_LIBRARY_HINTS})

add_library(swiftCxx STATIC IMPORTED GLOBAL)
target_include_directories(swiftCxx INTERFACE
  "${swiftCxx_INCLUDE_DIR}")
set_target_properties(swiftCxx PROPERTIES
  IMPORTED_LOCATION "${swiftCxx_LIBRARY}")

find_path(swiftCxxStdlib_INCLUDE_DIR
  ${swiftCxxStdlib_MODULE_NAME}
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${SwiftCxxOverlay_INCLUDE_DIR_HINTS})
find_library(swiftCxxStdlib_LIBRARY
  NAMES
    ${swiftCxxStdlib_NAMES}
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${SwiftCxxOverlay_LIBRARY_HINTS})

add_library(swiftCxxStdlib STATIC IMPORTED GLOBAL)
target_include_directories(swiftCxxStdlib INTERFACE
  "${swiftCxxStdlib_INCLUDE_DIR}")
set_target_properties(swiftCxxStdlib PROPERTIES
  IMPORTED_LOCATION "${swiftCxxStdlib_LIBRARY}")

if(cxxshims_MODULE_NAME)
  find_path(cxxshims_INCLUDE_DIR
    ${cxxshims_MODULE_NAME}
    NO_CMAKE_FIND_ROOT_PATH
    HINTS
      ${SwiftCxxOverlay_INCLUDE_DIR_HINTS})

  add_library(cxxshims INTERFACE IMPORTED GLOBAL)
  target_compile_options(cxxshims INTERFACE
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -fmodule-map-file=${cxxshims_INCLUDE_DIR}/${cxxshims_MODULE_NAME}>")
endif()
if(libstdcxx_MODULE_NAME)
  find_path(libstdcxx_INCLUDE_DIR
    ${libstdcxx_MODULE_NAME}
    NO_CMAKE_FIND_ROOT_PATH
    HINTS
      ${SwiftCxxOverlay_INCLUDE_DIR_HINTS})

  add_library(libstdcxx INTERFACE IMPORTED GLOBAL)
  target_compile_options(libstdcxx INTERFACE
    "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -fmodule-map-file=${libstdcxx_INCLUDE_DIR}/${libstdcxx_MODULE_NAME}>")
endif()

set(vars_to_check swiftCxx_LIBRARY swiftCxx_INCLUDE_DIR swiftCxxStdlib_LIBRARY swiftCxxStdlib_INCLUDE_DIR)
if(cxxshims_MODULE_NAME)
  list(APPEND vars_to_check cxxshims_INCLUDE_DIR)
endif()
if(libstdcxx_MODULE_NAME)
  list(APPEND vars_to_check libstdcxx_INCLUDE_DIR)
endif()

find_package_handle_standard_args(SwiftCxxOverlay DEFAULT_MSG
  ${vars_to_check})


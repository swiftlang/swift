#[=======================================================================[.rst:
FindSwiftCore
------------

Find SwiftCore, deferring to SwiftCoreConfig.cmake when requested.
This is meant to find the core library to be linked by the Supplemental libraries.

Imported Targets
^^^^^^^^^^^^^^^^

The following :prop_tgt:`IMPORTED` TARGETS may be defined:

 ``swiftCore``

Hint Variables
^^^^^^^^^^^^^^

 ``SDKROOT`` (environment variable)
   Set the path to the Swift SDK Root.
   This only affects Windows builds.

 ``Swift_SDKROOT``
   Set the path to the Swift SDK installation.
   This only affects Linux and Windows builds.
   Apple builds always use the library provided by the SDK.

 ``SwiftCore_USE_STATIC_LIBS``
   Set to boolean true to find static libraries. If not
   set explicitly this is inferred from the value of
   `BUILD_SHARED_LIBS` and the current platform.

 ``SwiftCore_INCLUDE_DIR_HINTS``
   Additional paths to search the swiftmodules into. These
   are prepended to the ones searched by this find module

 ``SwiftCore_LIBRARY_HINTS``
   Additional paths to search the libraries into. These
   are prepended to the ones searched by this find module

 ``SwiftShims_INCLUDE_DIR_HINTS``
   Additional paths to search the shims into. These
   are prepended to the ones searched by this find module

 ``SwiftCore_NAMES``
   Additional names for the Core library

 ``SwiftOnoneSupport_NAMES``
   Additional names for the SwiftOnoneSupport library

 ``SwiftConcurrency_NAMES``
   Additional names for the Swift_Concurrency library

Result targets
^^^^^^^^^^^^^^

If no error is generated, the following targets are available to the users

  * ``swiftCore``
  * ``swiftSwiftOnoneSupport``
  * ``swift_Concurrency``
  * ``swiftShims``

Result Variables
^^^^^^^^^^^^^^^^

The module may set the following variables if `SwiftCore_DIR` is not set.
(although we suggest relying on the targets above instead)

 ``SwiftCore_FOUND``
   true if core was found

 ``SwiftCore_INCLUDE_DIR``
   the directory containing the Swift.swiftmodule folder

 ``SwiftCore_LIBRARY``
   path to the swiftCore library

 ``SwiftOnoneSupport_INCLUDE_DIR``
   the directory containing the SwiftOnoneSupport.swiftmodule folder

 ``SwiftOnoneSupport_LIBRARY``
   path to the SwiftOnoneSupport library

 ``SwiftConcurrency_INCLUDE_DIR``
   the directory containing the _Concurrency.swiftmodule folder

 ``SwiftConcurrency_LIBRARY``
   path to the swift_Concurrency library

 ``SwiftShims_INCLUDE_DIR``
   the directory containing the Swift shims directory
   (i.e. .../usr/lib/swift, not .../usr/lib/swift/shims)

#]=======================================================================]

include_guard(GLOBAL)

# If the SwiftCore_DIR_FLAG is specified, look there instead. The cmake-generated
# config file is more accurate, but requires that the SDK has one available.
if(SwiftCore_DIR)
  if(SwiftCore_FIND_REQUIRED)
    list(APPEND args REQUIRED)
  endif()
  if(SwiftCore_FIND_QUIETLY)
    list(APPEND args QUIET)
  endif()
  find_package(SwiftCore NO_MODULE ${args})
  return()
endif()

include(FindPackageHandleStandardArgs)
include(PlatformInfo)

# This was loosely modelled after other find modules
# (namely FindGLEW), where the equivalent parameter
# is not stored in cache (possibly because we want
# the project importing it to be able to
# it "immediately")
if(NOT DEFINED SwiftCore_USE_STATIC_LIBS)
  set(SwiftCore_USE_STATIC_LIBS OFF)
  if(NOT BUILD_SHARED_LIBS AND NOT APPLE)
    set(SwiftCore_USE_STATIC_LIBS ON)
  endif()
endif()

if(APPLE)
  list(APPEND SwiftCore_INCLUDE_DIR_HINTS
    "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  list(APPEND SwiftCore_LIBRARY_HINTS
    "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  list(APPEND SwiftShims_INCLUDE_DIR_HINTS
    "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  # When building for Apple platforms, SwiftCore always comes from within the
  # SDK as a tbd for a shared library in the shared cache.
  list(APPEND SwiftCore_NAMES libswiftCore.tbd)
  list(APPEND SwiftOnoneSupport_NAMES libswiftSwiftOnoneSupport.tbd)
  list(APPEND SwiftConcurrency_NAMES libswift_Concurrency.tbd)
elseif(LINUX)
  if (SwiftCore_USE_STATIC_LIBS)
    list(APPEND SwiftCore_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/linux-static")
    list(APPEND SwiftCore_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/linux-static")
    list(APPEND SwiftShims_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static"
      "${Swift_SDKROOT}/usr/lib/swift")
    list(APPEND SwiftCore_NAMES libswiftCore.a)
    list(APPEND SwiftOnoneSupport_NAMES libswiftSwiftOnoneSupport.a)
    list(APPEND SwiftConcurrency_NAMES libswift_Concurrency.a)
  else()
    list(APPEND SwiftCore_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/linux")
    list(APPEND SwiftCore_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/linux")
    list(APPEND SwiftShims_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift")
    list(APPEND SwiftCore_NAMES libswiftCore.so)
    list(APPEND SwiftOnoneSupport_NAMES libswiftSwiftOnoneSupport.so)
    list(APPEND SwiftConcurrency_NAMES libswift_Concurrency.so)
  endif()
elseif(WIN32)
  list(APPEND SwiftCore_INCLUDE_DIR_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/windows"
    "$ENV{SDKROOT}/usr/lib/swift/windows")
  list(APPEND SwiftCore_LIBRARY_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}"
    "${Swift_SDKROOT}/usr/lib/swift"
    "$ENV{SDKROOT}/usr/lib/swift/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}"
    "$ENV{SDKROOT}/usr/lib/swift")
  list(APPEND SwiftShims_INCLUDE_DIR_HINTS
    "${Swift_SDKROOT}/usr/lib/swift"
    "$ENV{SDKROOT}/usr/lib/swift")
  if (SwiftCore_USE_STATIC_LIBS)
    list(APPEND SwiftCore_NAMES libswiftCore.lib)
    list(APPEND SwiftOnoneSupport_NAMES libswiftSwiftOnoneSupport.lib)
    list(APPEND SwiftConcurrency_NAMES libswift_Concurrency.lib)
  else()
    list(APPEND SwiftCore_NAMES swiftCore.lib)
    list(APPEND SwiftOnoneSupport_NAMES swiftSwiftOnoneSupport.lib)
    list(APPEND SwiftConcurrency_NAMES swift_Concurrency.lib)
  endif()
elseif(ANDROID)
  if (SwiftCore_USE_STATIC_LIBS)
    list(APPEND SwiftCore_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/android"
      "$ENV{SDKROOT}/usr/lib/swift_static/android")
    list(APPEND SwiftCore_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
      "${Swift_SDKROOT}/usr/lib/swift_static"
      "$ENV{SDKROOT}/usr/lib/swift_static/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
      "$ENV{SDKROOT}/usr/lib/swift_static")
    list(APPEND SwiftShims_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static")
    list(APPEND SwiftCore_NAMES libswiftCore.a)
    list(APPEND SwiftOnoneSupport_NAMES libswiftSwiftOnoneSupport.a)
    list(APPEND SwiftConcurrency_NAMES libswift_Concurrency.a)
  else()
    list(APPEND SwiftCore_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/android"
      "$ENV{SDKROOT}/usr/lib/swift/android")
    list(APPEND SwiftCore_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
      "${Swift_SDKROOT}/usr/lib/swift"
      "$ENV{SDKROOT}/usr/lib/swift/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
      "$ENV{SDKROOT}/usr/lib/swift")
    list(APPEND SwiftShims_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift")
    list(APPEND SwiftCore_NAMES libswiftCore.so)
    list(APPEND SwiftOnoneSupport_NAMES libswiftSwiftOnoneSupport.so)
    list(APPEND SwiftConcurrency_NAMES libswift_Concurrency.so)
  endif()
else()
  message(FATAL_ERROR "FindSwiftCore.cmake module search not implemented for targeted platform\n"
  " Build Core for your platform and set `SwiftCore_DIR` to"
  " the directory containing SwiftCoreConfig.cmake\n")
endif()

find_path(SwiftCore_INCLUDE_DIR
  "Swift.swiftmodule"
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${SwiftCore_INCLUDE_DIR_HINTS})
find_library(SwiftCore_LIBRARY
  NAMES
    ${SwiftCore_NAMES}
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${SwiftCore_LIBRARY_HINTS})

find_path(SwiftOnoneSupport_INCLUDE_DIR
  "SwiftOnoneSupport.swiftmodule"
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${SwiftCore_INCLUDE_DIR_HINTS})
find_library(SwiftOnoneSupport_LIBRARY
  NAMES
    ${SwiftOnoneSupport_NAMES}
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${SwiftCore_LIBRARY_HINTS})

find_path(SwiftConcurrency_INCLUDE_DIR
  "_Concurrency.swiftmodule"
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${SwiftCore_INCLUDE_DIR_HINTS})
find_library(SwiftConcurrency_LIBRARY
  NAMES
    ${SwiftConcurrency_NAMES}
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${SwiftCore_LIBRARY_HINTS})

if(SwiftCore_USE_STATIC_LIBS)
  add_library(swiftCore STATIC IMPORTED GLOBAL)
  add_library(swiftSwiftOnoneSupport STATIC IMPORTED GLOBAL)
  add_library(swift_Concurrency STATIC IMPORTED GLOBAL)
else()
  add_library(swiftCore SHARED IMPORTED GLOBAL)
  add_library(swiftSwiftOnoneSupport SHARED IMPORTED GLOBAL)
  add_library(swift_Concurrency SHARED IMPORTED GLOBAL)
endif()

set_target_properties(swiftCore PROPERTIES
  INTERFACE_INCLUDE_DIRECTORIES "${SwiftCore_INCLUDE_DIR}")
set_target_properties(swiftSwiftOnoneSupport PROPERTIES
  INTERFACE_INCLUDE_DIRECTORIES "${SwiftOnoneSupport_INCLUDE_DIR}")
set_target_properties(swift_Concurrency PROPERTIES
  INTERFACE_INCLUDE_DIRECTORIES "${SwiftConcurrency_INCLUDE_DIR}")

if(LINUX OR ANDROID)
  set_target_properties(swiftCore PROPERTIES
    IMPORTED_LOCATION "${SwiftCore_LIBRARY}")
  set_target_properties(swiftSwiftOnoneSupport PROPERTIES
    IMPORTED_LOCATION "${SwiftOnoneSupport_LIBRARY}")
  set_target_properties(swift_Concurrency PROPERTIES
    IMPORTED_LOCATION "${SwiftConcurrency_LIBRARY}")
else()
  set_target_properties(swiftCore PROPERTIES
    IMPORTED_IMPLIB "${SwiftCore_LIBRARY}")
  set_target_properties(swiftSwiftOnoneSupport PROPERTIES
    IMPORTED_IMPLIB "${SwiftOnoneSupport_LIBRARY}")
  set_target_properties(swift_Concurrency PROPERTIES
    IMPORTED_IMPLIB "${SwiftConcurrency_LIBRARY}")
endif()

find_path(SwiftShims_INCLUDE_DIR "shims/module.modulemap" HINTS
  ${SwiftShims_INCLUDE_DIR_HINTS})
add_library(swiftShims INTERFACE IMPORTED GLOBAL)
target_include_directories(swiftShims INTERFACE
# This is needed for targets that import headers from
# include/swift (e.g. include/swift/ABI/HeapObject.h)
# that assumes the shims are located in `swift/shims`
# relative path
  "${SwiftShims_INCLUDE_DIR}/.."
  "${SwiftShims_INCLUDE_DIR}/shims")
target_compile_options(swiftShims INTERFACE
  "$<$<COMPILE_LANGUAGE:Swift>:SHELL:-Xcc -fmodule-map-file=\"${SwiftShims_INCLUDE_DIR}/shims/module.modulemap\">")

find_package_handle_standard_args(SwiftCore DEFAULT_MSG
  SwiftCore_LIBRARY SwiftCore_INCLUDE_DIR
  SwiftShims_INCLUDE_DIR
  SwiftOnoneSupport_LIBRARY SwiftOnoneSupport_INCLUDE_DIR
  SwiftConcurrency_LIBRARY SwiftConcurrency_INCLUDE_DIR)

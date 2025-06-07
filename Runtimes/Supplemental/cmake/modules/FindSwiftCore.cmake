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

Result Variables
^^^^^^^^^^^^^^^^

The module may set the following variables if `SwiftCore_DIR` is not set.

 ``SwiftCore_FOUND``
   true if core was found

 ``SwiftCore_INCLUDE_DIR``
   the directory containing the Swift.swiftmodule folder

#]=======================================================================]

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

set(NO_CMAKE_FIND_ROOT_PATH_KEYWORD)
if(APPLE)
  set(SwiftCore_INCLUDE_DIR_HINTS
    "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  set(SwiftCore_LIBRARY_HINTS
    "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
elseif(LINUX)
  if (NOT BUILD_SHARED_LIBS)
    set(SwiftCore_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/linux-static")
    set(SwiftCore_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/linux-static")
  else()
    set(SwiftCore_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/linux")
    set(SwiftCore_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/linux")
  endif()
elseif(WIN32)
  set(SwiftCore_INCLUDE_DIR_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/windows"
    "$ENV{SDKROOT}/usr/lib/swift/windows")
  set(SwiftCore_LIBRARY_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/${SwiftCore_PLATFORM_SUBDIR}/${SwiftCore_ARCH_SUBDIR}"
    "${Swift_SDKROOT}/usr/lib/swift"
    "$ENV{SDKROOT}/usr/lib/swift/${SwiftCore_PLATFORM_SUBDIR}/${SwiftCore_ARCH_SUBDIR}"
    "$ENV{SDKROOT}/usr/lib/swift")
elseif(ANDROID)
  set(NO_CMAKE_FIND_ROOT_PATH_KEYWORD "NO_CMAKE_FIND_ROOT_PATH")
  if (NOT BUILD_SHARED_LIBS)
    set(SwiftCore_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/android"
      "$ENV{SDKROOT}/usr/lib/swift_static/android")
    set(SwiftCore_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/android/${SwiftCore_ARCH_SUBDIR}"
      "${Swift_SDKROOT}/usr/lib/swift_static"
      "$ENV{SDKROOT}/usr/lib/swift_static/android/${SwiftCore_ARCH_SUBDIR}"
      "$ENV{SDKROOT}/usr/lib/swift_static")
  else()
    set(SwiftCore_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/android"
      "$ENV{SDKROOT}/usr/lib/swift/android")
    set(SwiftCore_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/android/${SwiftCore_ARCH_SUBDIR}"
      "${Swift_SDKROOT}/usr/lib/swift"
      "$ENV{SDKROOT}/usr/lib/swift/android/${SwiftCore_ARCH_SUBDIR}"
      "$ENV{SDKROOT}/usr/lib/swift")
  endif()
else()
  message(FATAL_ERROR "FindSwiftCore.cmake module search not implemented for targeted platform\n"
  " Build Core for your platform and set `SwiftCore_DIR` to"
  " the directory containing SwiftCoreConfig.cmake\n")
endif()

find_path(SwiftCore_INCLUDE_DIR
  "Swift.swiftmodule"
  ${NO_CMAKE_FIND_ROOT_PATH_KEYWORD}
  HINTS
    ${SwiftCore_INCLUDE_DIR_HINTS})
find_library(SwiftCore_LIBRARY
  NAMES
    "libswiftCore.tbd"
    "libswiftCore.lib"
    "libswiftCore.a"
    "libswiftCore.so"
  ${NO_CMAKE_FIND_ROOT_PATH_KEYWORD}
  HINTS
    ${SwiftCore_LIBRARY_HINTS})
if(NOT APPLE AND NOT BUILD_SHARED_LIBS)
  add_library(swiftCore STATIC IMPORTED GLOBAL)
else()
  add_library(swiftCore SHARED IMPORTED GLOBAL)
endif()

set_target_properties(swiftCore PROPERTIES
  INTERFACE_INCLUDE_DIRECTORIES "${SwiftCore_INCLUDE_DIR}")

if(LINUX OR ANDROID)
  set_target_properties(swiftCore PROPERTIES
    IMPORTED_LOCATION "${SwiftCore_LIBRARY}")
else()
  set_target_properties(swiftCore PROPERTIES
    IMPORTED_IMPLIB "${SwiftCore_LIBRARY}")
endif()

find_package_handle_standard_args(SwiftCore DEFAULT_MSG
  SwiftCore_LIBRARY SwiftCore_INCLUDE_DIR)

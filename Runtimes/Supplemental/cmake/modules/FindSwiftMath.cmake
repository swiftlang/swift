#[=======================================================================[.rst:
FindSwiftMath
------------

Find the Swift `_math` module interface, deferring to mathConfig.cmake when
requested.

This module locates the Swift `_math` module, which provides standard math
functions to Swift code. The module looks for the `_math.swiftinterface` file
in the SDK and sets up an imported target for use in CMake.
Imported Targets
^^^^^^^^^^^^^^^^

The following :prop_tgt:`IMPORTED` TARGETS may be defined:

 ``math``

Hint Variables
^^^^^^^^^^^^^^

 ``swift_SDKROOT``
   Set the path to the Swift SDK installation.
   This only affects Linux and Windows builds.
   Apple builds always use the library provided by the SDK.

Result Variables
^^^^^^^^^^^^^^^^

The module may set the following variables if `SwiftMath_DIR` is not set.

 ``SwiftMath_FOUND``
   true if the `_math` module interface (and required library) were found.

 ``SwiftMath_INCLUDE_DIR``
   The directory containing the `_math.swiftinterface` file

 ``SwiftMath_LIBRARIES`` OR ``SwiftMath_IMPLIB``
   the libraries to be linked

#]=======================================================================]

# If the math_DIR is specified, look there instead. The cmake-generated
# config file is more accurate, but requires that the SDK has one available.
if(SwiftMath_DIR)
  if(SwiftMath_FIND_REQUIRED)
    list(APPEND args REQUIRED)
  endif()
  if(SwiftMath_FIND_QUIETLY)
    list(APPEND args QUIET)
  endif()
  find_package(SwiftMath NO_MODULE ${args})
  return()
endif()

include(FindPackageHandleStandardArgs)
include(PlatformInfo)

if(APPLE)
  find_path(SwiftMath_INCLUDE_DIR
    NAMES _math.swiftmodule
    PATHS ${CMAKE_OSX_SYSROOT}/usr/lib/swift)
  find_path(SwiftMath_IMPLIB
    NAMES libm.tbd
    PATHS usr/lib)
  add_library(SwiftMath SHARED IMPORTED GLOBAL)
  set_target_properties(SwiftMath PROPERTIES
    IMPORTED_IMPLIB ${SwiftMath_IMPLIB}
    INTERFACE_INCLUDE_DIRECTORIES ${SwiftMath_INCLUDE_DIR})
  find_package_handle_standard_args(SwiftMath DEFAULT_MSG
    SwiftMath_IMPLIB SwiftMath_INCLUDE_DIR)
elseif(LINUX)
  find_path(SwiftMath_INCLUDE_DIR
    glibc.modulemap
    PATHS ${Swift_SDKROOT}/usr/lib/swift/linux/${SwiftSupplemental_ARCH_SUBDIR})
  find_path(SwiftMath_GLIBC_DIR
    Glibc.swiftmodule
    PATHS ${Swift_SDKROOT}/usr/lib/swift/linux)
  add_library(SwiftMath INTERFACE IMPORTED GLOBAL)
  set_target_properties(SwiftMath PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES ${SwiftMath_INCLUDE_DIR} ${SwiftMath_GLIBC_DIR})
  find_package_handle_standard_args(SwiftMath DEFAULT_MSG SwiftMath_INCLUDE_DIR)
else()
  # TODO: Windows
  message(FATAL_ERROR "FindSwiftMath.cmake module search not implemented for targeted platform\n")
endif()

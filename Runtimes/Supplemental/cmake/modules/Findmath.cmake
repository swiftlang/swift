#[=======================================================================[.rst:
Findmath
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

 ``math_STATIC``
   Look for the libmath static archive instead of the dynamic library.

Result Variables
^^^^^^^^^^^^^^^^

The module may set the following variables if `math_DIR` is not set.

 ``math_FOUND``
   true if the `_math` module interface (and required library) were found.

 ``math_INCLUDE_DIR``
   The directory containing the `_math.swiftinterface` file

 ``math_LIBRARIES`` OR ``math_IMPLIB``
   the libraries to be linked

#]=======================================================================]

# If the math_DIR is specified, look there instead. The cmake-generated
# config file is more accurate, but requires that the SDK has one available.
if(math_DIR)
  if(math_FIND_REQUIRED)
    list(APPEND args REQUIRED)
  endif()
  if(math_FIND_QUIETLY)
    list(APPEND args QUIET)
  endif()
  find_package(math NO_MODULE ${args})
  return()
endif()

include(FindPackageHandleStandardArgs)
include(PlatformInfo)

if(APPLE)
  find_path(math_INCLUDE_DIR
    NAMES _math.swiftmodule
    PATHS ${CMAKE_OSX_SYSROOT}/usr/lib/swift)
  find_path(math_IMPLIB
    NAMES libm.tbd
    PATHS usr/lib)
  add_library(math SHARED IMPORTED GLOBAL)
  set_target_properties(math PROPERTIES
    IMPORTED_IMPLIB ${math_IMPLIB}
    INTERFACE_INCLUDE_DIRECTORIES ${math_INCLUDE_DIR})
  find_package_handle_standard_args(math DEFAULT_MSG
    math_IMPLIB math_INCLUDE_DIR)
elseif(LINUX)
  find_path(math_INCLUDE_DIR
    glibc.modulemap
    PATHS ${Swift_SDKROOT}/usr/lib/swift/linux/${SwiftSupplemental_ARCH_SUBDIR})
  find_path(math_GLIBC_DIR
    Glibc.swiftmodule
    PATHS ${Swift_SDKROOT}/usr/lib/swift/linux)
  find_library(math_LIBRARY NAMES m)
  if(math_STATIC)
    add_library(math STATIC IMPORTED GLOBAL)
  else()
    add_library(math SHARED IMPORTED GLOBAL)
  endif()
  set_target_properties(math PROPERTIES
    IMPORTED_LOCATION ${math_LIBRARY}
    INTERFACE_INCLUDE_DIRECTORIES ${math_INCLUDE_DIR} ${math_GLIBC_DIR})
  find_package_handle_standard_args(math DEFAULT_MSG
    math_LIBRARY math_INCLUDE_DIR)
else()
  # TODO: Windows
  message(FATAL_ERROR "Findmath.cmake module search not implemented for targeted platform\n")
endif()

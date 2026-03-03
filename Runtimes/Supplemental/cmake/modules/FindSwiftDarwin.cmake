#[=======================================================================[.rst:
FindSwiftDarwin
------------

Find swiftDarwin in the underlying SDK (the only way we obtain such overlay).

Imported Targets
^^^^^^^^^^^^^^^^

The following :prop_tgt:`IMPORTED` TARGETS may be defined:

 ``swiftDarwin``

#]=======================================================================]

include_guard(GLOBAL)

# This was loosely modelled after other find modules
# (namely FindGLEW), where the equivalent parameter
# is not stored in cache (possibly because we want
# the project importing it to be able to use
# it "immediately")
if(NOT DEFINED SwiftDarwin_USE_STATIC_LIBS)
  set(SwiftDarwin_USE_STATIC_LIBS OFF)
endif()

include(FindPackageHandleStandardArgs)
include(PlatformInfo)

if(NOT APPLE)
  message(WARNING "Darwin is only produced on Apple platforms, so not attempting to search it here.")
  return()
endif()

# Look in the SDK
list(APPEND swiftDarwin_INCLUDE_DIR_HINTS
  "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
list(APPEND swiftDarwin_LIBRARY_HINTS
  "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
# When building for Apple platforms, swiftDarwin always comes from within the
# SDK as a tbd for a shared library in the shared cache.
list(APPEND swiftDarwin_NAMES libswiftDarwin.tbd)
set(swiftDarwin_MODULE_NAME "Darwin.swiftmodule")

find_path(swiftDarwin_INCLUDE_DIR
  ${swiftDarwin_MODULE_NAME}
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${swiftDarwin_INCLUDE_DIR_HINTS})
find_library(swiftDarwin_LIBRARY
  NAMES
    ${swiftDarwin_NAMES}
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${swiftDarwin_LIBRARY_HINTS})

if(SwiftDarwin_USE_STATIC_LIBS)
  add_library(swiftDarwin STATIC IMPORTED GLOBAL)
else()
  add_library(swiftDarwin SHARED IMPORTED GLOBAL)
endif()

target_include_directories(swiftDarwin INTERFACE
  "${swiftDarwin_INCLUDE_DIR}")

set_target_properties(swiftDarwin PROPERTIES
  IMPORTED_IMPLIB "${swiftDarwin_LIBRARY}")

find_package_handle_standard_args(SwiftDarwin DEFAULT_MSG
  "swiftDarwin_LIBRARY" "swiftDarwin_INCLUDE_DIR")

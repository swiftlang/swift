#[=======================================================================[.rst:
FindSwiftOverlay
------------

Find SwiftOverlay, deferring to the associated SwiftOverlayConfig.cmake when requested.
This is meant to find the platform overlays to be linked by the Supplemental libraries.

Imported Targets
^^^^^^^^^^^^^^^^

The following :prop_tgt:`IMPORTED` TARGETS may be defined:

 ``SwiftOverlay``

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

# This was loosely modelled after other find modules
# (namely FindGLEW), where the equivalent parameter
# is not stored in cache (possibly because we want
# the project importing it to be able to
# it "immediately")
if(NOT DEFINED SwiftOverlay_USE_STATIC_LIBS)
  set(SwiftOverlay_USE_STATIC_LIBS OFF)
  if(NOT BUILD_SHARED_LIBS AND NOT APPLE)
    set(SwiftOverlay_USE_STATIC_LIBS ON)
  endif()
endif()

if(SwiftOverlay_DIR)
  if(${CMAKE_FIND_PACKAGE_NAME}_FIND_REQUIRED)
    list(APPEND args REQUIRED)
  endif()
  if(${CMAKE_FIND_PACKAGE_NAME}_FIND_QUIETLY)
    list(APPEND args QUIET)
  endif()
  find_package(SwiftOverlay CONFIG ${args})
  return()
endif()

include(FindPackageHandleStandardArgs)
include(PlatformInfo)

if(APPLE)
  list(APPEND OVERLAY_TARGET_NAMES "swiftDarwin")
  # Look in the SDK
  list(APPEND swiftDarwin_INCLUDE_DIR_HINTS
    "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  list(APPEND swiftDarwin_LIBRARY_HINTS
    "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  # When building for Apple platforms, swiftDarwin always comes from within the
  # SDK as a tbd for a shared library in the shared cache.
  list(APPEND swiftDarwin_NAMES libswiftDarwin.tbd)
  set(swiftDarwin_MODULE_NAME "Darwin.swiftmodule")

  list(APPEND swift_Builtin_float_INCLUDE_DIR_HINTS
    "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  list(APPEND swift_Builtin_float_LIBRARY_HINTS
    "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  list(APPEND swift_Builtin_float_NAMES libswift_Builtin_float.tbd)
elseif(LINUX)
  #ToDo(swiftlang/swift/issues/83014): Handle the static MUSL SDK case
  list(APPEND OVERLAY_TARGET_NAMES "swiftGlibc")

  # Look in the SDK
  if (SwiftOverlay_USE_STATIC_LIBS)
    list(APPEND swiftGlibc_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/linux")
    list(APPEND swiftGlibc_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/linux")
    list(APPEND swiftGlibc_NAMES libswiftGlibc.a)

    list(APPEND swift_Builtin_float_INCLUDE_DIR_HINTS
      "${CMAKE_OSX_SYSROOT}/usr/lib/swift_static/linux")
    list(APPEND swift_Builtin_float_LIBRARY_HINTS
      "${CMAKE_OSX_SYSROOT}/usr/lib/swift_static/linux")
    list(APPEND swift_Builtin_float_NAMES libswift_Builtin_float.a)
  else()
    list(APPEND swiftGlibc_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/linux/")
    list(APPEND swiftGlibc_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/linux")
    list(APPEND swiftGlibc_NAMES libswiftGlibc.so)

    list(APPEND swift_Builtin_float_INCLUDE_DIR_HINTS
      "${CMAKE_OSX_SYSROOT}/usr/lib/swift/linux")
    list(APPEND swift_Builtin_float_LIBRARY_HINTS
      "${CMAKE_OSX_SYSROOT}/usr/lib/swift/linux")
    list(APPEND swift_Builtin_float_NAMES libswift_Builtin_float.so)
  endif()
  set(swiftGlibc_MODULE_NAME "Glibc.swiftmodule")
elseif(WIN32)
  list(APPEND OVERLAY_TARGET_NAMES "swiftWinSDK")
  list(APPEND OVERLAY_TARGET_NAMES "swiftCRT")

  list(APPEND swiftWinSDK_INCLUDE_DIR_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/windows"
    "$ENV{SDKROOT}/usr/lib/swift/windows")
  list(APPEND swiftWinSDK_LIBRARY_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}"
    "${Swift_SDKROOT}/usr/lib/swift"
    "$ENV{SDKROOT}/usr/lib/swift/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}"
    "$ENV{SDKROOT}/usr/lib/swift")

  if(SwiftOverlay_USE_STATIC_LIBS)
    list(APPEND swiftWinSDK_NAMES libswiftWinSDK.lib)
  else()
    list(APPEND swiftWinSDK_NAMES swiftWinSDK.lib)
  endif()
  list(APPEND swiftWinSDK_MODULE_NAME "WinSDK.swiftmodule")

  list(APPEND swiftCRT_INCLUDE_DIR_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/windows"
    "$ENV{SDKROOT}/usr/lib/swift/windows")
  list(APPEND swiftCRT_LIBRARY_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}"
    "${Swift_SDKROOT}/usr/lib/swift"
    "$ENV{SDKROOT}/usr/lib/swift/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}"
    "$ENV{SDKROOT}/usr/lib/swift")

 if(SwiftOverlay_USE_STATIC_LIBS)
    list(APPEND swiftCRT_NAMES libswiftCRT.lib)
  else()
    list(APPEND swiftCRT_NAMES swiftCRT.lib)
  endif()
  set(swiftCRT_MODULE_NAME "CRT.swiftmodule")

  list(APPEND swift_Builtin_float_INCLUDE_DIR_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/windows"
    "$ENV{SDKROOT}/usr/lib/swift/windows")
  list(APPEND swift_Builtin_float_LIBRARY_HINTS
    "${Swift_SDKROOT}/usr/lib/swift/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}"
    "${Swift_SDKROOT}/usr/lib/swift"
    "$ENV{SDKROOT}/usr/lib/swift/${${PROJECT_NAME}_PLATFORM_SUBDIR}/${${PROJECT_NAME}_ARCH_SUBDIR}"
    "$ENV{SDKROOT}/usr/lib/swift")

 if(SwiftOverlay_USE_STATIC_LIBS)
    list(APPEND swift_Builtin_float_NAMES libswift_Builtin_float.lib)
  else()
    list(APPEND swift_Builtin_float_NAMES swift_Builtin_float.lib)
  endif()
elseif(ANDROID)
  list(APPEND OVERLAY_TARGET_NAMES "swiftAndroid")

  if (SwiftOverlay_USE_STATIC_LIBS)
    list(APPEND swiftAndroid_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/android"
      "$ENV{SDKROOT}/usr/lib/swift_static/android")
    list(APPEND swiftAndroid_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
      "${Swift_SDKROOT}/usr/lib/swift_static"
      "$ENV{SDKROOT}/usr/lib/swift_static/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
      "$ENV{SDKROOT}/usr/lib/swift_static")
    list(APPEND swiftAndroid_NAMES libswiftAndroid.a)

    list(APPEND swift_Builtin_float_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/android"
      "$ENV{SDKROOT}/usr/lib/swift_static/android")
    list(APPEND swift_Builtin_float_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift_static/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
      "${Swift_SDKROOT}/usr/lib/swift_static"
      "$ENV{SDKROOT}/usr/lib/swift_static/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
      "$ENV{SDKROOT}/usr/lib/swift_static")
    list(APPEND swift_Builtin_float_NAMES libswift_Builtin_float.a)
  else()
    list(APPEND swiftAndroid_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/android"
      "$ENV{SDKROOT}/usr/lib/swift/android")
    list(APPEND swiftAndroid_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
      "${Swift_SDKROOT}/usr/lib/swift"
      "$ENV{SDKROOT}/usr/lib/swift/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
      "$ENV{SDKROOT}/usr/lib/swift")
    list(APPEND swiftAndroid_NAMES libswiftAndroid.so)

    list(APPEND swift_Builtin_float_INCLUDE_DIR_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/android"
      "$ENV{SDKROOT}/usr/lib/swift/android")
    list(APPEND swift_Builtin_float_LIBRARY_HINTS
      "${Swift_SDKROOT}/usr/lib/swift/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
      "${Swift_SDKROOT}/usr/lib/swift"
      "$ENV{SDKROOT}/usr/lib/swift/android/${${PROJECT_NAME}_ARCH_SUBDIR}"
      "$ENV{SDKROOT}/usr/lib/swift")
    list(APPEND swift_Builtin_float_NAMES libswift_Builtin_float.so)
  endif()
  set(swiftAndroid_MODULE_NAME "Android.swiftmodule")
else()
  message(FATAL_ERROR "FindSwiftOverlay.cmake module search not implemented for targeted platform\n"
  " Build the Overlays for your platform and set the appropriate `SwiftOverlay_DIR` variable to"
  " the directory containing SwiftOverlayConfig.cmake\n")
endif()

# Setup SwiftOverlay interface library and link it against the explicit
# overlay targets
add_library(SwiftOverlay INTERFACE)

foreach(OVERLAY_TARGET ${OVERLAY_TARGET_NAMES})
  find_path(${OVERLAY_TARGET}_INCLUDE_DIR
    ${${OVERLAY_TARGET}_MODULE_NAME}
    NO_CMAKE_FIND_ROOT_PATH
    HINTS
      ${${OVERLAY_TARGET}_INCLUDE_DIR_HINTS})
  find_library(${OVERLAY_TARGET}_LIBRARY
    NAMES
      ${${OVERLAY_TARGET}_NAMES}
    NO_CMAKE_FIND_ROOT_PATH
    HINTS
      ${${OVERLAY_TARGET}_LIBRARY_HINTS})

  if(SwiftOverlay_USE_STATIC_LIBS)
    add_library(${OVERLAY_TARGET} STATIC IMPORTED GLOBAL)
  else()
    add_library(${OVERLAY_TARGET} SHARED IMPORTED GLOBAL)
  endif()

  target_include_directories(${OVERLAY_TARGET} INTERFACE
    "${${OVERLAY_TARGET}_INCLUDE_DIR}")

  if(LINUX OR ANDROID)
    set_target_properties(${OVERLAY_TARGET} PROPERTIES
      IMPORTED_LOCATION "${${OVERLAY_TARGET}_LIBRARY}")
  else()
    set_target_properties(${OVERLAY_TARGET} PROPERTIES
      IMPORTED_IMPLIB "${${OVERLAY_TARGET}_LIBRARY}")
  endif()

  target_link_libraries(SwiftOverlay INTERFACE
      ${OVERLAY_TARGET})
endforeach()

find_path(swift_Builtin_float_INCLUDE_DIR
  "_Builtin_float.swiftmodule"
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${swift_Builtin_float_INCLUDE_DIR_HINTS})
find_library(swift_Builtin_float_LIBRARY
  NAMES
    ${swift_Builtin_float_NAMES}
  NO_CMAKE_FIND_ROOT_PATH
  HINTS
    ${swift_Builtin_float_LIBRARY_HINTS})

if(SwiftOverlay_USE_STATIC_LIBS)
  add_library(swift_Builtin_float STATIC IMPORTED GLOBAL)
else()
  add_library(swift_Builtin_float SHARED IMPORTED GLOBAL)
endif()

target_include_directories(swift_Builtin_float INTERFACE
  "${swift_Builtin_float_INCLUDE_DIR}")

if(LINUX OR ANDROID)
  set_target_properties(swift_Builtin_float PROPERTIES
    IMPORTED_LOCATION "${swift_Builtin_float_LIBRARY}")
else()
  set_target_properties(swift_Builtin_float PROPERTIES
    IMPORTED_IMPLIB "${swift_Builtin_float_LIBRARY}")
endif()

foreach(OVERLAY_TARGET ${OVERLAY_TARGET_NAMES})
  list(APPEND vars_to_check "${OVERLAY_TARGET}_LIBRARY" "${OVERLAY_TARGET}_INCLUDE_DIR")
endforeach()
list(APPEND vars_to_check "swift_Builtin_float_LIBRARY" "swift_Builtin_float_INCLUDE_DIR")

find_package_handle_standard_args(SwiftOverlay DEFAULT_MSG
  ${vars_to_check})

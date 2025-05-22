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

if(APPLE)
  # When building for Apple platforms, SwiftCore always comes from within the
  # SDK as a tbd for a shared library in the shared cache.
  find_path(SwiftCore_INCLUDE_DIR
      "Swift.swiftmodule"
    HINTS
      "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  find_library(SwiftCore_IMPLIB
    NAMES "libswiftCore.tbd"
    HINTS
      "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  add_library(swiftCore SHARED IMPORTED GLOBAL)
  set_target_properties(swiftCore PROPERTIES
    IMPORTED_IMPLIB "${SwiftCore_IMPLIB}"
    INTERFACE_INCLUDE_DIRECTORIES "${SwiftCore_INCLUDE_DIR}")

  find_path(Shims_INCLUDE_DIR
      "shims/module.modulemap"
    HINTS
      "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  add_library(swiftShims INTERFACE IMPORTED GLOBAL)
  set_target_properties(swiftShims PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES "${Shims_INCLUDE_DIR}/shims")
  target_link_libraries(swiftCore
   INTERFACE
    swiftShims)

  find_path(SwiftOnoneSupport_INCLUDE_DIR
      "SwiftOnoneSupport.swiftmodule"
    HINTS
      "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  find_library(SwiftOnoneSupport_IMPLIB
    NAMES "libswiftSwiftOnoneSupport.tbd"
    HINTS
      "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  add_library(swiftOnoneSupport SHARED IMPORTED GLOBAL)
  set_target_properties(swiftOnoneSupport PROPERTIES
    IMPORTED_IMPLIB "${SwiftOnoneSupport_IMPLIB}"
    INTERFACE_INCLUDE_DIRECTORIES "${SwiftOnoneSupport_INCLUDE_DIR}")

  find_path(SwiftConcurrency_INCLUDE_DIR
      "_Concurrency.swiftmodule"
    HINTS
      "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  find_library(SwiftConcurrency_IMPLIB
    NAMES "libswift_Concurrency.tbd"
    HINTS
      "${CMAKE_OSX_SYSROOT}/usr/lib/swift")
  add_library(swift_Concurrency SHARED IMPORTED GLOBAL)
  set_target_properties(swift_Concurrency PROPERTIES
    IMPORTED_IMPLIB "${SwiftConcurrency_IMPLIB}"
    INTERFACE_INCLUDE_DIRECTORIES "${SwiftConcurrency_INCLUDE_DIR}")

  find_package_handle_standard_args(SwiftCore DEFAULT_MSG
    SwiftCore_IMPLIB SwiftCore_INCLUDE_DIR
    Shims_INCLUDE_DIR
    SwiftOnoneSupport_IMPLIB SwiftOnoneSupport_INCLUDE_DIR
    SwiftConcurrency_IMPLIB SwiftConcurrency_INCLUDE_DIR)
elseif(LINUX)
  if (NOT BUILD_SHARED_LIBS)
    find_path(SwiftCore_INCLUDE_DIR
      "Swift.swiftmodule"
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift_static/linux-static")
    find_library(SwiftCore_LIBRARY
      NAMES "libswiftCore.a"
      HINTS "${Swift_SDKROOT}/usr/lib/swift_static/linux-static")
    add_library(swiftCore STATIC IMPORTED GLOBAL)

    find_path(Shims_INCLUDE_DIR
        "shims/module.modulemap"
        HINTS
          "${Swift_SDKROOT}/usr/lib/swift")
    add_library(swiftShims INTERFACE IMPORTED GLOBAL)

    find_path(SwiftOnoneSupport_INCLUDE_DIR
      "SwiftOnoneSupport.swiftmodule"
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift_static/linux-static")
    find_library(SwiftOnoneSupport_LIBRARY
      NAMES "libswiftSwiftOnoneSupport.a"
      HINTS "${Swift_SDKROOT}/usr/lib/swift_static/linux-static")
    add_library(swiftOnoneSupport STATIC IMPORTED GLOBAL)

    find_path(SwiftConcurrency_INCLUDE_DIR
      "_Concurrency.swiftmodule"
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift_static/linux-static")
    find_library(SwiftConcurrency_LIBRARY
      NAMES "libswift_Concurrency.a"
      HINTS "${Swift_SDKROOT}/usr/lib/swift_static/linux-static")
    add_library(swift_Concurrency STATIC IMPORTED GLOBAL)
  else()
    find_path(SwiftCore_INCLUDE_DIR
      "Swift.swiftmodule"
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift/linux")
    find_library(SwiftCore_LIBRARY
      NAMES "libswiftCore.so"
      HINTS "${Swift_SDKROOT}/usr/lib/swift/linux")
    add_library(swiftCore SHARED IMPORTED GLOBAL)

    find_path(Shims_INCLUDE_DIR
        "shims/module.modulemap"
        HINTS
          "${Swift_SDKROOT}/usr/lib/swift")
    add_library(swiftShims INTERFACE IMPORTED GLOBAL)

    find_path(SwiftOnoneSupport_INCLUDE_DIR
      "SwiftOnoneSupport.swiftmodule"
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift/linux")
    find_library(SwiftOnoneSupport_LIBRARY
      NAMES "libswiftSwiftOnoneSupport.so"
      HINTS "${Swift_SDKROOT}/usr/lib/swift/linux")
    add_library(swiftOnoneSupport SHARED IMPORTED GLOBAL)

    find_path(SwiftConcurrency_INCLUDE_DIR
      "_Concurrency.swiftmodule"
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift/linux")
    find_library(SwiftConcurrency_LIBRARY
      NAMES "libswift_Concurrency.so"
      HINTS "${Swift_SDKROOT}/usr/lib/swift/linux")
    add_library(swift_Concurrency SHARED IMPORTED GLOBAL)
  endif()

  set_target_properties(swiftCore PROPERTIES
    IMPORTED_LOCATION "${SwiftCore_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${SwiftCore_INCLUDE_DIR}")
  set_target_properties(swiftShims PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES "${Shims_INCLUDE_DIR}/shims")
  set_target_properties(swiftOnoneSupport PROPERTIES
    IMPORTED_LOCATION "${SwiftOnoneSupport_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${SwiftOnoneSupport_INCLUDE_DIR}")
  set_target_properties(swift_Concurrency PROPERTIES
    IMPORTED_LOCATION "${SwiftConcurrency_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${SwiftConcurrency_INCLUDE_DIR}")

  target_link_libraries(swiftCore
    INTERFACE
    swiftShims)

  find_package_handle_standard_args(SwiftCore DEFAULT_MSG
    SwiftCore_LIBRARY SwiftCore_INCLUDE_DIR
    Shims_INCLUDE_DIR
    SwiftOnoneSupport_LIBRARY SwiftOnoneSupport_INCLUDE_DIR
    SwiftConcurrency_LIBRARY SwiftConcurrency_INCLUDE_DIR)
elseif(WIN32)
  find_path(SwiftCore_INCLUDE_DIR
    "Swift.swiftmodule"
    HINTS
      "${Swift_SDKROOT}/usr/lib/swift/windows"
      "$ENV{SDKROOT}/usr/lib/swift/windows")
  find_library(SwiftCore_LIBRARY
    NAMES "swiftCore.lib"
    HINTS
      "${Swift_SDKROOT}/usr/lib/swift/${SwiftCore_PLATFORM_SUBDIR}/${SwiftCore_ARCH_SUBDIR}"
      "${Swift_SDKROOT}/usr/lib/swift"
      "$ENV{SDKROOT}/usr/lib/swift/${SwiftCore_PLATFORM_SUBDIR}/${SwiftCore_ARCH_SUBDIR}"
      "$ENV{SDKROOT}/usr/lib/swift")

  add_library(swiftCore SHARED IMPORTED GLOBAL)
  set_target_properties(swiftCore PROPERTIES
    IMPORTED_IMPLIB "${SwiftCore_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${SwiftCore_INCLUDE_DIR}")

  find_path(Shims_INCLUDE_DIR
    "shims/module.modulemap"
    HINTS
      "${Swift_SDKROOT}/usr/lib/swift"
      "$ENV{SDKROOT}/usr/lib/swift")
  add_library(swiftShims INTERFACE IMPORTED GLOBAL)
  set_target_properties(swiftShims PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES "${Shims_INCLUDE_DIR}/shims")
  target_link_libraries(swiftCore
    INTERFACE
    swiftShims)

  find_path(SwiftOnoneSupport_INCLUDE_DIR
    "SwiftOnoneSupport.swiftmodule"
    HINTS
      "${Swift_SDKROOT}/usr/lib/swift/windows"
      "$ENV{SDKROOT}/usr/lib/swift/windows")
  find_library(SwiftOnoneSupport_LIBRARY
    NAMES "swiftSwiftOnoneSupport.lib"
    HINTS
      "${Swift_SDKROOT}/usr/lib/swift/${SwiftCore_PLATFORM_SUBDIR}/${SwiftCore_ARCH_SUBDIR}"
      "${Swift_SDKROOT}/usr/lib/swift"
      "$ENV{SDKROOT}/usr/lib/swift/${SwiftCore_PLATFORM_SUBDIR}/${SwiftCore_ARCH_SUBDIR}"
      "$ENV{SDKROOT}/usr/lib/swift")

  add_library(swiftOnoneSupport SHARED IMPORTED GLOBAL)
  set_target_properties(swiftOnoneSupport PROPERTIES
    IMPORTED_IMPLIB "${SwiftOnoneSupport_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${SwiftOnoneSupport_INCLUDE_DIR}")

  find_path(SwiftConcurrency_INCLUDE_DIR
    "_Concurrency.swiftmodule"
    HINTS
      "${Swift_SDKROOT}/usr/lib/swift/windows"
      "$ENV{SDKROOT}/usr/lib/swift/windows")
  find_library(SwiftConcurrency_LIBRARY
    NAMES "swift_Concurrency.lib"
    HINTS
      "${Swift_SDKROOT}/usr/lib/swift/${SwiftCore_PLATFORM_SUBDIR}/${SwiftCore_ARCH_SUBDIR}"
      "${Swift_SDKROOT}/usr/lib/swift"
      "$ENV{SDKROOT}/usr/lib/swift/${SwiftCore_PLATFORM_SUBDIR}/${SwiftCore_ARCH_SUBDIR}"
      "$ENV{SDKROOT}/usr/lib/swift")

  add_library(swift_Concurrency SHARED IMPORTED GLOBAL)
  set_target_properties(swift_Concurrency PROPERTIES
    IMPORTED_IMPLIB "${SwiftConcurrency_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${SwiftConcurrency_INCLUDE_DIR}")

  find_package_handle_standard_args(SwiftCore DEFAULT_MSG
    SwiftCore_LIBRARY SwiftCore_INCLUDE_DIR
    Shims_INCLUDE_DIR
    SwiftOnoneSupport_LIBRARY SwiftOnoneSupport_INCLUDE_DIR
    SwiftConcurrency_LIBRARY SwiftConcurrency_INCLUDE_DIR)
elseif(ANDROID)
  if(BUILD_SHARED_LIBS)
    find_path(SwiftCore_INCLUDE_DIR
      "Swift.swiftmodule"
      NO_CMAKE_FIND_ROOT_PATH
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift/android"
        "$ENV{SDKROOT}/usr/lib/swift/android")
    find_library(SwiftCore_LIBRARY
      NO_CMAKE_FIND_ROOT_PATH
      NAMES "libswiftCore.so"
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift/android/${SwiftCore_ARCH_SUBDIR}"
        "${Swift_SDKROOT}/usr/lib/swift"
        "$ENV{SDKROOT}/usr/lib/swift/android/${SwiftCore_ARCH_SUBDIR}"
        "$ENV{SDKROOT}/usr/lib/swift")

    add_library(swiftCore SHARED IMPORTED GLOBAL)

    find_path(Shims_INCLUDE_DIR
        "shims/module.modulemap"
        NO_CMAKE_FIND_ROOT_PATH
        HINTS
          "${Swift_SDKROOT}/usr/lib/swift")
    add_library(swiftShims INTERFACE IMPORTED GLOBAL)

    find_path(SwiftOnoneSupport_INCLUDE_DIR
      "SwiftOnoneSupport.swiftmodule"
      NO_CMAKE_FIND_ROOT_PATH
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift/android"
        "$ENV{SDKROOT}/usr/lib/swift/android")
    find_library(SwiftOnoneSupport_LIBRARY
      NO_CMAKE_FIND_ROOT_PATH
      NAMES "libswiftSwiftOnoneSupport.so"
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift/android/${SwiftCore_ARCH_SUBDIR}"
        "${Swift_SDKROOT}/usr/lib/swift"
        "$ENV{SDKROOT}/usr/lib/swift/android/${SwiftCore_ARCH_SUBDIR}"
        "$ENV{SDKROOT}/usr/lib/swift")
    add_library(swiftOnoneSupport SHARED IMPORTED GLOBAL)

    find_path(SwiftConcurrency_INCLUDE_DIR
      "_Concurrency.swiftmodule"
      NO_CMAKE_FIND_ROOT_PATH
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift/android"
        "$ENV{SDKROOT}/usr/lib/swift/android")
    find_library(SwiftConcurrency_LIBRARY
      NO_CMAKE_FIND_ROOT_PATH
      NAMES "libswift_Concurrency.so"
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift/android/${SwiftCore_ARCH_SUBDIR}"
        "${Swift_SDKROOT}/usr/lib/swift"
        "$ENV{SDKROOT}/usr/lib/swift/android/${SwiftCore_ARCH_SUBDIR}"
        "$ENV{SDKROOT}/usr/lib/swift")
    add_library(swift_Concurrency SHARED IMPORTED GLOBAL)
  else()
    find_path(SwiftCore_INCLUDE_DIR
      "Swift.swiftmodule"
      NO_CMAKE_FIND_ROOT_PATH
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift_static/android"
        "$ENV{SDKROOT}/usr/lib/swift_static/android")
    find_library(SwiftCore_LIBRARY
      NO_CMAKE_FIND_ROOT_PATH
      NAMES "libswiftCore.a"
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift_static/android/${SwiftCore_ARCH_SUBDIR}"
        "${Swift_SDKROOT}/usr/lib/swift_static"
        "$ENV{SDKROOT}/usr/lib/swift_static/android/${SwiftCore_ARCH_SUBDIR}"
        "$ENV{SDKROOT}/usr/lib/swift_static")

    add_library(swiftCore STATIC IMPORTED GLOBAL)

    find_path(Shims_INCLUDE_DIR
        "shims/module.modulemap"
        NO_CMAKE_FIND_ROOT_PATH
        HINTS
          "${Swift_SDKROOT}/usr/lib/swift")
    add_library(swiftShims INTERFACE IMPORTED GLOBAL)

    find_path(SwiftOnoneSupport_INCLUDE_DIR
      "SwiftOnoneSupport.swiftmodule"
      NO_CMAKE_FIND_ROOT_PATH
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift_static/android"
        "$ENV{SDKROOT}/usr/lib/swift_static/android")
    find_library(SwiftOnoneSupport_LIBRARY
      NO_CMAKE_FIND_ROOT_PATH
      NAMES "libswiftSwiftOnoneSupport.a"
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift_static/android/${SwiftCore_ARCH_SUBDIR}"
        "${Swift_SDKROOT}/usr/lib/swift_static"
        "$ENV{SDKROOT}/usr/lib/swift_static/android/${SwiftCore_ARCH_SUBDIR}"
        "$ENV{SDKROOT}/usr/lib/swift_static")
    add_library(swiftOnoneSupport STATIC IMPORTED GLOBAL)

    find_path(SwiftConcurrency_INCLUDE_DIR
      "_Concurrency.swiftmodule"
      NO_CMAKE_FIND_ROOT_PATH
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift_static/android"
        "$ENV{SDKROOT}/usr/lib/swift_static/android")
    find_library(SwiftConcurrency_LIBRARY
      NO_CMAKE_FIND_ROOT_PATH
      NAMES "libswift_Concurrency.a"
      HINTS
        "${Swift_SDKROOT}/usr/lib/swift_static/android/${SwiftCore_ARCH_SUBDIR}"
        "${Swift_SDKROOT}/usr/lib/swift_static"
        "$ENV{SDKROOT}/usr/lib/swift_static/android/${SwiftCore_ARCH_SUBDIR}"
        "$ENV{SDKROOT}/usr/lib/swift_static")
    add_library(swift_Concurrency STATIC IMPORTED GLOBAL)
  endif()

  set_target_properties(swiftCore PROPERTIES
    IMPORTED_LOCATION "${SwiftCore_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${SwiftCore_INCLUDE_DIR}")
  set_target_properties(swiftShims PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES "${Shims_INCLUDE_DIR}/shims")
  set_target_properties(swiftOnoneSupport PROPERTIES
    IMPORTED_LOCATION "${SwiftOnoneSupport_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${SwiftOnoneSupport_INCLUDE_DIR}")
  set_target_properties(swift_Concurrency PROPERTIES
    IMPORTED_LOCATION "${SwiftConcurrency_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${SwiftConcurrency_INCLUDE_DIR}")

  target_link_libraries(swiftCore
    INTERFACE
    swiftShims)

  find_package_handle_standard_args(SwiftCore DEFAULT_MSG
    SwiftCore_LIBRARY SwiftCore_INCLUDE_DIR
    Shims_INCLUDE_DIR
    SwiftOnoneSupport_LIBRARY SwiftOnoneSupport_INCLUDE_DIR
    SwiftConcurrency_LIBRARY SwiftConcurrency_INCLUDE_DIR)
else()
  message(FATAL_ERROR "FindSwiftCore.cmake module search not implemented for targeted platform\n"
  " Build Core for your platform and set `SwiftCore_DIR` to"
  " the directory containing SwiftCoreConfig.cmake\n")
endif()

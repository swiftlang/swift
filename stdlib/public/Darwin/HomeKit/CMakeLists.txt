cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

foreach(sdk ${SWIFT_SDKS})

if(${sdk} STREQUAL IOS OR ${sdk} STREQUAL IOS_SIMULATOR
  OR ${sdk} STREQUAL TVOS OR ${sdk} STREQUAL TVOS_SIMULATOR
  OR ${sdk} STREQUAL WATCHOS OR ${sdk} STREQUAL WATCHOS_SIMULATOR)
else()
  continue()
endif()

select_swift_overlay_dependenies(module_depends
  SDK ${sdk}
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreImage CoreGraphics Metal UIKit Dispatch Foundation QuartzCore CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreImage CoreGraphics Metal UIKit Dispatch Foundation QuartzCore CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin CoreGraphics Dispatch Foundation CoreFoundation ObjectiveC # auto-updated
    UIKit # required in some configurations but not found by tool
)

if(${sdk} STREQUAL WATCHOS)
  list(APPEND module_depends UIKit) # required in some configurations but not found by tool
endif()

add_swift_target_library_single_sdk(swiftHomeKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  HomeKit.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SDK ${sdk}
  SWIFT_MODULE_DEPENDS ${module_depends}
  FRAMEWORK_DEPENDS_WEAK HomeKit

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_HOMEKIT_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_HOMEKIT_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_HOMEKIT_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_HOMEKIT_WATCHOS}
  INSTALL_IN_COMPONENT sdk-overlay
)

endforeach()

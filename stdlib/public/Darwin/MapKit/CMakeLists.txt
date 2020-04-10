cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

foreach(sdk ${SWIFT_SDKS})

select_swift_overlay_dependenies(module_depends
  SDK ${sdk}
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreImage CoreGraphics Metal Dispatch IOKit Foundation CoreData CoreLocation QuartzCore AppKit XPC CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreImage CoreGraphics Metal UIKit Dispatch Foundation CoreLocation QuartzCore CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreImage CoreGraphics Metal UIKit Dispatch Foundation CoreLocation QuartzCore CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin CoreGraphics UIKit Dispatch Foundation CoreLocation CoreFoundation ObjectiveC # auto-updated
)

if(${sdk} STREQUAL WATCHOS)
  list(APPEND module_depends UIKit) # required in some configurations but not found by tool
endif()

add_swift_target_library_single_sdk(swiftMapKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY

  GYB_SOURCES
    NSValue.swift.gyb

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SDK ${sdk}
  SWIFT_MODULE_DEPENDS ${module_depends}
  FRAMEWORK_DEPENDS MapKit

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_MAPKIT_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_MAPKIT_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_MAPKIT_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_MAPKIT_WATCHOS}
  INSTALL_IN_COMPONENT sdk-overlay
)

endforeach()

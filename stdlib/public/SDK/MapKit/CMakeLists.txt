add_swift_target_library(swiftMapKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  NSValue.swift.gyb
  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SWIFT_MODULE_DEPENDS_OSX Darwin AppKit CoreData CoreGraphics CoreImage CoreLocation Dispatch Foundation IOKit ObjectiveC QuartzCore XPC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreGraphics CoreImage CoreLocation Dispatch Foundation ObjectiveC QuartzCore UIKit # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreGraphics CoreImage CoreLocation Dispatch Foundation ObjectiveC QuartzCore UIKit # auto-updated
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin CoreGraphics CoreLocation Dispatch Foundation ObjectiveC # auto-updated
    UIKit # required in some configurations but not found by tool
  FRAMEWORK_DEPENDS MapKit

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_MAPKIT_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_MAPKIT_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_MAPKIT_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_MAPKIT_WATCHOS}
)

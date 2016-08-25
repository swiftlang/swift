add_swift_library(swiftMapKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  NSValue.swift.gyb
  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SWIFT_MODULE_DEPENDS ObjectiveC Foundation CoreLocation
  SWIFT_MODULE_DEPENDS_IOS CoreGraphics CoreImage CoreLocation Dispatch Foundation ObjectiveC os QuartzCore UIKit
  SWIFT_MODULE_DEPENDS_OSX AppKit CoreData CoreGraphics CoreImage CoreLocation Dispatch Foundation IOKit ObjectiveC os QuartzCore XPC
  SWIFT_MODULE_DEPENDS_TVOS CoreGraphics CoreImage CoreLocation Dispatch Foundation ObjectiveC os QuartzCore UIKit
  FRAMEWORK_DEPENDS MapKit)


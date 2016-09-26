add_swift_library(swiftGLKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  GLKit.swift.gyb

  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation simd
  SWIFT_MODULE_DEPENDS_OSX AppKit CoreData CoreGraphics CoreImage Dispatch Foundation IOKit ObjectiveC os QuartzCore simd XPC
  SWIFT_MODULE_DEPENDS_IOS CoreGraphics CoreImage Dispatch Foundation ObjectiveC os QuartzCore simd UIKit
  SWIFT_MODULE_DEPENDS_TVOS CoreGraphics CoreImage Dispatch Foundation ObjectiveC os QuartzCore simd UIKit
  FRAMEWORK_DEPENDS GLKit)


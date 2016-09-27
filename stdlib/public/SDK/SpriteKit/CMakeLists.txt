add_swift_library(swiftSpriteKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  SpriteKit.swift
  SpriteKitQuickLooks.swift.gyb

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation simd
  SWIFT_MODULE_DEPENDS_OSX AppKit CoreData CoreGraphics CoreImage Dispatch Foundation GLKit IOKit ObjectiveC os QuartzCore simd XPC
  SWIFT_MODULE_DEPENDS_IOS CoreGraphics CoreImage Dispatch Foundation GLKit ObjectiveC os QuartzCore simd UIKit
  SWIFT_MODULE_DEPENDS_TVOS CoreGraphics CoreImage Dispatch Foundation GLKit ObjectiveC os QuartzCore simd UIKit
  SWIFT_MODULE_DEPENDS_WATCHOS CoreGraphics Dispatch Foundation ObjectiveC os simd UIKit
  FRAMEWORK_DEPENDS SpriteKit)


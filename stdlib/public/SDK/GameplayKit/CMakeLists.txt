add_swift_library(swiftGameplayKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  GameplayKit.swift
  GameplayKit.mm

  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation simd GLKit SpriteKit
  SWIFT_MODULE_DEPENDS_OSX AppKit CoreData CoreGraphics CoreImage Dispatch Foundation GLKit IOKit ObjectiveC os QuartzCore simd SpriteKit XPC
  SWIFT_MODULE_DEPENDS_IOS CoreGraphics CoreImage Dispatch Foundation GLKit ObjectiveC os QuartzCore simd SpriteKit UIKit
  SWIFT_MODULE_DEPENDS_TVOS CoreGraphics CoreImage Dispatch Foundation GLKit ObjectiveC os QuartzCore simd SpriteKit UIKit
  FRAMEWORK_DEPENDS_WEAK GameplayKit)

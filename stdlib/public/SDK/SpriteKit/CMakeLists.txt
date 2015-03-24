add_swift_library(swiftSpriteKit IS_SDK_OVERLAY
  SpriteKit.swift
  SpriteKitMirrors.swift.gyb

  SWIFT_MODULE_DEPENDS Foundation GLKit
  SWIFT_MODULE_DEPENDS_OSX AppKit
  SWIFT_MODULE_DEPENDS_IOS_TVOS UIKit
  FRAMEWORK_DEPENDS SpriteKit)


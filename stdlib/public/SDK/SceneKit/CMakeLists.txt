add_swift_library(swiftSceneKit IS_SDK_OVERLAY
  SceneKit.swift

  SWIFT_MODULE_DEPENDS Foundation GLKit
  SWIFT_MODULE_DEPENDS_OSX AppKit QuartzCore
  SWIFT_MODULE_DEPENDS_IOS_TVOS UIKit
  FRAMEWORK_DEPENDS SceneKit)


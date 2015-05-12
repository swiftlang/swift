add_swift_library(swiftSceneKit IS_SDK_OVERLAY
  SceneKit.swift
  Thunks.mm

  SWIFT_MODULE_DEPENDS Foundation GLKit simd CoreImage
  SWIFT_MODULE_DEPENDS_OSX AppKit
  SWIFT_MODULE_DEPENDS_IOS_TVOS UIKit
  FRAMEWORK_DEPENDS SceneKit)


add_swift_library(swiftGameCenter IS_SDK_OVERLAY
  GameCenter.swift
  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation
  SWIFT_MODULE_DEPENDS_OSX AppKit
  SWIFT_MODULE_DEPENDS_IOS_TVOS UIKit
  FRAMEWORK_DEPENDS GameKit # some of these types used to be in GameKit
  )

add_swift_library(swiftHomeKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  HomeKit.swift
  TARGET_SDKS IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation
  SWIFT_MODULE_DEPENDS_IOS CoreGraphics CoreImage Dispatch Foundation ObjectiveC os QuartzCore UIKit
  SWIFT_MODULE_DEPENDS_TVOS CoreGraphics CoreImage Dispatch Foundation ObjectiveC os QuartzCore UIKit
  SWIFT_MODULE_DEPENDS_WATCHOS CoreGraphics Dispatch Foundation ObjectiveC os
  FRAMEWORK_DEPENDS_WEAK HomeKit)

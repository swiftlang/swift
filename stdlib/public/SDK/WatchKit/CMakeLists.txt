add_swift_library(swiftWatchKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  WatchKit.swift
  TARGET_SDKS IOS IOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation UIKit CoreGraphics CoreLocation SceneKit
  FRAMEWORK_DEPENDS_WEAK WatchKit
  SWIFT_COMPILE_FLAGS_WATCHOS -Xfrontend -disable-autolink-framework -Xfrontend CoreText
)

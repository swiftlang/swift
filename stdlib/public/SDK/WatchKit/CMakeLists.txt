add_swift_library(swiftWatchKit IS_SDK_OVERLAY
  WatchKit.swift
  TARGET_SDKS IOS IOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation UIKit CoreGraphics
  FRAMEWORK_DEPENDS_WEAK WatchKit
  SWIFT_COMPILE_FLAGS -Xfrontend -disable-availability-checking
  SWIFT_COMPILE_FLAGS_WATCHOS -Xfrontend -disable-autolink-framework -Xfrontend CoreText
)

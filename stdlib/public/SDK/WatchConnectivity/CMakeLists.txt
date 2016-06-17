add_swift_library(swiftWatchConnectivity IS_SDK_OVERLAY
  WatchConnectivity.swift
  TARGET_SDKS IOS IOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation
  FRAMEWORK_DEPENDS_WEAK WatchConnectivity)

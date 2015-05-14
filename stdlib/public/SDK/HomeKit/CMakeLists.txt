add_swift_library(swiftHomeKit IS_SDK_OVERLAY
  HomeKit.swift
  TARGET_SDKS IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation)

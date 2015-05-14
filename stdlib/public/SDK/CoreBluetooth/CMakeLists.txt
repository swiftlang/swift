add_swift_library(swiftCoreBluetooth IS_SDK_OVERLAY
  CoreBluetooth.swift
  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation
  FRAMEWORK_DEPENDS CoreBluetooth)

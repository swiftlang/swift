add_swift_library(swiftMultipeerConnectivity IS_SDK_OVERLAY
  MultipeerConnectivity.swift
  TARGET_SDKS OSX IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation)

add_swift_library(swiftLocalAuthentication IS_SDK_OVERLAY
  LocalAuthentication.swift
  TARGET_SDKS OSX IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation
  FRAMEWORK_DEPENDS LocalAuthentication)

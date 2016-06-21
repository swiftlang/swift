add_swift_library(swiftLocalAuthentication ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  LocalAuthentication.swift
  TARGET_SDKS OSX IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation
  FRAMEWORK_DEPENDS_WEAK LocalAuthentication)

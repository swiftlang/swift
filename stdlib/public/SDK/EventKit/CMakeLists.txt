add_swift_library(swiftEventKit IS_SDK_OVERLAY
  EventKit.swift

  TARGET_SDKS OSX IOS IOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation CoreGraphics CoreLocation)

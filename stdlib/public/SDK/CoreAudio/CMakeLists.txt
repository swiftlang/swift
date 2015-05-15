add_swift_library(swiftCoreAudio IS_SDK_OVERLAY
  CoreAudio.swift

  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Dispatch
  # Also depends on: CoreFoundation IOKit
  FRAMEWORK_DEPENDS CoreAudio)


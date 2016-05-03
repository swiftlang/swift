add_swift_library(swiftCoreAudio IS_SDK_OVERLAY
  CoreAudio.swift
  ../../../public/core/WriteBackMutableSlice.swift

  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Dispatch
  SWIFT_MODULE_DEPENDS_OSX IOKit
  # Also depends on: CoreFoundation
  FRAMEWORK_DEPENDS CoreAudio)


add_swift_library(swiftCoreAudio ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  CoreAudio.swift
  ../../../public/core/WriteBackMutableSlice.swift

  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR WATCHOS
  SWIFT_MODULE_DEPENDS Dispatch
  SWIFT_MODULE_DEPENDS_OSX IOKit
  # Also depends on: CoreFoundation
  FRAMEWORK_DEPENDS CoreAudio)


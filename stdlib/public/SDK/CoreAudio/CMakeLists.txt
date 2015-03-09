add_swift_library(swiftCoreAudio IS_SDK_OVERLAY
  CoreAudio.swift
  SWIFT_MODULE_DEPENDS Dispatch
  # Also depends on: CoreFoundation IOKit
  FRAMEWORK_DEPENDS CoreAudio)


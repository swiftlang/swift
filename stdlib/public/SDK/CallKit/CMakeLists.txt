add_swift_library(swiftCallKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  CallKit.swift
  CXProviderConfiguration.swift

  TARGET_SDKS IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_IOS Darwin Dispatch Foundation ObjectiveC # auto-updated
  FRAMEWORK_DEPENDS_WEAK CallKit
)

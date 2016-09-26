add_swift_library(swiftAssetsLibrary ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  AssetsLibrary.swift
  TARGET_SDKS IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS ObjectiveC Foundation CoreGraphics
  SWIFT_MODULE_DEPENDS_IOS CoreGraphics Dispatch Foundation ObjectiveC os
  FRAMEWORK_DEPENDS AssetsLibrary)


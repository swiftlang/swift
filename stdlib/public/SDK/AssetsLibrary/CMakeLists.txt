add_swift_library(swiftAssetsLibrary ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  AssetsLibrary.swift
  TARGET_SDKS IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS ObjectiveC Foundation CoreGraphics
  FRAMEWORK_DEPENDS AssetsLibrary)


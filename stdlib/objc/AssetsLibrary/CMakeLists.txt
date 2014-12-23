add_swift_library(swiftAssetsLibrary IS_SDK_OVERLAY
  AssetsLibrary.swift
  TARGET_SDKS IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS ObjectiveC Foundation CoreGraphics
  FRAMEWORK_DEPENDS AssetsLibrary)


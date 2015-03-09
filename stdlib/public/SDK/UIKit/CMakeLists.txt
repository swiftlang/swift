add_swift_library(swiftUIKit IS_SDK_OVERLAY
  DesignatedInitializers.mm
  UIKit.swift
  TARGET_SDKS IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS ObjectiveC Foundation CoreImage
  FRAMEWORK_DEPENDS UIKit)


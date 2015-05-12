add_swift_library(swiftAppKit IS_SDK_OVERLAY
  AppKit.swift
  TARGET_SDKS OSX
  SWIFT_MODULE_DEPENDS ObjectiveC Foundation CoreImage
  FRAMEWORK_DEPENDS AppKit)


add_swift_library(swiftIOKit IS_SDK_OVERLAY
  IOKit.swift
  TARGET_SDKS OSX
  SWIFT_MODULE_DEPENDS ObjectiveC Dispatch
  FRAMEWORK_DEPENDS IOKit)

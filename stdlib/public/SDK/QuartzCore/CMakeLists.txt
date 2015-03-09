add_swift_library(swiftQuartzCore IS_SDK_OVERLAY
  QuartzCore.swift
  TARGET_SDKS OSX
  SWIFT_MODULE_DEPENDS Foundation ObjectiveC
  FRAMEWORK_DEPENDS QuartzCore)


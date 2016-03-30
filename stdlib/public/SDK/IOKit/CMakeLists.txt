add_swift_library(swiftIOKit IS_SDK_OVERLAY
  IOKit.swift
  TARGET_SDKS OSX
  FRAMEWORK_DEPENDS IOKit)

add_swift_library(swiftXPC IS_SDK_OVERLAY
  XPC.swift
  XPC.mm
  TARGET_SDKS OSX
  SWIFT_MODULE_DEPENDS ObjectiveC Dispatch)

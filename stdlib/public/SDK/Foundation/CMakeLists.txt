add_swift_library(swiftFoundation IS_SDK_OVERLAY
  Foundation.swift
  NSError.swift
  NSStringAPI.swift
  NSValue.swift
  ExtraStringAPIs.swift
  Thunks.mm

  SWIFT_MODULE_DEPENDS ObjectiveC CoreGraphics Dispatch
  FRAMEWORK_DEPENDS Foundation)


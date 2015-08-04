add_swift_library(swiftFoundation IS_SDK_OVERLAY
  Foundation.swift
  FoundationMirrors.swift.gyb
  NSError.swift
  NSStringAPI.swift
  NSValue.swift
  Thunks.mm

  SWIFT_MODULE_DEPENDS ObjectiveC CoreGraphics Dispatch
  FRAMEWORK_DEPENDS Foundation)


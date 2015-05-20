add_swift_library(swiftFoundation IS_SDK_OVERLAY
  Foundation.swift
  FoundationMirrors.swift.gyb
  ExtraStringAPIs.swift
  NSError.swift
  NSStringAPI.swift
  NSValue.swift
  Thunks.mm

  SWIFT_MODULE_DEPENDS ObjectiveC CoreGraphics Dispatch Security
  FRAMEWORK_DEPENDS Foundation)


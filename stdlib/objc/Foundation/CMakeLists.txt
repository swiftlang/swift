add_swift_library(swiftFoundation IS_SDK_OVERLAY
  Foundation.swift
  FoundationMirrors.swift.gyb
  ExtraStringAPIs.swift
  Misc.mm
  MixedStringNSStringOperations.swift.gyb
  NSStringAPI.swift
  NSValue.swift
  SWIFT_MODULE_DEPENDS ObjectiveC CoreGraphics Dispatch Security
  FRAMEWORK_DEPENDS Foundation)


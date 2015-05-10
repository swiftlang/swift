add_swift_library(swiftCoreGraphics IS_SDK_OVERLAY
  CoreGraphics.swift
  CGFloat.swift.gyb
  CoreGraphicsMirrors.swift.gyb
  SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all
  SWIFT_MODULE_DEPENDS ObjectiveC Dispatch Darwin
  FRAMEWORK_DEPENDS CoreGraphics)


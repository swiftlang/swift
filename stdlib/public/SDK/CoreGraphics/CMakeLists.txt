add_swift_library(swiftCoreGraphics IS_SDK_OVERLAY
  CoreGraphics.swift
  CGFloat.swift.gyb
  # rdar://problem/20891746
  # SWIFT_COMPILE_FLAGS ${STDLIB_SIL_SERIALIZE_ALL}
  SWIFT_MODULE_DEPENDS ObjectiveC Dispatch Darwin
  SWIFT_MODULE_DEPENDS_OSX IOKit
  FRAMEWORK_DEPENDS CoreGraphics)


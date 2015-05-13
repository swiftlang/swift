add_swift_library(swiftCoreMedia IS_SDK_OVERLAY
  CoreMedia.swift
  CMTime.swift
  CMTimeRange.swift

  SWIFT_MODULE_DEPENDS Dispatch CoreAudio CoreGraphics Foundation
  SWIFT_MODULE_DEPENDS_OSX AppKit
  FRAMEWORK_DEPENDS CoreMedia)


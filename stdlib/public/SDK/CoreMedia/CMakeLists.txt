add_swift_library(swiftCoreMedia IS_SDK_OVERLAY
  CoreMedia.swift
  CMTime.swift
  CMTimeRange.swift

  SWIFT_MODULE_DEPENDS Dispatch CoreAudio Foundation
  FRAMEWORK_DEPENDS CoreMedia)


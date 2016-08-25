add_swift_library(swiftCoreMedia ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  CMTime.swift
  CMTimeRange.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Dispatch CoreAudio CoreGraphics Foundation
  SWIFT_MODULE_DEPENDS_OSX AppKit CoreAudio CoreData CoreGraphics CoreImage Dispatch Foundation IOKit ObjectiveC os QuartzCore XPC
  SWIFT_MODULE_DEPENDS_IOS CoreAudio CoreGraphics Dispatch Foundation ObjectiveC os
  SWIFT_MODULE_DEPENDS_TVOS CoreAudio CoreGraphics Dispatch Foundation ObjectiveC os
  FRAMEWORK_DEPENDS CoreMedia)


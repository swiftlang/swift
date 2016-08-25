add_swift_library(swiftAVFoundation ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  AVError.swift
  NSValue.swift.gyb

  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SWIFT_MODULE_DEPENDS Foundation CoreMedia
  SWIFT_MODULE_DEPENDS_OSX AppKit CoreAudio CoreData CoreGraphics CoreImage CoreMedia Dispatch Foundation IOKit ObjectiveC os QuartzCore XPC
  SWIFT_MODULE_DEPENDS_IOS CoreAudio CoreGraphics CoreMedia Dispatch Foundation ObjectiveC os QuartzCore
  SWIFT_MODULE_DEPENDS_TVOS CoreAudio CoreGraphics CoreMedia Dispatch Foundation ObjectiveC os QuartzCore
  FRAMEWORK_DEPENDS AVFoundation)

add_swift_library(swiftCoreImage ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  CoreImage.swift

  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation ObjectiveC
  SWIFT_MODULE_DEPENDS_IOS CoreGraphics Dispatch Foundation ObjectiveC os
  SWIFT_MODULE_DEPENDS_TVOS CoreGraphics Dispatch Foundation ObjectiveC os
  FRAMEWORK_DEPENDS_OSX QuartzCore
  FRAMEWORK_DEPENDS_IOS_TVOS CoreImage)


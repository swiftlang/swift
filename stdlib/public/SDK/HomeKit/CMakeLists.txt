add_swift_library(swiftHomeKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  HomeKit.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreGraphics CoreImage Dispatch Foundation ObjectiveC QuartzCore UIKit # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreGraphics CoreImage Dispatch Foundation ObjectiveC QuartzCore UIKit # auto-updated
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin CoreGraphics Dispatch Foundation ObjectiveC # auto-updated
    UIKit # required in some configurations but not found by tool
  FRAMEWORK_DEPENDS_WEAK HomeKit
)

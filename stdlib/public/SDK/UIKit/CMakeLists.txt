add_swift_library(swiftUIKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  DesignatedInitializers.mm
  UIKit.swift
  UIKit_FoundationExtensions.swift.gyb

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR
  SWIFT_MODULE_DEPENDS ObjectiveC Foundation
  SWIFT_MODULE_DEPENDS_IOS CoreGraphics CoreImage Dispatch Foundation ObjectiveC os QuartzCore
  SWIFT_MODULE_DEPENDS_TVOS CoreGraphics CoreImage Dispatch Foundation ObjectiveC os QuartzCore
  SWIFT_MODULE_DEPENDS_WATCHOS CoreGraphics Dispatch Foundation ObjectiveC os
  SWIFT_COMPILE_FLAGS_WATCHOS -Xfrontend -disable-autolink-framework -Xfrontend CoreText
  FRAMEWORK_DEPENDS UIKit)


add_swift_library(swiftUIKit IS_SDK_OVERLAY
  DesignatedInitializers.mm
  UIKit.swift
  TARGET_SDKS IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR
  SWIFT_MODULE_DEPENDS ObjectiveC Foundation CoreGraphics
  SWIFT_MODULE_DEPENDS_IOS Contacts CoreImage CoreGraphics
  SWIFT_MODULE_DEPENDS_TVOS CoreImage CoreGraphics
  SWIFT_COMPILE_FLAGS_WATCHOS -Xfrontend -disable-autolink-framework -Xfrontend CoreText
  FRAMEWORK_DEPENDS UIKit)


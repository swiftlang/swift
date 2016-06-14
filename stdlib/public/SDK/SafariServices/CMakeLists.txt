add_swift_library(swiftSafariServices ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  SafariServices.mm
  SafariServices.swift

  TARGET_SDKS OSX
  SWIFT_MODULE_DEPENDS Foundation AppKit
  FRAMEWORK_DEPENDS_WEAK SafariServices)

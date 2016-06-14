add_swift_library(swiftSafariServices IS_SDK_OVERLAY
  SafariServices.mm
  SafariServices.swift

  TARGET_SDKS OSX
  SWIFT_MODULE_DEPENDS Foundation AppKit
  FRAMEWORK_DEPENDS_WEAK SafariServices)

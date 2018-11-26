add_swift_library(swiftTestUtils SHARED
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftTestUtils.swift

  SWIFT_MODULE_DEPENDS Foundation
  INSTALL_IN_COMPONENT swift-syntax
  TARGET_SDKS OSX
  IS_STDLIB)

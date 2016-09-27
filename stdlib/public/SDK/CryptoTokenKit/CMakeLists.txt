add_swift_library(swiftCryptoTokenKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  CryptoTokenKit.swift

  TARGET_SDKS OSX
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreGraphics Dispatch Foundation IOKit ObjectiveC # auto-updated
  FRAMEWORK_DEPENDS CryptoTokenKit
)

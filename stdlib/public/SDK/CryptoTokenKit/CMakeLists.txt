add_swift_library(swiftCryptoTokenKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  CryptoTokenKit.swift

  TARGET_SDKS OSX
  SWIFT_MODULE_DEPENDS Foundation
  FRAMEWORK_DEPENDS CryptoTokenKit)

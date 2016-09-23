add_swift_library(swiftCoreLocation ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  CoreLocation.swift.gyb
  SWIFT_MODULE_DEPENDS Foundation
  FRAMEWORK_DEPENDS CoreLocation)

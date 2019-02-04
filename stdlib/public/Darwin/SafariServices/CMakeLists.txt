cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_target_library(swiftSafariServices ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  SafariServices.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX
  SWIFT_MODULE_DEPENDS_OSX Darwin AppKit CoreData CoreGraphics CoreImage Dispatch Foundation IOKit ObjectiveC QuartzCore XPC # auto-updated
  FRAMEWORK_DEPENDS_WEAK SafariServices

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_SAFARISERVICES_OSX}
)

cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_target_library(swiftMetal ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    Metal.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreFoundation CoreGraphics Dispatch Foundation IOKit ObjectiveC XPC # auto-updated
    os
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreFoundation Dispatch Foundation ObjectiveC # auto-updated
    os
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreFoundation Dispatch Foundation ObjectiveC # auto-updated
    os

  FRAMEWORK_DEPENDS_WEAK Metal

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_METAL_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_METAL_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_METAL_TVOS}
)

cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_target_library(swiftAccelerate ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  BNNS.swift.gyb

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR
  SWIFT_COMPILE_FLAGS -parse-stdlib
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreFoundation CoreGraphics Dispatch Foundation IOKit Metal ObjectiveC XPC # auto-updated
    os
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreFoundation CoreGraphics Dispatch Foundation Metal ObjectiveC os # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreFoundation CoreGraphics Dispatch Foundation Metal ObjectiveC os # auto-updated
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin CoreFoundation CoreGraphics Dispatch Foundation ObjectiveC os # auto-updated

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_SIMD_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_SIMD_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_SIMD_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_SIMD_WATCHOS}
)

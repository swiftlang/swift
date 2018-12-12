cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_target_library(swiftVision ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  Vision.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreImage CoreGraphics Metal Dispatch IOKit simd Foundation XPC CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreImage CoreGraphics Metal Dispatch simd Foundation CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreImage CoreGraphics Metal Dispatch simd Foundation CoreFoundation ObjectiveC # auto-updated

  FRAMEWORK_DEPENDS_WEAK Vision

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_VISION_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_VISION_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_VISION_TVOS}
)

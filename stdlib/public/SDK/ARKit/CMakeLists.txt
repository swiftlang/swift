cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_library(swiftARKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  ARKit.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_IOS Darwin AVFoundation CoreAudio CoreFoundation CoreGraphics CoreImage CoreMedia Dispatch Foundation GLKit ObjectiveC QuartzCore SceneKit simd SpriteKit UIKit # auto-updated
  FRAMEWORK_DEPENDS_WEAK ARKit

  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_ARKIT_IOS}
)

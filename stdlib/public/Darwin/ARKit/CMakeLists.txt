cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_target_library(swiftARKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  ARKit.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS IOS IOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreImage CoreGraphics Metal UIKit Dispatch GLKit SceneKit simd Foundation AVFoundation SpriteKit CoreMedia QuartzCore ModelIO CoreFoundation CoreAudio ObjectiveC # auto-updated
  FRAMEWORK_DEPENDS_WEAK ARKit

  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_ARKIT_IOS}
  INSTALL_IN_COMPONENT sdk-overlay
)

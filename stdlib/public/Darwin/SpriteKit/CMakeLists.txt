cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_target_library(swiftSpriteKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  SpriteKit.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  GYB_SOURCES
    SpriteKitQuickLooks.swift.gyb

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR WATCHOS WATCHOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreImage CoreGraphics Metal Dispatch IOKit GLKit simd Foundation CoreData QuartzCore AppKit ModelIO XPC CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreImage CoreGraphics Metal UIKit Dispatch GLKit simd Foundation QuartzCore ModelIO CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreImage CoreGraphics Metal UIKit Dispatch GLKit simd Foundation QuartzCore ModelIO CoreFoundation ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin CoreGraphics UIKit Dispatch simd Foundation CoreFoundation ObjectiveC # auto-updated
  FRAMEWORK_DEPENDS SpriteKit

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_SPRITEKIT_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_SPRITEKIT_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_SPRITEKIT_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_SPRITEKIT_WATCHOS}
  INSTALL_IN_COMPONENT sdk-overlay
)

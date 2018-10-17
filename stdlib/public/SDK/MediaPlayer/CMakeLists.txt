cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

swift_add_target_library(swiftMediaPlayer ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  MPMusicPlayerPlayParameters.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS IOS IOS_SIMULATOR

  SWIFT_MODULE_DEPENDS_IOS Darwin AVFoundation CoreAudio CoreFoundation CoreGraphics CoreImage CoreMedia Dispatch Foundation Metal ObjectiveC QuartzCore simd UIKit os CoreData # auto-updated
  FRAMEWORK_DEPENDS_WEAK MediaPlayer

  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_MEDIAPLAYER_IOS}
)

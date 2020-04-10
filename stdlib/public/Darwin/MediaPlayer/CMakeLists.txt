cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

foreach(target_sdk ${SWIFT_SDKS})

if(${target_sdk} STREQUAL IOS OR ${target_sdk} STREQUAL IOS_SIMULATOR)
else()
  continue()
endif()

set(
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreImage CoreGraphics Metal UIKit Dispatch simd Foundation AVFoundation CoreMedia QuartzCore CoreFoundation CoreAudio ObjectiveC # auto-updated
)

add_swift_target_library_single_sdk(swiftMediaPlayer ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  MPMusicPlayerPlayParameters.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}" "-L${sdk}/usr/lib/swift/"
  TARGET_SDKS IOS IOS_SIMULATOR

  SDK ${target_sdk}
  SWIFT_MODULE_DEPENDS ${SWIFT_MODULE_DEPENDS_IOS}
  SWIFT_MODULE_DEPENDS_FROM_SDK CoreMIDI
  FRAMEWORK_DEPENDS_WEAK MediaPlayer

  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_MEDIAPLAYER_IOS}
  INSTALL_IN_COMPONENT sdk-overlay
)

endforeach()

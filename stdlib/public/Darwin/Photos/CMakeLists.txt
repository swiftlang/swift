cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_target_library(swiftPhotos ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  PHChange.swift
  PHProjectChangeRequest.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR OSX
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreImage CoreGraphics Metal Dispatch simd Foundation AVFoundation CoreMedia CoreLocation QuartzCore CoreFoundation CoreAudio ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreImage CoreGraphics Metal Dispatch simd Foundation AVFoundation CoreMedia CoreLocation QuartzCore CoreFoundation CoreAudio ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreImage CoreGraphics Metal Dispatch IOKit simd Foundation AVFoundation CoreMedia CoreLocation QuartzCore XPC CoreFoundation CoreAudio ObjectiveC # auto-updated
  
  FRAMEWORK_DEPENDS Photos

  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_PHOTOS_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_PHOTOS_TVOS}
  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_PHOTOS_OSX}
  INSTALL_IN_COMPONENT sdk-overlay
)

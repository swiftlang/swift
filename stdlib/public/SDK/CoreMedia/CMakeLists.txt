cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

swift_add_target_library(swiftCoreMedia ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  CMTime.swift
  CMTimeRange.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_OSX Darwin AppKit CoreAudio CoreData CoreGraphics CoreImage Dispatch Foundation IOKit Metal ObjectiveC QuartzCore XPC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreAudio CoreGraphics Dispatch Foundation Metal ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreAudio CoreGraphics Dispatch Foundation Metal ObjectiveC # auto-updated
  FRAMEWORK_DEPENDS CoreMedia

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_COREMEDIA_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_COREMEDIA_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_COREMEDIA_TVOS}
)

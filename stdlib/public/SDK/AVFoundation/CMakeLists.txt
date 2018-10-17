cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

swift_add_target_library(swiftAVFoundation ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  AVCaptureDevice.swift
  AVCapturePhotoOutput.swift
  AVCaptureSynchronizedDataCollection.swift
  AVCaptureVideoDataOutput.swift
  AVError.swift
  AVMetadataObject.swift
  NSValue.swift.gyb

  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SWIFT_MODULE_DEPENDS_OSX Darwin AppKit CoreAudio CoreData CoreGraphics CoreImage CoreMedia Dispatch Foundation IOKit ObjectiveC QuartzCore simd XPC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreAudio CoreGraphics CoreMedia Dispatch Foundation ObjectiveC QuartzCore simd # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreAudio CoreGraphics CoreMedia Dispatch Foundation ObjectiveC QuartzCore simd # auto-updated
  FRAMEWORK_DEPENDS AVFoundation

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_AVFOUNDATION_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_AVFOUNDATION_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_AVFOUNDATION_TVOS}
)

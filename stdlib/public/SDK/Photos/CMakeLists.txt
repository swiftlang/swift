cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_library(swiftPhotos ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  PHChange.swift
  PHProjectChangeRequest.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR OSX
  SWIFT_MODULE_DEPENDS_IOS Darwin AVFoundation CoreAudio CoreFoundation CoreGraphics CoreData CoreImage CoreLocation CoreMedia Dispatch Foundation ObjectiveC QuartzCore simd UIKit os # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin AVFoundation CoreAudio CoreFoundation CoreGraphics CoreData CoreImage CoreLocation CoreMedia Dispatch Foundation ObjectiveC QuartzCore simd UIKit # auto-updated
  SWIFT_MODULE_DEPENDS_OSX Darwin AppKit AVFoundation CoreAudio CoreFoundation CoreData CoreGraphics CoreImage CoreLocation CoreMedia Dispatch Foundation ObjectiveC QuartzCore # auto-updated
  
  FRAMEWORK_DEPENDS Photos

  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_PHOTOS_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_PHOTOS_TVOS}
  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_PHOTOS_OSX}
)

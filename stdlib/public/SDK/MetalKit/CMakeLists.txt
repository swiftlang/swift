cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_library(swiftMetalKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
    MetalKit.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS_OSX Darwin AppKit CoreData CoreFoundation CoreGraphics Dispatch Foundation IOKit Metal ModelIO ObjectiveC QuartzCore simd XPC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreFoundation CoreGraphics Dispatch Foundation Metal ModelIO ObjectiveC QuartzCore simd UIKit # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreFoundation CoreGraphics Dispatch Foundation Metal ModelIO ObjectiveC QuartzCore simd UIKit # auto-updated
  
  FRAMEWORK_DEPENDS_WEAK MetalKit
  FRAMEWORK_DEPENDS_WEAK Metal

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_METALKIT_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_METALKIT_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_METALKIT_TVOS}
)

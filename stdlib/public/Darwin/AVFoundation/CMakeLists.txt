cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

foreach(target_sdk ${SWIFT_SDKS})

select_swift_overlay_dependenies(module_depends
  SDK ${target_sdk}
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreImage CoreGraphics Metal Dispatch IOKit simd Foundation CoreMedia QuartzCore XPC CoreFoundation CoreAudio ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreGraphics Metal Dispatch simd Foundation CoreMedia QuartzCore CoreFoundation CoreAudio ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreGraphics Metal Dispatch simd Foundation CoreMedia QuartzCore CoreFoundation CoreAudio ObjectiveC # auto-updated
)

if(${target_sdk} STREQUAL OSX
  OR ${target_sdk} STREQUAL IOS OR ${target_sdk} STREQUAL IOS_SIMULATOR
  OR ${target_sdk} STREQUAL TVOS OR ${target_sdk} STREQUAL TVOS_SIMULATOR)
else()
  continue()
endif()

add_swift_target_library_single_sdk(swiftAVFoundation ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  AVCaptureDevice.swift
  AVCapturePhotoOutput.swift
  AVCaptureSynchronizedDataCollection.swift
  AVCaptureVideoDataOutput.swift
  AVError.swift
  AVMetadataObject.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  GYB_SOURCES
    NSValue.swift.gyb

  SDK ${target_sdk}
  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}" "-L${sdk}/usr/lib/swift/"
  SWIFT_MODULE_DEPENDS ${module_depends}
  FRAMEWORK_DEPENDS AVFoundation

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_AVFOUNDATION_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_AVFOUNDATION_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_AVFOUNDATION_TVOS}
  INSTALL_IN_COMPONENT sdk-overlay
)

endforeach()

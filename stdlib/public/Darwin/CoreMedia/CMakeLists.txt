cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

foreach(sdk ${SWIFT_SDKS})

if(${sdk} STREQUAL OSX
  OR ${sdk} STREQUAL IOS OR ${sdk} STREQUAL IOS_SIMULATOR
  OR ${sdk} STREQUAL TVOS OR ${sdk} STREQUAL TVOS_SIMULATOR)
else()
  continue()
endif()

select_swift_overlay_dependenies(module_depends
  SDK ${sdk}
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreGraphics Metal Dispatch IOKit Foundation XPC CoreFoundation CoreAudio ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreGraphics Metal Dispatch Foundation CoreFoundation CoreAudio ObjectiveC # auto-updated
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreGraphics Metal Dispatch Foundation CoreFoundation CoreAudio ObjectiveC # auto-updated
)

add_swift_target_library_single_sdk(swiftCoreMedia ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  CMTime.swift
  CMTimeRange.swift

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SDK ${sdk}
  SWIFT_MODULE_DEPENDS ${module_depends}
  FRAMEWORK_DEPENDS CoreMedia

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_COREMEDIA_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_COREMEDIA_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_COREMEDIA_TVOS}
  INSTALL_IN_COMPONENT sdk-overlay
)

endforeach()

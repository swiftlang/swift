cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

foreach(sdk ${SWIFT_SDKS})

if(NOT (${sdk} STREQUAL IOS OR ${sdk} STREQUAL IOS_SIMULATOR))
  continue()
endif()

set(
  SWIFT_MODULE_DEPENDS_IOS Darwin CoreGraphics Dispatch Foundation CoreFoundation ObjectiveC # auto-updated
)

add_swift_target_library_single_sdk(swiftAssetsLibrary ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  ALAssetsLibrary.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SDK ${sdk}
  SWIFT_MODULE_DEPENDS ${SWIFT_MODULE_DEPENDS_IOS}
  FRAMEWORK_DEPENDS AssetsLibrary

  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_ASSETSLIBRARY_IOS}
  INSTALL_IN_COMPONENT sdk-overlay
)

endforeach()

cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")
if(NOT OSX IN_LIST SWIFT_SDKS)
  return()
endif()

set(
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreImage CoreGraphics Metal Dispatch IOKit Foundation CoreData QuartzCore XPC CoreFoundation ObjectiveC # auto-updated
)

add_swift_target_library_single_sdk(swiftAppKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  AppKit.swift
  AppKit_FoundationExtensions.swift
  NSError.swift
  NSEvent.swift
  NSGraphics.swift
  NSOpenGL.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SDK OSX
  SWIFT_MODULE_DEPENDS ${SWIFT_MODULE_DEPENDS_OSX}
  FRAMEWORK_DEPENDS AppKit

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_APPKIT_OSX}
  INSTALL_IN_COMPONENT sdk-overlay
)

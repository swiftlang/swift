cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

if(NOT ${sdk} STREQUAL OSX)
  continue()
endif()

set(
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreGraphics Dispatch IOKit Foundation XPC CoreFoundation ObjectiveC # auto-updated
)

add_swift_target_library_single_sdk(swiftCryptoTokenKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  TKSmartCard.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SDK OSX
  SWIFT_MODULE_DEPENDS ${SWIFT_MODULE_DEPENDS_OSX}
  FRAMEWORK_DEPENDS CryptoTokenKit

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_CRYPTOTOKENKIT_OSX}
  INSTALL_IN_COMPONENT sdk-overlay
)

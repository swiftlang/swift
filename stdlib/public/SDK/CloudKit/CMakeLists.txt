cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_library(swiftCloudKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  CKError.swift

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS ALL_APPLE_PLATFORMS

  # The dependency on the 'os' module only appears in particular Apple-internal
  # configurations, but it causes no harm to specify it unconditionally.
  # The ./utils/find-overlay-dependencies.sh tool only touches the
  # OSX|IOS|TVOS|WATCHOS lines, so the standalone "os" lines remain.
  SWIFT_MODULE_DEPENDS_OSX Darwin Contacts CoreGraphics CoreLocation Dispatch Foundation IOKit ObjectiveC # auto-updated
    os
  SWIFT_MODULE_DEPENDS_IOS Darwin Contacts CoreLocation Dispatch Foundation ObjectiveC # auto-updated
    os
  SWIFT_MODULE_DEPENDS_TVOS Darwin CoreLocation Dispatch Foundation ObjectiveC # auto-updated
    os
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin CoreLocation Dispatch Foundation ObjectiveC # auto-updated
    os
  FRAMEWORK_DEPENDS_WEAK CloudKit

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_CLOUDKIT_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_CLOUDKIT_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_CLOUDKIT_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_CLOUDKIT_WATCHOS}
)

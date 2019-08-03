cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

set(DISABLE_APPLICATION_EXTENSION ON)

add_swift_target_library(swiftXCTest ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  XCTestCaseAdditions.mm
  XCTest.swift

  SWIFT_COMPILE_FLAGS ${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}

    # XCTest has a type called XCTest, which exposes an issue in module
    # interfaces -- because types are fully-qualified, the compiler currently
    # doesn't disambiguate between XCTest-the-module and XCTest-the-class.
    # rdar://48445154
    -Xfrontend -module-interface-preserve-types-as-written
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  TARGET_SDKS OSX IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS ObjectiveC Foundation
  FRAMEWORK_DEPENDS Foundation XCTest
  SWIFT_MODULE_DEPENDS_OSX AppKit
  SWIFT_MODULE_DEPENDS_IOS CoreMedia UIKit
  SWIFT_MODULE_DEPENDS_TVOS UIKit
  DONT_EMBED_BITCODE

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_XCTEST_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_XCTEST_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_XCTEST_TVOS}
  INSTALL_IN_COMPONENT sdk-overlay
)

cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_library(swiftSceneKit
                    ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES}
                    IS_SDK_OVERLAY

                    SceneKit.swift.gyb

                    SWIFT_COMPILE_FLAGS
                      "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}" "-swift-version" "3"
                    LINK_FLAGS
                      "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

                    TARGET_SDKS
                      OSX
                      IOS
                      IOS_SIMULATOR
                      TVOS
                      TVOS_SIMULATOR
                      WATCHOS
                      WATCHOS_SIMULATOR

                    SWIFT_MODULE_DEPENDS_DARWIN
                      Darwin CoreGraphics Dispatch Foundation ObjectiveC simd # auto-updated
                    SWIFT_MODULE_DEPENDS_OSX
                      AppKit CoreData CoreImage GLKit IOKit XPC # auto-updated
                    SWIFT_MODULE_DEPENDS_IOS
                      CoreAudio CoreImage CoreMedia GLKit os UIKit # auto-updated
                    SWIFT_MODULE_DEPENDS_TVOS
                      CoreImage UIKit # auto-updated
                    SWIFT_MODULE_DEPENDS_WATCHOS
                      UIKit # required in some configurations but not found by tool

                    FRAMEWORK_DEPENDS_WEAK
                      SceneKit

                    DEPLOYMENT_VERSION_OSX
                      ${SWIFTLIB_DEPLOYMENT_VERSION_SCENEKIT_OSX}
                    DEPLOYMENT_VERSION_IOS
                      ${SWIFTLIB_DEPLOYMENT_VERSION_SCENEKIT_IOS}
                    DEPLOYMENT_VERSION_TVOS
                      ${SWIFTLIB_DEPLOYMENT_VERSION_SCENEKIT_TVOS}
                    DEPLOYMENT_VERSION_WATCHOS
                      ${SWIFTLIB_DEPLOYMENT_VERSION_SCENEKIT_WATCHOS})

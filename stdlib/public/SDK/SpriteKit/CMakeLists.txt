cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_library(swiftSpriteKit
                    ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES}
                    IS_SDK_OVERLAY

                    SpriteKit.swift
                    SpriteKitQuickLooks.swift.gyb

                    SWIFT_COMPILE_FLAGS
                      "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
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
                      AppKit CoreData CoreImage QuartzCore XPC # auto-updated
                    SWIFT_MODULE_DEPENDS_IOS
                      CoreImage GLKit os QuartzCore UIKit # auto-updated
                    SWIFT_MODULE_DEPENDS_TVOS
                      CoreImage GLKit QuartzCore UIKit # auto-updated
                    SWIFT_MODULE_DEPENDS_WATCHOS
                      UIKit # auto-updated

                    FRAMEWORK_DEPENDS
                      SpriteKit

                    DEPLOYMENT_VERSION_OSX
                      ${SWIFTLIB_DEPLOYMENT_VERSION_SPRITEKIT_OSX}
                    DEPLOYMENT_VERSION_IOS
                      ${SWIFTLIB_DEPLOYMENT_VERSION_SPRITEKIT_IOS}
                    DEPLOYMENT_VERSION_TVOS
                      ${SWIFTLIB_DEPLOYMENT_VERSION_SPRITEKIT_TVOS}
                    DEPLOYMENT_VERSION_WATCHOS
                      ${SWIFTLIB_DEPLOYMENT_VERSION_SPRITEKIT_WATCHOS})

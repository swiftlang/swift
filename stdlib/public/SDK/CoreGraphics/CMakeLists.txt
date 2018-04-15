cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_library(swiftCoreGraphics
                    ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES}
                    IS_SDK_OVERLAY

                    CoreGraphics.swift
                    CGFloat.swift.gyb
                    Private.swift

                    SWIFT_COMPILE_FLAGS
                      "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}" "-swift-version" "3"
                    LINK_FLAGS
                      "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

                    SWIFT_MODULE_DEPENDS_DARWIN
                      Darwin CoreFoundation Dispatch ObjectiveC # auto-updated
                    SWIFT_MODULE_DEPENDS_OSX
                      IOKit # auto-updated

                    FRAMEWORK_DEPENDS
                      CoreGraphics

                    DEPLOYMENT_VERSION_OSX
                      ${SWIFTLIB_DEPLOYMENT_VERSION_COREGRAPHICS_OSX}
                    DEPLOYMENT_VERSION_IOS
                      ${SWIFTLIB_DEPLOYMENT_VERSION_COREGRAPHICS_IOS}
                    DEPLOYMENT_VERSION_TVOS
                      ${SWIFTLIB_DEPLOYMENT_VERSION_COREGRAPHICS_TVOS}
                    DEPLOYMENT_VERSION_WATCHOS
                      ${SWIFTLIB_DEPLOYMENT_VERSION_COREGRAPHICS_WATCHOS})

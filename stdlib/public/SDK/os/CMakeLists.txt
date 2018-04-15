cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_library(swiftos
                    ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES}
                    IS_SDK_OVERLAY

                    os_log.swift
                    os_log.m
                    os_trace_blob.c
                    thunks.mm

                    SWIFT_COMPILE_FLAGS
                      "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
                    LINK_FLAGS
                      "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"

                    SWIFT_MODULE_DEPENDS_DARWIN
                      Darwin Dispatch ObjectiveC # auto-updated
                    SWIFT_MODULE_DEPENDS_OSX
                      XPC # auto-updated

                    DEPLOYMENT_VERSION_OSX
                      ${SWIFTLIB_DEPLOYMENT_VERSION_OS_OSX}
                    DEPLOYMENT_VERSION_IOS
                      ${SWIFTLIB_DEPLOYMENT_VERSION_OS_IOS}
                    DEPLOYMENT_VERSION_TVOS
                      ${SWIFTLIB_DEPLOYMENT_VERSION_OS_TVOS}
                    DEPLOYMENT_VERSION_WATCHOS
                      ${SWIFTLIB_DEPLOYMENT_VERSION_OS_WATCHOS})

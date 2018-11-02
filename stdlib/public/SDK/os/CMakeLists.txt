cmake_minimum_required(VERSION 3.4.3)
include("../../../../cmake/modules/StandaloneOverlay.cmake")

add_swift_library(swiftos ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  format.m
  os.m
  os_log.swift
  os_signpost.swift
  os_trace_blob.c
  thunks.mm

  SWIFT_COMPILE_FLAGS "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  SWIFT_MODULE_DEPENDS_OSX Darwin Dispatch ObjectiveC XPC # auto-updated
  SWIFT_MODULE_DEPENDS_IOS Darwin Dispatch ObjectiveC # auto-updated
    Dispatch # required in some configurations but not found by tool
  SWIFT_MODULE_DEPENDS_TVOS Darwin Dispatch ObjectiveC # auto-updated
    Dispatch # required in some configurations but not found by tool
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin Dispatch ObjectiveC # auto-updated
    Dispatch # required in some configurations but not found by tool

  DEPLOYMENT_VERSION_OSX ${SWIFTLIB_DEPLOYMENT_VERSION_OS_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFTLIB_DEPLOYMENT_VERSION_OS_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFTLIB_DEPLOYMENT_VERSION_OS_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFTLIB_DEPLOYMENT_VERSION_OS_WATCHOS}
)

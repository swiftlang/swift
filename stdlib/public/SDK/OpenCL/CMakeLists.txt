add_swift_library(swiftOpenCL ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  OpenCL.swift
  TARGET_SDKS OSX
  SWIFT_MODULE_DEPENDS_OSX Darwin Dispatch ObjectiveC # auto-updated
  FRAMEWORK_DEPENDS OpenCL
)

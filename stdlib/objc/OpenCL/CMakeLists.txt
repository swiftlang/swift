add_swift_library(swiftOpenCL IS_SDK_OVERLAY
  OpenCL.swift
  TARGET_SDKS OSX
  SWIFT_MODULE_DEPENDS Dispatch Darwin # also, IOSurface
  FRAMEWORK_DEPENDS OpenCL)


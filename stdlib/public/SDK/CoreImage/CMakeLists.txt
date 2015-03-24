add_swift_library(swiftCoreImage IS_SDK_OVERLAY
  CoreImage.swift
  TARGET_SDKS IOS IOS_SIMULATOR TVOS TVOS_SIMULATOR
  SWIFT_MODULE_DEPENDS Foundation ObjectiveC
  FRAMEWORK_DEPENDS CoreImage)


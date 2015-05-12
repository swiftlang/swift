add_swift_library(swiftCoreImage IS_SDK_OVERLAY
  CoreImage.swift
  SWIFT_MODULE_DEPENDS Foundation ObjectiveC
  FRAMEWORK_DEPENDS_OSX QuartzCore
  FRAMEWORK_DEPENDS_IOS_TVOS CoreImage)


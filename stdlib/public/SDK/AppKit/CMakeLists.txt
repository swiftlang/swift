add_swift_library(swiftAppKit IS_SDK_OVERLAY
  AppKit.swift
  AppKit_FoundationExtensions.swift
  NSError.swift
  TARGET_SDKS OSX
  SWIFT_MODULE_DEPENDS ObjectiveC Foundation CoreData CoreImage
  FRAMEWORK_DEPENDS AppKit)


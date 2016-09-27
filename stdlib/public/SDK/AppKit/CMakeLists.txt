add_swift_library(swiftAppKit ${SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES} IS_SDK_OVERLAY
  AppKit.swift
  AppKit_FoundationExtensions.swift
  NSError.swift
  TARGET_SDKS OSX
  SWIFT_MODULE_DEPENDS_OSX Darwin CoreData CoreGraphics CoreImage Dispatch Foundation IOKit ObjectiveC QuartzCore XPC # auto-updated
  FRAMEWORK_DEPENDS AppKit
)

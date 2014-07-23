set(SHARED_LIBRARY ON)
add_swift_library(swiftAppKit INSTALL TARGET_LIBRARY
  AppKit.swift
  TARGET_SDKS "OSX"
  DEPENDS swiftCore swiftObjectiveC swiftFoundation swiftQuartzCore
  FRAMEWORK_DEPENDS AppKit QuartzCore)

set(SHARED_LIBRARY ON)
add_swift_library(swiftAppKit INSTALL TARGET_LIBRARY
  AppKit.swift
  TARGET_SDKS "OSX"
  DEPENDS swift_stdlib_core swiftObjectiveC swiftFoundation swiftQuartzCore
  FRAMEWORK_DEPENDS AppKit QuartzCore)


add_swift_optimization_flags(swiftAppKit)

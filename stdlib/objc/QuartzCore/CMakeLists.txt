set(SHARED_LIBRARY ON)
add_swift_library(swiftQuartzCore INSTALL TARGET_LIBRARY
  QuartzCore.swift
  TARGET_SDKS OSX
  DEPENDS swift_stdlib_core swiftFoundation swiftObjectiveC
  FRAMEWORK_DEPENDS QuartzCore)

add_swift_optimization_flags(swiftQuartzCore)

set(SHARED_LIBRARY ON)
add_swift_library(swiftQuartzCore INSTALL TARGET_LIBRARY
  QuartzCore.swift
  TARGET_SDKS OSX
  DEPENDS swiftCore swiftFoundation swiftObjectiveC
  FRAMEWORK_DEPENDS QuartzCore)

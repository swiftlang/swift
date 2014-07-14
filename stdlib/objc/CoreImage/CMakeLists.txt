set(SHARED_LIBRARY ON)
add_swift_library(swiftCoreImage INSTALL TARGET_LIBRARY
  CoreImage.swift
  TARGET_SDKS IOS IOS_SIMULATOR
  DEPENDS swiftCore swiftFoundation swiftObjectiveC
  FRAMEWORK_DEPENDS CoreImage)

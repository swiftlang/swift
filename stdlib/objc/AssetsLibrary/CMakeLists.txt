set(SHARED_LIBRARY ON)

add_swift_library(swiftAssetsLibrary INSTALL TARGET_LIBRARY
  AssetsLibrary.swift
  TARGET_SDKS IOS IOS_SIMULATOR
  DEPENDS swiftCore swiftObjectiveC swiftFoundation swiftCoreGraphics
  FRAMEWORK_DEPENDS AssetsLibrary)


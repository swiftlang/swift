set(SHARED_LIBRARY ON)
add_swift_library(swiftUIKit INSTALL TARGET_LIBRARY
  DesignatedInitializers.mm
  UIKit.swift
  TARGET_SDKS IOS IOS_SIMULATOR
  DEPENDS swiftCore swiftObjectiveC swiftFoundation swiftCoreImage
  FRAMEWORK_DEPENDS UIKit)

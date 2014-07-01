set(SHARED_LIBRARY ON)
add_swift_library(swiftUIKit INSTALL TARGET_LIBRARY
  DesignatedInitializers.mm
  UIViewControllerAdditions.mm
  UIKit.swift
  TARGET_SDKS IOS IOS_SIMULATOR
  DEPENDS swift_stdlib_core swiftObjectiveC swiftFoundation
  FRAMEWORK_DEPENDS UIKit)

add_swift_optimization_flags(swiftUIKit)

set(SHARED_LIBRARY ON)

add_swift_library(swiftCoreGraphics 
  CoreGraphics.swift
  INSTALL TARGET_LIBRARY
  DEPENDS swift_stdlib_core swiftObjectiveC swiftDispatch
  FRAMEWORK_DEPENDS CoreGraphics)

add_swift_optimization_flags(swiftCoreGraphics)

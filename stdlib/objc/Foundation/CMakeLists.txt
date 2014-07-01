set(SHARED_LIBRARY ON)

add_swift_library(swiftFoundation INSTALL TARGET_LIBRARY
  Foundation.swift
  NSStringAPI.swift
  Misc.mm
  DEPENDS swift_stdlib_core swiftObjectiveC swiftCoreGraphics swiftDispatch
  FRAMEWORK_DEPENDS Foundation)

add_swift_optimization_flags(swiftFoundation)

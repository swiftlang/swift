set(SHARED_LIBRARY ON)

add_swift_library(swiftFoundation INSTALL TARGET_LIBRARY
  Foundation.swift
  ExtraStringAPIs.swift
  Misc.mm
  NSStringAPI.swift
  DEPENDS swift_stdlib_core swiftObjectiveC swiftCoreGraphics swiftDispatch
  FRAMEWORK_DEPENDS Foundation)

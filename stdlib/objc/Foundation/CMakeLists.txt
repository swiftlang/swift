set(SHARED_LIBRARY ON)

add_swift_library(swiftFoundation INSTALL TARGET_LIBRARY
  Foundation.swift
  FoundationMirrors.swift.gyb
  ExtraStringAPIs.swift
  Misc.mm
  MixedStringNSStringOperations.swift.gyb
  NSStringAPI.swift
  NSValue.swift
  DEPENDS swiftCore swiftObjectiveC swiftCoreGraphics swiftDispatch
  FRAMEWORK_DEPENDS Foundation)

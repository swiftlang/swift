set(SHARED_LIBRARY ON)

add_swift_library(swiftFoundation INSTALL TARGET_LIBRARY
  Foundation.swift
  FoundationMirrors.swift.gyb
  ExtraStringAPIs.swift
  Misc.mm
  NSStringAPI.swift
  NSValue.swift.gyb
  DEPENDS swiftCore swiftObjectiveC swiftCoreGraphics swiftDispatch
  FRAMEWORK_DEPENDS Foundation
  # FIXME: NotificationCenter arbitrarily attached here; there is no overlay.
  API_NOTES Foundation NotificationCenter)

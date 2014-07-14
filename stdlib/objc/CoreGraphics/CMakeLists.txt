set(SHARED_LIBRARY ON)

add_swift_library(swiftCoreGraphics 
  CoreGraphics.swift
  CGFloat.swift
  CoreGraphicsMirrors.swift.gyb
  INSTALL TARGET_LIBRARY
  DEPENDS swiftCore swiftObjectiveC swiftDispatch
  FRAMEWORK_DEPENDS CoreGraphics)

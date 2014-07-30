set(SHARED_LIBRARY ON)

add_swift_library(swiftCoreGraphics 
  CoreGraphics.swift
  CGFloat.swift.gyb
  CoreGraphicsMirrors.swift.gyb
  INSTALL TARGET_LIBRARY
  DEPENDS swiftCore swiftObjectiveC swiftDispatch swiftDarwin
  FRAMEWORK_DEPENDS CoreGraphics)

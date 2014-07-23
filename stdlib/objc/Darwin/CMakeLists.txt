set(SHARED_LIBRARY ON)

add_swift_library(swiftDarwin INSTALL TARGET_LIBRARY
  Darwin.swift
  Misc.mm
  tgmath.swift.gyb
  DEPENDS swiftCore
  API_NOTES_NON_OVERLAY)

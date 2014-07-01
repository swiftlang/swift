set(SHARED_LIBRARY ON)

add_swift_library(swiftDarwin INSTALL TARGET_LIBRARY
  Darwin.swift
  Misc.mm
  tgmath.swift.gyb
  DEPENDS swift_stdlib_core)

add_swift_optimization_flags(swiftDarwin)

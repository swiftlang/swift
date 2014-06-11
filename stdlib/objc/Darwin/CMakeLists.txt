set(SHARED_LIBRARY ON)
set(SWIFTFOUNDATION_SOURCES
    Darwin.swift
    Misc.mm
    tgmath.swift.gyb
)

add_swift_library(swiftDarwin INSTALL
  ${SWIFTFOUNDATION_SOURCES}
  DEPENDS swift_stdlib_core)

add_swift_optimization_flags(swiftDarwin)

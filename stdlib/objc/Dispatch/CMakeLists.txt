set(SHARED_LIBRARY ON)
set(SWIFTFOUNDATION_SOURCES
    Dispatch.swift
)

add_swift_library(swiftDispatch INSTALL
  ${SWIFTFOUNDATION_SOURCES}
  DEPENDS swift_stdlib_core)

add_swift_optimization_flags(swiftDispatch)

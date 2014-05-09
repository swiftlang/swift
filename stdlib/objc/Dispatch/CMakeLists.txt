set(SHARED_LIBRARY ON)
set(SWIFTFOUNDATION_SOURCES
    Dispatch.swift
    Dispatch.mm
)

add_swift_library(swiftDispatch INSTALL
  ${SWIFTFOUNDATION_SOURCES}
  DEPENDS swift_stdlib_core swiftObjectiveC)

add_swift_optimization_flags(swiftDispatch)

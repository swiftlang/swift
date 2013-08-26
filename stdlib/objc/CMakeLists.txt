# Build the swiftObjC dylib.
set(SHARED_LIBRARY ON)
add_swift_library(swiftObjectiveC INSTALL
  ObjectiveC.swift
  BridgeObjectiveC.mm
  DEPENDS swift_stdlib_core)

add_subdirectory(Foundation)
add_subdirectory(AppKit)

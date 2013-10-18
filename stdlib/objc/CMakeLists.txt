# Build the swiftObjC dylib.
set(SHARED_LIBRARY ON)
add_swift_library(swiftObjectiveC INSTALL
  ObjectiveC.swift
  BridgeObjectiveC.mm
  DEPENDS swift_stdlib_core)

add_subdirectory(Foundation)

if (SWIFT_DEPLOYMENT_OS MATCHES "^macosx")
  add_subdirectory(AppKit)
else()
  add_subdirectory(UIKit)
endif()

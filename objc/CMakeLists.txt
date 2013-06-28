# Build the swiftObjC dylib.
set(SHARED_LIBRARY ON)
add_swift_library(swiftObjectiveC INSTALL
  ObjectiveC.swift
  DEPENDS swift_stdlib_core)

# Link against the system dylib.
target_link_libraries(swiftObjectiveC
  "-lobjc")

add_subdirectory(Foundation)
add_subdirectory(AppKit)

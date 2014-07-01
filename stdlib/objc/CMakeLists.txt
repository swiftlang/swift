# All libraries in this directory tree are overlays.
list(APPEND SWIFT_EXTRA_FLAGS
  "-autolink-force-load")

# Build the swiftObjC dylib.
set(SHARED_LIBRARY ON)

add_swift_library(swiftObjectiveC INSTALL TARGET_LIBRARY
  ObjectiveC.swift
  DEPENDS swift_stdlib_core swiftDarwin)


add_subdirectory(Darwin)
add_subdirectory(Dispatch)
add_subdirectory(CoreGraphics)
add_subdirectory(Foundation)
add_subdirectory(SpriteKit)

if (SWIFT_SDKS)
  add_subdirectory(AppKit)
  add_subdirectory(UIKit)
else()
  if (SWIFT_DEPLOYMENT_OS MATCHES "^macosx")
    add_subdirectory(AppKit)
    add_subdirectory(QuartzCore)
  else()
    add_subdirectory(CoreImage)
    add_subdirectory(UIKit)
  endif()
endif()

# FIXME: Temporarily allow one to disable building this overlay module.
option(SWIFT_BUILD_XCTEST "Build XCTest overlay" YES)
if (SWIFT_BUILD_XCTEST)
  add_subdirectory(XCTest)
endif()

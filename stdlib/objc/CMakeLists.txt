# Build the swiftObjC dylib.
set(SHARED_LIBRARY ON)

set(SWIFTOBJC_SOURCES
    ObjectiveC.swift
    BridgeObjectiveC.mm)

# Definition of ObjCBool is platform-specific.
if (SWIFT_DEPLOYMENT_OS MATCHES "^macosx")
  list(APPEND SWIFTOBJC_SOURCES 
    ObjectiveC-macosx.swift)
elseif (SWIFT_DEPLOYMENT_OS MATCHES "^iphone")
  list(APPEND SWIFTOBJC_SOURCES 
    ObjectiveC-iphoneos.swift)
endif()

add_swift_library(swiftObjectiveC INSTALL
  ${SWIFTOBJC_SOURCES}
  DEPENDS swift_stdlib_core)

add_subdirectory(Foundation)

if (SWIFT_DEPLOYMENT_OS MATCHES "^macosx")
  add_subdirectory(AppKit)
else()
  add_subdirectory(UIKit)
endif()

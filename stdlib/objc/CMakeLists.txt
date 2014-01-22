# Build the swiftObjC dylib.
set(SHARED_LIBRARY ON)

set(SWIFTOBJC_SOURCES
    ObjectiveC.swift
    BridgeObjectiveC.mm)

# Definition of ObjCBool is platform-specific.
if (SWIFT_DEPLOYMENT_OS MATCHES "^macosx" 
    OR
    (SWIFT_DEPLOYMENT_OS MATCHES "^iphone" 
     AND NOT (CMAKE_OSX_ARCHITECTURES MATCHES "64$")))
  # OS X or 32-bit iOS: Objective-C BOOL is signed char
  list(APPEND SWIFTOBJC_SOURCES 
    ObjectiveC-BOOLchar.swift)
else()
  # Others: Objective-C BOOL is C99/C++ bool.
  list(APPEND SWIFTOBJC_SOURCES 
    ObjectiveC-BOOLbool.swift)
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

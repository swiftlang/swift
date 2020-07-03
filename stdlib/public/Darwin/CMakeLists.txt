# All libraries in this directory tree are overlays that depend on Darwin SDK.

set(SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES)
if(SWIFT_BUILD_DYNAMIC_SDK_OVERLAY)
  list(APPEND SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES SHARED)
endif()
if(SWIFT_BUILD_STATIC_SDK_OVERLAY)
  list_intersect("${SWIFT_APPLE_PLATFORMS}" "${SWIFT_SDKS}" building_darwin_sdks)
  if(building_darwin_sdks)
    message(SEND_ERROR "cannot build static standard library for Darwin SDKs")
  else()
    list(APPEND SWIFT_SDK_OVERLAY_LIBRARY_BUILD_TYPES STATIC)
  endif()
endif()

set(all_overlays
  CoreFoundation
  CoreGraphics
  Dispatch
  Foundation
  ObjectiveC
  XCTest
)

if(DEFINED SWIFT_OVERLAY_TARGETS)
  set(overlays_to_build ${SWIFT_OVERLAY_TARGETS})
else()
  set(overlays_to_build ${all_overlays})
endif()

message(STATUS "Building overlays: ${overlays_to_build}")
foreach(overlay ${overlays_to_build})
  message(STATUS "INCLUDING OVERLAY: ${overlay}")
  add_subdirectory(${overlay})
endforeach()

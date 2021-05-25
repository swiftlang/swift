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

if(DEFINED SWIFT_OVERLAY_TARGETS)
  list_subtract("${all_overlays}" "${SWIFT_OVERLAY_TARGETS}" OVERLAYS_FROM_SDKS)
  if(OVERLAYS_FROM_SDKS)
    include(SimulateOverlayDeps)
    message(STATUS "Cleaning up stale overlays in build folder: ${OVERLAYS_FROM_SDKS}")
    foreach(overlay IN LISTS OVERLAYS_FROM_SDKS)
      remove_overlay_from_build(${overlay} TARGET_SDKS ${SWIFT_SDKS})
    endforeach()
    
  endif()
  list_intersect("${OVERLAYS_FROM_SDKS}" "Foundation;ObjectiveC" OVERLAYS_TO_FAKE)
  if(OVERLAYS_TO_FAKE)
    include(SimulateOverlayDeps)
    message(STATUS "Overlays to fake: ${OVERLAYS_TO_FAKE}")
    foreach(overlay IN LISTS OVERLAYS_TO_FAKE)
      add_overlay_targets(${overlay} TARGET_SDKS ${SWIFT_SDKS})
    endforeach()
  endif()
endif()

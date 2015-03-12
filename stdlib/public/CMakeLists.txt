if(SWIFT_BUILD_STDLIB)
  # These must be kept in dependency order so that any referenced targets
  # exist at the time we look for them in add_swift_*.
  add_subdirectory(SwiftShims)
  add_subdirectory(runtime)
  add_subdirectory(core)
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  if(SWIFT_BUILD_SDK_OVERLAY)
    add_subdirectory(SDK)
  endif()
endif()


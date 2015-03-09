if(SWIFT_BUILD_STDLIB)
  add_subdirectory(SwiftShims)
  add_subdirectory(core)
  add_subdirectory(runtime)
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  if(SWIFT_BUILD_SDK_OVERLAY)
    add_subdirectory(SDK)
  endif()
endif()


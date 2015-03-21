if(SWIFT_BUILD_STDLIB)
  add_subdirectory(SwiftPrivate)
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  if(SWIFT_BUILD_SDK_OVERLAY)
    # FIXME: there is nothing Darwin-specific in StdlibUnittest, but to use
    # POSIX APIs it imports the Darwin module on Apple platforms, so it can't
    # be built separately from the SDK overlay.
    add_subdirectory(StdlibUnittest)
    add_subdirectory(StdlibUnittestFoundationExtras)
    add_subdirectory(SwiftPrivateDarwinExtras)
    add_subdirectory(SwiftPrivatePthreadExtras)
    add_subdirectory(SwiftPrivateSerialization)
  endif()
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  add_subdirectory(Glibc)
  add_subdirectory(StdlibUnittest)
  add_subdirectory(SwiftPrivateDarwinExtras)
  add_subdirectory(SwiftPrivatePthreadExtras)
  add_subdirectory(SwiftPrivateSerialization)
endif()

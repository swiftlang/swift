if(SWIFT_BUILD_STDLIB OR SWIFT_BUILD_SDK_OVERLAY)
  add_subdirectory(SwiftPrivate)
endif()

if(SWIFT_BUILD_SDK_OVERLAY)
  add_subdirectory(StdlibUnittest)
  add_subdirectory(StdlibUnicodeUnittest)
  add_subdirectory(StdlibCollectionUnittest)
  add_subdirectory(SwiftPrivateLibcExtras)
  # SwiftPrivateThreadExtras makes use of Darwin/glibc/MSVCRT, which is part of the
  # SDK overlay. It can't be built separately from the SDK overlay.
  add_subdirectory(SwiftPrivateThreadExtras)

  if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    add_subdirectory(StdlibUnittestFoundationExtras)
    if (SWIFT_INCLUDE_TESTS)
      add_subdirectory(SwiftReflectionTest)
    endif()
  endif()
endif()

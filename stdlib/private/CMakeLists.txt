if(SWIFT_BUILD_STDLIB OR SWIFT_BUILD_SDK_OVERLAY)
  add_subdirectory(SwiftPrivate)
endif()

if(SWIFT_BUILD_SDK_OVERLAY)
  # SwiftPrivateThreadExtras makes use of Darwin/Glibc, which is part of the
  # SDK overlay. It can't be built separately from the SDK overlay.
  if(SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING)
    add_subdirectory(DifferentiationUnittest)
  endif()
  add_subdirectory(RuntimeUnittest)
  add_subdirectory(StdlibUnicodeUnittest)
  add_subdirectory(StdlibCollectionUnittest)
  add_subdirectory(SwiftPrivateLibcExtras)
  add_subdirectory(SwiftPrivateThreadExtras)

  # NOTE(compnerd) this must come after SwiftPrivateLibcExtras and
  # SwiftPrivateThreadExtras to ensure that the dependency targets are setup in
  # the correct order for Windows.
  add_subdirectory(StdlibUnittest)

  add_subdirectory(OSLog)

  if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    add_subdirectory(StdlibUnittestFoundationExtras)
  endif()
  # Currently SwiftReflectionTest cannot be built on Windows, due to 
  # dependencies on POSIX symbols 
  if (SWIFT_INCLUDE_TESTS AND (NOT CMAKE_SYSTEM_NAME STREQUAL "Windows"))
    add_subdirectory(SwiftReflectionTest)
  endif()
endif()

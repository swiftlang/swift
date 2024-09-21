if(SWIFT_BUILD_SDK_OVERLAY
   OR (SWIFT_BUILD_TEST_SUPPORT_MODULES
       AND NOT SWIFT_BUILD_DYNAMIC_SDK_OVERLAY_default
       AND (SWIFT_ENABLE_REFLECTION
            OR NOT SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)))
  # SwiftPrivateThreadExtras makes use of Darwin/Glibc, which is part of the
  # SDK overlay. If the SDK overlay doesn't build by default, then it should
  # be available in the SDK and DifferentiationUnittest can still be built.
  # However, the overlay in the Apple SDKs requires the standard library to
  # have reflection enabled.
  if(SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING)
    add_subdirectory(DifferentiationUnittest)
  endif()
  add_subdirectory(SwiftPrivate)
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

# Keep in sync with stdlib/tools/CMakeLists.txt: swift-reflection-test is
# only used when testing dynamic stdlib.
if(SWIFT_BUILD_DYNAMIC_STDLIB AND SWIFT_INCLUDE_TESTS)
  # NOTE create a stub BlocksRuntime library that can be used for the
  # reflection tests
  add_subdirectory(BlocksRuntimeStubs)
endif()


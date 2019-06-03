if(SWIFT_BUILD_STDLIB OR SWIFT_BUILD_SDK_OVERLAY)
  add_subdirectory(SwiftPrivate)
endif()

if(SWIFT_BUILD_SDK_OVERLAY)
  # SwiftPrivateThreadExtras makes use of Darwin/Glibc, which is part of the
  # SDK overlay. It can't be built separately from the SDK overlay.
  add_subdirectory(DifferentiationUnittest)
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

  # SWIFT_ENABLE_TENSORFLOW
  # Only build unittest if we are also building swift apis.
  if(SWIFT_ENABLE_TENSORFLOW AND TENSORFLOW_SWIFT_APIS)
    add_subdirectory(TensorFlowUnittest)
  endif()

  if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    add_subdirectory(StdlibUnittestFoundationExtras)
    if (SWIFT_INCLUDE_TESTS)
      add_subdirectory(SwiftReflectionTest)
    endif()
  endif()
endif()

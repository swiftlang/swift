if(SWIFT_BUILD_SDK_OVERLAY)
set(swift_reflection_test_darwin_dependencies Darwin)
else()
set(swift_reflection_test_darwin_dependencies)
endif()

if((SWIFT_BUILD_CLANG_OVERLAYS
    OR SWIFT_BUILD_TEST_SUPPORT_MODULES)
   AND (NOT DEFINED SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT
        OR NOT SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT))
  # We need to make this dependency explicit because this library
  # imports Darwin and Darwin re-exports _Builtin_float,
  # but in most configurations we pull Darwin from the SDK,
  # meaning we specify no Darwin dependency in the build system
  list(APPEND swift_reflection_test_darwin_dependencies _Builtin_float)
endif()

if (SWIFT_INCLUDE_TESTS AND SWIFT_BUILD_DYNAMIC_STDLIB)
  add_swift_target_library(swiftSwiftReflectionTest ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
    SwiftReflectionTest.swift
    SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    SWIFT_COMPILE_FLAGS_LINUX -Xcc -D_GNU_SOURCE
    SWIFT_MODULE_DEPENDS_OSX ${swift_reflection_test_darwin_dependencies}
    SWIFT_MODULE_DEPENDS_IOS ${swift_reflection_test_darwin_dependencies}
    SWIFT_MODULE_DEPENDS_TVOS ${swift_reflection_test_darwin_dependencies}
    SWIFT_MODULE_DEPENDS_WATCHOS ${swift_reflection_test_darwin_dependencies}
    SWIFT_MODULE_DEPENDS_XROS ${swift_reflection_test_darwin_dependencies}
    SWIFT_MODULE_DEPENDS_ANDROID Android
    SWIFT_MODULE_DEPENDS_LINUX Glibc
    SWIFT_MODULE_DEPENDS_LINUX_STATIC Musl
    SWIFT_MODULE_DEPENDS_FREEBSD Glibc
    SWIFT_MODULE_DEPENDS_OPENBSD Glibc
    SWIFT_MODULE_DEPENDS_WINDOWS CRT
    INSTALL_IN_COMPONENT stdlib-experimental
    DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}"

    TARGET_SDKS NOT LINUX_STATIC)

  foreach(SDK ${SWIFT_SDKS})
    if ("${SDK}" STREQUAL "LINUX_STATIC")
      continue()
    endif()

    foreach(ARCH ${SWIFT_SDK_${SDK}_ARCHITECTURES})
      set(VARIANT_SUFFIX "-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-${ARCH}")
      add_dependencies("swiftSwiftReflectionTest${VARIANT_SUFFIX}"
        "swift-reflection-test${VARIANT_SUFFIX}")
    endforeach()
  endforeach()
endif()

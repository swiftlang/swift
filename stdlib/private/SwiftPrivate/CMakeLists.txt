set(swift_swiftprivate_compile_flags
    "-parse-stdlib"
    "-Xfrontend" "-disable-access-control")

set(swift_swiftprivate_darwin_dependencies)

if((SWIFT_BUILD_CLANG_OVERLAYS
    OR SWIFT_BUILD_TEST_SUPPORT_MODULES)
   AND (NOT DEFINED SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT
        OR NOT SWIFT_BUILD_CLANG_OVERLAYS_SKIP_BUILTIN_FLOAT))
  # This library imports Darwin from the SDK, which re-exports
  # _Builtin_float which is part of this build.
  list(APPEND swift_swiftprivate_darwin_dependencies _Builtin_float)
endif()

add_swift_target_library(swiftSwiftPrivate ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftPrivate.swift

  IO.swift
  ShardedAtomicCounter.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  GYB_SOURCES
    AtomicInt.swift.gyb

  SWIFT_MODULE_DEPENDS_OSX ${swift_swiftprivate_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_IOS ${swift_swiftprivate_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_TVOS ${swift_swiftprivate_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_WATCHOS ${swift_swiftprivate_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_MACCATALYST ${swift_swiftprivate_darwin_dependencies}
  SWIFT_MODULE_DEPENDS_FREESTANDING "${SWIFT_FREESTANDING_TEST_DEPENDENCIES}"
  SWIFT_MODULE_DEPENDS_ANDROID Android
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_LINUX_STATIC Musl
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_OPENBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_MODULE_DEPENDS_WASI WASILibc
  SWIFT_MODULE_DEPENDS_WINDOWS CRT WinSDK

  DEPLOYMENT_VERSION_OSX ${SWIFT_DARWIN_TEST_DEPLOYMENT_VERSION_OSX}
  DEPLOYMENT_VERSION_IOS ${SWIFT_DARWIN_TEST_DEPLOYMENT_VERSION_IOS}
  DEPLOYMENT_VERSION_TVOS ${SWIFT_DARWIN_TEST_DEPLOYMENT_VERSION_TVOS}
  DEPLOYMENT_VERSION_WATCHOS ${SWIFT_DARWIN_TEST_DEPLOYMENT_VERSION_WATCHOS}
  DEPLOYMENT_VERSION_XROS ${SWIFT_DARWIN_TEST_DEPLOYMENT_VERSION_XROS}

  SWIFT_COMPILE_FLAGS ${swift_swiftprivate_compile_flags} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT stdlib-experimental
  DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}"
  MACCATALYST_BUILD_FLAVOR "zippered"
  )

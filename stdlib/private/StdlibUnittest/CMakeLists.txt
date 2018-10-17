
set(swift_stdlib_unittest_compile_flags
  "-Xfrontend" "-disable-objc-attr-requires-foundation-module")
if (SWIFT_RUNTIME_ENABLE_LEAK_CHECKER)
  list(APPEND swift_stdlib_unittest_compile_flags "-DSWIFT_RUNTIME_ENABLE_LEAK_CHECKER")
endif()

is_build_type_optimized("${SWIFT_STDLIB_BUILD_TYPE}" IS_BUILD_TYPE_OPTIMIZED)
if (NOT IS_BUILD_TYPE_OPTIMIZED)
  list(APPEND swift_stdlib_unittest_compile_flags "-DSWIFT_STDLIB_DEBUG")
endif()

# TODO: support this on non-POSIX platforms.  It cannot be currently as it
# depends on pthreads.
swift_add_target_library(swiftStdlibUnittest ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  StdlibUnittest.swift

  CheckStrideable.swift
  InspectValue.cpp
  InspectValue.swift
  InterceptTraps.cpp
  LifetimeTracked.swift
  MinimalTypes.swift
  OpaqueIdentityFunctions.cpp
  OpaqueIdentityFunctions.swift
  RaceTest.swift
  Statistics.swift
  StdlibCoreExtras.swift
  StringConvertible.swift
  TestHelpers.swift
  TypeIndexed.swift
  GetOSVersion.mm

  SWIFT_MODULE_DEPENDS SwiftPrivate SwiftPrivatePthreadExtras SwiftPrivateLibcExtras
  SWIFT_MODULE_DEPENDS_IOS Darwin Foundation
  SWIFT_MODULE_DEPENDS_OSX Darwin Foundation
  SWIFT_MODULE_DEPENDS_TVOS Darwin Foundation
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin Foundation
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_COMPILE_FLAGS ${swift_stdlib_unittest_compile_flags}
  TARGET_SDKS ALL_POSIX_PLATFORMS
  INSTALL_IN_COMPONENT stdlib-experimental)


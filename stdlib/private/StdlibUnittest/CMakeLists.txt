
set(swift_stdlib_unittest_compile_flags
  "-Xfrontend" "-disable-objc-attr-requires-foundation-module")
if (SWIFT_RUNTIME_ENABLE_LEAK_CHECKER)
  list(APPEND swift_stdlib_unittest_compile_flags "-DSWIFT_RUNTIME_ENABLE_LEAK_CHECKER")
endif()
if(SWIFT_SERIALIZE_STDLIB_UNITTEST)
  list(APPEND swift_stdlib_unittest_compile_flags "-Xfrontend" "-sil-serialize-all")
endif()

# TODO: extend this through a similar mechanism to SWIFT_MODULE_DEPENDS
set(swift_stdlib_unittest_framework_depends)
if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  list(APPEND swift_stdlib_unittest_framework_depends Foundation)
endif()

add_swift_library(swiftStdlibUnittest ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  StdlibUnittest.swift.gyb

  InterceptTraps.cpp
  LifetimeTracked.swift
  MinimalTypes.swift.gyb
  OpaqueIdentityFunctions.cpp
  OpaqueIdentityFunctions.swift
  RaceTest.swift
  Statistics.swift
  StdlibCoreExtras.swift
  StringConvertible.swift.gyb
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
  SWIFT_COMPILE_FLAGS ${swift_stdlib_unittest_compile_flags}
  FRAMEWORK_DEPENDS ${swift_stdlib_unittest_framework_depends}
  INSTALL_IN_COMPONENT stdlib-experimental)


set(swift_stdlib_unittest_platform_sources)
set(swift_stdlib_unittest_module_depends
  SwiftPrivate SwiftPrivatePthreadExtras SwiftPrivateLibcExtras)
set(swift_stdlib_unittest_framework_depends)
set(swift_stdlib_unittest_private_link_libraries)
set(swift_stdlib_unittest_compile_flags
  "-Xfrontend" "-disable-objc-attr-requires-foundation-module")

if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  list(APPEND swift_stdlib_unittest_platform_sources
    GetOSVersion.mm)
  list(APPEND swift_stdlib_unittest_module_depends
    Darwin
    Foundation)
  list(APPEND swift_stdlib_unittest_framework_depends
    Foundation)
endif()
if(SWIFT_HOST_VARIANT STREQUAL "linux" OR SWIFT_HOST_VARIANT STREQUAL "freebsd")
  list(APPEND swift_stdlib_unittest_module_depends
    Glibc)
endif()
if(SWIFT_SERIALIZE_STDLIB_UNITTEST)
  list(APPEND swift_stdlib_unittest_compile_flags "-Xfrontend" "-sil-serialize-all")
endif()

add_swift_library(swiftStdlibUnittest SHARED IS_STDLIB
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
  ${swift_stdlib_unittest_platform_sources}

  PRIVATE_LINK_LIBRARIES ${swift_stdlib_unittest_private_link_libraries}
  SWIFT_MODULE_DEPENDS ${swift_stdlib_unittest_module_depends}
  SWIFT_COMPILE_FLAGS ${swift_stdlib_unittest_compile_flags}
  FRAMEWORK_DEPENDS ${swift_stdlib_unittest_framework_depends}
  INSTALL_IN_COMPONENT stdlib-experimental)


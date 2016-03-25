set(swift_reflection_test_compile_flags
  "-Xfrontend" "-enable-reflection-metadata")

add_swift_library(swiftSwiftReflectionTest SHARED IS_STDLIB
  SwiftReflectionTest.swift
  SWIFT_MODULE_DEPENDS Darwin
  SWIFT_COMPILE_FLAGS ${swift_reflection_test_compile_flags}
  INSTALL_IN_COMPONENT stdlib-experimental)

add_swift_target_library(swiftDifferentiationUnittest ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  GenericLifetimeTracked.swift
  SWIFT_COMPILE_FLAGS
  TARGET_SDKS ALL_POSIX_PLATFORMS
  INSTALL_IN_COMPONENT stdlib-experimental)

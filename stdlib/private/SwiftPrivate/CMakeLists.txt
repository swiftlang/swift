swift_add_target_library(swiftSwiftPrivate ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftPrivate.swift
  IO.swift
  ShardedAtomicCounter.swift

  SWIFT_COMPILE_FLAGS
  INSTALL_IN_COMPONENT stdlib-experimental)


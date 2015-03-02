add_swift_library(swiftSwiftUnstable SHARED IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftUnstable.swift
  IO.swift
  PRNG.swift
  ShardedAtomicCounter.swift

  SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all
  INSTALL_IN_COMPONENT stdlib-experimental)


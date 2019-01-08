set(swift_swiftprivate_compile_flags
    "-parse-stdlib"
    "-Xfrontend" "-disable-access-control")

add_swift_target_library(swiftSwiftPrivate ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftPrivate.swift

  AtomicInt.swift.gyb
  IO.swift
  ShardedAtomicCounter.swift

  SWIFT_COMPILE_FLAGS ${swift_swiftprivate_compile_flags}
  INSTALL_IN_COMPONENT stdlib-experimental)


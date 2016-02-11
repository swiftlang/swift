set(swift_private_private_link_libraries)

add_swift_library(swiftSwiftPrivate SHARED IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftPrivate.swift
  IO.swift
  PRNG.swift
  ShardedAtomicCounter.swift

  SWIFT_COMPILE_FLAGS ${STDLIB_SIL_SERIALIZE_ALL}
  PRIVATE_LINK_LIBRARIES ${swift_private_private_link_libraries}
  INSTALL_IN_COMPONENT stdlib-experimental)


set(swift_swiftprivate_compile_flags
    "-parse-stdlib"
    "-Xfrontend" "-disable-access-control")

add_swift_target_library(swiftSwiftPrivate ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftPrivate.swift

  IO.swift
  ShardedAtomicCounter.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  GYB_SOURCES
    AtomicInt.swift.gyb

  SWIFT_MODULE_DEPENDS_WINDOWS MSVCRT WinSDK
  SWIFT_COMPILE_FLAGS ${swift_swiftprivate_compile_flags} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT stdlib-experimental
  DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}")


set(swift_private_private_link_libraries)
if(SWIFT_HOST_VARIANT STREQUAL "linux")
  # Needed for arc4random
  find_package(BSD REQUIRED)
  list(APPEND swift_private_private_link_libraries
    ${BSD_LIBRARIES})
endif()

add_swift_library(swiftSwiftPrivate SHARED IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftPrivate.swift
  IO.swift
  PRNG.swift
  ShardedAtomicCounter.swift

  SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all
  PRIVATE_LINK_LIBRARIES ${swift_private_private_link_libraries}
  INSTALL_IN_COMPONENT stdlib-experimental)


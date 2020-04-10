set(swift_swiftprivate_compile_flags
    "-parse-stdlib"
    "-Xfrontend" "-disable-access-control")

foreach(sdk ${SWIFT_SDKS})

set(module_depends)

if(${sdk} STREQUAL WINDOWS)
  list(APPEND module_depends MSVCRT WinSDK)
endif()

add_swift_target_library_single_sdk(swiftSwiftPrivate ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftPrivate.swift

  IO.swift
  ShardedAtomicCounter.swift

  "${SWIFT_SOURCE_DIR}/stdlib/linker-support/magic-symbols-for-install-name.c"

  GYB_SOURCES
    AtomicInt.swift.gyb

  SDK ${sdk}
  SWIFT_MODULE_DEPENDS ${module_depends}
  SWIFT_COMPILE_FLAGS ${swift_swiftprivate_compile_flags} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT stdlib-experimental
  DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}")

endforeach()
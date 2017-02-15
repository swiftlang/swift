add_swift_target_library(swiftSwiftExperimental ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftExperimental.swift

  SWIFT_COMPILE_FLAGS ${STDLIB_SIL_SERIALIZE_ALL} -parse-stdlib
  INSTALL_IN_COMPONENT stdlib-experimental)


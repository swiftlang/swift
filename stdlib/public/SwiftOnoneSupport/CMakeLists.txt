add_swift_library(swiftSwiftOnoneSupport ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftOnoneSupport.swift
  SWIFT_COMPILE_FLAGS ${STDLIB_SIL_SERIALIZE_ALL} "-parse-stdlib"  "${SWIFT_RUNTIME_SWIFT_COMPILE_FLAGS}"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  INSTALL_IN_COMPONENT stdlib)

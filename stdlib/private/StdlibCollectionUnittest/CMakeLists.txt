set(swift_stdlib_unittest_compile_flags)
if(SWIFT_SERIALIZE_STDLIB_UNITTEST)
  list(APPEND swift_stdlib_unittest_compile_flags "-Xfrontend" "-sil-serialize-all")
endif()

# TODO: support this on non-POSIX platforms.  It cannot be currently as it
# depends on pthreads.
add_swift_library(swiftStdlibCollectionUnittest ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  StdlibCollectionUnittest.swift

  CheckCollectionInstance.swift.gyb
  CheckCollectionType.swift.gyb
  CheckMutableCollectionType.swift.gyb
  CheckRangeReplaceableCollectionType.swift
  CheckRangeReplaceableSliceType.swift
  CheckSequenceInstance.swift.gyb
  CheckSequenceType.swift
  LoggingWrappers.swift.gyb
  MinimalCollections.swift.gyb
  RangeSelection.swift
  ../../public/core/WriteBackMutableSlice.swift

  SWIFT_MODULE_DEPENDS StdlibUnittest
  SWIFT_COMPILE_FLAGS ${swift_stdlib_unittest_compile_flags}
  TARGET_SDKS ALL_POSIX_PLATFORMS
  INSTALL_IN_COMPONENT stdlib-experimental)


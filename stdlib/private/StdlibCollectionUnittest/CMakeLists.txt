set(swift_stdlib_unittest_compile_flags)
if(SWIFT_SERIALIZE_STDLIB_UNITTEST)
  list(APPEND swift_stdlib_unittest_compile_flags "-Xfrontend" "-sil-serialize-all")
endif()

set(swift_stdlib_unittest_framework_depends)
if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  list(APPEND swift_stdlib_unittest_framework_depends
    Foundation)
endif()

add_swift_library(swiftStdlibCollectionUnittest SHARED IS_STDLIB
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
  ../../public/core/WriteBackMutableSlice.swift

  PRIVATE_LINK_LIBRARIES ${swift_stdlib_unittest_private_link_libraries}
  SWIFT_MODULE_DEPENDS StdlibUnittest
  SWIFT_COMPILE_FLAGS ${swift_stdlib_unittest_compile_flags}
  FRAMEWORK_DEPENDS ${swift_stdlib_unittest_framework_depends}
  INSTALL_IN_COMPONENT stdlib-experimental)


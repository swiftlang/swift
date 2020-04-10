set(swift_stdlib_unittest_compile_flags)

foreach(sdk ${SWIFT_SDKS})

set(module_depends)
set(glibc_sdks LINUX FREEBSD CYGWIN HAIKU)
if(${sdk} IN_LIST glibc_sdks)
  list(APPEND module_depends Glibc)
elseif(${sdk} STREQUAL WINDOWS)
  list(APPEND module_depends MSVCRT)
endif()

add_swift_target_library_single_sdk(swiftStdlibCollectionUnittest ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  StdlibCollectionUnittest.swift

  CheckCollectionInstance.swift
  CheckCollectionType.swift
  CheckMutableCollectionType.swift
  CheckRangeReplaceableCollectionType.swift
  CheckRangeReplaceableSliceType.swift
  CheckSequenceInstance.swift
  CheckSequenceType.swift
  COWLoggingArray.swift
  LoggingWrappers.swift
  MinimalCollections.swift
  RangeSelection.swift
  WriteBackMutableSlice.swift

  SDK ${sdk}
  SWIFT_MODULE_DEPENDS StdlibUnittest ${module_depends}
  SWIFT_COMPILE_FLAGS ${swift_stdlib_unittest_compile_flags} ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
  INSTALL_IN_COMPONENT stdlib-experimental
  DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}")

endforeach()

add_swift_target_library(swiftSwiftPrivatePthreadExtras ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftPrivatePthreadExtras.swift
  PthreadBarriers.swift

  SWIFT_MODULE_DEPENDS_IOS Darwin
  SWIFT_MODULE_DEPENDS_OSX Darwin
  SWIFT_MODULE_DEPENDS_TVOS Darwin
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_COMPILE_FLAGS ${STDLIB_SIL_SERIALIZE_ALL}
  TARGET_SDKS ALL_POSIX_PLATFORMS
  INSTALL_IN_COMPONENT stdlib-experimental)


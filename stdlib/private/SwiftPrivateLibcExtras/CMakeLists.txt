set(swift_private_darwin_extras_module_depends
  SwiftPrivate)

if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  list(APPEND swift_private_darwin_extras_module_depends
    Darwin)
elseif(SWIFT_HOST_VARIANT STREQUAL "linux" OR SWIFT_HOST_VARIANT STREQUAL "freebsd")
  list(APPEND swift_private_darwin_extras_module_depends
    Glibc)
endif()

add_swift_library(swiftSwiftPrivateLibcExtras SHARED IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftPrivateLibcExtras.swift
  Subprocess.c
  Subprocess.swift

  SWIFT_MODULE_DEPENDS ${swift_private_darwin_extras_module_depends}
  SWIFT_COMPILE_FLAGS ${STDLIB_SIL_SERIALIZE_ALL}
  INSTALL_IN_COMPONENT stdlib-experimental)

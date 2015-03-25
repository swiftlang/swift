set(swift_private_darwin_extras_module_depends
  SwiftPrivate)

if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  list(APPEND swift_private_darwin_extras_module_depends
    Darwin)
elseif(SWIFT_HOST_VARIANT STREQUAL "linux")
  list(APPEND swift_private_darwin_extras_module_depends
    Glibc)
endif()

add_swift_library(swiftSwiftPrivateDarwinExtras SHARED IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftPrivateDarwinExtras.swift
  Subprocess.swift

  SWIFT_MODULE_DEPENDS ${swift_private_darwin_extras_module_depends}
  SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all
  INSTALL_IN_COMPONENT stdlib-experimental)

set(swift_private_pthread_extras_module_depends)

if(SWIFT_HOST_VARIANT MATCHES "${SWIFT_DARWIN_VARIANTS}")
  list(APPEND swift_private_pthread_extras_module_depends
    Darwin)
elseif(SWIFT_HOST_VARIANT STREQUAL "linux")
  list(APPEND swift_private_pthread_extras_module_depends
    Glibc)
endif()

add_swift_library(swiftSwiftPrivatePthreadExtras SHARED IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftPrivatePthreadExtras.swift
  PthreadBarriers.swift
  SwiftBlockToCFunctionThunks.cpp

  SWIFT_MODULE_DEPENDS ${swift_private_pthread_extras_module_depends}
  SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all
  INSTALL_IN_COMPONENT stdlib-experimental)


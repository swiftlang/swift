set(swift_unstable_darwin_extras_module_depends
  SwiftUnstable)

if(SWIFT_HOST_VARIANT STREQUAL "macosx")
  list(APPEND swift_unstable_darwin_extras_module_depends
    Darwin)
elseif(SWIFT_HOST_VARIANT STREQUAL "linux")
  list(APPEND swift_unstable_darwin_extras_module_depends
    Glibc)
endif()

add_swift_library(swiftSwiftUnstableDarwinExtras SHARED IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftUnstableDarwinExtras.swift
  Subprocess.swift

  SWIFT_MODULE_DEPENDS ${swift_unstable_darwin_extras_module_depends}
  SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all
  INSTALL_IN_COMPONENT stdlib-experimental)

add_swift_library(swiftSwiftExperimental SHARED IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftExperimental.swift

  SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all -parse-stdlib
  INSTALL_IN_COMPONENT stdlib-experimental)


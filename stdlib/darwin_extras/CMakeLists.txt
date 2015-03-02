add_swift_library(swiftSwiftUnstableDarwinExtras SHARED IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftUnstableDarwinExtras.swift
  Subprocess.swift

  SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all
  INSTALL_IN_COMPONENT stdlib-experimental)


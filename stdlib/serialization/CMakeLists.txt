add_swift_library(swiftSwiftUnstableSerialization SHARED IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftUnstableSerialization.swift.gyb
  MsgPack.swift
  MsgPackSerialization.swift.gyb

  SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all
  INSTALL_IN_COMPONENT stdlib-experimental)


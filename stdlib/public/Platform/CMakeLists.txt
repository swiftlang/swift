add_swift_library(swiftDarwin IS_SDK_OVERLAY
  Darwin.swift
  POSIXError.swift
  MachError.swift
  Misc.c
  tgmath.swift.gyb
  SWIFT_COMPILE_FLAGS -Xfrontend -disable-objc-attr-requires-foundation-module
  API_NOTES_NON_OVERLAY)


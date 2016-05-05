add_swift_library(swiftStdlibUnittestFoundationExtras SHARED IS_STDLIB
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  StdlibUnittestFoundationExtras.swift

  SWIFT_MODULE_DEPENDS Foundation
  INSTALL_IN_COMPONENT stdlib-experimental)


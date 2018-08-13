add_swift_library(swiftTestUtils SHARED
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftTestUtils.swift

  INSTALL_IN_COMPONENT never_install
  IS_STDLIB)

set(SHARED_LIBRARY ON)

add_swift_library(swiftSwiftExperimental INSTALL TARGET_LIBRARY
  # This file should be listed the first.  Module name is inferred from the
  # filename.
  SwiftExperimental.swift

  SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all

  FRAMEWORK_DEPENDS Foundation
  DEPENDS swiftCore)


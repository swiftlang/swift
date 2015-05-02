add_swift_library(swiftsimd SHARED IS_STDLIB
  simd.swift.gyb

  SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all
  SWIFT_MODULE_DEPENDS Darwin
  INSTALL_IN_COMPONENT stdlib)

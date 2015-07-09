add_swift_library(swiftsimd IS_SDK_OVERLAY
  simd.swift.gyb

  SWIFT_COMPILE_FLAGS -Xfrontend -sil-serialize-all -Xfrontend -parse-stdlib
  SWIFT_MODULE_DEPENDS Darwin)

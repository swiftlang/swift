add_swift_library(swiftsimd IS_SDK_OVERLAY
  simd.swift.gyb

  SWIFT_COMPILE_FLAGS ${STDLIB_SIL_SERIALIZE_ALL} -parse-stdlib
  SWIFT_MODULE_DEPENDS Darwin)

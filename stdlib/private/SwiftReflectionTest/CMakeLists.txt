set(swift_reflection_test_compile_flags
  "-Xfrontend" "-enable-reflection-names")

add_swift_library(swiftSwiftReflectionTest SHARED IS_STDLIB
  SwiftReflectionTest.swift
  SWIFT_MODULE_DEPENDS Darwin
  SWIFT_COMPILE_FLAGS ${swift_reflection_test_compile_flags}
  INSTALL_IN_COMPONENT stdlib-experimental)

foreach(SDK ${SWIFT_SDKS})
  foreach(ARCH ${SWIFT_SDK_${SDK}_ARCHITECTURES})
    set(VARIANT_SUFFIX "-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-${ARCH}")
    add_dependencies(
      "swiftSwiftReflectionTest${VARIANT_SUFFIX}"
      "swift-reflection-test${VARIANT_SUFFIX}")
  endforeach()
endforeach()

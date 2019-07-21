
if (SWIFT_INCLUDE_TESTS)
  add_swift_target_library(swiftSwiftReflectionTest ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
    SwiftReflectionTest.swift
    SWIFT_MODULE_DEPENDS Darwin
    TARGET_SDKS ALL_APPLE_PLATFORMS
    SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    INSTALL_IN_COMPONENT stdlib-experimental
    DARWIN_INSTALL_NAME_DIR "${SWIFT_DARWIN_STDLIB_PRIVATE_INSTALL_NAME_DIR}")

  foreach(SDK ${SWIFT_SDKS})
    if("${SDK}" IN_LIST SWIFT_APPLE_PLATFORMS)
      foreach(ARCH ${SWIFT_SDK_${SDK}_ARCHITECTURES})
        set(VARIANT_SUFFIX "-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-${ARCH}")
        add_dependencies(
          "swiftSwiftReflectionTest${VARIANT_SUFFIX}"
          "swift-reflection-test${VARIANT_SUFFIX}")
      endforeach()
    endif()
  endforeach()
endif()

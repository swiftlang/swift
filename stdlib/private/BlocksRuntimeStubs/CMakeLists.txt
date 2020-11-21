list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../test/cmake/modules")

include(SwiftTestUtils)

foreach(SDK ${SWIFT_SDKS})
  foreach(ARCH ${SWIFT_SDK_${SDK}_ARCHITECTURES})
    get_swift_test_build_flavors(build_flavors "${SDK}")

    foreach(BUILD_FLAVOR build_flavors)
      get_swift_test_variant_suffix(VARIANT_SUFFIX "${SDK}" "${ARCH}" "${BUILD_FLAVOR}")

      set(test_bin_dir "${CMAKE_BINARY_DIR}/test${VARIANT_SUFFIX}")

      _add_swift_target_library_single(
        BlocksRuntimeStub${VARIANT_SUFFIX}
        BlocksRuntimeStub
        SHARED
        ARCHITECTURE ${ARCH}
        SDK ${SDK}
        INSTALL_IN_COMPONENT dev
        BlocksRuntime.c
      )
      set_target_properties(BlocksRuntimeStub${VARIANT_SUFFIX} PROPERTIES
        ARCHIVE_OUTPUT_DIRECTORY ${test_bin_dir}
        LIBRARY_OUTPUT_DIRECTORY ${test_bin_dir}
        RUNTIME_OUTPUT_DIRECTORY ${test_bin_dir}
        OUTPUT_NAME BlocksRuntime)
    endforeach()
  endforeach()
endforeach()


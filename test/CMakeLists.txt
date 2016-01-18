function(swift_configure_lit_site_cfg source_path destination_path installed_name)
  if (CMAKE_CFG_INTDIR STREQUAL ".")
    set(SWIFT_BUILD_MODE ".")
  else ()
    set(SWIFT_BUILD_MODE "%(build_mode)s")
  endif ()

  string(REPLACE ${CMAKE_CFG_INTDIR} ${SWIFT_BUILD_MODE} LLVM_TOOLS_DIR ${LLVM_TOOLS_BINARY_DIR})
  string(REPLACE ${CMAKE_CFG_INTDIR} ${SWIFT_BUILD_MODE} LLVM_LIBS_DIR  ${LLVM_LIBRARY_DIR})

  if (XCODE)
    string(REPLACE ${CMAKE_CFG_INTDIR} Debug LIT_SWIFTLIB_DIR ${SWIFTLIB_DIR})
  else ()
    set(LIT_SWIFTLIB_DIR ${SWIFTLIB_DIR})
  endif ()

  configure_lit_site_cfg("${source_path}" "${destination_path}")

  if(NOT "${installed_name}" STREQUAL "")
    swift_install_in_component(testsuite-tools
        FILES "${destination_path}"
        RENAME "${installed_name}"
        DESTINATION "share/swift/testsuite")
  endif()
endfunction()

function(normalize_boolean_spelling var_name)
  if(${var_name})
    set("${var_name}" TRUE PARENT_SCOPE)
  else()
    set("${var_name}" FALSE PARENT_SCOPE)
  endif()
endfunction()

function(get_test_dependencies SDK result_var_name)
  set(deps)

  if(SWIFT_BUILD_STDLIB)
    list(APPEND deps SwiftUnitTests)
  endif()

  set(deps_binaries
      swift swift-ide-test sil-opt swift-llvm-opt swift-demangle sil-extract swift-compress
      lldb-moduleimport-test)
  if(NOT SWIFT_BUILT_STANDALONE)
    list(APPEND deps_binaries llc)
  endif()
  if(SWIFT_BUILD_SOURCEKIT)
    list(APPEND deps_binaries sourcekitd-test complete-test)
  endif()

  if(("${SDK}" STREQUAL "IOS") OR
     ("${SDK}" STREQUAL "TVOS") OR
     ("${SDK}" STREQUAL "WATCHOS") OR
     ("${SDK}" STREQUAL "OSX") OR
     ("${SDK}" STREQUAL "IOS_SIMULATOR") OR
     ("${SDK}" STREQUAL "TVOS_SIMULATOR") OR
     ("${SDK}" STREQUAL "WATCHOS_SIMULATOR") OR
     ("${SDK}" STREQUAL "LINUX") OR
     ("${SDK}" STREQUAL "FREEBSD"))
    # No extra dependencies.
  else()
    message(FATAL_ERROR "Unknown SDK: ${SDK}")
  endif()

  if(XCODE)
    # Xcode's build paths have a configuration variable in them,
    # so CMake can't match them at compile time. Just use target names.
    list(APPEND deps ${deps_binaries})
  else()
    foreach(binary ${deps_binaries})
      list(APPEND deps "${SWIFT_NATIVE_SWIFT_TOOLS_PATH}/${binary}")
    endforeach()
  endif()

  set("${result_var_name}" "${deps}" PARENT_SCOPE)
endfunction()

include(FindPythonInterp)
if(PYTHONINTERP_FOUND)
  if(PATH_TO_LLVM_SOURCE)
    set(LIT "${PATH_TO_LLVM_SOURCE}/utils/lit/lit.py")
  else()
    set(LIT "${PATH_TO_LLVM_BUILD}/${CMAKE_CFG_INTDIR}/bin/llvm-lit")
    # Installed LLVM does not contain ${CMAKE_CFG_INTDIR} in paths.
    if(NOT EXISTS "${LIT}")
      set(LIT "${PATH_TO_LLVM_BUILD}/llvm-lit/bin")
    endif()
  endif()

  set(SWIFT_TEST_EXTRA_ARGS)

  if(NOT SWIFT_BUILD_TOOLS)
    list(APPEND SWIFT_TEST_EXTRA_ARGS
         "--path=${SWIFT_NATIVE_LLVM_TOOLS_PATH}"
         "--path=${SWIFT_NATIVE_CLANG_TOOLS_PATH}"
         "--path=${SWIFT_NATIVE_SWIFT_TOOLS_PATH}")
    if(SWIFT_BUILD_STDLIB)
      list(APPEND SWIFT_TEST_EXTRA_ARGS
           "--param" "test_resource_dir=${SWIFTLIB_DIR}")
    endif()
  endif()

  option(SWIFT_TEST_USE_LEAKS "Run Swift stdlib tests under leaks" FALSE)
  if (SWIFT_TEST_USE_LEAKS)
    list(APPEND SWIFT_TEST_EXTRA_ARGS "--param" "leaks-all")
  endif()

  if(NOT CMAKE_CFG_INTDIR STREQUAL ".")
    list(APPEND SWIFT_TEST_EXTRA_ARGS
         "--param" "build_mode=${CMAKE_CFG_INTDIR}")
  endif()

  if (LLVM_USE_SANITIZER STREQUAL "Address")
    set(SWIFT_ASAN_BUILD TRUE)
  endif()

  set(TEST_MODES optimize_none optimize optimize_unchecked)

  foreach(SDK ${SWIFT_SDKS})
    foreach(ARCH ${SWIFT_SDK_${SDK}_ARCHITECTURES})
      foreach(TEST_MODE ${TEST_MODES})
        # Configure variables for this subdirectory.
        set(VARIANT_SUFFIX "-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-${ARCH}")
        set(VARIANT_TRIPLE "${SWIFT_SDK_${SDK}_ARCH_${ARCH}_TRIPLE}")
        set(VARIANT_SDK "${SWIFT_SDK_${SDK}_PATH}")
        is_build_type_optimized("${SWIFT_STDLIB_BUILD_TYPE}" SWIFT_OPTIMIZED)

        # Normalize spelling of boolean values.
        normalize_boolean_spelling(LLVM_ENABLE_ASSERTIONS)
        normalize_boolean_spelling(SWIFT_STDLIB_ASSERTIONS)
        normalize_boolean_spelling(SWIFT_AST_VERIFIER)
        normalize_boolean_spelling(SWIFT_ASAN_BUILD)

        # A directory where to put the xUnit-style XML test results.
        set(swift_test_results_dir
            "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/swift-test-results/${VARIANT_TRIPLE}")

        set(command_clean_test_results_dir
            COMMAND "${CMAKE_COMMAND}" -E remove_directory "${swift_test_results_dir}"
            COMMAND "${CMAKE_COMMAND}" -E make_directory "${swift_test_results_dir}")

        set(LIT_ARGS "${SWIFT_TEST_EXTRA_ARGS} ${LLVM_LIT_ARGS}")
        separate_arguments(LIT_ARGS)

        set(test_mode_target_suffix "")
        if(NOT TEST_MODE STREQUAL "optimize_none")
          if(TEST_MODE STREQUAL "optimize")
            set(test_mode_target_suffix "-optimize")
            list(APPEND LIT_ARGS "--param" "run_only_tests=executable_test")
          endif()
          if(TEST_MODE STREQUAL "optimize_unchecked")
            set(test_mode_target_suffix "-optimize-unchecked")
            list(APPEND LIT_ARGS "--param" "run_only_tests=executable_test")
          endif()
          list(APPEND LIT_ARGS "--param" "swift_test_mode=${TEST_MODE}")
        endif()

        if(NOT SWIFT_BUILD_STDLIB)
          list(APPEND LIT_ARGS
              "--param" "test_sdk_overlay_dir=${SWIFTLIB_DIR}/${SWIFT_SDK_${SDK}_LIB_SUBDIR}")
        endif()

        list(APPEND LIT_ARGS "--xunit-xml-output=${swift_test_results_dir}/lit-tests.xml")

        set(lit_command ${PYTHON_EXECUTABLE} "${LIT}" ${LIT_ARGS})

        set(command_upload_stdlib)
        if("${SDK}" STREQUAL "IOS" OR "${SDK}" STREQUAL "TVOS" OR "${SDK}" STREQUAL "WATCHOS")
          # These are supported testing SDKs.
        endif()

        set(test_dependencies)
        get_test_dependencies("${SDK}" test_dependencies)
        list(APPEND test_dependencies
            "swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}")

        set(test_bin_dir "${CMAKE_CURRENT_BINARY_DIR}${VARIANT_SUFFIX}")
        set(validation_test_bin_dir
            "${CMAKE_CURRENT_BINARY_DIR}/../validation-test${VARIANT_SUFFIX}")

        swift_configure_lit_site_cfg(
            "${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.in"
            "${test_bin_dir}/lit.site.cfg"
            "test${VARIANT_SUFFIX}.lit.site.cfg")

        swift_configure_lit_site_cfg(
            "${CMAKE_CURRENT_SOURCE_DIR}/Unit/lit.site.cfg.in"
            "${test_bin_dir}/Unit/lit.site.cfg"
            "")

        swift_configure_lit_site_cfg(
            "${CMAKE_CURRENT_SOURCE_DIR}/../validation-test/lit.site.cfg.in"
            "${validation_test_bin_dir}/lit.site.cfg"
            "validation-test${VARIANT_SUFFIX}.lit.site.cfg")

        add_custom_target("check-swift${test_mode_target_suffix}${VARIANT_SUFFIX}"
            ${command_upload_stdlib}
            ${command_clean_test_results_dir}
            COMMAND ${lit_command} "${test_bin_dir}"
            DEPENDS ${test_dependencies}
            COMMENT "Running Swift tests for ${VARIANT_TRIPLE}"
            ${cmake_3_2_USES_TERMINAL})

        add_custom_target("check-swift-validation${test_mode_target_suffix}${VARIANT_SUFFIX}"
            ${command_upload_stdlib}
            ${command_clean_test_results_dir}
            COMMAND ${lit_command} "${validation_test_bin_dir}"
            DEPENDS ${test_dependencies}
            COMMENT "Running Swift validation tests for ${VARIANT_TRIPLE}"
            ${cmake_3_2_USES_TERMINAL})

        add_custom_target("check-swift-all${test_mode_target_suffix}${VARIANT_SUFFIX}"
            ${command_upload_stdlib}
            ${command_clean_test_results_dir}
            COMMAND ${lit_command} "${validation_test_bin_dir}" "${test_bin_dir}"
            DEPENDS ${test_dependencies}
            COMMENT "Running all Swift tests for ${VARIANT_TRIPLE}"
            ${cmake_3_2_USES_TERMINAL})
      endforeach()
    endforeach()
  endforeach()

  # Add shortcuts for the default variant.
  foreach(TEST_MODE ${TEST_MODES})
    set(test_mode_target_suffix)
    if(TEST_MODE STREQUAL "optimize_none")
      set(test_mode_target_suffix "")
    endif()
    if(TEST_MODE STREQUAL "optimize")
      set(test_mode_target_suffix "-optimize")
    endif()
    if(TEST_MODE STREQUAL "optimize_unchecked")
      set(test_mode_target_suffix "-optimize-unchecked")
    endif()

    add_custom_target(check-swift${test_mode_target_suffix}
        DEPENDS "check-swift${test_mode_target_suffix}${SWIFT_PRIMARY_VARIANT_SUFFIX}")

    add_custom_target(check-swift-validation${test_mode_target_suffix}
        DEPENDS "check-swift-validation${test_mode_target_suffix}${SWIFT_PRIMARY_VARIANT_SUFFIX}")

    add_custom_target(check-swift-all${test_mode_target_suffix}
        DEPENDS "check-swift-all${test_mode_target_suffix}${SWIFT_PRIMARY_VARIANT_SUFFIX}")
  endforeach()

endif()


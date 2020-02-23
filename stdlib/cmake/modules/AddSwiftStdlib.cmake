
include(AddSwift)

# Add an executable compiled for a given variant.
#
# Don't use directly, use add_swift_executable and add_swift_target_executable
# instead.
#
# See add_swift_executable for detailed documentation.
#
# Additional parameters:
#   [SDK sdk]
#     SDK to build for.
#
#   [ARCHITECTURE architecture]
#     Architecture to build for.
function(_add_swift_target_executable_single name)
  set(options)
  set(single_parameter_options
    ARCHITECTURE
    SDK)
  set(multiple_parameter_options
    COMPILE_FLAGS
    DEPENDS
    LLVM_LINK_COMPONENTS)
  cmake_parse_arguments(SWIFTEXE_SINGLE
    "${options}"
    "${single_parameter_options}"
    "${multiple_parameter_options}"
    ${ARGN})

  set(SWIFTEXE_SINGLE_SOURCES ${SWIFTEXE_SINGLE_UNPARSED_ARGUMENTS})

  # Check arguments.
  precondition(SWIFTEXE_SINGLE_SDK MESSAGE "Should specify an SDK")
  precondition(SWIFTEXE_SINGLE_ARCHITECTURE MESSAGE "Should specify an architecture")

  # Determine compiler flags.
  set(c_compile_flags)
  set(link_flags)

  # Prepare linker search directories.
  set(library_search_directories
        "${SWIFTLIB_DIR}/${SWIFT_SDK_${SWIFTEXE_SINGLE_SDK}_LIB_SUBDIR}")

  # Add variant-specific flags.
  _add_variant_c_compile_flags(
    SDK "${SWIFTEXE_SINGLE_SDK}"
    ARCH "${SWIFTEXE_SINGLE_ARCHITECTURE}"
    BUILD_TYPE "${CMAKE_BUILD_TYPE}"
    ENABLE_ASSERTIONS "${LLVM_ENABLE_ASSERTIONS}"
    ENABLE_LTO "${SWIFT_TOOLS_ENABLE_LTO}"
    ANALYZE_CODE_COVERAGE "${SWIFT_ANALYZE_CODE_COVERAGE}"
    RESULT_VAR_NAME c_compile_flags)
  _add_variant_link_flags(
    SDK "${SWIFTEXE_SINGLE_SDK}"
    ARCH "${SWIFTEXE_SINGLE_ARCHITECTURE}"
    BUILD_TYPE "${CMAKE_BUILD_TYPE}"
    ENABLE_ASSERTIONS "${LLVM_ENABLE_ASSERTIONS}"
    ENABLE_LTO "${SWIFT_TOOLS_ENABLE_LTO}"
    LTO_OBJECT_NAME "${name}-${SWIFTEXE_SINGLE_SDK}-${SWIFTEXE_SINGLE_ARCHITECTURE}"
    ANALYZE_CODE_COVERAGE "${SWIFT_ANALYZE_CODE_COVERAGE}"
    RESULT_VAR_NAME link_flags
    LINK_LIBRARIES_VAR_NAME link_libraries
    LIBRARY_SEARCH_DIRECTORIES_VAR_NAME library_search_directories)

  handle_swift_sources(
      dependency_target
      unused_module_dependency_target
      unused_sib_dependency_target
      unused_sibopt_dependency_target
      unused_sibgen_dependency_target
      SWIFTEXE_SINGLE_SOURCES SWIFTEXE_SINGLE_EXTERNAL_SOURCES ${name}
      DEPENDS
        ${SWIFTEXE_SINGLE_DEPENDS}
      MODULE_NAME ${name}
      SDK ${SWIFTEXE_SINGLE_SDK}
      ARCHITECTURE ${SWIFTEXE_SINGLE_ARCHITECTURE}
      COMPILE_FLAGS ${SWIFTEXE_SINGLE_COMPILE_FLAGS}
      IS_MAIN)
  add_swift_source_group("${SWIFTEXE_SINGLE_EXTERNAL_SOURCES}")

  add_executable(${name}
      ${SWIFTEXE_SINGLE_SOURCES}
      ${SWIFTEXE_SINGLE_EXTERNAL_SOURCES})

  add_dependencies_multiple_targets(
      TARGETS "${name}"
      DEPENDS
        ${dependency_target}
        ${LLVM_COMMON_DEPENDS}
        ${SWIFTEXE_SINGLE_DEPENDS})
  llvm_update_compile_flags("${name}")

  if(SWIFTEXE_SINGLE_SDK STREQUAL WINDOWS)
    swift_windows_include_for_arch(${SWIFTEXE_SINGLE_ARCHITECTURE}
      ${SWIFTEXE_SINGLE_ARCHITECTURE}_INCLUDE)
    target_include_directories(${name} SYSTEM PRIVATE
      ${${SWIFTEXE_SINGLE_ARCHITECTURE}_INCLUDE})

    if(NOT ${CMAKE_C_COMPILER_ID} STREQUAL MSVC)
      # MSVC doesn't support -Xclang. We don't need to manually specify
      # the dependent libraries as `cl` does so.
      target_compile_options(${name} PRIVATE
        "SHELL:-Xclang --dependent-lib=oldnames"
        # TODO(compnerd) handle /MT, /MTd
        "SHELL:-Xclang --dependent-lib=msvcrt$<$<CONFIG:Debug>:d>")
    endif()
  endif()
  target_compile_options(${name} PRIVATE
    ${c_compile_flags})
  target_link_directories(${name} PRIVATE
    ${library_search_directories})
  target_link_options(${name} PRIVATE
    ${link_flags})
  target_link_libraries(${name} PRIVATE
    ${link_libraries})
  if (SWIFT_PARALLEL_LINK_JOBS)
    set_property(TARGET ${name} PROPERTY JOB_POOL_LINK swift_link_job_pool)
  endif()
  if(${SWIFTEXE_SINGLE_SDK} IN_LIST SWIFT_APPLE_PLATFORMS)
    set_target_properties(${name} PROPERTIES
      BUILD_WITH_INSTALL_RPATH YES
      INSTALL_RPATH "@executable_path/../lib/swift/${SWIFT_SDK_${SWIFTEXE_SINGLE_SDK}_LIB_SUBDIR}")
  endif()
  set_output_directory(${name}
      BINARY_DIR ${SWIFT_RUNTIME_OUTPUT_INTDIR}
      LIBRARY_DIR ${SWIFT_LIBRARY_OUTPUT_INTDIR})

  swift_common_llvm_config("${name}" ${SWIFTEXE_SINGLE_LLVM_LINK_COMPONENTS})

  # NOTE(compnerd) use the C linker language to invoke `clang` rather than
  # `clang++` as we explicitly link against the C++ runtime.  We were previously
  # actually passing `-nostdlib++` to avoid the C++ runtime linkage.
  if(${SWIFTEXE_SINGLE_SDK} STREQUAL ANDROID)
    set_property(TARGET "${name}" PROPERTY
      LINKER_LANGUAGE "C")
  else()
    set_property(TARGET "${name}" PROPERTY
      LINKER_LANGUAGE "CXX")
  endif()

  set_target_properties(${name} PROPERTIES FOLDER "Swift executables")
endfunction()

# Add an executable for each target variant. Executables are given suffixes
# with the variant SDK and ARCH.
#
# See add_swift_executable for detailed documentation.
function(add_swift_target_executable name)
  # Parse the arguments we were given.
  cmake_parse_arguments(SWIFTEXE_TARGET
    "EXCLUDE_FROM_ALL;;BUILD_WITH_STDLIB"
    ""
    "DEPENDS;LLVM_LINK_COMPONENTS;LINK_LIBRARIES"
    ${ARGN})

  set(SWIFTEXE_TARGET_SOURCES ${SWIFTEXE_TARGET_UNPARSED_ARGUMENTS})

  if(SWIFTEXE_TARGET_EXCLUDE_FROM_ALL)
    message(SEND_ERROR "${name} is using EXCLUDE_FROM_ALL which is deprecated.")
  endif()

  # All Swift executables depend on the standard library.
  list(APPEND SWIFTEXE_TARGET_LINK_LIBRARIES swiftCore)
  # All Swift executables depend on the swiftSwiftOnoneSupport library.
  list(APPEND SWIFTEXE_TARGET_DEPENDS swiftSwiftOnoneSupport)

  foreach(sdk ${SWIFT_SDKS})
    foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
      set(VARIANT_SUFFIX "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
      set(VARIANT_NAME "${name}${VARIANT_SUFFIX}")

      if(SWIFTEXE_TARGET_BUILD_WITH_STDLIB)
        add_dependencies("swift-test-stdlib${VARIANT_SUFFIX}" ${VARIANT_NAME})
      endif()

      # Don't add the ${arch} to the suffix.  We want to link against fat
      # libraries.
      _list_add_string_suffix(
          "${SWIFTEXE_TARGET_DEPENDS}"
          "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}"
          SWIFTEXE_TARGET_DEPENDS_with_suffix)
      _add_swift_target_executable_single(
          ${VARIANT_NAME}
          ${SWIFTEXE_TARGET_SOURCES}
          DEPENDS ${SWIFTEXE_TARGET_DEPENDS_with_suffix}
          LLVM_LINK_COMPONENTS ${SWIFTEXE_TARGET_LLVM_LINK_COMPONENTS}
          SDK "${sdk}"
          ARCHITECTURE "${arch}")

      _list_add_string_suffix(
          "${SWIFTEXE_TARGET_LINK_LIBRARIES}"
          "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}"
          SWIFTEXE_TARGET_LINK_LIBRARIES_TARGETS)
      target_link_libraries(${VARIANT_NAME} PRIVATE
          ${SWIFTEXE_TARGET_LINK_LIBRARIES_TARGETS})

      if(NOT "${VARIANT_SUFFIX}" STREQUAL "${SWIFT_PRIMARY_VARIANT_SUFFIX}")
        # By default, don't build executables for target SDKs to avoid building
        # target stdlibs.
        set_target_properties(${VARIANT_NAME} PROPERTIES
          EXCLUDE_FROM_ALL TRUE)
      endif()

      if(${sdk} IN_LIST SWIFT_APPLE_PLATFORMS)
        add_custom_command_target(unused_var2
         COMMAND "codesign" "-f" "-s" "-" "${SWIFT_RUNTIME_OUTPUT_INTDIR}/${VARIANT_NAME}"
         CUSTOM_TARGET_NAME "${VARIANT_NAME}_signed"
         OUTPUT "${SWIFT_RUNTIME_OUTPUT_INTDIR}/${VARIANT_NAME}_signed"
         DEPENDS ${VARIANT_NAME})
      else()
        # No code signing on other platforms.
        add_custom_command_target(unused_var2
         CUSTOM_TARGET_NAME "${VARIANT_NAME}_signed"
         OUTPUT "${SWIFT_RUNTIME_OUTPUT_INTDIR}/${VARIANT_NAME}_signed"
         DEPENDS ${VARIANT_NAME})
       endif()
    endforeach()
  endforeach()
endfunction()

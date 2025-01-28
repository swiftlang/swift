
# TODO(compnerd) once we have a newer CMake we should be able to use the new
# `Swift` LANGUAGE support in CMake to simplify the build.  For now, just add
# the project so that it is possible to start working on extracting the Swift
# standard library from the Swift compiler build.
project(swift-stdlib LANGUAGES C CXX)

# CMake passes `-external:I` to clang-cl which results in the search order being
# altered, and this impacts the definitions of the intrinsics. When building
# with a MSVC toolset 19.29.30036.3 or newer, this will prevent the runtime from
# being built on Windows.  Since we know that we only support `clang-cl` as the
# compiler for the runtime due to the use of the Swift calling convention, we
# simply override the CMake behaviour unconditionally.
set(CMAKE_INCLUDE_SYSTEM_FLAG_C "-Isystem")
set(CMAKE_INCLUDE_SYSTEM_FLAG_CXX "-Isystem")

# Add path for custom CMake modules.
list(APPEND CMAKE_MODULE_PATH
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

if(SWIFT_BUILD_SDK_OVERLAY OR SWIFT_INCLUDE_TESTS)
  set(SWIFT_BUILD_TEST_SUPPORT_MODULES_default TRUE)
else()
  set(SWIFT_BUILD_TEST_SUPPORT_MODULES_default FALSE)
endif()

# TODO(compnerd) use a target to avoid re-creating this file all the time
function(generate_windows_vfs_overlay)
  file(TO_CMAKE_PATH ${VCToolsInstallDir} VCToolsInstallDir)
  file(TO_CMAKE_PATH ${UniversalCRTSdkDir} UniversalCRTSdkDir)
  configure_file("${PROJECT_SOURCE_DIR}/cmake/WindowsVFS.yaml.in"
                 "${CMAKE_CURRENT_BINARY_DIR}/windows-vfs-overlay.yaml"
                 @ONLY)
endfunction()
if(WINDOWS IN_LIST SWIFT_SDKS)
  generate_windows_vfs_overlay()
  file(TO_CMAKE_PATH "${CMAKE_CURRENT_BINARY_DIR}/windows-vfs-overlay.yaml"
       SWIFT_WINDOWS_VFS_OVERLAY)
endif()

#
# User-configurable options for the standard library.
#

# New options should be added to stdlib/cmake/modules/StdlibOptions.cmake,
# so that they are considered in configurations using StandaloneOverlay.cmake

# NOTE: Some of these variables are also initialized in StandaloneOverlay.cmake
# so that interfaces are emitted when overlays are separately built.

# TODO: migrate this section to StdlibOptions.cmake to reduce duplication

option(SWIFT_BUILD_TEST_SUPPORT_MODULES
       "Whether to build StdlibUnittest and other test support modules. Defaults to On when SWIFT_BUILD_SDK_OVERLAY is On, or when SWIFT_INCLUDE_TESTS is On."
       "${SWIFT_BUILD_TEST_SUPPORT_MODULES_default}")

# New options should be added to stdlib/cmake/modules/StdlibOptions.cmake,
# so that they are considered in configurations using StandaloneOverlay.cmake
include(StdlibOptions)

#
# End of user-configurable options.
#

set(SWIFT_STDLIB_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

# Remove llvm-project/llvm/include directory from -I search part when building
# the stdlib. We have our own fork of LLVM includes (Support, ADT) in
# stdlib/include/llvm and we don't want to silently use headers from LLVM.
get_property(stdlib_include_directories DIRECTORY PROPERTY INCLUDE_DIRECTORIES)
list(REMOVE_ITEM stdlib_include_directories "${LLVM_MAIN_INCLUDE_DIR}")
set_property(DIRECTORY PROPERTY INCLUDE_DIRECTORIES ${stdlib_include_directories})

include(AddSwiftStdlib)

# Create convenience targets for the Swift standard library.

# NOTE(compnerd) this will pass the *build* configuration to the *host*
# libraries.  Explicitly indicate to CMake that it should **NOT** track the
# implicit language runtimes.  This can go away once we migrate to an external
# project with its own configure with the CMAKE_SYSTEM_NAME set rather than
# using the custom cross-compilation solution
set(CMAKE_C_IMPLICIT_LINK_LIBRARIES "")
set(CMAKE_CXX_IMPLICIT_LINK_LIBRARIES "")

set(CMAKE_C_IMPLICIT_LINK_DIRECTORIES "")
set(CMAKE_CXX_IMPLICIT_LINK_DIRECTORIES "")

set(CMAKE_C_VISIBILITY_PRESET hidden)
set(CMAKE_CXX_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN YES)

if(SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER)
  if(NOT "${CMAKE_C_COMPILER_ID}" MATCHES "Clang")
    message(FATAL_ERROR "Building the swift runtime is not supported with ${CMAKE_C_COMPILER_ID}. Use the just-built clang instead.")
  else()
    message(WARNING "Building the swift runtime using the host compiler, and not the just-built clang.")
  endif()
else()
  # If we use Clang-cl or MSVC, CMake provides default compiler and linker flags that are incompatible
  # with the frontend of Clang or Clang++.
  if(SWIFT_COMPILER_IS_MSVC_LIKE)
    set(CMAKE_CXX_COMPILER "${SWIFT_NATIVE_CLANG_TOOLS_PATH}/clang-cl")
    set(CMAKE_C_COMPILER "${SWIFT_NATIVE_CLANG_TOOLS_PATH}/clang-cl")
  else()
    set(CMAKE_CXX_COMPILER "${SWIFT_NATIVE_CLANG_TOOLS_PATH}/clang++")
    set(CMAKE_C_COMPILER "${SWIFT_NATIVE_CLANG_TOOLS_PATH}/clang")
  endif()

  if(CMAKE_C_COMPILER_LAUNCHER MATCHES ".*distcc")
    set(CMAKE_C_COMPILER_LAUNCHER "")
  endif()
  if(CMAKE_CXX_COMPILER_LAUNCHER MATCHES ".*distcc")
    set(CMAKE_CXX_COMPILER_LAUNCHER "")
  endif()

  # The sanitizers require using the same version of the compiler for
  # everything and there are various places where we link runtime code with
  # code built by the host compiler. Disable sanitizers for the runtime for
  # now.
  add_compile_options(-fno-sanitize=all)
endif()

# Do not enforce checks for LLVM's ABI-breaking build settings.
# The Swift runtime uses some header-only code from LLVM's ADT classes,
# but we do not want to link libSupport into the runtime. These checks rely
# on the presence of symbols in libSupport to identify how the code was
# built and cause link failures for mismatches. Without linking that library,
# we get link failures regardless, so instead, this just disables the checks.
add_compile_definitions($<$<OR:$<COMPILE_LANGUAGE:C>,$<COMPILE_LANGUAGE:CXX>>:LLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1>)

set(SWIFT_STDLIB_LIBRARY_BUILD_TYPES)
if(SWIFT_BUILD_DYNAMIC_STDLIB)
  list(APPEND SWIFT_STDLIB_LIBRARY_BUILD_TYPES SHARED)
endif()
if(SWIFT_BUILD_STATIC_STDLIB)
  list_intersect("${SWIFT_DARWIN_PLATFORMS}" "${SWIFT_SDKS}" building_darwin_sdks)
  if(building_darwin_sdks)
    message(SEND_ERROR "cannot build static standard library for Darwin SDKs")
  else()
    list(APPEND SWIFT_STDLIB_LIBRARY_BUILD_TYPES STATIC)
  endif()
endif()

function(swift_create_stdlib_targets name variant define_all_alias)
  if(NOT variant STREQUAL "")
    set(variant "-${variant}")
  endif()

  if(define_all_alias)
    add_custom_target(${name}${variant}-all)
    set_target_properties(${name}${variant}-all
      PROPERTIES
      FOLDER "Swift libraries/Aggregate")
  endif()

  foreach(sdk ${SWIFT_SDKS})
    add_custom_target(${name}-${SWIFT_SDK_${sdk}_LIB_SUBDIR}${variant})
    set_target_properties(${name}-${SWIFT_SDK_${sdk}_LIB_SUBDIR}${variant}
      PROPERTIES
      FOLDER "Swift libraries/Aggregate")

    foreach(arch ${SWIFT_SDK_${sdk}_ARCHITECTURES})
      set(target_variant -${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch})

      add_custom_target(${name}${target_variant}${variant})
      set_target_properties(${name}${target_variant}${variant}
        PROPERTIES
        FOLDER "Swift libraries/Aggregate")
      if(define_all_alias)
        add_dependencies(${name}${variant}-all
          ${name}${target_variant}${variant})
      endif()
      add_dependencies(${name}-${SWIFT_SDK_${sdk}_LIB_SUBDIR}${variant}
        ${name}${target_variant}${variant})
    endforeach()
  endforeach()

  if(NOT define_all_alias)
    set(ALL_keyword ALL)
  endif()

  # When cross-compiling host tools for multiple architectures, targeting a
  # different SDK, the primary variant is not one of the variants being built,
  # so it can't be added as a target here. build-script will invoke the
  # more-specific target, so just skip creating this target and warn in case
  # someone is using the CMake more directly.
  if(SWIFT_PRIMARY_VARIANT_SDK IN_LIST SWIFT_SDKS)
    add_custom_target(${name}${variant}
      ${ALL_keyword}
      DEPENDS
      ${name}${SWIFT_PRIMARY_VARIANT_SUFFIX}${variant})
    set_target_properties(${name}${variant}
      PROPERTIES
      FOLDER "Swift libraries/Aggregate")
  else()
    message(WARNING "Primary variant ${SWIFT_PRIMARY_VARIANT_SDK} is not being built, not creating ${name}${variant} alias target for it.")
  endif()
endfunction()

if("${SWIFT_CONCURRENCY_GLOBAL_EXECUTOR}" STREQUAL "singlethreaded"
   AND NOT SWIFT_STDLIB_SINGLE_THREADED_CONCURRENCY)
  message(SEND_ERROR "Cannot enable the single-threaded global executor without enabling SWIFT_STDLIB_SINGLE_THREADED_CONCURRENCY")
endif()

if(SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY)
  if(SWIFT_STDLIB_SINGLE_THREADED_CONCURRENCY)
    message(SEND_ERROR "Cannot use the single-threaded concurrency with task-to-thread concurrency model")
  endif()
  if(NOT "${SWIFT_CONCURRENCY_GLOBAL_EXECUTOR}" STREQUAL "none")
    message(SEND_ERROR "Task-to-thread concurrency model requires no global executor")
  endif()
else()
  if("${SWIFT_CONCURRENCY_GLOBAL_EXECUTOR}" STREQUAL "none")
    message(SEND_ERROR "Only task-to-thread concurrency model is usable with no global executor")
  endif()
endif()

swift_create_stdlib_targets("swift-stdlib" "" TRUE)
if(SWIFT_STDLIB_ENABLE_SIB_TARGETS)
  swift_create_stdlib_targets("swift-stdlib" "sib" TRUE)
  swift_create_stdlib_targets("swift-stdlib" "sibopt" TRUE)
  swift_create_stdlib_targets("swift-stdlib" "sibgen" TRUE)
endif()
swift_create_stdlib_targets("swift-test-stdlib" "" FALSE)

# Create the libexec targets
swift_create_stdlib_targets("swift-libexec" "" TRUE)
swift_create_stdlib_targets("swift-test-libexec" "" FALSE)

# FIXME: Include the toolchain directory before the public directory. Otherwise
# the clang resource directory symlink stops installing correctly.
add_subdirectory(toolchain)
add_subdirectory(public)
if(SWIFT_STDLIB_BUILD_PRIVATE)
  add_subdirectory(private)
endif()
add_subdirectory(tools)

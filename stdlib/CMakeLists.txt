
# TODO(compnerd) once we have a newer CMake we should be able to use the new
# `Swift` LANGUAGE support in CMake to simplify the build.  For now, just add
# the project so that it is possible to start working on extracting the Swift
# standard library from the Swift compiler build.
project(swift-stdlib LANGUAGES C CXX)

# NOTE: This is meant to provide a transition from the toolchain-integrated
# build to the standalone build.  This variable should be replaced with
# `CMAKE_BUILD_TYPE`.  Currently, although this defaults to `CMAKE_BUILD_TYPE`,
# it is a cache variable, allowing the user to override the value by explicitly
# specifying `SWIFT_STDLIB_BUILD_TYPE`.
set(SWIFT_STDLIB_BUILD_TYPE "${CMAKE_BUILD_TYPE}" CACHE STRING
  "Build type for the Swift standard library and SDK overlays [Debug, RelWithDebInfo, Release, MinSizeRel]")

if(DEFINED SWIFT_STDLIB_ASSERTIONS)
  # NOTE: this clause is meant to help transition away from use of
  # `SWIFT_STDLIB_ASSERTIONS`.  This was used in the code base along with
  # `LLVM_ENABLE_ASSERTIONS`.  Rather than have two different options,
  # consolidate into a single option.
  message(WARNING "Use of `SWIFT_STDLIB_ASSERTIONS` to control assertions in the swift standard library is deprecated and will be removed in a future release.  Please use `SWIFT_RUNTIME_ENABLE_ASSERTIONS` to toggle the assertions behaviour of the Swift runtime")
  set(SWIFT_RUNTIME_ENABLE_ASSERTIONS_default ${SWIFT_STDLIB_ASSERTIONS})
elseif(DEFINED LLVM_ENABLE_ASSERTIONS)
  # NOTE: this clause is meant to help transition away from use of
  # `LLVM_ENABLE_ASSERTIONS`.  This was used in the code base along with
  # `SWIFT_STDLIB_ASSERTIONS`.  Rather than have two different options,
  # consolidate into a single option.
  message(WARNING "Use of `LLVM_ENABLE_ASSERTIONS` to control assertions in the swift standard library is deprecated and will be removed in a future release.  Please use `SWIFT_RUNTIME_ENABLE_ASSERTIONS` to toggle the assertions behaviour of the Swift runtime")
  set(SWIFT_RUNTIME_ENABLE_ASSERTIONS_default ${LLVM_ENABLE_ASSERTIONS})
else()
  # NOTE: this clause can be removed when the standard library becomes a fully
  # standalone build.
  if(SWIFT_STDLIB_BUILD_TYPE MATCHES DEBUG)
    set(SWIFT_RUNTIME_ENABLE_ASSERTIONS_default YES)
  else()
    set(SWIFT_RUNTIME_ENABLE_ASSERTIONS_default NO)
  endif()
endif()
option(SWIFT_RUNTIME_ENABLE_ASSERTIONS
  "Unexpected conditions in the runtime will assert rather than silently continue"
  ${SWIFT_RUNTIME_ENABLE_ASSERTIONS_default})

option(SWIFT_RUNTIME_CLOBBER_FREED_OBJECTS
  "Ovewrite memory for deallocated Swift objects"
  ${SWIFT_RUNTIME_ENABLE_ASSERTIONS})

# Add path for custom CMake modules.
list(APPEND CMAKE_MODULE_PATH
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

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
    set(CMAKE_CXX_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang-cl")
    set(CMAKE_C_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang-cl")
  else()
    set(CMAKE_CXX_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang++")
    set(CMAKE_C_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang")
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
  list_intersect("${SWIFT_APPLE_PLATFORMS}" "${SWIFT_SDKS}" building_darwin_sdks)
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
  add_custom_target(${name}${variant}
    ${ALL_keyword}
    DEPENDS
    ${name}${SWIFT_PRIMARY_VARIANT_SUFFIX}${variant})
  set_target_properties(${name}${variant}
    PROPERTIES
    FOLDER "Swift libraries/Aggregate")
endfunction()

swift_create_stdlib_targets("swift-stdlib" "" TRUE)
if(SWIFT_STDLIB_ENABLE_SIB_TARGETS)
  swift_create_stdlib_targets("swift-stdlib" "sib" TRUE)
  swift_create_stdlib_targets("swift-stdlib" "sibopt" TRUE)
  swift_create_stdlib_targets("swift-stdlib" "sibgen" TRUE)
endif()
swift_create_stdlib_targets("swift-test-stdlib" "" FALSE)

# FIXME: Include the toolchain directory before the public directory. Otherwise
# the clang resource directory symlink stops installing correctly.
add_subdirectory(toolchain)
add_subdirectory(public)
add_subdirectory(private)
add_subdirectory(tools)

# Create convenience targets for the Swift standard library.

# NOTE(compnerd) save the original compiler for the host swiftReflection that
# we build
set(HOST_CMAKE_CXX_COMPILER ${CMAKE_CXX_COMPILER})
set(HOST_CMAKE_C_COMPILER_INITIAL ${CMAKE_C_COMPILER})

if(SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER)
  if((NOT "${CMAKE_C_COMPILER_ID}" STREQUAL "Clang") AND
     (NOT "${CMAKE_C_COMPILER_ID}" STREQUAL "AppleClang"))
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

  set(CMAKE_CXX_COMPILER_ARG1 "")
  set(CMAKE_C_COMPILER_ARG1 "")
  # The sanitizers require using the same version of the compiler for
  # everything and there are various places where we link runtime code with
  # code built by the host compiler. Disable sanitizers for the runtime for
  # now.
  append("-fno-sanitize=all" CMAKE_C_FLAGS CMAKE_CXX_FLAGS)
endif()

# Do not enforce checks for LLVM's ABI-breaking build settings.
# The Swift runtime uses some header-only code from LLVM's ADT classes,
# but we do not want to link libSupport into the runtime. These checks rely
# on the presence of symbols in libSupport to identify how the code was
# built and cause link failures for mismatches. Without linking that library,
# we get link failures regardless, so instead, this just disables the checks.
append("-DLLVM_DISABLE_ABI_BREAKING_CHECKS_ENFORCING=1" CMAKE_C_FLAGS CMAKE_CXX_FLAGS)

set(SWIFT_STDLIB_LIBRARY_BUILD_TYPES)
if(SWIFT_BUILD_DYNAMIC_STDLIB)
  list(APPEND SWIFT_STDLIB_LIBRARY_BUILD_TYPES SHARED)
endif()
if(SWIFT_BUILD_STATIC_STDLIB)
  list(APPEND SWIFT_STDLIB_LIBRARY_BUILD_TYPES STATIC)
endif()

include(AddSwiftStdlib)

swift_create_stdlib_targets("swift-stdlib" "" TRUE)
swift_create_stdlib_targets("swift-stdlib" "sib" TRUE)
swift_create_stdlib_targets("swift-stdlib" "sibopt" TRUE)
swift_create_stdlib_targets("swift-stdlib" "sibgen" TRUE)
swift_create_stdlib_targets("swift-test-stdlib" "" FALSE)

add_subdirectory(public)
add_subdirectory(private)


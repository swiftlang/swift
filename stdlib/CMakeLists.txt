# Create convenience targets for the Swift standard library.


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
swift_create_stdlib_targets("swift-stdlib" "sib" TRUE)
swift_create_stdlib_targets("swift-stdlib" "sibopt" TRUE)
swift_create_stdlib_targets("swift-stdlib" "sibgen" TRUE)
swift_create_stdlib_targets("swift-test-stdlib" "" FALSE)

add_subdirectory(public)
add_subdirectory(private)


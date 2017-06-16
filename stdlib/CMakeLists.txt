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

add_custom_target(swift-stdlib-all)
add_custom_target(swift-stdlib-sib-all)
add_custom_target(swift-stdlib-sibopt-all)
add_custom_target(swift-stdlib-sibgen-all)
foreach(SDK ${SWIFT_SDKS})
  add_custom_target("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}")
  add_custom_target("swift-test-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}")
  add_custom_target("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-sib")
  add_custom_target("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-sibopt")
  add_custom_target("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-sibgen")
  foreach(ARCH ${SWIFT_SDK_${SDK}_ARCHITECTURES})
    set(VARIANT_SUFFIX "-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-${ARCH}")
    add_custom_target("swift-stdlib${VARIANT_SUFFIX}")
    add_custom_target("swift-stdlib${VARIANT_SUFFIX}-sib")
    add_custom_target("swift-stdlib${VARIANT_SUFFIX}-sibopt")
    add_custom_target("swift-stdlib${VARIANT_SUFFIX}-sibgen")
    add_custom_target("swift-test-stdlib${VARIANT_SUFFIX}")

    add_dependencies(swift-stdlib-all "swift-stdlib${VARIANT_SUFFIX}")
    add_dependencies(swift-stdlib-sib-all "swift-stdlib${VARIANT_SUFFIX}-sib")
    add_dependencies(swift-stdlib-sibopt-all "swift-stdlib${VARIANT_SUFFIX}-sibopt")
    add_dependencies(swift-stdlib-sibgen-all "swift-stdlib${VARIANT_SUFFIX}-sibgen")

    add_dependencies("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}"
      "swift-stdlib${VARIANT_SUFFIX}")
    add_dependencies("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-sib"
      "swift-stdlib${VARIANT_SUFFIX}-sib")
    add_dependencies("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-sibopt"
        "swift-stdlib${VARIANT_SUFFIX}-sibopt")
    add_dependencies("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-sibgen"
        "swift-stdlib${VARIANT_SUFFIX}-sibgen")

    add_dependencies("swift-test-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}"
        "swift-test-stdlib${VARIANT_SUFFIX}")
  endforeach()
endforeach()
add_custom_target(swift-stdlib
    DEPENDS "swift-stdlib${SWIFT_PRIMARY_VARIANT_SUFFIX}")
add_custom_target(swift-stdlib-sib
  DEPENDS "swift-stdlib${SWIFT_PRIMARY_VARIANT_SUFFIX}-sib")
add_custom_target(swift-stdlib-sibopt
  DEPENDS "swift-stdlib${SWIFT_PRIMARY_VARIANT_SUFFIX}-sibopt")
add_custom_target(swift-stdlib-sibgen
    DEPENDS "swift-stdlib${SWIFT_PRIMARY_VARIANT_SUFFIX}-sibgen")
add_custom_target(swift-test-stdlib ALL
    DEPENDS "swift-test-stdlib${SWIFT_PRIMARY_VARIANT_SUFFIX}")

if(SWIFT_STDLIB_ENABLE_RESILIENCE)
  set(STDLIB_SIL_SERIALIZE_ALL)
  set(STDLIB_SIL_SERIALIZE_WITNESS_TABLES)
else()
  if(SWIFT_STDLIB_SIL_SERIALIZE_ALL)
    set(STDLIB_SIL_SERIALIZE_ALL "-Xfrontend" "-sil-serialize-all")
    set(STDLIB_SIL_SERIALIZE_WITNESS_TABLES "-Xfrontend" "-sil-serialize-witness-tables")
  else()
    set(STDLIB_SIL_SERIALIZE_ALL)
    set(STDLIB_SIL_SERIALIZE_WITNESS_TABLES)
  endif()
endif()

add_subdirectory(public)
add_subdirectory(internal)
add_subdirectory(private)


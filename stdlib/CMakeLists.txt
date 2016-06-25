# Create convenience targets for the Swift standard library.


if(SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER)
  message(WARNING "Building the swift runtime using the host compiler, and not the just-built clang.")
else()
  set(CMAKE_CXX_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang++")
  set(CMAKE_C_COMPILER "${SWIFT_NATIVE_LLVM_TOOLS_PATH}/clang")
  set(CMAKE_CXX_COMPILER_ARG1 "")
  set(CMAKE_C_COMPILER_ARG1 "")
endif()

set(SWIFT_STDLIB_LIBRARY_BUILD_TYPES)
if(SWIFT_BUILD_DYNAMIC_STDLIB)
  list(APPEND SWIFT_STDLIB_LIBRARY_BUILD_TYPES SHARED)
endif()
if(SWIFT_BUILD_STATIC_STDLIB)
  list(APPEND SWIFT_STDLIB_LIBRARY_BUILD_TYPES STATIC)
endif()

add_custom_target(swift-stdlib-all)
foreach(SDK ${SWIFT_SDKS})
  add_custom_target("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}")
  add_custom_target("swift-test-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}")
  foreach(ARCH ${SWIFT_SDK_${SDK}_ARCHITECTURES})
    set(VARIANT_SUFFIX "-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-${ARCH}")
    add_custom_target("swift-stdlib${VARIANT_SUFFIX}")
    add_custom_target("swift-test-stdlib${VARIANT_SUFFIX}")
    add_dependencies(swift-stdlib-all "swift-stdlib${VARIANT_SUFFIX}")
    add_dependencies("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}"
        "swift-stdlib${VARIANT_SUFFIX}")
    add_dependencies("swift-test-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}"
        "swift-test-stdlib${VARIANT_SUFFIX}")
  endforeach()
endforeach()
add_custom_target(swift-stdlib
    DEPENDS "swift-stdlib${SWIFT_PRIMARY_VARIANT_SUFFIX}")
add_custom_target(swift-test-stdlib ALL
    DEPENDS "swift-test-stdlib${SWIFT_PRIMARY_VARIANT_SUFFIX}")

if(SWIFT_STDLIB_ENABLE_RESILIENCE)
  set(STDLIB_SIL_SERIALIZE_ALL)
else()
  if(SWIFT_STDLIB_SIL_SERIALIZE_ALL)
    set(STDLIB_SIL_SERIALIZE_ALL "-Xfrontend" "-sil-serialize-all")
  else()
    set(STDLIB_SIL_SERIALIZE_ALL)
  endif()
endif()

add_subdirectory(public)
add_subdirectory(internal)
add_subdirectory(private)


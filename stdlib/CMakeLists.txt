# Create convenience targets for the Swift standard library.
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

if(SWIFT_HOST_VARIANT STREQUAL "linux")
  find_package(BSD REQUIRED)
endif()

if(SWIFT_STDLIB_ENABLE_RESILIENCE)
  set(STDLIB_SIL_SERIALIZE_ALL)
else()
  set(STDLIB_SIL_SERIALIZE_ALL "-Xfrontend" "-sil-serialize-all")
endif()

add_subdirectory(public)
add_subdirectory(internal)
add_subdirectory(private)


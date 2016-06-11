# Create convenience targets for the Swift standard library.
add_custom_target(swift-stdlib-all)
foreach(SDK ${SWIFT_SDKS})
  add_custom_target("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}")
  foreach(ARCH ${SWIFT_SDK_${SDK}_ARCHITECTURES})
    set(VARIANT_SUFFIX "-${SWIFT_SDK_${SDK}_LIB_SUBDIR}-${ARCH}")
    add_custom_target("swift-stdlib${VARIANT_SUFFIX}")
    add_dependencies(swift-stdlib-all "swift-stdlib${VARIANT_SUFFIX}")
    add_dependencies("swift-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}"
        "swift-stdlib${VARIANT_SUFFIX}")
  endforeach()
endforeach()
add_custom_target(swift-stdlib ALL
    DEPENDS "swift-stdlib${SWIFT_PRIMARY_VARIANT_SUFFIX}")

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


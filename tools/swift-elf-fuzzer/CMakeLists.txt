add_swift_fuzzer_host_tool(swift-elf-fuzzer
  swift-elf-fuzzer.c
  SWIFT_COMPONENT compiler
  )

set(sdk "${SWIFT_HOST_VARIANT_SDK}")
set(arch "${SWIFT_HOST_VARIANT_ARCH}")
set(variant_suffix "-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")

target_link_libraries(swift-elf-fuzzer
                      PRIVATE
                        "swiftRuntime${variant_suffix}")
add_dependencies(swift-elf-fuzzer "swiftRuntime${variant_suffix}")

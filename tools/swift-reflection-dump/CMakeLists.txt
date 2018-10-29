set(LLVM_LINK_COMPONENTS Support Object)
add_swift_host_tool(swift-reflection-dump
  swift-reflection-dump.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(swift-reflection-dump
                      PRIVATE
                        swiftReflection-${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}-${SWIFT_HOST_VARIANT_ARCH})

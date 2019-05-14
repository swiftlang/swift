add_swift_host_tool(swift-reflection-dump
  swift-reflection-dump.cpp
  LLVM_LINK_COMPONENTS object support
  SWIFT_COMPONENTS tools
)
target_link_libraries(swift-reflection-dump
                      PRIVATE
                        swiftReflection)

set(LLVM_LINK_COMPONENTS Object Support)
add_swift_host_tool(swift-reflection-dump
  swift-reflection-dump.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(swift-reflection-dump
                      PRIVATE
                        swiftReflection)

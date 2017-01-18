add_swift_host_tool(swift-reflection-dump
  swift-reflection-dump.cpp
  LINK_FAT_LIBRARIES
    swiftReflection
  LLVM_COMPONENT_DEPENDS object support
  SWIFT_COMPONENT tools
)

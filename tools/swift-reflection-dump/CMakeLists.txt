add_swift_executable(swift-reflection-dump
  swift-reflection-dump.cpp
  LINK_FAT_LIBRARIES
    swiftReflection
  LLVM_COMPONENT_DEPENDS object support
)

swift_install_in_component(tools
    TARGETS swift-reflection-dump
    RUNTIME DESTINATION bin)


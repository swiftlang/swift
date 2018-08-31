add_swift_host_tool(swift-demangle-yamldump
  swift-demangle-yamldump.cpp
  LINK_LIBRARIES swiftDemangling
  LLVM_COMPONENT_DEPENDS support
  SWIFT_COMPONENT tools
  )

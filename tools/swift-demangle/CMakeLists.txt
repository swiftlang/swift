add_swift_host_tool(swift-demangle
  swift-demangle.cpp
  LINK_LIBRARIES swiftDemangling
  LLVM_COMPONENT_DEPENDS support
  SWIFT_COMPONENT compiler
  )

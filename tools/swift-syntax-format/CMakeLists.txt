add_swift_host_tool(swift-syntax-format
  swift-syntax-format.cpp
  LINK_LIBRARIES
    swiftAST
    swiftDriver
    swiftFrontend
    swiftSyntax
  LLVM_COMPONENT_DEPENDS
    Support
  SWIFT_COMPONENT tools
)

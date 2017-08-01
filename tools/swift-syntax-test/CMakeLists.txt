add_swift_host_tool(swift-syntax-test
  swift-syntax-test.cpp
  DEPENDS
    swift-syntax-generated-headers
  LINK_LIBRARIES
    swiftAST
    swiftDriver
    swiftFrontend
    swiftSema
    swiftSyntax
  LLVM_COMPONENT_DEPENDS
    Support
  SWIFT_COMPONENT tools
)

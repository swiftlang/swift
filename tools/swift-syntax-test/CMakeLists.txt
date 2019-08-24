add_swift_host_tool(swift-syntax-test
  swift-syntax-test.cpp
  LLVM_LINK_COMPONENTS
    Support
  SWIFT_COMPONENT tools
)
target_link_libraries(swift-syntax-test
                      PRIVATE
                        swiftAST
                        swiftDriver
                        swiftFrontend
                        swiftSema
                        swiftSyntax)

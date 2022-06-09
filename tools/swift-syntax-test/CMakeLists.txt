add_swift_host_tool(swift-syntax-test
  swift-syntax-test.cpp
  HAS_SWIFT_MODULES
  LLVM_LINK_COMPONENTS
    Support
  SWIFT_COMPONENT testsuite-tools
)
target_link_libraries(swift-syntax-test
                      PRIVATE
                        swiftAST
                        swiftDriver
                        swiftFrontend
                        swiftSema
                        swiftSyntax
                        swiftCompilerModules_SwiftSyntax)

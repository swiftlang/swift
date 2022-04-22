add_swift_host_tool(swift-ast-script
  ASTScriptConfiguration.cpp
  ASTScriptParser.cpp
  ASTScriptEvaluator.cpp
  swift-ast-script.cpp
  SWIFT_COMPONENT tools
  THINLTO_LD64_ADD_FLTO_CODEGEN_ONLY
)
target_link_libraries(swift-ast-script
                      PRIVATE
                        swiftAST
                        swiftFrontendTool)

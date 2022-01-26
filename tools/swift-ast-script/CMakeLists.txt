add_swift_host_tool(swift-ast-script
  ASTScriptConfiguration.cpp
  ASTScriptParser.cpp
  ASTScriptEvaluator.cpp
  swift-ast-script.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(swift-ast-script
                      PRIVATE
                        swiftAST
                        swiftFrontendTool)

target_thinlto_ld64_add_flto_codegen_only(swift-ast-script)

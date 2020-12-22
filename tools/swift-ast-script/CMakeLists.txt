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
if (NOT LLVM_ENABLE_ASSERTIONS)
  target_link_options(
    swift-ast-script
    PRIVATE
    "LINKER:-weak_library"
    "LINKER:$<TARGET_FILE:swiftExternalSILVerifier>")
endif()

add_swift_host_tool(swift-dependency-tool
  swift-dependency-tool.cpp
  SWIFT_COMPONENT tools
  THINLTO_LD64_ADD_FLTO_CODEGEN_ONLY
)
target_link_libraries(swift-dependency-tool
                      PRIVATE
                        swiftAST
                        swiftParse
                        swiftSerialization
                        swiftClangImporter)


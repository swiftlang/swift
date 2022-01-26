add_swift_host_tool(swift-dependency-tool
  swift-dependency-tool.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(swift-dependency-tool
                      PRIVATE
                        swiftAST
                        swiftParse
                        swiftClangImporter)

target_thinlto_ld64_add_flto_codegen_only(swift-dependency-tool)

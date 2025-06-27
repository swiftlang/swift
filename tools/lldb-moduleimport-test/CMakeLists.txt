add_swift_host_tool(lldb-moduleimport-test
  lldb-moduleimport-test.cpp
  SWIFT_COMPONENT testsuite-tools
  HAS_SWIFT_MODULES
)
target_link_libraries(lldb-moduleimport-test
                      PRIVATE
                        swiftAST
                        swiftASTSectionImporter
                        swiftClangImporter
                        swiftFrontend
                        swiftCompilerModules)


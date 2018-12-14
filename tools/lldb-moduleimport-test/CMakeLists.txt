add_swift_host_tool(lldb-moduleimport-test
  lldb-moduleimport-test.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(lldb-moduleimport-test
                      PRIVATE
                        swiftASTSectionImporter
                        swiftClangImporter
                        swiftDWARFImporter
                        swiftFrontend)


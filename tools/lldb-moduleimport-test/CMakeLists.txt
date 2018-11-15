add_swift_host_tool(lldb-moduleimport-test
  lldb-moduleimport-test.cpp
  LINK_LIBRARIES
    swiftASTSectionImporter swiftFrontend swiftClangImporter
  SWIFT_COMPONENT tools
)


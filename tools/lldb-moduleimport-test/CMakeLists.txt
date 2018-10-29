add_swift_host_tool(lldb-moduleimport-test
  lldb-moduleimport-test.cpp
  LLVM_COMPONENT_DEPENDS
    DebugInfoCodeView
  SWIFT_COMPONENT tools
)
target_link_libraries(lldb-moduleimport-test
                      PRIVATE
                        swiftASTSectionImporter
                        swiftFrontend
                        swiftClangImporter)

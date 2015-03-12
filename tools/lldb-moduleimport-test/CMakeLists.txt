add_swift_executable(lldb-moduleimport-test
  lldb-moduleimport-test.cpp
  LINK_LIBRARIES
    swiftASTSectionImporter swiftFrontend swiftClangImporter
  COMPONENT_DEPENDS
    bitreader bitwriter ipo objcarcopts option support ${LLVM_TARGETS_TO_BUILD})

swift_install_in_component(testsuite-tools
    TARGETS lldb-moduleimport-test
    RUNTIME DESTINATION "bin")


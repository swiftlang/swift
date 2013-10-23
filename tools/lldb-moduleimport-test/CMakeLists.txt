add_swift_executable(lldb-moduleimport-test
  lldb-moduleimport-test.cpp
  DEPENDS swiftASTSectionImporter swiftFrontend swiftClangImporter
  COMPONENT_DEPENDS bitreader bitwriter support ${LLVM_TARGETS_TO_BUILD})

target_link_libraries(swift)

install(TARGETS lldb-moduleimport-test
  RUNTIME DESTINATION bin)

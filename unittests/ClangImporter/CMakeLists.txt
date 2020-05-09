add_swift_unittest(SwiftClangImporterTests
  ClangImporterTests.cpp
)

target_link_libraries(SwiftClangImporterTests
    PRIVATE
    swiftClangImporter
    swiftParse
    swiftAST
)

add_swift_unittest(SwiftClangImporterTests
  ClangImporterTests.cpp
)

target_link_libraries(SwiftClangImporterTests
    PRIVATE
    swiftClangImporter
    swiftParse
    swiftAST
)

target_compile_definitions(SwiftClangImporterTests PRIVATE
  SWIFTLIB_DIR=\"${SWIFTLIB_DIR}\")

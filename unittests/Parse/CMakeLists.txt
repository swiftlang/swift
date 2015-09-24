add_swift_unittest(SwiftParseTests
  BuildConfigTests.cpp
  LexerTests.cpp
)

target_link_libraries(SwiftParseTests
    swiftClangImporter
    swiftSIL
    swiftSema
    swiftParse
    swiftAST)


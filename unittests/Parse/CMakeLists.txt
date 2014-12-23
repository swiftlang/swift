add_swift_unittest(SwiftParseTests
  LexerTests.cpp
)

target_link_libraries(SwiftParseTests
    swiftClangImporter
    swiftSIL
    swiftSema
    swiftParse
    swiftAST)


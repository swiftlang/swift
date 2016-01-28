add_swift_unittest(SwiftParseTests
  BuildConfigTests.cpp
  LexerTests.cpp
  TokenizerTests.cpp
)

target_link_libraries(SwiftParseTests
    swiftSIL
    swiftSema
    swiftClangImporter
    swiftParse
    swiftAST)


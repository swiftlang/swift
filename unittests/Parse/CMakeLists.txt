add_swift_unittest(SwiftParseTests
  BuildConfigTests.cpp
  LexerTests.cpp
  TokenizerTests.cpp
)

target_link_libraries(SwiftParseTests
    PRIVATE
    swiftParse
    swiftAST
    swiftClangImporter
    swiftSIL
    # FIXME: Sema must go last because of circular dependencies with AST.
    swiftSema
)

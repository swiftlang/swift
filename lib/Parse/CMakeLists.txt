add_swift_library(swiftParse STATIC
  Confusables.cpp
  Lexer.cpp
  ParseDecl.cpp
  ParseExpr.cpp
  ParseGeneric.cpp
  ParseIfConfig.cpp
  ParsePattern.cpp
  Parser.cpp
  ParseStmt.cpp
  ParseType.cpp
  PersistentParserState.cpp
  Scope.cpp
  SyntaxParsingContext.cpp
  LINK_LIBRARIES
    swiftSyntax
)

target_link_libraries(swiftParse INTERFACE swiftAST)

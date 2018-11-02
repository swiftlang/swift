add_swift_host_library(swiftParse STATIC
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
  SyntaxParsingCache.cpp
  SyntaxParsingContext.cpp
  LINK_LIBS
    swiftAST
    swiftSyntax
)


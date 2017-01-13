add_swift_library(swiftParse STATIC
  Lexer.cpp
  ParseDecl.cpp
  ParseExpr.cpp
  ParseGeneric.cpp
  ParsePattern.cpp
  Parser.cpp
  ParseSIL.cpp
  ParseStmt.cpp
  ParseType.cpp
  PersistentParserState.cpp
  Scope.cpp
  LINK_LIBRARIES
    swiftAST
    swiftSIL
    swiftSyntax
)


add_swift_library(swiftParse
  Confusables.cpp.gyb
  Lexer.cpp
  Parser.cpp
  ParseDecl.cpp
  ParseExpr.cpp
  ParseGeneric.cpp
  ParsePattern.cpp
  ParseSIL.cpp
  ParseStmt.cpp
  ParseType.cpp
  PersistentParserState.cpp
  Scope.cpp
  LINK_LIBRARIES
    swiftSIL
    swiftAST)


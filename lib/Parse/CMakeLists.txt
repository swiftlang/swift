add_swift_library(swiftParse
  Lexer.cpp
  Parser.cpp
  ParseDecl.cpp
  ParseExpr.cpp
  ParseGeneric.cpp
  ParsePattern.cpp
  ParseStmt.cpp
  ParseType.cpp
  Scope.cpp
  DEPENDS swiftAST)

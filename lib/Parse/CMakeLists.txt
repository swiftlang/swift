add_swift_library(swiftParse
  Lexer.cpp
  Parser.cpp
  Parser.h
  ParseDecl.cpp
  ParseExpr.cpp
  ParseGeneric.cpp
  ParsePattern.cpp
  ParseStmt.cpp
  ParseType.cpp
  Scope.cpp
  Scope.h
  DEPENDS swiftAST)

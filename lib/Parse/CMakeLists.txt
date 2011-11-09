add_swift_library(swiftParse
  Lexer.cpp
  Lexer.h
  Parser.cpp
  Parser.h
  ParseDecl.cpp
  ParseExpr.cpp
  ParseResult.h
  ParseStmt.cpp
  ParseType.cpp
  Scope.cpp
  Scope.h
  Token.h
  DEPENDS swiftAST)

add_swift_library(swiftParse
  Lexer.cpp
  Parser.cpp
  ParseDecl.cpp
  ParseExpr.cpp
  ParseStmt.cpp
  ParseType.cpp
  Scope.cpp
  DEPENDS swiftBasic swiftAST)

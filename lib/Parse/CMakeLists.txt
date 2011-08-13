add_swift_library(swiftParse
  Lexer.cpp
  Parser.cpp
  ParseExpr.cpp
  ParseStmt.cpp
  ParseType.cpp
  Scope.cpp
  Sema.cpp
  SemaBase.cpp
  SemaDecl.cpp
  DEPENDS swiftAST)

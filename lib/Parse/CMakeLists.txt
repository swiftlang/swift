add_swift_library(swiftParse
  Lexer.cpp
  Parser.cpp
  ParseType.cpp
  Scope.cpp
  Sema.cpp
  SemaBase.cpp
  SemaDecl.cpp
  SemaExpr.cpp
  DEPENDS swiftAST)

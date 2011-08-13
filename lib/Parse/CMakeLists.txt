add_swift_library(swiftParse
  Lexer.cpp
  Parser.cpp
  Scope.cpp
  Sema.cpp
  SemaBase.cpp
  SemaDecl.cpp
  SemaExpr.cpp
  SemaType.cpp
  DEPENDS swiftAST)

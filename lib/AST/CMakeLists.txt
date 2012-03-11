add_swift_library(swiftAST
  ASTContext.cpp
  ASTWalker.cpp
  Attr.cpp
  Builtins.cpp
  Decl.cpp
  Diagnostics.cpp
  DiagnosticEngine.cpp
  Expr.cpp
  Identifier.cpp
  Module.cpp
  Pattern.cpp
  PrettyStackTrace.cpp
  Stmt.cpp
  Type.cpp
  Verifier.cpp
  DEPENDS swiftBasic)


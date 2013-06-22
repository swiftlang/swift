add_swift_library(swiftAST
  ASTContext.cpp
  ASTPrinter.cpp
  ASTWalker.cpp
  Attr.cpp
  Builtins.cpp
  Decl.cpp
  DiagnosticList.cpp
  DiagnosticEngine.cpp
  Expr.cpp
  Identifier.cpp
  LookupVisibleDecls.cpp
  Module.cpp
  NameLookup.cpp
  Pattern.cpp
  PrettyStackTrace.cpp
  Stmt.cpp
  Type.cpp
  Verifier.cpp
  DEPENDS swiftBasic)

if( NOT SWIFT_BUILT_STANDALONE )
  add_dependencies(swiftAST intrinsics_gen)
endif()

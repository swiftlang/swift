add_swift_library(swiftAST
  ArchetypeBuilder.cpp
  ASTContext.cpp
  ASTDumper.cpp
  ASTNode.cpp
  ASTPrinter.cpp
  ASTWalker.cpp
  Attr.cpp
  Builtins.cpp
  CaptureInfo.cpp
  ConcreteDeclRef.cpp
  Decl.cpp
  DeclContext.cpp
  DiagnosticList.cpp
  DiagnosticEngine.cpp
  Expr.cpp
  Identifier.cpp
  LookupVisibleDecls.cpp
  Mangle.cpp
  Module.cpp
  ModuleNameLookup.cpp
  NameLookup.cpp
  Pattern.cpp
  PrettyStackTrace.cpp
  ProtocolConformance.cpp
  RawComment.cpp
  Stmt.cpp
  Type.cpp
  TypeRepr.cpp
  TypeWalker.cpp
  USRGeneration.cpp
  Verifier.cpp
  DEPENDS swiftBasic LLVMCore clangIndex)

if( NOT SWIFT_BUILT_STANDALONE )
  add_dependencies(swiftAST intrinsics_gen)
endif()

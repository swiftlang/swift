add_swift_library(swiftAST
  ArchetypeBuilder.cpp
  ASTContext.cpp
  ASTDumper.cpp
  ASTNode.cpp
  ASTPrinter.cpp
  ASTWalker.cpp
  Attr.cpp
  AvailabilitySpec.cpp
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
  PlatformKind.cpp
  PrettyStackTrace.cpp
  ProtocolConformance.cpp
  RawComment.cpp
  Stmt.cpp
  Substitution.cpp
  Type.cpp
  TypeRefinementContext.cpp
  TypeRepr.cpp
  TypeWalker.cpp
  USRGeneration.cpp
  Verifier.cpp
  LINK_LIBRARIES
    swiftReST
    swiftBasic

    # Clang dependencies.
    # FIXME: Clang should really export these in some reasonable manner.
    clangIndex
    clangFormat
    clangToolingCore
    clangFrontendTool
    clangFrontend
    clangDriver
    clangSerialization
    clangParse
    clangSema
    clangAnalysis
    clangEdit
    clangRewriteFrontend
    clangRewrite
    clangAST
    clangLex
    clangAPINotes
    clangBasic

  COMPONENT_DEPENDS
    profiledata instrumentation object mc mcparser bitreader option core
    support)

if(NOT SWIFT_BUILT_STANDALONE)
  add_dependencies(swiftAST intrinsics_gen)
endif()


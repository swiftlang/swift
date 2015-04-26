add_swift_library(swiftAST
  ArchetypeBuilder.cpp
  ASTContext.cpp
  ASTDumper.cpp
  ASTNode.cpp
  ASTPrinter.cpp
  ASTWalker.cpp
  Attr.cpp
  Availability.cpp
  AvailabilitySpec.cpp
  Builtins.cpp
  CaptureInfo.cpp
  ConcreteDeclRef.cpp
  Decl.cpp
  DeclContext.cpp
  DiagnosticList.cpp
  DiagnosticEngine.cpp
  DocComment.cpp
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
    swiftMarkup
    swiftBasic

    # Clang dependencies.
    # FIXME: Clang should really export these in some reasonable manner.
    clangCodeGen
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
    bitreader bitwriter irreader debuginfoDWARF
    profiledata instrumentation object objcarcopts mc mcparser
    bitreader bitwriter ipo option core support ${LLVM_TARGETS_TO_BUILD}
  )

if(NOT SWIFT_BUILT_STANDALONE)
  add_dependencies(swiftAST intrinsics_gen)
endif()

set(swift_ast_verifier_flag)
if(SWIFT_AST_VERIFIER)
  set(swift_ast_verifier_flag " -USWIFT_DISABLE_AST_VERIFIER")
else()
  set(swift_ast_verifier_flag " -DSWIFT_DISABLE_AST_VERIFIER=1")
endif()

set_property(SOURCE Verifier.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
  "${swift_ast_verifier_flag}")


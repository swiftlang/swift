
if (SWIFT_FORCE_OPTIMIZED_TYPECHECKER)
  set(EXTRA_AST_FLAGS "FORCE_BUILD_OPTIMIZED")
endif()

add_swift_library(swiftAST STATIC
  ASTContext.cpp
  ASTDumper.cpp
  ASTMangler.cpp
  ASTNode.cpp
  ASTPrinter.cpp
  ASTScope.cpp
  ASTVerifier.cpp
  ASTWalker.cpp
  Attr.cpp
  Availability.cpp
  AvailabilitySpec.cpp
  Builtins.cpp
  CaptureInfo.cpp
  ConcreteDeclRef.cpp
  ConformanceLookupTable.cpp
  Decl.cpp
  DeclContext.cpp
  DeclNameLoc.cpp
  DefaultArgumentKind.cpp
  DiagnosticEngine.cpp
  DiagnosticList.cpp
  DocComment.cpp
  Expr.cpp
  GenericEnvironment.cpp
  GenericSignature.cpp
  GenericSignatureBuilder.cpp
  Identifier.cpp
  LayoutConstraint.cpp
  LookupVisibleDecls.cpp
  Mangle.cpp
  Module.cpp
  ModuleNameLookup.cpp
  NameLookup.cpp
  Parameter.cpp
  Pattern.cpp
  PlatformKind.cpp
  PrettyStackTrace.cpp
  ProtocolConformance.cpp
  RawComment.cpp
  SILLayout.cpp
  Stmt.cpp
  SourceEntityWalker.cpp
  Substitution.cpp
  SubstitutionList.cpp
  SubstitutionMap.cpp
  SwiftNameTranslation.cpp
  Type.cpp
  TypeJoinMeet.cpp
  TypeRefinementContext.cpp
  TypeRepr.cpp
  TypeWalker.cpp
  USRGeneration.cpp
  LINK_LIBRARIES
    swiftMarkup
    swiftBasic

  INTERFACE_LINK_LIBRARIES
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

  LLVM_COMPONENT_DEPENDS
    bitreader bitwriter coroutines coverage irreader debuginfoDWARF
    profiledata instrumentation object objcarcopts mc mcparser
    bitreader bitwriter lto ipo option core support ${LLVM_TARGETS_TO_BUILD}

  ${EXTRA_AST_FLAGS}
  )

# intrinsics_gen is the LLVM tablegen target that generates the include files
# where intrinsics and attributes are declared. swiftAST depends on these
# headers. When we build swift out of tree, this is not an issue since LLVM must
# finish building before we build Swift. But when we are in tree, the build
# system must be made aware of the dependency between swiftAST and
# intrinsics_gen.
if(NOT SWIFT_BUILT_STANDALONE)
  get_property(CLANG_TABLEGEN_TARGETS GLOBAL PROPERTY CLANG_TABLEGEN_TARGETS)
  add_dependencies(swiftAST intrinsics_gen ${CLANG_TABLEGEN_TARGETS})
endif()

set(swift_ast_verifier_flag)
if(SWIFT_AST_VERIFIER)
  set(swift_ast_verifier_flag " -USWIFT_DISABLE_AST_VERIFIER")
else()
  set(swift_ast_verifier_flag " -DSWIFT_DISABLE_AST_VERIFIER=1")
endif()

set_property(SOURCE Verifier.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
  "${swift_ast_verifier_flag}")


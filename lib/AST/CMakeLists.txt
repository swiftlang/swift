
if (SWIFT_FORCE_OPTIMIZED_TYPECHECKER)
  set(EXTRA_AST_FLAGS "FORCE_BUILD_OPTIMIZED")
endif()

if(SWIFT_ENABLE_TENSORFLOW)
  find_package(TensorFlow REQUIRED)
  include_directories(BEFORE "${TF_INCLUDE_DIR}")
endif()

add_swift_host_library(swiftAST STATIC
  AccessScopeChecker.cpp
  AccessRequests.cpp
  ASTContext.cpp
  ASTDemangler.cpp
  ASTDumper.cpp
  ASTMangler.cpp
  ASTNode.cpp
  ASTPrinter.cpp
  ASTScope.cpp
  ASTVerifier.cpp
  ASTWalker.cpp
  Attr.cpp
  # SWIFT_ENABLE_TENSORFLOW
  AutoDiff.cpp
  Availability.cpp
  AvailabilitySpec.cpp
  Builtins.cpp
  CaptureInfo.cpp
  ConcreteDeclRef.cpp
  ConformanceLookupTable.cpp
  Decl.cpp
  DeclContext.cpp
  DeclNameLoc.cpp
  DiagnosticConsumer.cpp
  DiagnosticEngine.cpp
  DiagnosticList.cpp
  DocComment.cpp
  Evaluator.cpp
  Expr.cpp
  GenericEnvironment.cpp
  GenericSignature.cpp
  GenericSignatureBuilder.cpp
  # SWIFT_ENABLE_TENSORFLOW
  GraphOperationBuilder.cpp
  GraphOperationInfo.cpp
  Identifier.cpp
  InlinableText.cpp
  LayoutConstraint.cpp
  LookupVisibleDecls.cpp
  Module.cpp
  ModuleLoader.cpp
  ModuleNameLookup.cpp
  NameLookup.cpp
  NameLookupRequests.cpp
  Parameter.cpp
  Pattern.cpp
  PlatformKind.cpp
  PrettyStackTrace.cpp
  ProtocolConformance.cpp
  RawComment.cpp
  RequirementEnvironment.cpp
  SyntaxASTMap.cpp
  SILLayout.cpp
  Stmt.cpp
  SubstitutionMap.cpp
  SwiftNameTranslation.cpp
  # SWIFT_ENABLE_TENSORFLOW
  TensorFlow.cpp
  Type.cpp
  TypeCheckRequests.cpp
  TypeJoinMeet.cpp
  TypeRefinementContext.cpp
  TypeRepr.cpp
  TypeWalker.cpp
  USRGeneration.cpp

  LINK_LIBRARIES
    swiftMarkup
    swiftBasic
    swiftSyntax

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
# headers.
# For more information see the comment at the top of lib/CMakeLists.txt.
add_dependencies(swiftAST intrinsics_gen clang-tablegen-targets)

set(swift_ast_verifier_flag)
if(SWIFT_AST_VERIFIER)
  set(swift_ast_verifier_flag " -USWIFT_DISABLE_AST_VERIFIER")
else()
  set(swift_ast_verifier_flag " -DSWIFT_DISABLE_AST_VERIFIER=1")
endif()

set_property(SOURCE Verifier.cpp APPEND_STRING PROPERTY COMPILE_FLAGS
  "${swift_ast_verifier_flag}")


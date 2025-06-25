
add_swift_host_library(swiftSema STATIC
  AssociatedTypeInference.cpp
  NonisolatedNonsendingByDefaultMigration.cpp
  BuilderTransform.cpp
  Comment.cpp
  CSApply.cpp
  CSBindings.cpp
  CSSyntacticElement.cpp
  CSGen.cpp
  CSRanking.cpp
  CSSimplify.cpp
  CSSolver.cpp
  CSStep.cpp
  CSTrail.cpp
  CSFix.cpp
  CSOptimizer.cpp
  CSDiagnostics.cpp
  CodeSynthesis.cpp
  CodeSynthesisDistributedActor.cpp
  ConstantnessSemaDiagnostics.cpp
  Constraint.cpp
  ConstraintGraph.cpp
  ConstraintLocator.cpp
  ConstraintSystem.cpp
  CompletionContextFinder.cpp
  DebuggerTestingTransform.cpp
  DerivedConformance/DerivedConformance.cpp
  DerivedConformance/DerivedConformanceActor.cpp
  DerivedConformance/DerivedConformanceAdditiveArithmetic.cpp
  DerivedConformance/DerivedConformanceCaseIterable.cpp
  DerivedConformance/DerivedConformanceCodable.cpp
  DerivedConformance/DerivedConformanceCodingKey.cpp
  DerivedConformance/DerivedConformanceComparable.cpp
  DerivedConformance/DerivedConformanceDifferentiable.cpp
  DerivedConformance/DerivedConformanceDistributedActor.cpp
  DerivedConformance/DerivedConformanceEquatableHashable.cpp
  DerivedConformance/DerivedConformanceError.cpp
  DerivedConformance/DerivedConformanceRawRepresentable.cpp
  ImportResolution.cpp
  InstrumenterSupport.cpp
  LegalConstExprVerifier.cpp
  LookupVisibleDecls.cpp
  MiscDiagnostics.cpp
  OpenedExistentials.cpp
  PCMacro.cpp
  PlaygroundTransform.cpp
  PreCheckTarget.cpp
  ResilienceDiagnostics.cpp
  SourceLoader.cpp
  SyntacticElementTarget.cpp
  TypeOfReference.cpp
  TypeCheckAccess.cpp
  TypeCheckAttr.cpp
  TypeCheckAttrABI.cpp
  TypeCheckAvailability.cpp
  TypeCheckBitwise.cpp
  TypeCheckCaptures.cpp
  TypeCheckCircularity.cpp
  TypeCheckCodeCompletion.cpp
  TypeCheckConcurrency.cpp
  TypeCheckDistributed.cpp
  TypeCheckConstraints.cpp
  TypeCheckDecl.cpp
  TypeCheckDeclObjC.cpp
  TypeCheckDeclOverride.cpp
  TypeCheckDeclPrimary.cpp
  TypeCheckEffects.cpp
  TypeCheckExpr.cpp
  TypeCheckExprObjC.cpp
  TypeCheckGeneric.cpp
  TypeCheckInvertible.cpp
  TypeCheckMacros.cpp
  TypeCheckNameLookup.cpp
  TypeCheckPattern.cpp
  TypeCheckPropertyWrapper.cpp
  TypeCheckProtocol.cpp
  TypeCheckRegex.cpp
  TypeCheckRequestFunctions.cpp
  TypeCheckStmt.cpp
  TypeCheckStorage.cpp
  TypeCheckSwitchStmt.cpp
  TypeCheckType.cpp
  TypeCheckUnsafe.cpp
  TypeChecker.cpp
  IDETypeCheckingRequests.cpp)
if(SWIFT_FORCE_OPTIMIZED_TYPECHECKER)
  if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC" OR CMAKE_CXX_SIMULATE_ID STREQUAL "MSVC")
    target_compile_options(swiftSema PRIVATE /O2 /Ob2)
  else()
    target_compile_options(swiftSema PRIVATE -O3)
  endif()
endif()

target_link_libraries(swiftSema PRIVATE
  swiftAST
  swiftParse
  swiftSerialization)

if (SWIFT_BUILD_SWIFT_SYNTAX)
  target_link_libraries(swiftSema PRIVATE
    swiftASTGen
    swiftMacroEvaluation)
endif()

set_swift_llvm_is_available(swiftSema)

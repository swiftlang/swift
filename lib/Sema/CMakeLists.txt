
if (SWIFT_FORCE_OPTIMIZED_TYPECHECKER)
  set(EXTRA_TYPECHECKER_FLAGS "FORCE_BUILD_OPTIMIZED")
endif()

add_swift_library(swiftSema STATIC
  CSApply.cpp
  CSBindings.cpp
  CSDiag.cpp
  CSGen.cpp
  CSRanking.cpp
  CSSimplify.cpp
  CSSolver.cpp
  CSStep.cpp
  CSFix.cpp
  CSDiagnostics.cpp
  CalleeCandidateInfo.cpp
  CodeSynthesis.cpp
  Constraint.cpp
  ConstraintGraph.cpp
  ConstraintLocator.cpp
  ConstraintSystem.cpp
  DebuggerTestingTransform.cpp
  DerivedConformanceCaseIterable.cpp
  DerivedConformanceCodable.cpp
  DerivedConformanceCodingKey.cpp
  DerivedConformanceEquatableHashable.cpp
  DerivedConformanceError.cpp
  # SWIFT_ENABLE_TENSORFLOW
  DerivedConformanceParameterGroup.cpp
  DerivedConformanceParameterized.cpp
  DerivedConformanceRawRepresentable.cpp
  DerivedConformances.cpp
  InstrumenterSupport.cpp
  MiscDiagnostics.cpp
  NameBinding.cpp
  PCMacro.cpp
  PlaygroundTransform.cpp
  ResilienceDiagnostics.cpp
  SourceLoader.cpp
  TypeCheckAccess.cpp
  TypeCheckAttr.cpp
  TypeCheckAvailability.cpp
  TypeCheckCaptures.cpp
  TypeCheckCircularity.cpp
  # SWIFT_ENABLE_TENSORFLOW
  TypeCheckCompilerEvaluable.cpp
  TypeCheckConstraints.cpp
  TypeCheckDecl.cpp
  TypeCheckDeclObjC.cpp
  TypeCheckDeclOverride.cpp
  TypeCheckError.cpp
  TypeCheckExpr.cpp
  TypeCheckExprObjC.cpp
  TypeCheckGeneric.cpp
  TypeCheckNameLookup.cpp
  TypeCheckPattern.cpp
  TypeCheckProtocol.cpp
  TypeCheckProtocolInference.cpp
  TypeCheckREPL.cpp
  TypeCheckRequestFunctions.cpp
  TypeCheckStmt.cpp
  TypeCheckSwitchStmt.cpp
  TypeCheckType.cpp
  TypeChecker.cpp
  LINK_LIBRARIES
    swiftParse
    swiftAST
    swiftSerialization
  ${EXTRA_TYPECHECKER_FLAGS}
)


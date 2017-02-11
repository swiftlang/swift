
if (SWIFT_FORCE_OPTIMIZED_TYPECHECKER)
  set(EXTRA_TYPECHECKER_FLAGS "FORCE_BUILD_OPTIMIZED")
endif()

add_swift_library(swiftSema STATIC
  CSApply.cpp
  CSDiag.cpp
  CSGen.cpp
  CSRanking.cpp
  CSSimplify.cpp
  CSSolver.cpp
  CodeSynthesis.cpp
  Constraint.cpp
  ConstraintGraph.cpp
  ConstraintLocator.cpp
  ConstraintSystem.cpp
  DerivedConformanceEquatableHashable.cpp
  DerivedConformanceError.cpp
  DerivedConformanceRawRepresentable.cpp
  DerivedConformances.cpp
  ITCDecl.cpp
  ITCNameLookup.cpp
  ITCType.cpp
  InstrumenterSupport.cpp
  IterativeTypeChecker.cpp
  MiscDiagnostics.cpp
  NameBinding.cpp
  PCMacro.cpp
  PlaygroundTransform.cpp
  ResilienceDiagnostics.cpp
  SourceLoader.cpp
  TypeCheckAttr.cpp
  TypeCheckAvailability.cpp
  TypeCheckCaptures.cpp
  TypeCheckConstraints.cpp
  TypeCheckDecl.cpp
  TypeCheckError.cpp
  TypeCheckExpr.cpp
  TypeCheckExprObjC.cpp
  TypeCheckGeneric.cpp
  TypeCheckNameLookup.cpp
  TypeCheckPattern.cpp
  TypeCheckProtocol.cpp
  TypeCheckREPL.cpp
  TypeCheckRequest.cpp
  TypeCheckStmt.cpp
  TypeCheckType.cpp
  TypeChecker.cpp
  LINK_LIBRARIES
    swiftParse
    swiftAST
  ${EXTRA_TYPECHECKER_FLAGS}
)


if (NOT SWIFT_BUILD_SWIFT_SYNTAX)
  # swiftRefactoring requires SwiftIDEUtilsBriding, which isn't built if SWIFT_BUILD_SWIFT_SYNTAX is `NO`.
  return()
endif()

add_swift_host_library(swiftRefactoring STATIC
  AddEquatableConformance.cpp
  AddExplicitCodableImplementation.cpp
  Async/AsyncConverter.cpp
  Async/AsyncHandlerDesc.cpp
  Async/AsyncRefactorings.cpp
  Async/CallbackClassifier.cpp
  Async/CallbackCondition.cpp
  Async/DeclCollector.cpp
  Async/DeclReferenceFinder.cpp
  Async/ReferenceCollector.cpp
  Async/ScopedDeclCollector.cpp
  Async/Utils.cpp
  CollapseNestedIfStmt.cpp
  ConvertGuardExprToIfLetExpr.cpp
  ConvertIfLetExprToGuardExpr.cpp
  ConvertStringConcatenationToInterpolation.cpp
  ConvertToComputedProperty.cpp
  ConvertToDoCatch.cpp
  ConvertToSwitchStmt.cpp
  ConvertToTernaryExpr.cpp
  ExpandMacro.cpp
  ExpandSwitchCases.cpp
  ExpandTernaryExpr.cpp
  ExtractExpr.cpp
  ExtractExprBase.cpp
  ExtractFunction.cpp
  ExtractRepeatedExpr.cpp
  FillProtocolStubs.cpp
  LocalizeString.cpp
  LocalRename.cpp
  MemberwiseInitLocalRefactoring.cpp
  MoveMembersToExtension.cpp
  Refactoring.cpp
  RefactoringAction.cpp
  ReplaceBodiesWithFatalError.cpp
  SimplifyNumberLiteral.cpp
  SyntacticRename.cpp
  SyntacticRenameRangeDetails.cpp
  TrailingClosure.cpp
  Utils.cpp
)

target_link_libraries(swiftRefactoring PRIVATE
  swiftAST
  swiftClangImporter
  swiftIDE
  swiftIDEUtilsBridging
  swiftIndex
  swiftParse
  swiftSema)

set_swift_llvm_is_available(swiftRefactoring)


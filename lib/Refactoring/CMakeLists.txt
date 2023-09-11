add_swift_host_library(swiftRefactoring STATIC
  AddEquatableConformance.cpp
  AddExplicitCodableImplementation.cpp
  CollapseNestedIfStmt.cpp
  ConvertStringConcatenationToInterpolation.cpp
  ConvertGuardExprToIfLetExpr.cpp
  ConvertIfLetExprToGuardExpr.cpp
  ConvertToDoCatch.cpp
  ConvertToSwitchStmt.cpp
  ConvertToTernaryExpr.cpp
  ExpandSwitchCases.cpp
  ExpandTernaryExpr.cpp
  ExtractExpr.cpp
  ExtractExprBase.cpp
  ExtractRepeatedExpr.cpp
  ExtractFunction.cpp
  FillProtocolStubs.cpp
  LocalizeString.cpp
  MemberwiseInitLocalRefactoring.cpp
  MoveMembersToExtension.cpp
  Refactoring.cpp
  Renamer.cpp
  ReplaceBodiesWithFatalError.cpp
  SimplifyNumberLiteral.cpp
  Utils.cpp
)

target_link_libraries(swiftRefactoring PRIVATE
  swiftAST
  swiftClangImporter
  swiftIDE
  swiftIndex
  swiftParse
  swiftSema)

set_swift_llvm_is_available(swiftRefactoring)


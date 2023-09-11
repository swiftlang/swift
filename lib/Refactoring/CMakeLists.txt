add_swift_host_library(swiftRefactoring STATIC
  CollapseNestedIfStmt.cpp
  ConvertStringConcatenationToInterpolation.cpp
  ConvertGuardExprToIfLetExpr.cpp
  ConvertIfLetExprToGuardExpr.cpp
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
  MoveMembersToExtension.cpp
  Refactoring.cpp
  Renamer.cpp
  ReplaceBodiesWithFatalError.cpp
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


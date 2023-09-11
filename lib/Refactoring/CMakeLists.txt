add_swift_host_library(swiftRefactoring STATIC
  CollapseNestedIfStmt.cpp
  ConvertStringConcatenationToInterpolation.cpp
  ConvertGuardExprToIfLetExpr.cpp
  ConvertToSwitchStmt.cpp
  ConvertToTernaryExpr.cpp
  ExtractRepeatedExpr.cpp
  MoveMembersToExtension.cpp
  Refactoring.cpp
  ExtractExprBase.cpp
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


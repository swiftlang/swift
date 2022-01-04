
add_swift_host_library(swiftIDE STATIC
  CodeCompletion.cpp
  CodeCompletionCache.cpp
  CodeCompletionDiagnostics.cpp
  CodeCompletionResultPrinter.cpp
  CommentConversion.cpp
  CompileInstance.cpp
  CompletionInstance.cpp
  ConformingMethodList.cpp
  ExprContextAnalysis.cpp
  Formatting.cpp
  FuzzyStringMatcher.cpp
  Refactoring.cpp
  ModuleInterfacePrinting.cpp
  REPLCodeCompletion.cpp
  SwiftSourceDocInfo.cpp
  SyntaxModel.cpp
  Utils.cpp
  IDETypeChecking.cpp
  APIDigesterData.cpp
  SourceEntityWalker.cpp
  TypeContextInfo.cpp
  IDERequests.cpp)
target_link_libraries(swiftIDE PRIVATE
  swiftAST
  swiftClangImporter
  swiftDriver
  swiftFrontend
  swiftFrontendTool
  swiftIndex
  swiftParse
  swiftSema)

set_swift_llvm_is_available(swiftIDE)

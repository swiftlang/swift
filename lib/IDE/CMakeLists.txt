
add_swift_host_library(swiftIDE STATIC
  CodeCompletion.cpp
  CodeCompletionCache.cpp
  CodeCompletionDiagnostics.cpp
  CodeCompletionResultBuilder.cpp
  CodeCompletionResultPrinter.cpp
  CodeCompletionResult.cpp
  CodeCompletionResultType.cpp
  CodeCompletionString.cpp
  CodeCompletionStringPrinter.cpp
  CommentConversion.cpp
  CompileInstance.cpp
  CompletionInstance.cpp
  CompletionLookup.cpp
  ConformingMethodList.cpp
  DependencyChecking.cpp
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
  IDERequests.cpp
  )
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

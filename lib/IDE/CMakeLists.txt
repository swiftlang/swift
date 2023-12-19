
add_swift_host_library(swiftIDE STATIC
  AfterPoundExprCompletion.cpp
  APIDigesterData.cpp
  ArgumentCompletion.cpp
  CodeCompletion.cpp
  CodeCompletionCache.cpp
  CodeCompletionContext.cpp
  CodeCompletionDiagnostics.cpp
  CodeCompletionResult.cpp
  CodeCompletionResultBuilder.cpp
  CodeCompletionResultPrinter.cpp
  CodeCompletionResultType.cpp
  CodeCompletionString.cpp
  CodeCompletionStringPrinter.cpp
  CommentConversion.cpp
  CompletionLookup.cpp
  CompletionOverrideLookup.cpp
  ConformingMethodList.cpp
  CursorInfo.cpp
  ExprCompletion.cpp
  ExprContextAnalysis.cpp
  Formatting.cpp
  FuzzyStringMatcher.cpp
  IDEBridging.cpp
  IDERequests.cpp
  IDETypeChecking.cpp
  ImportDepth.cpp
  KeyPathCompletion.cpp
  ModuleInterfacePrinting.cpp
  PostfixCompletion.cpp
  REPLCodeCompletion.cpp
  SelectedOverloadInfo.cpp
  SourceEntityWalker.cpp
  SwiftSourceDocInfo.cpp
  SyntaxModel.cpp
  TypeCheckCompletionCallback.cpp
  TypeContextInfo.cpp
  UnresolvedMemberCompletion.cpp
  Utils.cpp
  )
target_link_libraries(swiftIDE PRIVATE
  swiftAST
  swiftClangImporter
  swiftDriver
  swiftParse
  swiftSema)

if (SWIFT_BUILD_SWIFT_SYNTAX)
  target_link_libraries(swiftIDE PRIVATE
    swiftIDEUtilsBridging
  )
endif()

set_swift_llvm_is_available(swiftIDE)

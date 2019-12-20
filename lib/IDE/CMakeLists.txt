add_swift_host_library(swiftIDE STATIC
  CodeCompletion.cpp
  CodeCompletionCache.cpp
  CommentConversion.cpp
  ConformingMethodList.cpp
  ExprContextAnalysis.cpp
  Formatting.cpp
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
  swiftFrontend
  swiftIndex
  swiftParse
  swiftSema)


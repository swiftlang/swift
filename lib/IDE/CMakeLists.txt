add_swift_host_library(swiftIDE STATIC
  CodeCompletion.cpp
  CodeCompletionCache.cpp
  CommentConversion.cpp
  ConformingMethodList.cpp
  ExprContextAnalysis.cpp
  Formatting.cpp
  Refactoring.cpp
  ModuleInterfacePrinting.cpp
  TypeReconstruction.cpp
  REPLCodeCompletion.cpp
  SwiftSourceDocInfo.cpp
  SyntaxModel.cpp
  Utils.cpp
  IDETypeChecking.cpp
  APIDigesterData.cpp
  SourceEntityWalker.cpp
  TypeContextInfo.cpp
  LINK_LIBRARIES
    swiftAST
    swiftFrontend
    swiftClangImporter
    swiftDWARFImporter
    swiftParse
    swiftSema
    swiftIndex)


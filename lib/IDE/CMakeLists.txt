add_swift_host_library(swiftIDE STATIC
  CodeCompletion.cpp
  CodeCompletionCache.cpp
  CommentConversion.cpp
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
  LINK_LIBS
    swiftFrontend
    swiftClangImporter
    swiftParse
    swiftSema
    swiftIndex)


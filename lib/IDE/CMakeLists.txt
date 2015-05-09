add_swift_library(swiftIDE
  CodeCompletion.cpp
  CodeCompletionCache.cpp
  CommentConversion.cpp
  ModuleInterfacePrinting.cpp
  REPLCodeCompletion.cpp
  SourceEntityWalker.cpp
  SyntaxModel.cpp
  Utils.cpp
  LINK_LIBRARIES
    swiftFrontend
    swiftClangImporter
    swiftParse
    swiftSema
    swiftAST)


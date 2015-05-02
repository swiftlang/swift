add_swift_library(swiftIDE
  CodeCompletion.cpp
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


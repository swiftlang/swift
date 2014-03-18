add_swift_library(swiftIDE
  CodeCompletion.cpp
  REPLCodeCompletion.cpp
  SourceEntityWalker.cpp
  SyntaxModel.cpp
  Utils.cpp
  DEPENDS
  swiftAST swiftParse swiftSema swiftClangImporter
  clangIndex clangFormat clangTooling)


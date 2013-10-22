add_swift_library(swiftIDE
  CodeCompletion.cpp
  REPLCodeCompletion.cpp
  SourceEntityWalker.cpp
  SyntaxModel.cpp
  DEPENDS swiftAST swiftParse swiftSema swiftClangImporter)


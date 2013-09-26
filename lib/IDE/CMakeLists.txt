add_swift_library(swiftIDE
  CodeCompletion.cpp
  REPLCodeCompletion.cpp
  SourceEntityWalker.cpp
  SyntaxColoring.cpp
  DEPENDS swiftAST swiftParse swiftSema swiftClangImporter)


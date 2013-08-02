add_swift_library(swiftIDE
  CodeCompletion.cpp
  REPLCodeCompletion.cpp
  SyntaxColoring.cpp
  DEPENDS swiftAST swiftParse swiftSema swiftClangImporter)


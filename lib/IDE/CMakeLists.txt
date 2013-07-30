add_swift_library(swiftIDE
  CodeCompletion.cpp
  REPLCodeCompletion.cpp
  DEPENDS swiftAST swiftParse swiftSema swiftClangImporter)


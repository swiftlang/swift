add_swift_library(swiftIDE
  CodeCompletion.cpp
  DEPENDS swiftAST swiftParse swiftSema swiftClangImporter)


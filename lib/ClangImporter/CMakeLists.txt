add_swift_library(swiftClangImporter
  ClangImporter.cpp
  ImportDecl.cpp
  ImportType.cpp
  DEPENDS swiftAST)

# Clang dependencies.
# FIXME: Clang should really export these in some reasonable manner.
target_link_libraries(swiftClangImporter
  clangDriver
  clangFrontendTool
  clangFrontend
  clangSerialization
  clangParse
  clangSema
  clangAnalysis
  clangEdit
  clangRewriteCore
  clangRewriteFrontend
  clangLex
  clangAST
  clangBasic)

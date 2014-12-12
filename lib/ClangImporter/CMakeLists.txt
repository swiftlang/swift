add_swift_library(swiftClangImporter
  ClangDiagnosticConsumer.cpp
  ClangImporter.cpp
  ImportDecl.cpp
  ImportMacro.cpp
  ImportType.cpp
  DEPENDS swiftAST)

# Clang dependencies.
# FIXME: Clang should really export these in some reasonable manner.
target_link_libraries(swiftClangImporter
  clangIndex
  clangFormat
  clangToolingCore
  clangFrontendTool
  clangFrontend
  clangDriver
  clangSerialization
  clangParse
  clangSema
  clangAnalysis
  clangEdit
  clangRewriteFrontend
  clangRewrite
  clangAST
  clangLex
  clangAPINotes
  clangBasic
  LLVMBitReader
  LLVMMC
  LLVMMCParser
  LLVMOption
  LLVMTransformUtils
  LLVMSupport)

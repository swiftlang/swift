# FIXME: The CMake-with-Xcode build puts things in a different
# directly layout than the autoconf build, which makes relative paths
# for finding the Clang location pretty horrible. Work around it.
if (XCODE)
  add_definitions(-DSWIFT_BUILT_WITH_CMAKE)
endif ()

add_swift_library(swiftClangImporter
  ClangImporter.cpp
  ImportDecl.cpp
  ImportMacro.cpp
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

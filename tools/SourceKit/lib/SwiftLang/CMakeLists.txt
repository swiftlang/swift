add_sourcekit_library(SourceKitSwiftLang
  CodeCompletionOrganizer.cpp
  SwiftASTManager.cpp
  SwiftCompletion.cpp
  SwiftDocSupport.cpp
  SwiftEditor.cpp
  SwiftEditorInterfaceGen.cpp
  SwiftIndexing.cpp
  SwiftLangSupport.cpp
  SwiftSourceDocInfo.cpp
  DEPENDS SourceKitCore swiftDriver swiftFrontend swiftClangImporter swiftIDE
          swiftAST swiftMarkup swiftParse swiftSIL swiftSILGen swiftSILOptimizer
          swiftSema swiftBasic swiftSerialization
          swiftOption cmark
    # Clang dependencies.
      clangFormat
      clangToolingCore
      clangIndex
      clangDriver
      clangFrontendTool
      clangFrontend
      clangCodeGen
      clangSerialization
      clangParse
      clangSema
      clangAnalysis
      clangEdit
      clangRewrite
      clangRewriteFrontend
      clangLex
      clangAST
      clangAPINotes
      clangBasic
  COMPONENT_DEPENDS ${LLVM_TARGETS_TO_BUILD}
      bitreader
      bitwriter
      debuginfodwarf
      instrumentation
      ipo
      mc
      mcparser
      option
      objcarcopts
      profiledata
)

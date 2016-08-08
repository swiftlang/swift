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
  DEPENDS SourceKitCore swiftDriver swiftFrontend swiftClangImporter swiftIndex swiftIDE
          swiftAST swiftMarkup swiftParse swiftSIL swiftSILGen swiftSILOptimizer
          swiftSema swiftBasic swiftSerialization
          swiftOption cmark
    # Clang dependencies.
      clangIndex
      clangFormat
      clangToolingCore
      clangFrontendTool
      clangFrontend
      clangDriver
      clangCodeGen
      clangSerialization
      clangParse
      clangSema
      clangAnalysis
      clangEdit
      clangRewriteFrontend
      clangRewrite
      clangLex
      clangAST
      clangAPINotes
      clangBasic
  LLVM_COMPONENT_DEPENDS ${LLVM_TARGETS_TO_BUILD}
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

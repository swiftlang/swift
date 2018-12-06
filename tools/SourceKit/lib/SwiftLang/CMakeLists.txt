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
  LINK_LIBS
    SourceKitCore swiftDriver swiftFrontend
    swiftDWARFImporter swiftClangImporter swiftIDE
    swiftAST swiftMarkup swiftParse swiftParseSIL swiftSIL swiftSILGen
    swiftSILOptimizer swiftIRGen swiftSema swiftBasic swiftSerialization
    swiftSyntax swiftOption libcmark_static
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
      coverage
      debuginfodwarf
      instrumentation
      ipo
      lto
      mc
      mcparser
      option
      objcarcopts
      profiledata
)

add_pure_swift_host_library(swiftMacroEvaluation STATIC CXX_INTEROP
  Macros.swift
  PluginHost.swift
  SourceManager.swift

  DEPENDENCIES
    swiftAST
  SWIFT_DEPENDENCIES
    _CompilerSwiftCompilerPluginMessageHandling
    _CompilerSwiftDiagnostics
    _CompilerSwiftOperators
    _CompilerSwiftParser
    _CompilerSwiftSyntax
    _CompilerSwiftSyntaxMacroExpansion
    swiftASTGen
)

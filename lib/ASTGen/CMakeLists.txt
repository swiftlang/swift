add_pure_swift_host_library(swiftLLVMJSON STATIC EMIT_MODULE
  Sources/LLVMJSON/LLVMJSON.swift

  DEPENDENCIES
    swiftBasic
)

add_pure_swift_host_library(swiftASTGen STATIC
  Sources/ASTGen/ASTGen.swift
  Sources/ASTGen/Bridge.swift
  Sources/ASTGen/Decls.swift
  Sources/ASTGen/Diagnostics.swift
  Sources/ASTGen/DiagnosticsBridge.swift
  Sources/ASTGen/Exprs.swift
  Sources/ASTGen/Generics.swift
  Sources/ASTGen/Literals.swift
  Sources/ASTGen/Macros.swift
  Sources/ASTGen/ParameterClause.swift
  Sources/ASTGen/PluginHost.swift
  Sources/ASTGen/SourceFile.swift
  Sources/ASTGen/SourceManager.swift
  Sources/ASTGen/SourceManager+MacroExpansionContext.swift
  Sources/ASTGen/Stmts.swift
  Sources/ASTGen/Types.swift

  DEPENDENCIES
    swiftAST
  SWIFT_DEPENDENCIES
    SwiftBasicFormat
    SwiftCompilerPluginMessageHandling
    SwiftDiagnostics
    SwiftOperators
    SwiftParser
    SwiftParserDiagnostics
    SwiftSyntax
    SwiftSyntaxBuilder
    SwiftSyntaxMacros
    SwiftSyntaxMacroExpansion
    swiftLLVMJSON
)

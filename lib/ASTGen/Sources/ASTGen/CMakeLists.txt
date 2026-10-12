# SARIF diagnostic serialization support is optional; the libraries only exist
# when SWIFT_BUILD_SARIF is enabled.
set(ASTGEN_SARIF_DEPENDENCIES)
set(ASTGEN_SARIF_SOURCES)
if(SWIFT_BUILD_SARIF)
  list(APPEND ASTGEN_SARIF_SOURCES SARIFDiagnostics.swift)
  list(APPEND ASTGEN_SARIF_DEPENDENCIES
    _CompilerSARIF
    _CompilerSARIFRecords
    _CompilerImmutableJSON)
endif()

add_pure_swift_host_library(swiftASTGen STATIC CXX_INTEROP
  ASTGen.swift
  ASTGen+CompilerBuildConfiguration.swift
  Availability.swift
  Bridge.swift
  BuiltinPound.swift
  CompilerBuildConfiguration.swift
  DeclAttrs.swift
  Decls.swift
  Diagnostics.swift
  DiagnosticsBridge.swift
  EmbeddedSupport.swift
  Exprs.swift
  Fingerprint.swift
  Generics.swift
  LexicalLookup.swift
  Literals.swift
  ParameterClause.swift
  Patterns.swift
  Regex.swift
  SourceFile.swift
  StableHasher.swift
  Stmts.swift
  TypeAttrs.swift
  Types.swift
  WarningGroupBehaviorConfiguration.swift
  ${ASTGEN_SARIF_SOURCES}

  DEPENDENCIES
    swiftAST
  SWIFT_DEPENDENCIES
    _CompilerSwiftCompilerPluginMessageHandling
    _CompilerRegexParser
    _CompilerSwiftSyntax
    _CompilerSwiftIfConfig
    _CompilerSwiftWarningControl
    _CompilerSwiftLexicalLookup
    _CompilerSwiftOperators
    _CompilerSwiftSyntaxBuilder
    _CompilerSwiftParser
    _CompilerSwiftParserDiagnostics
    _CompilerSwiftDiagnostics
    ${ASTGEN_SARIF_DEPENDENCIES}
    swiftBasicSwift
)

# SARIF diagnostic serialization support is optional; the libraries only exist
# when SWIFT_BUILD_SARIF is enabled.
set(ASTGEN_SARIF_DEPENDENCIES)
set(ASTGEN_SARIF_SOURCES)
if(SWIFT_BUILD_SARIF)
  list(APPEND ASTGEN_SARIF_SOURCES SARIFDiagnostics.swift)
  list(APPEND ASTGEN_SARIF_DEPENDENCIES
    SARIF
    SARIFRecords
    ImmutableJSON)
endif()

# Generate Swift bindings for the diagnostics declared in DiagnosticsParse.def,
# so that ASTGen can emit the compiler's existing diagnostics rather than
# ad-hoc strings. The generated file refers to each diagnostic symbolically, as
# `swift.DiagID.<id>`, so a stale or misgenerated binding is a build error.
set(_astgen_generated_diagnostics
  "${CMAKE_CURRENT_BINARY_DIR}/DiagnosticsParse.swift")
add_custom_command(
  OUTPUT "${_astgen_generated_diagnostics}"
  COMMAND "${Python3_EXECUTABLE}"
    "${SWIFT_SOURCE_DIR}/utils/generate_swift_diagnostics.py"
    "${SWIFT_SOURCE_DIR}/include/swift/AST/DiagnosticsParse.def"
    -o "${_astgen_generated_diagnostics}"
  DEPENDS
    "${SWIFT_SOURCE_DIR}/utils/generate_swift_diagnostics.py"
    "${SWIFT_SOURCE_DIR}/include/swift/AST/DiagnosticsParse.def"
  COMMENT "Generating Swift diagnostic bindings from DiagnosticsParse.def"
  VERBATIM)

add_pure_swift_host_library(swiftASTGen STATIC CXX_INTEROP
  ASTGen.swift
  ASTGen+CompilerBuildConfiguration.swift
  Availability.swift
  Bridge.swift
  BuiltinPound.swift
  CompilerBuildConfiguration.swift
  CompilerDiagnostics.swift
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
  "${_astgen_generated_diagnostics}"

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

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

# Generate Swift bindings for the diagnostics declared in the compiler's
# `Diagnostics*.def` files, so that ASTGen can emit the compiler's existing
# diagnostics rather than ad-hoc strings. The generated files refer to each
# diagnostic symbolically, as `swift.DiagID.<id>`, so a stale or misgenerated
# binding is a build error.
#
# Diagnostic IDs are unique across all of these files, since they are all one
# enum, so the generated extensions cannot collide.
set(_astgen_diagnostic_def_files
  DiagnosticsCommon
  DiagnosticsParse
  DiagnosticsSema)

set(_astgen_generated_diagnostics)
foreach(def_file ${_astgen_diagnostic_def_files})
  set(_generated "${CMAKE_CURRENT_BINARY_DIR}/${def_file}.swift")
  add_custom_command(
    OUTPUT "${_generated}"
    COMMAND "${Python3_EXECUTABLE}"
      "${SWIFT_SOURCE_DIR}/utils/generate_swift_diagnostics.py"
      "${SWIFT_SOURCE_DIR}/include/swift/AST/${def_file}.def"
      -o "${_generated}"
    DEPENDS
      "${SWIFT_SOURCE_DIR}/utils/generate_swift_diagnostics.py"
      "${SWIFT_SOURCE_DIR}/include/swift/AST/${def_file}.def"
    COMMENT "Generating Swift diagnostic bindings from ${def_file}.def"
    VERBATIM)
  list(APPEND _astgen_generated_diagnostics "${_generated}")
endforeach()

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
  DelayedBodies.swift
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
  ${_astgen_generated_diagnostics}

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

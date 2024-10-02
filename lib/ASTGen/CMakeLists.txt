set(ASTGen_Swift_dependencies)

# If requested, build the regular expression parser into the compiler itself.
if(SWIFT_BUILD_REGEX_PARSER_IN_COMPILER)
  file(GLOB_RECURSE _COMPILER_REGEX_PARSER_SOURCES
    "${SWIFT_PATH_TO_STRING_PROCESSING_SOURCE}/Sources/_RegexParser/*.swift")
  set(COMPILER_REGEX_PARSER_SOURCES)
  foreach(source ${_COMPILER_REGEX_PARSER_SOURCES})
    file(TO_CMAKE_PATH "${source}" source)
    list(APPEND COMPILER_REGEX_PARSER_SOURCES ${source})
  endforeach()
  message(STATUS "Using Experimental String Processing library for _CompilerRegexParser (${SWIFT_PATH_TO_STRING_PROCESSING_SOURCE}).")

  add_pure_swift_host_library(_CompilerRegexParser STATIC
    "${COMPILER_REGEX_PARSER_SOURCES}"
  )

  list(APPEND ASTGen_Swift_dependencies _CompilerRegexParser)
endif()

add_pure_swift_host_library(swiftASTGen STATIC CXX_INTEROP
  Sources/ASTGen/ASTGen.swift
  Sources/ASTGen/ASTGen+CompilerBuildConfiguration.swift
  Sources/ASTGen/Bridge.swift
  Sources/ASTGen/CompilerBuildConfiguration.swift
  Sources/ASTGen/DeclAttrs.swift
  Sources/ASTGen/Decls.swift
  Sources/ASTGen/Diagnostics.swift
  Sources/ASTGen/DiagnosticsBridge.swift
  Sources/ASTGen/Exprs.swift
  Sources/ASTGen/Generics.swift
  Sources/ASTGen/LegacyParse.swift
  Sources/ASTGen/Literals.swift
  Sources/ASTGen/ParameterClause.swift
  Sources/ASTGen/Patterns.swift
  Sources/ASTGen/Regex.swift
  Sources/ASTGen/SourceFile.swift
  Sources/ASTGen/Stmts.swift
  Sources/ASTGen/TypeAttrs.swift
  Sources/ASTGen/Types.swift

  DEPENDENCIES
    swiftAST
  SWIFT_DEPENDENCIES
    _CompilerSwiftSyntax
    _CompilerSwiftIfConfig
    _CompilerSwiftOperators
    _CompilerSwiftSyntaxBuilder
    _CompilerSwiftParser
    _CompilerSwiftParserDiagnostics
    _CompilerSwiftDiagnostics
    ${ASTGen_Swift_dependencies}
)

add_pure_swift_host_library(swiftMacros STATIC CXX_INTEROP
  Sources/Macros/Macros.swift
  Sources/Macros/PluginHost.swift
  Sources/Macros/SourceManager.swift

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

add_pure_swift_host_library(swiftIDEUtilsBridging CXX_INTEROP
  Sources/SwiftIDEUtilsBridging/NameMatcherBridging.swift
  
  DEPENDENCIES
    swiftAST
  SWIFT_DEPENDENCIES
    _CompilerSwiftIDEUtils
    _CompilerSwiftSyntax
    _CompilerSwiftIDEUtils
    swiftASTGen
)

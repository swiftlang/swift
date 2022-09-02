

if(CMAKE_HOST_SYSTEM_NAME STREQUAL Windows)
  set(SWIFT_GYB_FLAGS --line-directive "^\"#line %(line)d \\\"%(file)s\\\"^\"")
else()
  set(SWIFT_GYB_FLAGS --line-directive "\'#line" "%(line)d" "\"%(file)s\"\'")
endif()

add_swift_host_library(swiftParse STATIC
  Confusables.cpp
  Lexer.cpp
  ParseDecl.cpp
  ParsedRawSyntaxNode.cpp
  ParsedRawSyntaxRecorder.cpp
  ParsedTrivia.cpp
  ParseExpr.cpp
  ParseGeneric.cpp
  ParseIfConfig.cpp
  ParsePattern.cpp
  Parser.cpp
  ParseRegex.cpp
  ParseRequests.cpp
  ParseStmt.cpp
  ParseType.cpp
  PersistentParserState.cpp
  SyntaxParsingCache.cpp
  SyntaxParsingContext.cpp)
_swift_gyb_target_sources(swiftParse PRIVATE
    ParsedSyntaxBuilders.cpp.gyb
    ParsedSyntaxNodes.cpp.gyb
    ParsedSyntaxRecorder.cpp.gyb)
target_link_libraries(swiftParse PRIVATE
  swiftAST
  swiftSyntax
  swiftSyntaxParse
)

if (SWIFT_SWIFT_PARSER)
  # Link against the SwiftSyntax parser and libraries it depends on. The actual
  # formulation of this is a hack to work around a CMake bug in Ninja file
  # generation that results in multiple Ninja targets producing the same file in
  # a downstream SourceKit target. This should be expressed as:
  #
  #   target_link_libraries(swiftParse
  #     PRIVATE
  #     SwiftSyntax::SwiftParser
  #   )
  target_link_libraries(swiftParse
    PRIVATE
    $<TARGET_OBJECTS:SwiftSyntax::SwiftParser>
    $<TARGET_OBJECTS:SwiftSyntax::SwiftDiagnostics>
    $<TARGET_OBJECTS:SwiftSyntax::SwiftSyntax>
  )

  target_include_directories(swiftParse
    PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../swift-syntax/Sources/SwiftParser
  )

  target_compile_definitions(swiftParse
    PRIVATE
    SWIFT_SWIFT_PARSER_ROUNDTRIP
  )
endif()

add_dependencies(swiftParse swift-parse-syntax-generated-headers)

set_swift_llvm_is_available(swiftParse)

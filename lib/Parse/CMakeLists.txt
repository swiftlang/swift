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
  ParsedSyntaxBuilders.cpp.gyb
  ParsedSyntaxNodes.cpp.gyb
  ParsedSyntaxRecorder.cpp.gyb
  ParseExpr.cpp
  ParseGeneric.cpp
  ParseIfConfig.cpp
  ParsePattern.cpp
  Parser.cpp
  ParseStmt.cpp
  ParseType.cpp
  PersistentParserState.cpp
  Scope.cpp
  SyntaxParsingCache.cpp
  SyntaxParsingContext.cpp
  LINK_LIBRARIES
    swiftAST
    swiftSyntax
)

add_dependencies(swiftParse swift-parse-syntax-generated-headers)

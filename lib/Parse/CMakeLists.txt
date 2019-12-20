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
  ParseRequests.cpp
  ParseStmt.cpp
  ParseType.cpp
  PersistentParserState.cpp
  Scope.cpp
  SyntaxParsingCache.cpp
  SyntaxParsingContext.cpp

  GYB_SOURCES
    ParsedSyntaxBuilders.cpp.gyb
    ParsedSyntaxNodes.cpp.gyb
    ParsedSyntaxRecorder.cpp.gyb)
target_link_libraries(swiftParse PRIVATE
  swiftAST
  swiftSyntax)

add_dependencies(swiftParse swift-parse-syntax-generated-headers)

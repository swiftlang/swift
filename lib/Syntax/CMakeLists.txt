if(CMAKE_HOST_SYSTEM_NAME STREQUAL Windows)
  set(SWIFT_GYB_FLAGS --line-directive "^\"#line %(line)d \\\"%(file)s\\\"^\"")
else()
  set(SWIFT_GYB_FLAGS --line-directive "\'#line" "%(line)d" "\"%(file)s\"\'")
endif()

add_swift_library(swiftSyntax STATIC
  SyntaxNodes.cpp.gyb
  SyntaxBuilders.cpp.gyb
  SyntaxKind.cpp.gyb
  SyntaxFactory.cpp.gyb
  SyntaxVisitor.cpp.gyb
  Trivia.cpp.gyb
  RawSyntax.cpp
  Syntax.cpp
  SyntaxArena.cpp
  SyntaxClassifier.cpp.gyb
  SyntaxData.cpp
  UnknownSyntax.cpp)

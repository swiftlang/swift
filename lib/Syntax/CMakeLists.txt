set(line_directive "#line" "%(line)d" "\"%(file)s\"")
set(SWIFT_GYB_FLAGS
  --line-directive "'${line_directive}'")

add_swift_library(swiftSyntax STATIC
  SyntaxNodes.cpp.gyb
  SyntaxBuilders.cpp.gyb
  SyntaxKind.cpp.gyb
  SyntaxFactory.cpp.gyb
  SyntaxVisitor.cpp.gyb
  Trivia.cpp
  RawSyntax.cpp
  Syntax.cpp
  SyntaxArena.cpp
  SyntaxData.cpp
  UnknownSyntax.cpp)

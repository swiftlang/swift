
set(SWIFT_GYB_FLAGS "--line-directive \"#line \"%(line)d\" \"%(file)s\"\" ")

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

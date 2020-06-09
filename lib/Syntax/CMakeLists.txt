if(CMAKE_HOST_SYSTEM_NAME STREQUAL Windows)
  set(SWIFT_GYB_FLAGS --line-directive "^\"#line %(line)d \\\"%(file)s\\\"^\"")
else()
  set(SWIFT_GYB_FLAGS --line-directive "\'#line" "%(line)d" "\"%(file)s\"\'")
endif()

add_swift_host_library(swiftSyntax STATIC
  RawSyntax.cpp
  Syntax.cpp
  SyntaxData.cpp
  UnknownSyntax.cpp)
_swift_gyb_target_sources(swiftSyntax PRIVATE
    SyntaxNodes.cpp.gyb
    SyntaxBuilders.cpp.gyb
    SyntaxKind.cpp.gyb
    SyntaxFactory.cpp.gyb
    SyntaxVisitor.cpp.gyb
    Trivia.cpp.gyb
    SyntaxSerialization.cpp.gyb)

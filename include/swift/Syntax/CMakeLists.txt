if(CMAKE_HOST_SYSTEM_NAME STREQUAL Windows)
  set(SWIFT_GYB_FLAGS --line-directive "^\"#line %(line)d \\\"%(file)s\\\"^\"")
else()
  set(SWIFT_GYB_FLAGS --line-directive "\'#line" "%(line)d" "\"%(file)s\"\'")
endif()

set(generated_include_sources
    SyntaxKind.h.gyb
    SyntaxNodes.h.gyb
    SyntaxBuilders.h.gyb
    SyntaxFactory.h.gyb
    SyntaxVisitor.h.gyb
    TokenKinds.def.gyb
    Trivia.h.gyb)

add_gyb_target(swift-syntax-generated-headers
    "${generated_include_sources}")
set_property(TARGET swift-syntax-generated-headers
    PROPERTY FOLDER "Miscellaneous")

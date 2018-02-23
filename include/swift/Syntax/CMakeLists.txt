set(SWIFT_GYB_FLAGS "--line-directive \"#line \"%(line)d\" \"%(file)s\"\" ")

set(generated_include_sources
    SyntaxKind.h.gyb
    SyntaxNodes.h.gyb
    SyntaxBuilders.h.gyb
    SyntaxFactory.h.gyb
    SyntaxVisitor.h.gyb)

add_gyb_target(swift-syntax-generated-headers
    "${generated_include_sources}")

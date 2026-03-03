// RUN: not %target-swift-frontend -typecheck %s

struct Foo : ExpressibleByUnicodeScalarLiteral {}
let _: Foo = "\\"

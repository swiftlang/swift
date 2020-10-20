// RUN: not %target-swift-frontend -typecheck %s

enum Foo {
    case BigA {
    }
    case littleA(BigA)
}

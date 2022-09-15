// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/50977

enum Foo {
    case BigA {
    }
    case littleA(BigA)
}

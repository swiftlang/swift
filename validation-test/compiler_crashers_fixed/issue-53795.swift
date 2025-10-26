// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/53795

protocol Foo {}
func foo(_ bar: Foo) {}
foo(true ? "a" : "b")

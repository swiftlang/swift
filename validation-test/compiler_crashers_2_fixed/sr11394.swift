// RUN: not %target-swift-frontend -typecheck %s

protocol Foo {}
func foo(_ bar: Foo) {}
foo(true ? "a" : "b")

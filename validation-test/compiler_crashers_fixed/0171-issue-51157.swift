// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/51157

protocol P {}
protocol Q {}
class A : P {}
class B : A {}

struct A<T:B> {
  var x: T { fatalError("death") }
}

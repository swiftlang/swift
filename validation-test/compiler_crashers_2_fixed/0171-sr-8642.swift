// RUN: not %target-swift-frontend -typecheck %s

protocol P {}
protocol Q {}
class A : P {}
class B : A {}

struct A<T:B> {
  var x: T { fatalError("death") }
}

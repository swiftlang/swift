// RUN: %target-swift-frontend -typecheck -verify %s

struct A {
  typealias Value = Int
}

protocol B {
  typealias Value = A.Value
  typealias T = String
}

protocol NestedProtocol {
  typealias _B = B
}

struct Something: NestedProtocol {

  struct InnerTest: _B {
    var value: Value = 42
    var t: T = "wait what?"
  }
}

protocol X {}

protocol Y {
  typealias _X = X
  var x: _X { get }
}

struct Struct: Y {
  var x: _X = __X()
}

extension Struct {
  struct __X: _X {}
}

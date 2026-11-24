// rdar://171863453 (https://github.com/swiftlang/swift/issues/87711)
// RUN: %target-typecheck-verify-swift

protocol P<N> where N: SignedNumeric {
  associatedtype N
}

protocol Q<Value> where Value: P, Value.N: SignedInteger {
  associatedtype Value
  var value: Value { get }
}

struct MyValue<Value> {
  let value: Value
}

extension MyValue: Q where Value: P, Value.N: SignedInteger {}

extension Q {
  var n: Value.N { 0 }
}

extension MyValue {
  func doSomething() {
    if case let qSelf as any Q<Value> = self {
      _ = qSelf.value
      _ = qSelf.n // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    }
  }
}

// RUN: %target-typecheck-verify-swift

protocol P : Equatable {
  associatedtype T = String
}

struct S : Hashable {
  var key: String

  init(_ key: String) {
    self.key = key
  }
}

extension S : ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        self.init(value)
    }
}

extension S : ExpressibleByStringInterpolation {
  init(stringInterpolation: DefaultStringInterpolation) {
    self.key = "foo"
  }
}

extension S : P {}

struct ConcP<F: P, S: P> : P where F.T == S.T {
  var lhs: F
  var rhs: S
}

struct Z : P {
}

extension P {
  func bar() -> Z { fatalError() }

  static func +<T : P>(lhs: Self, rhs: T) -> ConcP<Self, T> {
    return ConcP(lhs: lhs, rhs: rhs)
  }
}

class Container<V> {
  var value: V
  init(_ value: V) {
    self.value = value
  }
}

struct A {
  enum Value : CustomStringConvertible {
    case foo, bar

    var description: String {
      switch self {
        case .foo: return "foo"
        case .bar: return "bar"
      }
    }
  }

  var value: Container<Value>

  func foo() {
    let value = self.value.value
    _ = S("A") + S("\(value)").bar() + S("B") // Ok
  }
}

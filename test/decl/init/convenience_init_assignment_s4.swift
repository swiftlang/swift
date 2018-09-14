// RUN: %target-typecheck-verify-swift -swift-version 4

final class A {
  static let a = A()
  convenience init(a: ()) {
    self = A.a // expected-error{{immutable}}
  }
}

private func _forceCastToSelf<T: AnyObject>(_ object: AnyObject) -> T {
  return object as! T
}

class B {
  static let b = B()

  convenience init(bWrong: ()) {
    self = B.b // expected-error{{immutable}}
  }

  convenience init(b: ()) {
    self = _forceCastToSelf(B.b) // expected-error{{immutable}}
  }
}

class C: B { }

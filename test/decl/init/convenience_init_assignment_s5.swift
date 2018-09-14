// RUN: %target-typecheck-verify-swift -swift-version 5

final class A {
  static let a = A()
  convenience init(a: ()) {
    self = A.a
  }
}

private func _forceCastToSelf<T: AnyObject>(_ object: AnyObject) -> T {
  return object as! T
}

protocol P {}

extension P {
  static var selfProperty: Self { fatalError() }
  static func selfReturningMethod() -> Self { fatalError() }
  static func selfLaunderingMethod(_: Self) -> Self { fatalError() }

  init(protocolExt: ()) { fatalError() }
}

class B: P {
  static let b = B()

  convenience init(otherInstance: B) {
    self = B.b // expected-error{{cannot assign value of type 'B' to type 'Self'}}
    self = otherInstance // expected-error{{cannot assign value of type 'B' to type 'Self'}}
    self = _forceCastToSelf(B.b)
    self = type(of: self).selfProperty
    self = type(of: self).selfReturningMethod()
    self = type(of: self).selfLaunderingMethod(otherInstance) // expected-error{{cannot assign value of type 'B' to type 'Self'}}
    self = type(of: self).selfLaunderingMethod(type(of: self).selfReturningMethod())
    self = type(of: self).init(protocolExt: ())
  }
}

class C: B { }

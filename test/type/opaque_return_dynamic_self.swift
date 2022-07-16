// RUN: %target-typecheck-verify-swift -disable-availability-checking

protocol P {
  init()
}

extension P {
  static func opaqueProtocolExtensionFunc() -> some P { return Self() }
}

class C: P {
  required init() {}

  static func opaqueFunc1() -> some P {
    return Self()
    // expected-error@-1 {{function with opaque return type cannot return the covariant 'Self' type of a class}}
  }

  static func opaqueFunc2() -> some P {
    return Self.opaqueProtocolExtensionFunc()
    // expected-error@-1 {{function with opaque return type cannot return the covariant 'Self' type of a class}}
  }

  var opaqueProperty: some P {
    return Self()
    // expected-error@-1 {{function with opaque return type cannot return the covariant 'Self' type of a class}}
  }

  subscript() -> some P {
    return Self()
    // expected-error@-1 {{function with opaque return type cannot return the covariant 'Self' type of a class}}
  }

  var opaqueStoredProperty: some P = Self()
  // expected-error@-1 {{covariant 'Self' type cannot be referenced from a stored property initializer}}
}

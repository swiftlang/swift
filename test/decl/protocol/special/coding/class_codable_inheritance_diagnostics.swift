// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Non-Decodable superclasses of synthesized Decodable classes must implement
// init().
class NonDecodableSuper { // expected-note {{cannot automatically synthesize 'init(from:)' because superclass does not have a callable 'init()'}}
  init(_: Int) {}
}

class NonDecodableSub : NonDecodableSuper, Decodable { // expected-error {{type 'NonDecodableSub' does not conform to protocol 'Decodable'}}
}

// Non-Decodable superclasses of synthesized Decodable classes must have
// designated init()'s.
class NonDesignatedNonDecodableSuper {
  convenience init() { // expected-note {{cannot automatically synthesize 'init(from:)' because implementation would need to call 'init()', which is not designated}}
    self.init(42)
  }

  init(_: Int) {}
}

class NonDesignatedNonDecodableSub : NonDesignatedNonDecodableSuper, Decodable { // expected-error {{type 'NonDesignatedNonDecodableSub' does not conform to protocol 'Decodable'}}
}

// Non-Decodable superclasses of synthesized Decodable classes must have an
// accessible init().
class InaccessibleNonDecodableSuper {
  private init() {} // expected-note {{cannot automatically synthesize 'init(from:)' because implementation would need to call 'init()', which is inaccessible due to 'private' protection level}}
}

class InaccessibleNonDecodableSub : InaccessibleNonDecodableSuper, Decodable { // expected-error {{type 'InaccessibleNonDecodableSub' does not conform to protocol 'Decodable'}}
}

// Subclasses of Decodable classes which can't inherit their initializers should
// produce diagnostics.
class DecodableSuper : Decodable {
  var value = 5
}

class DecodableSubWithoutInitialValue : DecodableSuper { // expected-error {{class 'DecodableSubWithoutInitialValue' has no initializers}}
  // expected-note@-1 {{did you mean to override 'init(from:)'?}}
  var value2: Int // expected-note {{stored property 'value2' without initial value prevents synthesized initializers}}
}

class DecodableSubWithInitialValue : DecodableSuper {
  var value2 = 10
}

// Subclasses of Codable classes which can't inherit their initializers should
// produce diagnostics.
class CodableSuper : Codable {
  var value = 5
}

class CodableSubWithoutInitialValue : CodableSuper { // expected-error {{class 'CodableSubWithoutInitialValue' has no initializers}}
  // expected-note@-1 {{did you mean to override 'init(from:)' and 'encode(to:)'?}}
  var value2: Int // expected-note {{stored property 'value2' without initial value prevents synthesized initializers}}
}

// We should only mention encode(to:) in the diagnostic if the subclass does not
// override it.
class EncodableSubWithoutInitialValue : CodableSuper { // expected-error {{class 'EncodableSubWithoutInitialValue' has no initializers}}
  // expected-note@-1 {{did you mean to override 'init(from:)'?}}
  var value2: Int // expected-note {{stored property 'value2' without initial value prevents synthesized initializers}}

  override func encode(to: Encoder) throws {}
}

class CodableSubWithInitialValue : CodableSuper {
  var value2 = 10
}

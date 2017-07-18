// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// Non-Decodable superclasses of synthesized Decodable classes must implement
// init().
class NonDecodableSuper { // expected-note {{cannot automatically synthesize 'init(from:)' because superclass does not have a callable 'init()' or 'init(from:)'}}
  init(_: Int) {}
}

class NonDecodableSub : NonDecodableSuper, Decodable { // expected-error {{type 'NonDecodableSub' does not conform to protocol 'Decodable'}}
}

// Non-Decodable superclasses of synthesized Decodable classes must have
// designated init()'s.
class NonDesignatedNonDecodableSuper {
  convenience init() { // expected-note {{cannot automatically synthesize 'init(from:)' because required superclass initializer 'init()' is not designated}}
    self.init(42)
  }

  init(_: Int) {}
}

class NonDesignatedNonDecodableSub : NonDesignatedNonDecodableSuper, Decodable { // expected-error {{type 'NonDesignatedNonDecodableSub' does not conform to protocol 'Decodable'}}
}

// Non-Decodable superclasses of synthesized Decodable classes must have an
// accessible init().
class InaccessibleNonDecodableSuper {
  private init() {} // expected-note {{cannot automatically synthesize 'init(from:)' because required superclass initializer 'init()' is inaccessible due to 'private' protection level}}
}

class InaccessibleNonDecodableSub : InaccessibleNonDecodableSuper, Decodable { // expected-error {{type 'InaccessibleNonDecodableSub' does not conform to protocol 'Decodable'}}
}

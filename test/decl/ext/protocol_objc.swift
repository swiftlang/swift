// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

@objc protocol OP1 {
  func reqOP1a() -> Bool // expected-note {{'reqOP1a' declared here}}
}

extension OP1 {
  func extOP1a() -> Bool { return !reqOP1a() }
}

class OC1 : OP1 {
  @objc func reqOP1a() -> Bool { return true }
}

func testOP1(_ oc1: OC1, ao: AnyObject) {
  _ = oc1.extOP1a()
  // expected-warning @+1 {{result of call to function returning 'Bool' is unused}}
  ao.reqOP1a!() // okay

  // Extension of @objc protocol does not have @objc members.
  ao.extOP1a!() // expected-error{{value of type 'AnyObject' has no member 'extOP1a'; did you mean 'reqOP1a'?}}
}

// Do not offer to add @objc to a witness in a protocol extension because
// Objective-C cannot represent members of protocol extensions.
@objc protocol ObjCProtocolWithExtensionWitnesses {
  func method() // expected-note {{requirement 'method()' declared here}}
  var property: Int { get } // expected-note {{requirement 'property' declared here}}
  subscript(index: Int) -> Int { get } // expected-note {{requirement 'subscript(_:)' declared here}}
}

extension ObjCProtocolWithExtensionWitnesses {
  func method() {} // expected-note {{members of protocol extensions cannot be exposed to Objective-C}}
  var property: Int { 0 } // expected-note {{members of protocol extensions cannot be exposed to Objective-C}}
  subscript(index: Int) -> Int { index } // expected-note {{members of protocol extensions cannot be exposed to Objective-C}}
}

class UsesExtensionWitnesses: ObjCProtocolWithExtensionWitnesses {}
// expected-error@-1 {{non-'@objc' method 'method()' does not satisfy requirement of '@objc' protocol 'ObjCProtocolWithExtensionWitnesses'}}
// expected-error@-2 {{non-'@objc' property 'property' does not satisfy requirement of '@objc' protocol 'ObjCProtocolWithExtensionWitnesses'}}
// expected-error@-3 {{non-'@objc' subscript does not satisfy requirement of '@objc' protocol 'ObjCProtocolWithExtensionWitnesses'}}

// REQUIRES: swift_feature_ProtocolMetatypeExtensions
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ProtocolMetatypeExtensions

protocol P {}

extension P.Protocol {
  var value: Int { 42 }
  func greet() -> String { "hello" }
}

// --- Protocol metatype extension members are accessible on the protocol.
let _: Int = P.value
let _: String = P.greet()

// --- Protocol metatype extension members are accessible on a stored metatype.
let p = P.self
let _: Int = p.value
let _: String = p.greet()

// --- Protocol metatype extension members are NOT accessible on conforming types.
struct ConcreteP: P {}
let _ = ConcreteP.value // expected-error {{type 'ConcreteP' has no member 'value'}}
let _ = ConcreteP.greet() // expected-error {{type 'ConcreteP' has no member 'greet'}}

// --- Protocol metatype extension members do NOT propagate through refinement.
protocol Q: P {}
let _ = Q.value // expected-error {{type 'any Q' has no member 'value'}}
let _ = Q.greet() // expected-error {{type 'any Q' has no member 'greet'}}

// --- Metatype extension on non-protocol types.
struct S {}
extension S.Protocol {} // expected-error {{metatype extension can only extend a protocol, not 'S'}}

class C {}
extension C.Protocol {} // expected-error {{metatype extension can only extend a protocol, not 'C'}}

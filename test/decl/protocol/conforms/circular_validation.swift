// RUN: %target-typecheck-verify-swift

// With a bit of effort, we could make this work -- but for now, let's just
// not crash.

protocol P {
  var x: Int { get set } // expected-note {{protocol requires property 'x' with type 'Int'}}
}

struct S : P { // expected-error {{type 'S' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}} 
  static var x = 0 // expected-note {{candidate operates on a type, not an instance as required}}
  var x = S.x // expected-note {{candidate references itself}}
}

// https://github.com/apple/swift/issues/51713
// FIXME: Lousy diagnostics on this case.
protocol P1_51713: P1P2_51713 {} // expected-error {{protocol 'P1_51713' refines itself}}
protocol P2_51713: P1P2_51713 {}
typealias P1P2_51713 = P1_51713 & P2_51713

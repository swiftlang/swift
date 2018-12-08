// RUN: %target-typecheck-verify-swift

// With a bit of effort, we could make this work -- but for now, let's just
// not crash.

protocol P {
  var x: Int { get set } // expected-note {{protocol requires property 'x' with type 'Int'; do you want to add a stub?}}
}

struct S : P { // expected-error {{type 'S' does not conform to protocol 'P'}}
  static var x = 0 // expected-note {{candidate operates on a type, not an instance as required}}
  var x = S.x // expected-note {{candidate references itself}}
}

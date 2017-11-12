// RUN: %target-typecheck-verify-swift -disable-objc-attr-requires-foundation-module

@objc protocol P {
  associatedtype T
  // expected-error@-1 {{associated type 'T' cannot be declared inside '@objc' protocol 'P'}}
}

extension P {
  func takesT(_: T) {}
}

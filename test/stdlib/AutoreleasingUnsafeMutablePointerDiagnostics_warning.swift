// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

func unsafePointerInitEphemeralConversions() {
  class C {}
  var c: C?

  _ = AutoreleasingUnsafeMutablePointer(&c) // expected-warning {{initialization of 'AutoreleasingUnsafeMutablePointer<C?>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'C?' to 'AutoreleasingUnsafeMutablePointer<C?>' produces a pointer valid only for the duration of the call to 'init(_:)'}}
}

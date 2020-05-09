// RUN: %target-typecheck-verify-swift -enable-invalid-ephemeralness-as-error
// REQUIRES: objc_interop

class C {}

func unsafePointerInitEphemeralConversions() {
  var c = C()
  var optC: C?

  _ = AutoreleasingUnsafeMutablePointer(&c) // expected-error {{initialization of 'AutoreleasingUnsafeMutablePointer<C>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'C' to 'AutoreleasingUnsafeMutablePointer<C>' produces a pointer valid only for the duration of the call to 'init(_:)'}}

  _ = AutoreleasingUnsafeMutablePointer<AnyObject>(&c) // expected-error {{initialization of 'AutoreleasingUnsafeMutablePointer<AnyObject>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'C' to 'UnsafeMutablePointer<C>' produces a pointer valid only for the duration of the call to 'init(_:)'}}
  // expected-note@-2 {{use 'withUnsafeMutablePointer' in order to explicitly convert argument to pointer valid for a defined scope}}

  _ = AutoreleasingUnsafeMutablePointer(&optC) // expected-error {{initialization of 'AutoreleasingUnsafeMutablePointer<C?>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'C?' to 'AutoreleasingUnsafeMutablePointer<C?>' produces a pointer valid only for the duration of the call to 'init(_:)'}}

  _ = AutoreleasingUnsafeMutablePointer<C>(&optC) // expected-error {{initialization of 'AutoreleasingUnsafeMutablePointer<C>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'C?' to 'UnsafeMutablePointer<C?>' produces a pointer valid only for the duration of the call to 'init(_:)'}}
  // expected-note@-2 {{use 'withUnsafeMutablePointer' in order to explicitly convert argument to pointer valid for a defined scope}}

  _ = AutoreleasingUnsafeMutablePointer<AnyObject>(&optC) // expected-error {{initialization of 'AutoreleasingUnsafeMutablePointer<AnyObject>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'C?' to 'UnsafeMutablePointer<C?>' produces a pointer valid only for the duration of the call to 'init(_:)'}}
  // expected-note@-2 {{use 'withUnsafeMutablePointer' in order to explicitly convert argument to pointer valid for a defined scope}}

  _ = AutoreleasingUnsafeMutablePointer<AnyObject?>(&optC) // expected-error {{initialization of 'AutoreleasingUnsafeMutablePointer<AnyObject?>' results in a dangling pointer}}
  // expected-note@-1 {{implicit argument conversion from 'C?' to 'UnsafeMutablePointer<C?>' produces a pointer valid only for the duration of the call to 'init(_:)'}}
  // expected-note@-2 {{use 'withUnsafeMutablePointer' in order to explicitly convert argument to pointer valid for a defined scope}}
}

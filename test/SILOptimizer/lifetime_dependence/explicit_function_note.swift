// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

// Lifetime diagnostics in explicitly written initializers and accessors must
// not be annotated with "error in compiler-generated '...'". That note is only
// meant for implicit functions.
// https://github.com/swiftlang/swift/issues/94426

struct NE: ~Escapable {
  let p: UnsafeRawPointer?
}

var shorthandGetter: NE {
  NE(p: nil) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note@-1 {{it depends on the lifetime of this parent value}}
} // expected-note {{this use causes the lifetime-dependent value to escape}}

var explicitGetter: NE {
  get {
    NE(p: nil) // expected-error {{lifetime-dependent value escapes its scope}}
    // expected-note@-1 {{it depends on the lifetime of this parent value}}
  } // expected-note {{this use causes the lifetime-dependent value to escape}}
}

struct Holder: ~Escapable {
  let ne: NE

  var member: NE {
    @_lifetime(copy self)
    get {
      NE(p: nil) // expected-error {{lifetime-dependent value escapes its scope}}
      // expected-note@-1 {{it depends on the lifetime of this parent value}}
    } // expected-note {{this use causes the lifetime-dependent value to escape}}
  }

  @_lifetime(immortal)
  init() { // expected-error {{lifetime-dependent variable 'self' escapes its scope}}
    self.ne = NE(p: nil) // expected-note {{it depends on the lifetime of this parent value}}
  } // expected-note {{this use causes the lifetime-dependent value to escape}}
}

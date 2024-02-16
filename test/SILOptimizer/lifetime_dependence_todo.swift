// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -Xllvm -enable-lifetime-dependence-diagnostics

// REQUIRES: asserts
// REQUIRES: swift_in_compiler

// Future tests for LifetimeDependenceDiagnostics.
// REQUIRES: disabled


// =============================================================================
// Diagnostics that should fail.

// @_unsafeResultDependsOn: Test that an unsafe dependence requires
// Builtin.lifetime_dependence.
//
func bv_derive_local(bv: consuming BV) -> _consume(bv) BV {
  let bv2 = BV(bv.p, bv.i)
  return bv2.derive() // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on the lifetime of variable 'bv2'}}
  // expected-note @-2 {{this use causes the lifetime-dependent value to escape}}
}

// =============================================================================
// Debug locations

// FIXME: Our debug locations on closure captures are incorrect.
// FIXME: We return two errors: one for the caller, and one for the closure.
func bvcons_capture_escape(bv: consuming BV) -> ()->Int {
  return { bv.c }  // expected-error{{lifetime-dependent variable 'bv' escapes its scope}}
  // expected-note @-1 {{this use causes the lifetime-dependent value to escape}}
}

// FIXME: Our debug locations on closure captures are incorrect.
// FIXME: We return two errors: one for the caller, and one for the closure.
func bvcons_capture_escapelet(bv: consuming BV) -> ()->Int {
  let closure = { bv.c } // expected-error{{lifetime-dependent variable 'bv' escapes its scope}}
  // expected-note @-1 {{this use causes the lifetime-dependent value to escape}}
  return closure
}

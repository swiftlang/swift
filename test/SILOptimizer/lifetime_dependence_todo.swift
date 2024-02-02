// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -Xllvm -enable-lifetime-dependence-diagnostics

// REQUIRES: swift_in_compiler

// Future tests for LifetimeDependenceDiagnostics.
// REQUIRES: disabled


// =============================================================================
// Diagnostics that should not fail.

// Recognize nested accesses as part of the same dependence scope.
func bv_get_mutate(container: inout NC) -> _mutate(container) BV {
  container.getBV()
}

// Test lifetime inheritance through chained consumes.
//
// This requires an inherit_lifetime marker on the argument.
func bv_derive(bv: consuming BV) -> _consume(bv) BV {
  bv.derive()
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

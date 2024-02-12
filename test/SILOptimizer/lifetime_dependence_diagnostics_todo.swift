// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -module-name test \
// RUN:   -enable-builtin-module \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -enable-experimental-feature NoncopyableGenerics

// REQUIRES: swift_in_compiler
// REQUIRES: noncopyable_generics

// Future tests for LifetimeDependenceDiagnostics.
// REQUIRES: disabled

// FIXME: Our debug locations on closure captures are incorrect.
// FIXME: We return two errors: one for the caller, and one for the closure.
func bvcons_capture_escape(bv: consuming BV) -> ()->Int {
  return { bv.c }  // expected-error{{lifetime-dependent variable 'bv' escapes its scope}}
  // expected-note @-1 {{this use causes the lifetime-dependent value to escape}}
}

func bvcons_capture_escapelet(bv: consuming BV) -> ()->Int {
  let closure = { bv.c } // expected-error{{lifetime-dependent variable 'bv' escapes its scope}}
  // expected-note @-1 {{this use causes the lifetime-dependent value to escape}}
  return closure
}

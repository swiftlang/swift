// RUN: %target-typecheck-verify-swift \
// RUN:   -disable-availability-checking

// These tests complement lifetime_depend_noattr.swift. If you add a test here, add one there.

// Check that functions that require lifetime dependence are prohibited without the flag.

// Don't allow empty initialization.
struct EmptyNonEscapable: ~Escapable {} // expected-error{{an implicit initializer with a ~Escapable result requires '-enable-experimental-feature LifetimeDependence'}}

// Don't allow non-Escapable return values.
func neReturn(span: RawSpan) -> RawSpan { span } // expected-error{{a function with a ~Escapable result requires '-enable-experimental-feature LifetimeDependence'}}

func neInout(span: inout RawSpan) {} // expected-error{{a function with a ~Escapable 'inout' parameter requires '-enable-experimental-feature LifetimeDependence'}}

struct S {
  func neReturn(span: RawSpan) -> RawSpan { span } // expected-error{{a method with a ~Escapable result requires '-enable-experimental-feature LifetimeDependence'}}

  func neInout(span: inout RawSpan) {} // expected-error{{a method with a ~Escapable 'inout' parameter requires '-enable-experimental-feature LifetimeDependence'}}
}

class C {
  func neReturn(span: RawSpan) -> RawSpan { span } // expected-error{{a method with a ~Escapable result requires '-enable-experimental-feature LifetimeDependence'}}

  func neInout(span: inout RawSpan) {} // expected-error{{a method with a ~Escapable 'inout' parameter requires '-enable-experimental-feature LifetimeDependence'}}
}

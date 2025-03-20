// RUN: %target-typecheck-verify-swift \
// RUN:   -disable-availability-checking \
// RUN:   -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_feature_LifetimeDependence

// These tests complement lifetime_depend_nofeature.swift. If you add a test here, add one there.
 
// Check that missing lifetime dependencies are diagnosed. Enabling LifetimeDependencies will issue more detailed
// diagnostics.

// Allow empty initialization.
struct EmptyNonEscapable: ~Escapable {} // OK - no dependence

// Don't allow non-Escapable return values.
func neReturn(span: RawSpan) -> RawSpan { span } // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@lifetime(borrow span)' or '@lifetime(copy span)'}}

func neInout(span: inout RawSpan) {} // OK - inferred

struct S {
  func neReturn(span: RawSpan) -> RawSpan { span } // expected-error{{a method with a ~Escapable result requires '@lifetime(...)}}

  func neInout(span: inout RawSpan) {} // OK - inferred
}

class C {
  func neReturn(span: RawSpan) -> RawSpan { span } // expected-error{{a method with a ~Escapable result requires '@lifetime(...)'}}

  func neInout(span: inout RawSpan) {} // OK - inferred
}

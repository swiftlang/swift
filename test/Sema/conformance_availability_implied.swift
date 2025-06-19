// RUN: %target-typecheck-verify-swift -swift-version 6
// REQUIRES: OS=macosx

// Protocols:

protocol Base {
  func f() // expected-note {{protocol requirement here}}
}

func takesBase<T: Base>(_: T.Type) {}

protocol Derived1: Base {}
protocol Derived2: Base {}

@_marker protocol MarkerBase {}

func takesMarkerBase<T: MarkerBase>(_: T.Type) {}

protocol MarkerDerived1: MarkerBase {}
protocol MarkerDerived2: MarkerBase {}

// Verify that the implied conformance is macOS 100:
struct Conformer1 {}

@available(macOS 100, *)
extension Conformer1: Derived1 {
  func f() {}  // okay!
}

@available(macOS 200, *)
extension Conformer1: Derived2 {}

takesBase(Conformer1.self)
// expected-error@-1 {{conformance of 'Conformer1' to 'Base' is only available in macOS 100 or newer}}
// expected-note@-2 {{add 'if #available' version check}}

// No warning about redundant MarkerBase conformance (rdar://142873265):
@available(macOS 100, *)
extension Conformer1: MarkerDerived1 {}

@available(macOS 200, *)
extension Conformer1: MarkerDerived2 {}

takesMarkerBase(Conformer1.self)
// expected-error@-1 {{conformance of 'Conformer1' to 'MarkerBase' is only available in macOS 100 or newer}}
// expected-note@-2 {{add 'if #available' version check}}

// Bad availability on the Base.f() witness:
struct Conformer2 {}

@available(macOS 100, *)
extension Conformer2: Derived1 {
// expected-error@-1 {{protocol 'Base' requires 'f()' to be available in macOS 100 and newer}}
}

@available(macOS 200, *)
extension Conformer2: Derived2 {
  func f() {} // expected-note {{'f()' declared here}}
}

// RUN: %target-swift-frontend -typecheck -verify %s %S/Inputs/conformance_availability_implied_other.swift -swift-version 6
// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/conformance_availability_implied_other.swift %s -swift-version 6
// REQUIRES: OS=macosx

protocol Base {
  func f()
}

func takesBase<T: Base>(_: T.Type) {}

protocol Derived1: Base {}
protocol Derived2: Base {}

// Verify that the implied conformance is macOS 100:
struct Conformer1 {}

@available(macOS 100, *)
extension Conformer1: Derived1 {
  func f() {}  // okay!
}

// Note that Conformer1: Derived2 is in the other file

func check1() {
// expected-note@-1 {{add '@available' attribute to enclosing global function}}
  takesBase(Conformer1.self)
  // expected-error@-1 {{conformance of 'Conformer1' to 'Base' is only available in macOS 100 or newer}}
  // expected-note@-2 {{add 'if #available' version check}}
}

// Verify that the implied conformance is macOS 200:
// FIXME: This appears to be unsound!
struct Conformer2 {}

@available(macOS 200, *)
extension Conformer2: Derived2 {
  func f() {}
}

// Note that Conformer2: Derived1 is in the other file

func check2() {
// expected-note@-1 {{add '@available' attribute to enclosing global function}}
  takesBase(Conformer2.self)
  // expected-error@-1 {{conformance of 'Conformer2' to 'Base' is only available in macOS 200 or newer}}
  // expected-note@-2 {{add 'if #available' version check}}
}


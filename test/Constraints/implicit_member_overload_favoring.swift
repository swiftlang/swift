// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -enable-experimental-feature ImplicitMemberOnFunctionType

// REQUIRES: swift_feature_ImplicitMemberOnFunctionType

// The constraint optimizer analyzes implicit member (leading-dot) arguments
// by member name to favor / prune overload choices. That analysis must be
// all-or-nothing across an overload set: when any choice's parameter type is
// unanalyzable (Double/CGFloat/String, optionals whose `Optional` type has a
// member with the name, existentials, ...), no favoring may be applied at
// all — otherwise the unanalyzable choice would be skipped by the solver and
// existing ambiguities (or baseline overload selections) would silently
// change. These tests pin the pre-optimizer behavior; they must behave
// identically with and without the feature flag.

// An analyzable nominal choice must not shadow an unanalyzable Double choice.
struct Q {
  static var pi = Q() // expected-note {{found this candidate}}
}
func f0(_ x: Q) {}
func f0(_ x: Double) {}

func testAmbiguousStaysAmbiguous() {
  f0(.pi) // expected-error {{ambiguous use of 'pi'}}
}

// When both overloads are viable but ranking prefers the exact `Double`
// match over optional injection, favoring must not flip the selection.
func f1(_ x: Q?) -> String { "" }
func f1(_ x: Double) -> Int { 0 }

func testSelectionNotFlipped() {
  let r = f1(.pi)
  let _: Int = r // baseline ranking picks f1(_: Double)
}

// A name that exists on `Optional` itself (`some`/`none`) makes the optional
// choice unanalyzable; the ambiguity with a same-named nominal member stays.
struct S2 {
  static func some(_ x: Int) -> S2 { S2() } // expected-note {{found this candidate}}
  static var none = S2() // expected-note {{found this candidate}}
}
func f2(_ x: S2) {}
func f2(_ x: Int?) {}

func testOptionalMemberNamesStayAmbiguous() {
  f2(.some(1)) // expected-error {{ambiguous use of 'some'}}
  f2(.none) // expected-error {{ambiguous use of 'none'}}
}

// Standard-library `Double.pi` vs a same-named enum case.
enum EPi { case pi } // expected-note {{found this candidate}}
func f3(_ x: Double) -> Int { 0 }
func f3(_ x: EPi) -> String { "" }

func testStdlibMemberStaysAmbiguous() {
  _ = f3(.pi) // expected-error {{ambiguous use of 'pi'}}
}

// Both choices unanalyzable (Double and Float are both on the bail list).
func f4(_ x: Double) -> Int { 0 }
func f4(_ x: Float) -> String { "" }

func testBothUnanalyzableStaysAmbiguous() {
  _ = f4(.pi) // expected-error {{ambiguous use of 'pi'}}
}

// An existential choice (SE-0299 static members on protocol extensions) is
// unanalyzable; ambiguity with an enum case of the same name stays.
protocol Style {}
struct Bold: Style {}
extension Style where Self == Bold {
  static var fancy: Bold { Bold() } // expected-note {{found this candidate}}
}
enum Mode { case fancy } // expected-note {{found this candidate}}
func apply(_ s: any Style) -> Int { 0 }
func apply(_ m: Mode) -> String { "" }

func testExistentialStaysAmbiguous() {
  _ = apply(.fancy) // expected-error {{ambiguous use of 'fancy'}}
}

// Positive control: when every choice is analyzable the favoring applies
// and selects the (only possible) value overload for a no-payload case.
enum E { case a; case b(Int) }
func g(_ x: E) -> Int { 0 }
func g<P>(_ x: (P) -> E) -> String { "" }

func testFavoringStillWorks() {
  let r = g(.a)
  let _: Int = r
}

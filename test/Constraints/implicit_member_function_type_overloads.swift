// RUN: %target-typecheck-verify-swift -enable-experimental-feature ImplicitMemberOnFunctionType

// REQUIRES: swift_feature_ImplicitMemberOnFunctionType

// Implicit member syntax through a function type behaves exactly like the
// explicit 'Type.member' form: it can't introduce an ambiguity that the
// explicit spelling doesn't already have, and the return-type look-through is
// ranked strictly below any direct member of the contextual type.

struct Theme {}
extension Theme {
  static var custom: (String) -> Theme { { _ in Theme() } }
  // An *instance* method is not a competitor: its unapplied reference carries
  // 'self' ('(Theme) -> (String) -> Theme'), so it can't be '(String) -> Theme'.
  func custom(name: String) -> Theme { self }
}

// Resolves unambiguously to the static var (the instance method doesn't match).
let a1: (String) -> Theme = .custom

struct Palette {}
extension Palette {
  static var custom: (String) -> Palette { { _ in Palette() } }
  // Even a static func with the same signature isn't ambiguous: a property is
  // preferred over an unapplied function of the same name (pre-existing rule).
  static func custom(_ name: String) -> Palette { Palette() }
}

let a2: (String) -> Palette = .custom

// An implicit member argument to an *overloaded* call that can't be satisfied
// must still produce a real diagnostic (regression: this used to emit the
// internal "failed to produce diagnostic" fallback).
struct Widget {}
extension Widget {
  func update(_ x: Int) -> Widget { self } // instance method: carries self
}

func process(_ handler: (Int) -> Widget) {}
func process(_ handler: (String) -> Widget) {}

func testInstanceMethodDoesNotMatch() {
  process(.update)
  // expected-error@-1 {{member 'update' expects argument of type 'Widget'}}
}

// A genuine ambiguity across the return type's members is diagnosed as such,
// not silently resolved.
struct View {}
extension Widget {
  static func make(_ x: Int) -> Widget { Widget() } // expected-note {{found this candidate}}
}
extension View {
  static func make(_ x: Double) -> View { View() } // expected-note {{found this candidate}}
}

func build(_ handler: (Int) -> Widget) {}
func build(_ handler: (Double) -> View) {}

func testGenuineAmbiguity() {
  build(.make) // expected-error {{ambiguous use of 'make'}}
}

// A static factory with the wrong arity for the parameter (e.g. a parameter
// was added and a call site wasn't updated) reports a conversion failure, not
// the internal "failed to produce diagnostic" fallback.
struct Card {}
extension Card {
  static func make(_ title: String, _ subtitle: String) -> Card { Card() }
}

func render(_ build: (String) -> Card) {}
func render(_ build: (Int) -> Card) {}

func testWrongArity() {
  render(.make)
  // expected-error@-1 {{cannot convert value of type '(String, String) -> Card' to expected argument type '(String) -> Card'}}
}

// Source compatibility: a member found *directly* on one overload's parameter
// type outranks a same-named member reached by *looking through* another
// overload's function-typed parameter -- even when both overloads are
// non-generic, so nothing else can break the tie. 'route(.item)' resolved to
// the value overload before this feature existed and must still do so; without
// the strictly-lower-priority ranking of the look-through the two would tie and
// this would regress to 'ambiguous use of item'.
struct Anchor { static let item = Anchor() }
enum Node { case item(Int) }

func route(_ x: Anchor) -> Int { 0 }
func route(_ f: (Int) -> Node) -> String { "" }

func testDirectMemberOutranksLookThrough() {
  let selected = route(.item)
  let _: Int = selected // Anchor.item (direct) wins: not ambiguous, not String
}

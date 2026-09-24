// A plain (non-extended) existential container -- `Any`, `AnyObject`, `any P`,
// `Error` -- has nowhere to record a suppressed requirement, so it always requires
// its payload to be both `Copyable` and `Escapable`. A type that suppresses either
// does not inhabit it, and the cast must fail.
//
// An existential that *does* suppress a requirement, like `any P & ~Escapable`, is
// an extended existential carrying the inverse in its requirement signature, and
// must keep working. Those are the two halves this checks.
//
// Every subject here is a *concrete* type, which is deliberate: the compiler
// decides these on its own, so this passes without the matching runtime change.
// The cases that reach the runtime instead -- a generic or existential subject,
// where the static type does not determine the dynamic type and so nothing can be
// folded -- need the invertible-requirement guard in `tryCast`, and are covered
// separately once that lands.

// RUN: %target-run-simple-swift(-enable-experimental-feature Lifetimes) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Lifetimes) | %FileCheck %s
// Both deployment targets, because they obtain the target metadata differently:
// the future target instantiates it from a mangled name, the default target builds
// it structurally in IRGen. Only the second dropped the inverse.
// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature Lifetimes) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -target %target-future-triple -enable-experimental-feature Lifetimes) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Lifetimes

protocol P: ~Escapable {}

/// Nonescapable, and conforming to `P` -- so the conformance check succeeds while
/// the container still cannot hold it. That combination is the whole point: a
/// conformance is not sufficient.
struct NE: P, ~Escapable {
  var t: Int
  @_lifetime(immortal) init(_ t: Int) { self.t = t }
}

/// Copyable and escapable. Every answer for this must be unchanged, or the new
/// rule is rejecting too much.
struct OK: P { var t = 1 }

var checks = 0
var failures = 0

func expect(_ label: String, _ got: Bool, _ want: Bool) {
  checks += 1
  if got != want {
    failures += 1
    print("WRONG \(label): got \(got), want \(want)")
  }
}

/// `is` and `as?` must agree. If they diverge, the rule reached only one of them.
func both(_ label: String, _ viaIs: Bool, _ viaAs: Bool, _ want: Bool) {
  expect("\(label) (is)", viaIs, want)
  expect("\(label) (as?)", viaAs, want)
}

// MARK: - a nonescapable value inhabits none of these

func neToAnyP(_ x: consuming NE) -> Bool { x is any P }
func neToAnyPAs(_ x: consuming NE) -> Bool { (x as? any P) != nil }
func neToAnyObject(_ x: consuming NE) -> Bool { x is AnyObject }
func neToAnyObjectAs(_ x: consuming NE) -> Bool { (x as? AnyObject) != nil }
func neToError(_ x: consuming NE) -> Bool { x is Error }
func neToErrorAs(_ x: consuming NE) -> Bool { (x as? Error) != nil }

both("NE -> any P", neToAnyP(NE(1)), neToAnyPAs(NE(1)), false)
both("NE -> AnyObject", neToAnyObject(NE(1)), neToAnyObjectAs(NE(1)), false)
both("NE -> Error", neToError(NE(1)), neToErrorAs(NE(1)), false)

// MARK: - the copyable, escapable control must be unaffected

func okToAny(_ x: consuming OK) -> Bool { x is Any }
func okToAnyAs(_ x: consuming OK) -> Bool { (x as? Any) != nil }
func okToAnyP(_ x: consuming OK) -> Bool { x is any P }
func okToAnyPAs(_ x: consuming OK) -> Bool { (x as? any P) != nil }
func okToAnyObject(_ x: consuming OK) -> Bool { x is AnyObject }
func okToAnyObjectAs(_ x: consuming OK) -> Bool { (x as? AnyObject) != nil }

both("OK -> Any", okToAny(OK()), okToAnyAs(OK()), true)
both("OK -> any P", okToAnyP(OK()), okToAnyPAs(OK()), true)
both("OK -> AnyObject", okToAnyObject(OK()), okToAnyObjectAs(OK()), true)

// Ordinary types must still land in `Any` and `AnyObject`. That would be a very
// large regression to miss, so it is worth stating rather than assuming.
func anyOf<T>(_ x: T) -> Bool { x is Any }
func anyObjectOf<T>(_ x: T) -> Bool { x is AnyObject }
expect("Int -> Any", anyOf(42), true)
expect("String -> Any", anyOf("s"), true)
expect("[Int] -> Any", anyOf([1, 2]), true)
expect("String -> AnyObject", anyObjectOf("s"), true)
expect("OK -> Any (generic)", anyOf(OK()), true)

// CHECK: checks={{[0-9]+}} failures=0
print("checks=\(checks) failures=\(failures)")

// MARK: - an existential that suppresses the requirement still accepts it

// This is the destination a `~Escapable` value *does* inhabit, and it pins the
// IRGen half. Below the availability threshold the target metadata is built
// structurally rather than instantiated from a mangled name, and the structural
// path considered only parameterized protocols when deciding whether an extended
// shape was needed. `any P & ~Escapable` therefore came out as plain `any P`, so
// the runtime was asked a different question than the source asked -- and answered
// that one correctly, giving `false` here.
func suppressing(_ x: consuming NE) -> Bool { x is any P & ~Escapable }
func suppressingOK(_ x: consuming OK) -> Bool { x is any P & ~Escapable }

// CHECK-NEXT: suppressing: true true
print("suppressing:", suppressing(NE(1)), suppressingOK(OK()))

// Verify dynamic casting for ~Escapable existentials

// Verify both with and without optimizations, and both back-deployed and future targets.

// RUN: %target-run-simple-swift(-enable-experimental-feature Lifetimes) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Lifetimes) | %FileCheck %s
// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature Lifetimes) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -target %target-future-triple -enable-experimental-feature Lifetimes) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Lifetimes
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

protocol P: ~Escapable {}

/// Nonescapable, and conforming to a protocol whose *existential* is still
/// `Copyable & Escapable` -- so `any P` must reject it even though the
/// conformance check succeeds.
struct NE: P, ~Escapable {
  var t: Int
  @_lifetime(immortal) init(_ t: Int) { self.t = t }
}

/// The copyable, escapable control. Every answer below must stay `true` for it,
/// or the guard is rejecting too much.
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

// Force the cast decision to occur at runtime without any compiler simplification
func isT<S: ~Escapable, T>(_ x: consuming S, _: T.Type) -> Bool { x is T }
func asT<S: ~Escapable, T>(_ x: consuming S, _: T.Type) -> Bool { (x as? T) != nil }

/// `is` and `as?` must give the same answer. If they diverge the guard was added
/// to only one of the two paths.
func both(_ label: String, _ viaIs: Bool, _ viaAs: Bool, _ want: Bool) {
  expect("\(label) (is)", viaIs, want)
  expect("\(label) (as?)", viaAs, want)
}

// MARK: - a nonescapable value inhabits none of these

both("NE -> Any", isT(NE(1), Any.self), asT(NE(1), Any.self), false)
both("NE -> any P", isT(NE(1), (any P).self), asT(NE(1), (any P).self), false)
both("NE -> AnyObject", isT(NE(1), AnyObject.self), asT(NE(1), AnyObject.self), false)
both("NE -> Any?", isT(NE(1), Optional<Any>.self), asT(NE(1), Optional<Any>.self), false)
both("NE -> Error", isT(NE(1), Error.self), asT(NE(1), Error.self), false)
both("NE -> AnyHashable", isT(NE(1), AnyHashable.self), asT(NE(1), AnyHashable.self), false)
// (No `NE -> NE` row: a generic `T` is default `Escapable`, so the identity
// case cannot be spelled through this helper. It never reaches an existential
// strategy anyway.)

// MARK: - the copyable, escapable control must be unaffected

both("OK -> Any", isT(OK(), Any.self), asT(OK(), Any.self), true)
both("OK -> any P", isT(OK(), (any P).self), asT(OK(), (any P).self), true)
both("OK -> AnyObject", isT(OK(), AnyObject.self), asT(OK(), AnyObject.self), true)
both("OK -> Any?", isT(OK(), Optional<Any>.self), asT(OK(), Optional<Any>.self), true)
both("OK -> OK", isT(OK(), OK.self), asT(OK(), OK.self), true)
both("OK -> Error", isT(OK(), Error.self), asT(OK(), Error.self), false)
both("OK -> Int", isT(OK(), Int.self), asT(OK(), Int.self), false)

// Ordinary types must still land in `Any` and `AnyObject`; this would be a very
// large regression to miss.
expect("Int -> Any", isT(42, Any.self), true)
expect("String -> Any", isT("s", Any.self), true)
expect("String -> AnyObject", isT("s", AnyObject.self), true)
expect("[Int] -> Any", isT([1, 2], Any.self), true)
expect("Int -> AnyHashable", isT(42, AnyHashable.self), true)

// CHECK: checks={{[0-9]+}} failures=0
print("checks=\(checks) failures=\(failures)")

// MARK: - explicit ~Escapable is required for existentials

func isSuppressing(_ x: consuming NE) -> Bool { x is any P & ~Escapable }
func isSuppressingOK(_ x: consuming OK) -> Bool { x is any P & ~Escapable }

// CHECK-NEXT: suppressing: true true
print("suppressing:", isSuppressing(NE(1)), isSuppressingOK(OK()))

// Verify the subject is still usable after the test; `is` should not consume it.

func repeated(_ x: borrowing any P & ~Escapable) -> String {
  "\(x is NE) \(x is NE) \(x is NE)"
}
// CHECK: repeated: true true true
print("repeated:", repeated(NE(7)))

// Executes the IRGen fallback for `checked_cast_addr_br test_only`: the path
// taken when the deployment target predates `swift_dynamicCastTest`. That is
// every shipping OS until the new runtime is generally available, so it is the
// path most code will actually run, yet only its IR shape was covered before.
//
// Note the absence of `-target %target-future-triple`: the default deployment
// target is what selects the fallback. The subjects here are all `Copyable`,
// because casting out of a non-`Copyable` existential is diagnosed at this
// deployment target -- which is exactly what makes the fallback's copy safe.

// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature NoncopyableCasting | %FileCheck %s --check-prefix=IR
// RUN: %target-run-simple-swift(-enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_NoncopyableCasting

// The fallback must go to the general entry point, not the dedicated one.
//
// IR-NOT: @swift_dynamicCastTest
// IR: @swift_dynamicCast
// IR-NOT: @swift_dynamicCastTest

protocol Q {}

/// Small enough to live inline in the container.
struct Small: Q { var t: Int }

/// Too large to store inline, so the container boxes it out-of-line. This is the
/// shape whose payload an older runtime copies rather than takes.
struct Boxed: Q { var a, b, c, d, e, f, g, h: Int }

/// Non-trivial: the fallback's copy-then-discard has to balance a retain.
final class Tracked: Q {
  static var live = 0
  init() { Tracked.live += 1 }
  deinit { Tracked.live -= 1 }
}

struct Unrelated: Q {}

/// Generic source and target, so the cast takes the address path and lowers to
/// `test_only`.
func isa<S, T>(_ x: S, _: T.Type) -> Bool { x is T }

// MARK: - answers

// CHECK: small: true false false
print("small:", isa(Small(t: 1), Small.self), isa(Small(t: 1), Boxed.self),
      isa(Small(t: 1), Unrelated.self))

// CHECK-NEXT: boxed: true false
print("boxed:", isa(Boxed(a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8), Boxed.self),
      isa(Boxed(a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8), Small.self))

// CHECK-NEXT: existential: true false
let q: any Q = Boxed(a: 9, b: 0, c: 0, d: 0, e: 0, f: 0, g: 0, h: 0)
print("existential:", isa(q, Boxed.self), isa(q, Small.self))

// The subject survives, so it can be tested repeatedly.
// CHECK-NEXT: repeatable: true true true
print("repeatable:", isa(q, Boxed.self), isa(q, Boxed.self), isa(q, Boxed.self))

// MARK: - the fallback copies and then discards; that must balance

func hammer() {
  let t = Tracked()
  let box: any Q = t
  var trues = 0
  for _ in 0..<500 {
    if isa(box, Tracked.self) { trues += 1 }
    _ = isa(box, Boxed.self)
    _ = isa(box, Unrelated.self)
    _ = isa(box, Small.self)
  }
  // CHECK-NEXT: hammer: 500 live=1
  print("hammer:", trues, "live=\(Tracked.live)")
  _ = t
}
hammer()

// CHECK-NEXT: afterScope: live=0
print("afterScope: live=\(Tracked.live)")

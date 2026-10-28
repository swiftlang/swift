// The back-deployment half of dynamic_cast_test_fallback.swift.
//
// That test pins IRGen's choice of the fallback and the refcount balance, but it
// runs under lit's default configuration, where the library load path points at
// the just-built stdlib. So it exercises the fallback against the *new*
// `swift_dynamicCast` -- including this series' refactor of
// `tryCastToExtendedExistential` -- which is not the code that will actually be
// underneath it on shipping systems.
//
// This file requires `use_os_stdlib`, so it only runs when lit is invoked with
// `--param use_os_stdlib` and the OS runtime is loaded instead. That is the
// configuration that tests the claim the back-deployment design rests on: that
// a shipped `swift_dynamicCast` handles what the fallback asks of it.
//
// The payloads here are all `Copyable`, which is the whole point. The fallback
// copies on success, and a non-`Copyable` payload cannot survive that -- which
// is why casting out of a non-`Copyable` existential is *diagnosed* at these
// deployment targets rather than reaching this path. Both halves have to hold
// for back deployment to be sound; this file covers the half that runs.

// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature NoncopyableCasting | %FileCheck %s --check-prefix=IR
// RUN: %target-run-simple-swift(-enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: use_os_stdlib
// REQUIRES: swift_feature_NoncopyableCasting

// If this ever starts calling the dedicated entry point, the test has stopped
// covering what it is for: an OS runtime does not have that symbol, so the
// failure would be a launch failure rather than a wrong answer.
//
// IR-NOT: @swift_dynamicCastTest
// IR: @swift_dynamicCast
// IR-NOT: @swift_dynamicCastTest

protocol Q {}

/// Stored inline in the container.
struct Small: Q { var t: Int }

/// Too large to store inline, so the container boxes it out-of-line. This is the
/// shape whose payload a shipped runtime copies rather than takes, because
/// `tryCastUnwrappingExistentialSource` passes `takeOnSuccess && (srcInnerValue
/// == srcValue)`.
struct Boxed: Q { var a, b, c, d, e, f, g, h: Int }

/// Non-trivial, so the copy the shipped runtime performs has to be balanced by
/// the discard the fallback emits.
final class Tracked: Q {
  static var live = 0
  init() { Tracked.live += 1 }
  deinit { Tracked.live -= 1 }
}

struct Unrelated: Q {}

/// Generic source and target, so the cast takes the address path and lowers to
/// `checked_cast_addr_br test_only`.
func isa<S, T>(_ x: S, _: T.Type) -> Bool { x is T }

// MARK: - answers must match what the dedicated entry point gives

// CHECK: small: true false false
print("small:", isa(Small(t: 1), Small.self), isa(Small(t: 1), Boxed.self),
      isa(Small(t: 1), Unrelated.self))

// CHECK-NEXT: boxed: true false
let bigOne = Boxed(a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8)
print("boxed:", isa(bigOne, Boxed.self), isa(bigOne, Small.self))

// CHECK-NEXT: existential: true false
let q: any Q = Boxed(a: 9, b: 0, c: 0, d: 0, e: 0, f: 0, g: 0, h: 0)
print("existential:", isa(q, Boxed.self), isa(q, Small.self))

// The source is untouched, so the same subject answers the same way repeatedly.
// CHECK-NEXT: repeatable: true true true
print("repeatable:", isa(q, Boxed.self), isa(q, Boxed.self), isa(q, Boxed.self))

// A class payload in the container: the interesting case for the copy.
// CHECK-NEXT: classPayload: true false
let tracked: any Q = Tracked()
print("classPayload:", isa(tracked, Tracked.self), isa(tracked, Small.self))

// MARK: - the shipped runtime's copy, and our discard, must balance

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
  // An over-release would have trapped long before here; a leak shows up as a
  // live count above the one reference this scope holds.
  // CHECK-NEXT: hammer: 500 live=2
  print("hammer:", trues, "live=\(Tracked.live)")
  _ = t
}
hammer()

// Only the `tracked` binding above is still alive.
// CHECK-NEXT: afterScope: live=1
print("afterScope: live=\(Tracked.live)")

// MARK: - `is` and `as?` must still agree on this runtime

// The full agreement matrix lives in dynamic_cast_is_as_agreement.swift, which
// cannot run here because its future-triple RUN lines need the dedicated entry
// point. This is the subset that matters for the fallback: `as?` keeps the
// value-producing lowering, so it is the reference for what `is` should answer
// when `is` goes through a scratch buffer instead.

var checks = 0
var mismatches = 0

func agree<S, T>(_ x: S, _: T.Type, _ label: String) {
  checks += 1
  let viaIs = x is T
  let viaAs = (x as? T) != nil
  if viaIs != viaAs {
    mismatches += 1
    print("MISMATCH \(label): is=\(viaIs) as?=\(viaAs)")
  }
}

func sweep<S>(_ v: S, _ n: String) {
  agree(v, Small.self, "\(n)->Small");      agree(v, Boxed.self, "\(n)->Boxed")
  agree(v, Tracked.self, "\(n)->Tracked");  agree(v, Unrelated.self, "\(n)->Unrelated")
  agree(v, (any Q).self, "\(n)->anyQ");     agree(v, Any.self, "\(n)->Any")
  agree(v, AnyObject.self, "\(n)->AnyObj"); agree(v, Int.self, "\(n)->Int")
  agree(v, [Int].self, "\(n)->[Int]");      agree(v, (Int, Int).self, "\(n)->tuple")
  agree(v, Optional<Int>.self, "\(n)->Int?")
}

sweep(Small(t: 1), "Small")
sweep(bigOne, "Boxed")
sweep(Tracked(), "Tracked")
sweep(Unrelated(), "Unrelated")
sweep(q, "anyQBoxed")
sweep(42, "Int")
sweep([1, 2, 3], "IntArr")
sweep((1, 2), "tuple")
sweep(Optional<Int>.some(3), "SomeInt")
sweep(Optional<Int>.none, "NoneInt")

// CHECK-NEXT: agreement: checks={{[0-9]+}} mismatches=0
print("agreement: checks=\(checks) mismatches=\(mismatches)")

// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// `as?` and `as!` with a non-`Copyable` type on either side: a concrete or
// generic source, and an existential target. The payload is moved out only on
// success, so a failed `as?` leaves the subject for the next attempt, and every
// value is destroyed exactly once.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}

final class Canary {
  static var live = 0
  init() { Canary.live += 1 }
  deinit { Canary.live -= 1 }
}

struct Small: P, ~Copyable { var tag: Int }
struct Both: P, Q, ~Copyable { var tag: Int }
struct WithCanary: P, Q, ~Copyable { let canary: Canary; var tag: Int }
struct Unrelated: ~Copyable { var tag: Int }

// Concrete source, existential target.
func concreteToExistential(_ x: consuming Both) -> Int {
  if let q = x as? (any Q & ~Copyable) { _ = q; return 1 }
  return 0
}

// Existential source, existential target.
func existentialToExistential(_ x: consuming any P & ~Copyable) -> Int {
  if let q = x as? (any Q & ~Copyable) { _ = q; return 1 }
  if let s = x as? Small { return s.tag }
  return 0
}

// Generic source.
func genericSource<T: ~Copyable>(_ x: consuming T) -> Int {
  if let s = x as? Small { return s.tag }
  if let q = x as? (any Q & ~Copyable) { _ = q; return 2 }
  return 0
}

func forced(_ x: consuming any P & ~Copyable) -> Int {
  let s = x as! Small
  return s.tag
}

func forcedToExistential(_ x: consuming Both) -> Int {
  let q = x as! (any Q & ~Copyable)
  _ = q
  return 1
}

// CHECK: concrete->existential: 1
print("concrete->existential:", concreteToExistential(Both(tag: 1)))
// CHECK: existential->existential: 1 7 0
print("existential->existential:",
      existentialToExistential(Both(tag: 1)),
      existentialToExistential(Small(tag: 7)),
      existentialToExistential(Small(tag: 7) as any P & ~Copyable) - 7)
// CHECK: generic: 3 2 0
print("generic:", genericSource(Small(tag: 3)), genericSource(Both(tag: 1)),
      genericSource(Unrelated(tag: 1)))
// CHECK: forced: 9 1
print("forced:", forced(Small(tag: 9)), forcedToExistential(Both(tag: 1)))

// A payload with a deinit is destroyed exactly once, bound or dropped.
Canary.live = 0
_ = concreteToExistential(Both(tag: 1))
_ = existentialToExistential(WithCanary(canary: Canary(), tag: 1))
_ = genericSource(WithCanary(canary: Canary(), tag: 1))
_ = genericSource(Unrelated(tag: 1))
// CHECK: live canaries: 0
print("live canaries:", Canary.live)

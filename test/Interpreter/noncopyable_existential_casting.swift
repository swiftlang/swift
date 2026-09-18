// RUN: %target-run-simple-swift(-enable-experimental-feature NoncopyableCasting)

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test

// Casting a noncopyable payload out of an existential requires the
// take-instead-of-copy support added to swift_dynamicCast (see
// tryCastUnwrappingExistentialSource). An older, OS-resident runtime copies
// the payload instead, which traps in the noncopyable type's copy value
// witness -- so this cannot run against the OS stdlib or a back-deployed one.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// End-to-end verification that `is`/`as?`/`as!` work correctly on a
// noncopyable existential value: SILGen and the move-only checker must
// agree that the cast fully consumes its source (a `checked_cast_addr_br`
// or `unconditional_checked_cast_addr` with `take_always` semantics), for
// both inline-sized and boxed (out-of-line) payloads.

protocol P: ~Copyable {}

// Small enough to be stored inline in the existential container.
struct Small: ~Copyable, P {
  var tag: Int
}

// Large enough to force the existential to box the payload out-of-line.
struct Big: ~Copyable, P {
  var tag: Int
  var pad0, pad1, pad2, pad3, pad4, pad5, pad6: Int
}

struct Unrelated: ~Copyable, P {}

final class Canary {
  static var deinitCount = 0
  deinit { Canary.deinitCount += 1 }
}

// Large enough to be boxed, and carries a class reference so we can verify
// the box is freed -- and its payload destroyed -- exactly once.
struct BigWithCanary: ~Copyable, P {
  let canary: Canary
  var pad0, pad1, pad2, pad3, pad4, pad5: Int
}

func asOptional(_ box: consuming any P & ~Copyable) -> Int? {
  if let s = box as? Small { return s.tag }
  return nil
}

func asBang(_ box: consuming any P & ~Copyable) -> Int {
  let s = box as! Small
  return s.tag
}

func isCheck(_ box: consuming any P & ~Copyable) -> Bool {
  return box is Small
}

func asOptionalBoxed(_ box: consuming any P & ~Copyable) -> Int? {
  if let b = box as? Big { return b.tag }
  return nil
}

func asBangBoxed(_ box: consuming any P & ~Copyable) -> Int {
  let b = box as! Big
  return b.tag
}

func isCheckBoxed(_ box: consuming any P & ~Copyable) -> Bool {
  return box is Big
}

func takeCanary(_ box: consuming any P & ~Copyable) -> Bool {
  return box is BigWithCanary
}

// CHECK: as? inline success: Optional(42)
print("as? inline success:", asOptional(Small(tag: 42)) as Any)
// CHECK: as? inline failure: nil
print("as? inline failure:", asOptional(Unrelated()) as Any)
// CHECK: as! inline: 43
print("as! inline:", asBang(Small(tag: 43)))
// CHECK: is inline true: true
print("is inline true:", isCheck(Small(tag: 0)))
// CHECK: is inline false: false
print("is inline false:", isCheck(Unrelated()))

// CHECK: as? boxed success: Optional(99)
print("as? boxed success:", asOptionalBoxed(
  Big(tag: 99, pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0, pad6: 0)
) as Any)
// CHECK: as? boxed failure: nil
print("as? boxed failure:", asOptionalBoxed(Unrelated()) as Any)
// CHECK: as! boxed: 100
print("as! boxed:", asBangBoxed(
  Big(tag: 100, pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0, pad6: 0)
))
// CHECK: is boxed true: true
print("is boxed true:", isCheckBoxed(
  Big(tag: 0, pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0, pad6: 0)
))
// CHECK: is boxed false: false
print("is boxed false:", isCheckBoxed(Unrelated()))

// Leak/double-free check: bind the boxed, canary-carrying payload to a local,
// exercise a successful `is` check on it, and confirm the class reference
// inside gets destroyed exactly once.
func canaryLocalCheck() {
  let canaryBox: any P & ~Copyable =
    BigWithCanary(canary: Canary(), pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0)
  _ = takeCanary(canaryBox)
}
canaryLocalCheck()
// CHECK: canary deinit count: 1
print("canary deinit count:", Canary.deinitCount)

// MARK: - Cast patterns (`if case let x as T`)
//
// A *failed* cast pattern must leave the subject untouched, so a later pattern
// can still match it.

func classifyInline(_ box: consuming any P & ~Copyable) -> Int {
  if case let u as Unrelated = box { _ = u; return -1 }
  if case let s as Small = box { return s.tag }
  return -2
}

func classifyBoxed(_ box: consuming any P & ~Copyable) -> Int {
  if case let u as Unrelated = box { _ = u; return -1 }
  if case let b as Big = box { return b.tag }
  return -2
}

// The first pattern fails, so the second must still see an intact subject.
// CHECK: if case chained inline: 7
print("if case chained inline:", classifyInline(Small(tag: 7)))
// CHECK: if case chained boxed: 8
print("if case chained boxed:", classifyBoxed(
  Big(tag: 8, pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0, pad6: 0)))

// All patterns fail: the subject was never consumed, so it must be destroyed
// exactly once when it goes out of scope -- not zero times (leak) and not
// twice (double free).
func allPatternsFail(_ box: consuming any P & ~Copyable) -> Int {
  if case let u as Unrelated = box { _ = u; return -1 }
  if case let s as Small = box { _ = s; return -2 }
  return 0
}
// CHECK: if case all failed: 0
print("if case all failed:", allPatternsFail(
  BigWithCanary(canary: Canary(), pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0)))
// CHECK: canary deinit count after failed patterns: 2
print("canary deinit count after failed patterns:", Canary.deinitCount)

// The subject may also be a local variable or a stored property, not just a
// consuming parameter. These reach the cast's source operand through different
// storage (project_box / struct_element_addr), so cover them too: a failed
// pattern must still leave the subject intact.

func chainedOnLocal() -> Int {
  let box: any P & ~Copyable =
    Big(tag: 11, pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0, pad6: 0)
  if case let u as Unrelated = box { _ = u; return -1 }
  if case let b as Big = box { return b.tag }
  return -2
}
// CHECK: if case chained on local: 11
print("if case chained on local:", chainedOnLocal())

struct Holder: ~Copyable {
  var inner: any P & ~Copyable
}

func chainedOnProperty(_ h: consuming Holder) -> Int {
  if case let u as Unrelated = h.inner { _ = u; return -1 }
  if case let b as Big = h.inner { return b.tag }
  return -2
}
// CHECK: if case chained on property: 12
print("if case chained on property:", chainedOnProperty(Holder(
  inner: Big(tag: 12, pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0, pad6: 0))))

// A local that no pattern matches is never consumed, so it must be destroyed
// exactly once when it goes out of scope.
func localAllPatternsFail() -> Int {
  let box: any P & ~Copyable =
    BigWithCanary(canary: Canary(), pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0)
  if case let u as Unrelated = box { _ = u; return -1 }
  return 0
}
// CHECK: if case local all failed: 0
print("if case local all failed:", localAllPatternsFail())
// CHECK: canary deinit count after local failed patterns: 3
print("canary deinit count after local failed patterns:", Canary.deinitCount)

// MARK: - `as?` consumes unconditionally, unlike a cast pattern
//
// Forming an `Optional` with `as?` consumes the subject whether or not the cast
// succeeds: the result is a value, so there is no failure edge to leave the
// subject on. Only control-flow-affecting cast operations (cast patterns)
// consume conditionally. These two probes read the deinit count *before* scope
// exit, which distinguishes "consumed by the failed cast" from "still alive,
// destroyed later at scope exit".

func optionalFormConsumesOnFailure() -> Int {
  let box: any P & ~Copyable =
    BigWithCanary(canary: Canary(), pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0)
  if let u = box as? Unrelated { _ = u; return -1 }
  // The `as?` failed, but it still consumed `box`, so the canary is already
  // gone by the time we get here.
  return Canary.deinitCount
}
// CHECK: as? consumed on failure (4 => yes): 4
print("as? consumed on failure (4 => yes):", optionalFormConsumesOnFailure())

func patternFormDoesNotConsumeOnFailure() -> Int {
  let before = Canary.deinitCount
  let box: any P & ~Copyable =
    BigWithCanary(canary: Canary(), pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0)
  if case let u as Unrelated = box { _ = u; return -1 }
  // The pattern failed and left `box` intact, so nothing has been destroyed
  // yet -- it goes out of scope after this.
  return Canary.deinitCount - before
}
// CHECK: pattern left subject alive on failure (0 => yes): 0
print("pattern left subject alive on failure (0 => yes):",
      patternFormDoesNotConsumeOnFailure())

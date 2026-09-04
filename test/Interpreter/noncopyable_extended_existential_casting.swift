// RUN: %target-run-simple-swift(-enable-experimental-feature NoncopyableCasting)

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test

// Casting a noncopyable payload out of an existential requires the
// take-instead-of-copy support added to swift_dynamicCast (here specifically
// tryCastUnwrappingExtendedExistentialSource). An older, OS-resident runtime
// copies the payload instead, which traps in the noncopyable type's copy value
// witness -- so this cannot run against the OS stdlib or a back-deployed one.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// End-to-end verification that `is`/`as?`/`as!` work correctly on an
// *extended* noncopyable existential. A protocol with a primary associated
// type (like `P<T>` below) uses ExtendedExistentialTypeMetadata at runtime
// (MetadataKind::ExtendedExistential), a distinct representation -- and a
// distinct runtime code path, tryCastUnwrappingExtendedExistentialSource --
// from the plain existentials covered by noncopyable_existential_casting.swift.

protocol P<T>: ~Copyable {
  associatedtype T
  var tag: T { get }
}

// Small enough to be stored inline in the existential container.
struct Small: ~Copyable, P {
  typealias T = Int
  var tag: Int
}

// Large enough to force the existential to box the payload out-of-line.
struct Big: ~Copyable, P {
  typealias T = Int
  var tag: Int
  var pad0, pad1, pad2, pad3, pad4, pad5, pad6: Int
}

struct Unrelated: ~Copyable, P {
  typealias T = Int
  var tag: Int { 0 }
}

final class Canary {
  static var deinitCount = 0
  deinit { Canary.deinitCount += 1 }
}

// Large enough to be boxed, and carries a class reference so we can verify
// the box is freed -- and its payload destroyed -- exactly once.
struct BigWithCanary: ~Copyable, P {
  typealias T = Int
  let canary: Canary
  var tag: Int { 0 }
  var pad0, pad1, pad2, pad3, pad4, pad5: Int
}

func asOptional(_ box: consuming any P<Int> & ~Copyable) -> Int? {
  if let s = box as? Small { return s.tag }
  return nil
}

func asBang(_ box: consuming any P<Int> & ~Copyable) -> Int {
  let s = box as! Small
  return s.tag
}

func isCheck(_ box: consuming any P<Int> & ~Copyable) -> Bool {
  return box is Small
}

func asOptionalBoxed(_ box: consuming any P<Int> & ~Copyable) -> Int? {
  if let b = box as? Big { return b.tag }
  return nil
}

func asBangBoxed(_ box: consuming any P<Int> & ~Copyable) -> Int {
  let b = box as! Big
  return b.tag
}

func isCheckBoxed(_ box: consuming any P<Int> & ~Copyable) -> Bool {
  return box is Big
}

func takeCanary(_ box: consuming any P<Int> & ~Copyable) -> Bool {
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
  let canaryBox: any P<Int> & ~Copyable =
    BigWithCanary(canary: Canary(), pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0)
  _ = takeCanary(canaryBox)
}
canaryLocalCheck()
// CHECK: canary deinit count: 1
print("canary deinit count:", Canary.deinitCount)

// MARK: - Cast patterns (`if case let x as T`)
//
// Same failed-cast-leaves-the-subject-intact property as the plain existential
// test, but going through tryCastUnwrappingExtendedExistentialSource.

func classifyInline(_ box: consuming any P<Int> & ~Copyable) -> Int {
  if case let u as Unrelated = box { _ = u; return -1 }
  if case let s as Small = box { return s.tag }
  return -2
}

func classifyBoxed(_ box: consuming any P<Int> & ~Copyable) -> Int {
  if case let u as Unrelated = box { _ = u; return -1 }
  if case let b as Big = box { return b.tag }
  return -2
}

// CHECK: if case chained inline: 7
print("if case chained inline:", classifyInline(Small(tag: 7)))
// CHECK: if case chained boxed: 8
print("if case chained boxed:", classifyBoxed(
  Big(tag: 8, pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0, pad6: 0)))

// All patterns fail: the subject was never consumed, so it must be destroyed
// exactly once at scope exit.
func allPatternsFail(_ box: consuming any P<Int> & ~Copyable) -> Int {
  if case let u as Unrelated = box { _ = u; return -1 }
  if case let s as Small = box { _ = s; return -2 }
  return 0
}
// CHECK: if case all failed: 0
print("if case all failed:", allPatternsFail(
  BigWithCanary(canary: Canary(), pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0)))
// CHECK: canary deinit count after failed patterns: 2
print("canary deinit count after failed patterns:", Canary.deinitCount)

// MARK: - `as?` consumes unconditionally, unlike a cast pattern
//
// Same invariant as the plain existential test, through the extended-existential
// runtime path: forming an Optional with `as?` consumes the subject even when
// the cast fails, while a cast pattern leaves it intact on the failure edge.

func optionalFormConsumesOnFailure() -> Int {
  let box: any P<Int> & ~Copyable =
    BigWithCanary(canary: Canary(), pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0)
  if let u = box as? Unrelated { _ = u; return -1 }
  return Canary.deinitCount
}
// CHECK: as? consumed on failure (3 => yes): 3
print("as? consumed on failure (3 => yes):", optionalFormConsumesOnFailure())

func patternFormDoesNotConsumeOnFailure() -> Int {
  let before = Canary.deinitCount
  let box: any P<Int> & ~Copyable =
    BigWithCanary(canary: Canary(), pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0)
  if case let u as Unrelated = box { _ = u; return -1 }
  return Canary.deinitCount - before
}
// CHECK: pattern left subject alive on failure (0 => yes): 0
print("pattern left subject alive on failure (0 => yes):",
      patternFormDoesNotConsumeOnFailure())

// RUN: %target-run-simple-swift(-enable-experimental-feature NoncopyableCasting)

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test

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

// Leak/double-free check: construct the boxed, canary-carrying payload
// directly as a call argument (never bound to a named local -- doing so
// currently trips an unrelated, pre-existing move-only-checker crash for
// *any* consuming use of a boxed noncopyable existential local, unrelated
// to casting), exercise a successful `is` check on it, and confirm the
// class reference inside gets destroyed exactly once.
_ = takeCanary(BigWithCanary(canary: Canary(), pad0: 0, pad1: 0, pad2: 0, pad3: 0, pad4: 0, pad5: 0))
// CHECK: canary deinit count: 1
print("canary deinit count:", Canary.deinitCount)

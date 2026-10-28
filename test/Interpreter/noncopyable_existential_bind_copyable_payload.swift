// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test

// Casting an existential that suppresses `Copyable` or `Escapable` needs
// `swift_getExtendedExistentialTypeMetadata_unique`, which older runtimes lack.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// A `~Copyable` existential can hold a `Copyable` payload, and that payload can
// be trivial. Extracting it with `load [take]` asserts, because a trivial type's
// value has `None` ownership where the load's result is assumed `Owned`:
//
//   Assertion failed: (value->getOwnershipKind() == OwnershipKind::None),
//     function forObjectRValueWithoutOwnership, ManagedValue.h
//
// The all-non-`Copyable` tests never reach this, because a non-`Copyable`
// payload cannot be bound at all -- that is still diagnosed as unimplemented.

protocol P: ~Copyable {}

struct NC: ~Copyable, P { var t: Int }

/// Copyable and trivial; small enough to live inline in the container.
struct Trivial: P { var t: Int }

/// Copyable and trivial, but too large to store inline, so the container boxes
/// it out-of-line.
struct Boxed: P { var a, b, c, d, e, f, g, h: Int }

/// Copyable and non-trivial: extracting it has to manage a retain/release.
final class Ref: P {
  let t: Int
  init(_ t: Int) { self.t = t }
}

func bindTrivial(_ box: consuming any P & ~Copyable) -> Int {
  if case let v as Trivial = box { return v.t }
  return -1
}
func bindBoxed(_ box: consuming any P & ~Copyable) -> Int {
  if case let v as Boxed = box { return v.a }
  return -1
}
func bindRef(_ box: consuming any P & ~Copyable) -> Int {
  if case let v as Ref = box { return v.t }
  return -1
}

// CHECK: 7 -1
print(bindTrivial(Trivial(t: 7)), bindTrivial(NC(t: 9)))
// CHECK-NEXT: 42 -1
print(bindBoxed(Boxed(a: 42, b: 0, c: 0, d: 0, e: 0, f: 0, g: 0, h: 0)), bindBoxed(NC(t: 9)))
// CHECK-NEXT: 5 -1
print(bindRef(Ref(5)), bindRef(NC(t: 9)))

// `as?` reaches the same extraction through a different entry point.
func asTrivial(_ box: consuming any P & ~Copyable) -> Int {
  if let v = box as? Trivial { return v.t }
  return -1
}
// CHECK-NEXT: 3 -1
print(asTrivial(Trivial(t: 3)), asTrivial(NC(t: 9)))

// A failed binding must leave the payload alive in the container, and a
// successful one must hand over a reference that outlives the container.
final class Tracked: P {
  static var live = 0
  init() { Tracked.live += 1 }
  deinit { Tracked.live -= 1 }
}

func extract(_ box: consuming any P & ~Copyable) -> Tracked? {
  if case let v as Tracked = box { return v }
  return nil
}

do {
  let survivor = extract(Tracked())
  // CHECK-NEXT: 1 true
  print(Tracked.live, survivor != nil)
}
// CHECK-NEXT: 0
print(Tracked.live)

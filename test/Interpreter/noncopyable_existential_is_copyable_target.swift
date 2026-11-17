// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// A `~Copyable` existential can hold a copyable payload, and the target of a
// cast out of one can be a copyable type. That combination exercises code the
// all-non-`Copyable` tests never reach: the payload's copy value witness exists,
// so the runtime is free to copy rather than take, and the extracted payload can
// be trivial -- which the binding path must load as [trivial], not [take].

protocol P: ~Copyable {}

struct NC: ~Copyable, P { var t: Int }

/// Copyable and trivial; small enough to live inline in the container.
struct Trivial: P { var t: Int }

/// Copyable and trivial, but too large to store inline, so the container boxes
/// it out-of-line. That is the shape an older runtime cannot take out of the
/// container -- it copies instead -- which is legal only because it is copyable.
struct Boxed: P { var a, b, c, d, e, f, g, h: Int }

/// Copyable and non-trivial: extracting it has to manage a retain/release.
final class Ref: P {
  let t: Int
  init(_ t: Int) { self.t = t }
}

// MARK: - `is` against a copyable target

func isTrivial(_ box: borrowing any P & ~Copyable) -> Bool { box is Trivial }
func isBoxed(_ box: borrowing any P & ~Copyable) -> Bool { box is Boxed }
func isRef(_ box: borrowing any P & ~Copyable) -> Bool { box is Ref }

// CHECK: true false false
print(isTrivial(Trivial(t: 1)), isTrivial(Boxed(a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8)),
      isTrivial(NC(t: 9)))
// CHECK-NEXT: true false
print(isBoxed(Boxed(a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8)), isBoxed(NC(t: 9)))
// CHECK-NEXT: true false
print(isRef(Ref(1)), isRef(NC(t: 9)))

// The subject survives the test, so it can be tested again.
//
// Deliberately two statements rather than `(box is Trivial) && (box is
// Trivial)`. `&&` takes its right operand as an autoclosure, and capturing a
// borrowed noncopyable existential in a closure copies it under
// -enable-sil-opaque-values. That is a SILGen capture bug with nothing to do
// with casting -- `check(box) && check(box)` reproduces it with no cast at all
// -- and testing twice is what this is about.
func isTrivialTwice(_ box: borrowing any P & ~Copyable) -> Bool {
  let first = box is Trivial
  let second = box is Trivial
  return first && second
}
// CHECK-NEXT: true
print(isTrivialTwice(Trivial(t: 1)))

// MARK: - `case let x as T` binding a copyable payload

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

// CHECK-NEXT: 7 -1
print(bindTrivial(Trivial(t: 7)), bindTrivial(NC(t: 9)))
// CHECK-NEXT: 42 -1
print(bindBoxed(Boxed(a: 42, b: 0, c: 0, d: 0, e: 0, f: 0, g: 0, h: 0)), bindBoxed(NC(t: 9)))
// CHECK-NEXT: 5 -1
print(bindRef(Ref(5)), bindRef(NC(t: 9)))

// MARK: - `as?` producing a copyable payload

func asTrivial(_ box: consuming any P & ~Copyable) -> Int {
  if let v = box as? Trivial { return v.t }
  return -1
}
func asBoxed(_ box: consuming any P & ~Copyable) -> Int {
  if let v = box as? Boxed { return v.a }
  return -1
}

// CHECK-NEXT: 3 -1
print(asTrivial(Trivial(t: 3)), asTrivial(NC(t: 9)))
// CHECK-NEXT: 11 -1
print(asBoxed(Boxed(a: 11, b: 0, c: 0, d: 0, e: 0, f: 0, g: 0, h: 0)), asBoxed(NC(t: 9)))

// MARK: - The class payload's lifetime is not cut short

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

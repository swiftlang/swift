// Deallocation accounting for weak and unowned references. A leaked weak or
// unowned reference is invisible to deinit -- deinit runs when the strong count
// hits 0, but the memory is freed only when the last weak/unowned reference goes
// away. Overriding malloc/free (Inputs/debug-malloc.c) makes that second step
// observable, so a leaked weak reference shows up as a missing free().
//
// Print only StaticStrings here: string interpolation allocates, which would
// bury the allocations under test in noise.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library %s -c -o %t/a.o
// RUN: %target-clang -x c -std=c11 -c %S/Inputs/debug-malloc.c -o %t/debug-malloc.o
// RUN: %target-clang -x c -c %S/Inputs/refcount-shims.c -o %t/refcount-shims.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o %t/debug-malloc.o %t/refcount-shims.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: PTRSIZE=64
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

final class Target {
  var pad: Int = 0
  deinit { print("deinit") }
}

final class WeakHolder {
  weak var ref: Target?
  init(_ r: Target?) { self.ref = r }
}

final class UnownedHolder {
  unowned var ref: Target
  init(_ r: Target) { self.ref = r }
}

struct WeakBox {
  weak var ref: Target?
}

// swift_weakTakeStrong is reached through a C shim: naming it in Swift claims
// its symbol and suppresses emission of the stdlib's definition.
@_extern(c, "test_weakTakeStrong")
func weakTakeStrong(_ ref: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer?

// No weak references: the object is freed as soon as the strong count hits 0.
@inline(never) func noWeakRefs() {
  print("noWeakRefs")
  // CHECK-LABEL: noWeakRefs
  var t: Target? = Target()
  // CHECK: malloc
  t = nil
  // CHECK: deinit
  // CHECK: free
  print("noWeakRefs done")
  // CHECK: noWeakRefs done
}

// A weak reference outliving the object: deinit runs at strong count 0, but the
// free waits for the weak reference to go away.
@inline(never) func weakOutlivesObject() {
  print("weakOutlives")
  // CHECK-LABEL: weakOutlives
  var t: Target? = Target()
  // CHECK: malloc
  let h = WeakHolder(t)
  t = nil
  // CHECK: deinit
  print("strongGone")
  // CHECK: strongGone
  h.ref = nil
  // CHECK: free
  print("weakOutlives done")
  // CHECK: weakOutlives done
}

// The object outliving the weak reference: dropping the weak reference frees
// nothing, and the object is freed when its strong count hits 0.
@inline(never) func objectOutlivesWeak() {
  print("objectOutlives")
  // CHECK-LABEL: objectOutlives
  var t: Target? = Target()
  // CHECK: malloc
  do {
    let h = WeakHolder(t)
    _ = h
  }
  print("weakGone")
  // CHECK: weakGone
  t = nil
  // CHECK: deinit
  // CHECK: free
  print("objectOutlives done")
  // CHECK: objectOutlives done
}

// An unowned reference outliving the object behaves the same way: the free waits
// for the unowned reference.
@inline(never) func unownedOutlivesObject() {
  print("unownedOutlives")
  // CHECK-LABEL: unownedOutlives
  var t: Target? = Target()
  // CHECK: malloc
  var h: UnownedHolder? = UnownedHolder(t!)
  t = nil
  // CHECK: deinit
  print("strongGone")
  // CHECK: strongGone
  h = nil
  // CHECK: free
  print("unownedOutlives done")
  // CHECK: unownedOutlives done
}

// Assigning one weak slot over another releases the destination's old weak
// reference. Its object is already dead, so that release must free it.
@inline(never) func copyAssignFreesOldTarget() {
  print("copyAssign")
  // CHECK-LABEL: copyAssign
  var dead: Target? = Target()
  // CHECK: malloc
  let live = Target()
  // CHECK: malloc
  let p = UnsafeMutablePointer<WeakBox>.allocate(capacity: 1)
  let q = UnsafeMutablePointer<WeakBox>.allocate(capacity: 1)
  p.initialize(to: WeakBox(ref: live))
  q.initialize(to: WeakBox(ref: dead))
  dead = nil
  // CHECK: deinit
  print("deadReleased")
  // CHECK: deadReleased
  // q's weak reference to the dead object is the last one; overwriting it must
  // free that object.
  q.update(from: p, count: 1)
  // CHECK: free
  print("assigned")
  // CHECK: assigned
  p.deinitialize(count: 1)
  q.deinitialize(count: 1)
  p.deallocate()
  q.deallocate()
}

// Move-assigning one weak slot over another releases the destination's old weak
// reference, the same as copy-assigning.
@inline(never) func takeAssignFreesOldTarget() {
  print("takeAssign")
  // CHECK-LABEL: takeAssign
  var dead: Target? = Target()
  // CHECK: malloc
  let live = Target()
  // CHECK: malloc
  let p = UnsafeMutablePointer<WeakBox>.allocate(capacity: 1)
  let q = UnsafeMutablePointer<WeakBox>.allocate(capacity: 1)
  p.initialize(to: WeakBox(ref: live))
  q.initialize(to: WeakBox(ref: dead))
  dead = nil
  // CHECK: deinit
  print("deadReleased")
  // CHECK: deadReleased
  q.moveUpdate(from: p, count: 1)
  // CHECK: free
  print("moveAssigned")
  // CHECK: moveAssigned
  q.deinitialize(count: 1)
  p.deallocate()
  q.deallocate()
}

// Copying a box that holds a weak reference must not leak a strong reference to
// the object: the object must still deinit when its own strong references go
// away, and be freed when the last weak reference does.
@inline(never) func copyInitDoesNotLeakStrong() {
  print("copyInit")
  // CHECK-LABEL: copyInit
  var t: Target? = Target()
  // CHECK: malloc
  let original = WeakBox(ref: t)
  // Copying the box copy-initializes a second weak slot from the first.
  var copy = original
  t = nil
  // CHECK: deinit
  print("strongGone")
  // CHECK: strongGone
  copy.ref = nil
  print("copyDropped")
  // CHECK: copyDropped
  withExtendedLifetime(original) {}
  print("copyInit done")
  // CHECK: copyInit done
  // CHECK: free
}

// Taking a weak reference whose object is already dead consumes the last weak
// reference to it, so the take must free the object rather than trap.
@inline(never) func takeStrongFreesDeadTarget() {
  print("takeStrong")
  // CHECK-LABEL: takeStrong
  var dead: Target? = Target()
  // CHECK: malloc
  let p = UnsafeMutablePointer<WeakBox>.allocate(capacity: 1)
  p.initialize(to: WeakBox(ref: dead))
  dead = nil
  // CHECK: deinit
  print("deadReleased")
  // CHECK: deadReleased
  let taken = weakTakeStrong(UnsafeMutableRawPointer(p))
  // CHECK: free
  if taken == nil {
    print("takenNil")
    // CHECK: takenNil
  }
  p.deallocate()
}

@main
struct Main {
  static func main() {
    noWeakRefs()
    weakOutlivesObject()
    objectOutlivesWeak()
    unownedOutlivesObject()
    copyAssignFreesOldTarget()
    takeAssignFreesOldTarget()
    copyInitDoesNotLeakStrong()
    takeStrongFreesDeadTarget()
    print("end")
    // CHECK: end
  }
}

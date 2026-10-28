// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// RUN: %target-swift-frontend -O -enable-experimental-feature Embedded -parse-as-library -module-name test %s -c -o %t/a.O.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.O.o -o %t/a.O.out -dead_strip
// RUN: %target-run %t/a.O.out | %FileCheck %s

// RUN: %target-swift-frontend -Osize -enable-experimental-feature Embedded -parse-as-library -module-name test %s -c -o %t/a.Osize.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.Osize.o -o %t/a.Osize.out -dead_strip
// RUN: %target-run %t/a.Osize.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: PTRSIZE=64
// REQUIRES: swift_feature_Embedded

// Test weak reference handling for the Embedded Swift runtime.
//
// The state space covered: a slot holding nil, a live object, or a dead object,
// crossed with assigning nil, a live object, or the same object; plus weak
// count 0 -> n -> 0, and a strong count that drops to 0 while weak references
// remain.

final class Target {
  let id: Int
  init(id: Int) { self.id = id }
  deinit { print("deinit \(id)") }
}

final class Holder {
  weak var ref: Target?
  init(_ r: Target?) { self.ref = r }
}

// -1 stands for "the weak reference read as nil".
func load(_ t: Target?) -> Int { return t?.id ?? -1 }

// Escaping a value into a global keeps the optimizer from folding away the
// control flow some scenarios below depend on.
var sink: Int = 0

// A slot initialized with nil reads as nil.
@inline(never) func initNil() {
  let h = Holder(nil)
  print("initNil \(load(h.ref))")
  // CHECK: initNil -1
}

// A slot initialized with a live object reads as that object, and reads as nil
// once the last strong reference goes away.
@inline(never) func initLiveThenDeath() {
  var t: Target? = Target(id: 1)
  let h = Holder(t)
  print("initLive \(load(h.ref))")
  // CHECK: initLive 1
  t = nil
  // CHECK: deinit 1
  print("initLive dead \(load(h.ref))")
  // CHECK: initLive dead -1

  // Initializing a fresh slot from a slot whose object is gone yields nil.
  let h2 = Holder(h.ref)
  print("initFromDead \(load(h2.ref))")
  // CHECK: initFromDead -1
}

// Every assignment transition between nil and a live object.
@inline(never) func assignTransitions() {
  let h = Holder(nil)
  var a: Target? = Target(id: 2)
  var b: Target? = Target(id: 3)

  h.ref = nil
  print("nil->nil \(load(h.ref))")
  // CHECK: nil->nil -1
  h.ref = a
  print("nil->live \(load(h.ref))")
  // CHECK: nil->live 2
  h.ref = b
  print("live->live \(load(h.ref))")
  // CHECK: live->live 3
  h.ref = b
  print("live->same \(load(h.ref))")
  // CHECK: live->same 3
  h.ref = nil
  print("live->nil \(load(h.ref))")
  // CHECK: live->nil -1

  a = nil
  // CHECK: deinit 2
  b = nil
  // CHECK: deinit 3
}

// Assigning over a slot whose object has already died.
@inline(never) func assignOverDead() {
  let h = Holder(nil)
  var c: Target? = Target(id: 4)
  h.ref = c
  c = nil
  // CHECK: deinit 4
  print("dead \(load(h.ref))")
  // CHECK: dead -1
  var e: Target? = Target(id: 5)
  h.ref = e
  print("dead->live \(load(h.ref))")
  // CHECK: dead->live 5
  h.ref = nil
  e = nil
  // CHECK: deinit 5
}

// Weak count 0 -> 3 -> 2 -> 0, spanning the object's death.
@inline(never) func manyWeakRefs() {
  var t: Target? = Target(id: 6)
  let h1 = Holder(t)
  let h2 = Holder(t)
  let h3 = Holder(t)
  print("three \(load(h1.ref)) \(load(h2.ref)) \(load(h3.ref))")
  // CHECK: three 6 6 6
  h2.ref = nil
  print("two \(load(h1.ref)) \(load(h2.ref)) \(load(h3.ref))")
  // CHECK: two 6 -1 6
  t = nil
  // CHECK: deinit 6
  print("dead \(load(h1.ref)) \(load(h2.ref)) \(load(h3.ref))")
  // CHECK: dead -1 -1 -1
  h1.ref = nil
  h3.ref = nil
}

// A successful weak load yields a strong reference, which keeps the object alive
// past the release of every other strong reference: strong count 1 -> 2 -> 1 -> 0.
@inline(never) func loadKeepsAlive() {
  var t: Target? = Target(id: 7)
  let h = Holder(t)
  if let loaded = h.ref {
    t = nil
    print("held \(loaded.id)")
    // CHECK: held 7
  }
  // CHECK: deinit 7
  print("released \(load(h.ref))")
  // CHECK: released -1
}

// A weak slot destroyed while its object is still alive: the weak count returns
// to 0 with the object untouched.
@inline(never) func destroyWhileLive() {
  let t = Target(id: 8)
  do {
    let h = Holder(t)
    print("scoped \(load(h.ref))")
    // CHECK: scoped 8
  }
  print("outlived \(t.id)")
  // CHECK: outlived 8
  // CHECK: deinit 8
}

// A weak reference formed to an object during its own deinit. The object is at
// the deallocating refcount, so the reference must read as nil rather than
// resurrect it. This holds for a stack promoted object too: a global-allocated
// object is the one with a saturated weak refcount, so a stack instance during
// deinit is not mistaken for one.
final class SelfWeaker {
  weak var ref: SelfWeaker?
  let id: Int
  var holder: SelfWeaker? = nil
  init(id: Int) { self.id = id }
  deinit {
    print("deinit \(id)")
    // CHECK: deinit 9
    guard let holder = holder else { return }
    holder.ref = self
    print("inDeinit \(holder.ref == nil ? -1 : holder.ref!.id)")
    // CHECK: inDeinit -1
  }
}

@inline(never) func weakDuringDeinit() {
  let holder = SelfWeaker(id: 10)
  var s: SelfWeaker? = SelfWeaker(id: 9)
  s!.holder = holder
  s = nil
  print("afterDeinit \(holder.ref == nil ? -1 : 1)")
  // CHECK: afterDeinit -1
  // CHECK: deinit 10
}

// A weak reference formed to an object during its own deinit, where the object's
// release ends up in a different basic block from its deallocation.
// ReleaseDevirtualizer only looks for the final release within the
// dealloc_stack_ref's own block, so it bails out and the object goes through
// swift_release_n_, which stores the immortal reference count before running the
// deinit. The scenario above reaches the other state, where the release was
// devirtualized and the count is still 1. Both must read as nil.
//
// Which state this produces is not under the test's control: at -O each branch
// keeps its own dealloc_stack_ref alongside the release and the devirtualizer
// fires, while at -Osize SimplifyCFG tail-merges dealloc_stack_ref into the join
// block, separating it from the release. Between the three runs both states are
// covered.
final class LateWeaker {
  weak var ref: LateWeaker?
  let id: Int
  var holder: LateWeaker? = nil
  init(id: Int) { self.id = id }
  deinit {
    print("deinit \(id)")
    // CHECK: deinit 11
    guard let holder = holder else { return }
    holder.ref = self
    print("lateDeinit \(holder.ref == nil ? -1 : holder.ref!.id)")
    // CHECK: lateDeinit -1
  }
}

@inline(never) func weakDuringDeinitAcrossBlocks(_ cond: Bool) {
  let holder = LateWeaker(id: 12)
  var w: LateWeaker? = LateWeaker(id: 11)
  w!.holder = holder
  if cond {
    sink = w!.id
    w = nil
  } else {
    w = nil
  }
  sink += 1
  print("acrossBlocks \(holder.ref == nil ? -1 : 1)")
  // CHECK: acrossBlocks -1
  // CHECK: deinit 12
}

@main
struct Main {
  static func main() {
    initNil()
    initLiveThenDeath()
    assignTransitions()
    assignOverDead()
    manyWeakRefs()
    loadKeepsAlive()
    destroyWhileLive()
    weakDuringDeinit()
    weakDuringDeinitAcrossBlocks(sink == 0)
    print("end")
    // CHECK: end
  }
}

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
// REQUIRES: swift_feature_Embedded
// REQUIRES: embedded_stdlib_default_codegen

// Reads of a dead unowned reference must trap; those live in
// weak-unowned-traps.swift. Everything here must not trap.

final class Target {
  let id: Int
  init(id: Int) { self.id = id }
  deinit { print("deinit \(id)") }
}

final class Holder {
  unowned var ref: Target
  init(_ r: Target) { self.ref = r }
}

final class OptionalHolder {
  unowned var ref: Target?
  init(_ r: Target?) { self.ref = r }
}

// Reading a live unowned reference; the holder dies before the target.
@inline(never) func liveRead() {
  var t: Target? = Target(id: 1)
  do {
    let h = Holder(t!)
    print("read \(h.ref.id) \(h.ref.id)")
    // CHECK: read 1 1
  }
  print("holderGone \(t!.id)")
  // CHECK: holderGone 1
  t = nil
  // CHECK: deinit 1
  print("liveRead done")
  // CHECK: liveRead done
}

// Reassigning an unowned reference: the old target loses its unowned reference
// and the new one gains one.
@inline(never) func reassign() {
  var a: Target? = Target(id: 2)
  var b: Target? = Target(id: 3)
  let h = Holder(a!)
  print("before \(h.ref.id)")
  // CHECK: before 2
  h.ref = b!
  print("after \(h.ref.id)")
  // CHECK: after 3
  h.ref = b!
  print("same \(h.ref.id)")
  // CHECK: same 3
  h.ref = a!
  print("back \(h.ref.id)")
  // CHECK: back 2
  b = nil
  // CHECK: deinit 3
  print("droppedB")
  // CHECK: droppedB
  a = nil
  // CHECK: deinit 2
  print("droppedA")
  // CHECK: droppedA
  _ = h
}

// Several unowned references to one object: unowned count 0 -> 3 -> 0.
@inline(never) func manyUnowned() {
  var t: Target? = Target(id: 4)
  var h1: Holder? = Holder(t!)
  var h2: Holder? = Holder(t!)
  var h3: Holder? = Holder(t!)
  print("three \(h1!.ref.id) \(h2!.ref.id) \(h3!.ref.id)")
  // CHECK: three 4 4 4
  h2 = nil
  print("two \(h1!.ref.id) \(h3!.ref.id)")
  // CHECK: two 4 4
  h1 = nil
  h3 = nil
  print("none \(t!.id)")
  // CHECK: none 4
  t = nil
  // CHECK: deinit 4
  print("manyUnowned done")
  // CHECK: manyUnowned done
}

// The target dies while an unowned reference still exists. That is legal as long
// as the reference is never read: the object's memory stays allocated until the
// unowned reference goes away.
@inline(never) func targetDiesFirst() {
  var t: Target? = Target(id: 5)
  var h: Holder? = Holder(t!)
  print("holding \(h!.ref.id)")
  // CHECK: holding 5
  t = nil
  // CHECK: deinit 5
  print("targetGone")
  // CHECK: targetGone
  h = nil
  print("targetDiesFirst done")
  // CHECK: targetDiesFirst done
}

// unowned in a struct: copying the struct retains the unowned reference, and
// consuming it converts to strong (swift_unownedRetainStrongAndRelease).
struct Box {
  unowned var ref: Target
}

@inline(never) func structUnowned() {
  var t: Target? = Target(id: 6)
  var box = Box(ref: t!)
  let copy = box
  print("box \(box.ref.id) \(copy.ref.id)")
  // CHECK: box 6 6
  box = Box(ref: t!)
  print("reassigned \(box.ref.id)")
  // CHECK: reassigned 6
  let consumed = consume copy
  print("consumed \(consumed.ref.id)")
  // CHECK: consumed 6
  _ = box
  t = nil
  // CHECK: deinit 6
  print("structUnowned done")
  // CHECK: structUnowned done
}

// unowned(unsafe) performs no reference counting at all.
final class UnsafeHolder {
  unowned(unsafe) var ref: Target
  init(_ r: Target) { self.ref = r }
}

@inline(never) func unownedUnsafe() {
  var t: Target? = Target(id: 7)
  let h = UnsafeHolder(t!)
  print("unsafe \(h.ref.id)")
  // CHECK: unsafe 7
  t = nil
  // CHECK: deinit 7
  print("unownedUnsafe done")
  // CHECK: unownedUnsafe done
}

// An unowned optional covers the nil state, which a non-optional unowned
// reference cannot reach. This runs last on purpose: nil is a state no other
// scenario here can produce, so a trap in it must not hide the rest.
@inline(never) func optionalUnowned() {
  let h = OptionalHolder(nil)
  print("nil \(h.ref?.id ?? -1)")
  // CHECK: nil -1
  var t: Target? = Target(id: 8)
  h.ref = t
  print("live \(h.ref?.id ?? -1)")
  // CHECK: live 8
  h.ref = nil
  print("cleared \(h.ref?.id ?? -1)")
  // CHECK: cleared -1
  t = nil
  // CHECK: deinit 8
  print("optionalUnowned done")
  // CHECK: optionalUnowned done
}

@main
struct Main {
  static func main() {
    liveRead()
    reassign()
    manyUnowned()
    targetDiesFirst()
    structUnowned()
    unownedUnsafe()
    optionalUnowned()
    print("end")
    // CHECK: end
  }
}

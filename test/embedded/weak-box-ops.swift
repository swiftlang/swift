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

// Coverage for the weak reference operations IRGen emits for address-only
// values: swift_weakCopyInit, swift_weakCopyAssign, swift_weakTakeInit, and
// swift_weakTakeAssign. A struct with a `weak` stored property is address-only,
// so copying and moving one through UnsafeMutablePointer reaches these entry
// points.

final class Target {
  let id: Int
  init(id: Int) { self.id = id }
  deinit { print("deinit \(id)") }
}

// A struct holding a weak reference is address-only.
struct Box {
  weak var ref: Target?
}

func load(_ t: Target?) -> Int { return t?.id ?? -1 }

// Copying a box copies the weak reference: both slots track the same object, and
// both read as nil once it dies.
@inline(never) func copyLive() {
  var t: Target? = Target(id: 1)
  let original = Box(ref: t)
  var copy = original
  print("copied \(load(original.ref)) \(load(copy.ref))")
  // CHECK: copied 1 1
  t = nil
  // CHECK: deinit 1
  print("dead \(load(original.ref)) \(load(copy.ref))")
  // CHECK: dead -1 -1
  copy.ref = nil
}

// Copying a box whose object is already gone yields nil.
@inline(never) func copyDead() {
  var t: Target? = Target(id: 2)
  let original = Box(ref: t)
  t = nil
  // CHECK: deinit 2
  let copy = original
  print("copyDead \(load(copy.ref))")
  // CHECK: copyDead -1
}

// Copying a box holding nil.
@inline(never) func copyNil() {
  let original = Box(ref: nil)
  let copy = original
  print("copyNil \(load(copy.ref))")
  // CHECK: copyNil -1
}

// swift_weakCopyInit and swift_weakCopyAssign through pointer memory:
// initialize() into uninitialized memory, update() over initialized memory.
@inline(never) func pointerCopy() {
  var t: Target? = Target(id: 3)
  var u: Target? = Target(id: 4)
  let p = UnsafeMutablePointer<Box>.allocate(capacity: 1)
  let q = UnsafeMutablePointer<Box>.allocate(capacity: 1)

  p.initialize(to: Box(ref: t))
  q.initialize(to: Box(ref: u))
  print("initialized \(load(p.pointee.ref)) \(load(q.pointee.ref))")
  // CHECK: initialized 3 4

  // q's old weak reference to 4 is dropped, and 3 gains one.
  q.update(from: p, count: 1)
  print("copyAssigned \(load(p.pointee.ref)) \(load(q.pointee.ref))")
  // CHECK: copyAssigned 3 3

  p.deinitialize(count: 1)
  q.deinitialize(count: 1)
  p.deallocate()
  q.deallocate()

  t = nil
  // CHECK: deinit 3
  u = nil
  // CHECK: deinit 4
}

// swift_weakTakeInit: moving a weak reference into uninitialized memory leaves
// the source uninitialized and changes no counts.
@inline(never) func pointerMoveInit() {
  var t: Target? = Target(id: 5)
  let p = UnsafeMutablePointer<Box>.allocate(capacity: 1)
  let q = UnsafeMutablePointer<Box>.allocate(capacity: 1)

  p.initialize(to: Box(ref: t))
  q.moveInitialize(from: p, count: 1)
  print("moved \(load(q.pointee.ref))")
  // CHECK: moved 5

  t = nil
  // CHECK: deinit 5
  print("movedDead \(load(q.pointee.ref))")
  // CHECK: movedDead -1

  q.deinitialize(count: 1)
  p.deallocate()
  q.deallocate()
}

// swift_weakTakeAssign: move-assigning a weak reference over an initialized slot
// takes src's reference without retaining and releases dest's old one.
@inline(never) func pointerMoveAssign() {
  var t: Target? = Target(id: 7)
  var u: Target? = Target(id: 8)
  let p = UnsafeMutablePointer<Box>.allocate(capacity: 1)
  let q = UnsafeMutablePointer<Box>.allocate(capacity: 1)

  p.initialize(to: Box(ref: t))
  q.initialize(to: Box(ref: u))
  q.moveUpdate(from: p, count: 1)
  print("moveAssigned \(load(q.pointee.ref))")
  // CHECK: moveAssigned 7

  u = nil
  // CHECK: deinit 8
  print("oldGone \(load(q.pointee.ref))")
  // CHECK: oldGone 7

  q.deinitialize(count: 1)
  p.deallocate()
  q.deallocate()

  t = nil
  // CHECK: deinit 7
}

// A box in an array: copying the array copies the weak references.
@inline(never) func arrayOfBoxes() {
  var t: Target? = Target(id: 6)
  var boxes = [Box(ref: t), Box(ref: t)]
  let copies = boxes
  print("array \(load(boxes[0].ref)) \(load(copies[1].ref))")
  // CHECK: array 6 6
  t = nil
  // CHECK: deinit 6
  print("arrayDead \(load(copies[0].ref))")
  // CHECK: arrayDead -1
  boxes.removeAll()
}

@main
struct Main {
  static func main() {
    copyLive()
    copyDead()
    copyNil()
    pointerCopy()
    pointerMoveInit()
    pointerMoveAssign()
    arrayOfBoxes()
    print("end")
    // CHECK: end
  }
}

// RUN: %target-build-swift -parse-stdlib %s -o %t/a.out
// RUN: %t/a.out | FileCheck %s

import Swift

class C {
  deinit { println("deallocated") }
}

// Try without any bits set.
if true {
  let x = C()
  let bo = Builtin.castToBridgeObject(x, 0.value)
  let bo2 = bo
  let x1: C = Builtin.castReferenceFromBridgeObject(bo)
  let x2: C = Builtin.castReferenceFromBridgeObject(bo2)
  // CHECK:      true
  println(x === x1)
  // CHECK-NEXT: true
  println(x === x2)
}
// CHECK-NEXT: deallocated

// Pick a bit that's up for grabs in BridgeObject for the current platform.
#if arch(i386) || arch(arm)

// We have no ObjC tagged pointers, and two low spare bits due to alignment.
// 1 is used as the native/ObjC flag.
let NATIVE_FLAG_BIT: UInt = 1
let SOME_SPARE_BIT: UInt = 2

#elseif arch(x86_64)

// We have ObjC tagged pointers in the lowest and highest bit. 2 is used as the
// native/ObjC flag, leaving 0x7F00_0000_0000_0004 free.
let NATIVE_FLAG_BIT: UInt = 2
let SOME_SPARE_BIT: UInt = 4

#elseif arch(arm64)

// We have ObjC tagged pointers in the highest bit. 0x4000_0000_0000_0000 is
// used as the native/ObjC flag, leaving 0x3F00_0000_0000_0007 free.
let NATIVE_FLAG_BIT: UInt = 0x4000_0000_0000_0000
let SOME_SPARE_BIT: UInt = 4

#endif

// Try with the native flag bit set.
if true {
  let x = C()
  let bo = Builtin.castToBridgeObject(x, NATIVE_FLAG_BIT.value)

  let bo2 = bo
  let x1: C = Builtin.castReferenceFromBridgeObject(bo)
  let x2: C = Builtin.castReferenceFromBridgeObject(bo2)
  // CHECK-NEXT: true
  println(x === x1)
  // CHECK-NEXT: true
  println(x === x2)
}
// CHECK-NEXT: deallocated

// Try with other spare bits set.
if true {
  let x = C()
  let bo = Builtin.castToBridgeObject(x, SOME_SPARE_BIT.value)

  let bo2 = bo
  let x1: C = Builtin.castReferenceFromBridgeObject(bo)
  let x2: C = Builtin.castReferenceFromBridgeObject(bo2)
  // CHECK-NEXT: true
  println(x === x1)
  // CHECK-NEXT: true
  println(x === x2)
}
// CHECK-NEXT: deallocated

#if os(OSX) || os(iOS)

import Foundation

// Try with a (probably) tagged pointer. No bits may be masked into a tagged
// pointer.
if true {
  let x = NSNumber(integer: 22)
  let bo = Builtin.castToBridgeObject(x, 0.value)
  let bo2 = bo
  let x1: NSNumber = Builtin.castReferenceFromBridgeObject(bo)
  let x2: NSNumber = Builtin.castReferenceFromBridgeObject(bo2)
  // CHECK-NEXT: true
  println(x === x1)
  // CHECK-NEXT: true
  println(x === x2)
}

#endif

func hitOptionalGenerically<T>(x: T?) {
  switch x {
  case .Some:
    println("Some")
  case .None:
    println("None")
  }
}

func hitOptionalSpecifically(x: Builtin.BridgeObject?) {
  switch x {
  case .Some:
    println("Some")
  case .None:
    println("None")
  }
}

if true {
  // CHECK-NEXT: true
  println(sizeof(Optional<Builtin.BridgeObject>.self)
            == sizeof(Builtin.BridgeObject.self))

  var bo: Builtin.BridgeObject? = nil

  // CHECK-NEXT: None
  hitOptionalSpecifically(bo)
  // CHECK-NEXT: None
  hitOptionalGenerically(bo)

  bo = Builtin.castToBridgeObject(C(), 0.value)
  // CHECK-NEXT: Some
  hitOptionalSpecifically(bo)
  // CHECK-NEXT: Some
  hitOptionalGenerically(bo)
}

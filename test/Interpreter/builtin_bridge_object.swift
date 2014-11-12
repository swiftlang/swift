// RUN: rm -rf %t  &&  mkdir -p %t
// RUN: %target-build-swift -parse-stdlib %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

import Swift
import SwiftShims

class C {
  deinit { println("deallocated") }
}

#if arch(i386) || arch(arm)

// We have no ObjC tagged pointers, and two low spare bits due to alignment.
// 1 is used as the native/ObjC flag.
let NATIVE_FLAG_BIT: UInt = 1
let SOME_SPARE_BIT: UInt = 2
let NON_POINTER_BITS: UInt = 3

#elseif arch(x86_64)

// We have ObjC tagged pointers in the lowest and highest bit. 2 is used as the
// native/ObjC flag, leaving 0x7F00_0000_0000_0004 free.
let NATIVE_FLAG_BIT: UInt = 2
let SOME_SPARE_BIT: UInt = 4
let NON_POINTER_BITS: UInt = 0x7F00_0000_0000_0006

#elseif arch(arm64)

// We have ObjC tagged pointers in the highest bit. 0x4000_0000_0000_0000 is
// used as the native/ObjC flag, leaving 0x3F00_0000_0000_0007 free.
let NATIVE_FLAG_BIT: UInt = 0x4000_0000_0000_0000
let SOME_SPARE_BIT: UInt = 4
let NON_POINTER_BITS: UInt = 0x7F00_0000_0000_0007

#endif

func bitPattern(x: Builtin.BridgeObject) -> UInt {
  return UInt(Builtin.castBitPatternFromBridgeObject(x))
}

func nonPointerBits(x: Builtin.BridgeObject) -> UInt {
  return bitPattern(x) & NON_POINTER_BITS
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

  println(nonPointerBits(bo) == 0)
  // CHECK-NEXT: true
  
  var bo3 = Builtin.castToBridgeObject(C(), 0.value)
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)) != 0)
  // CHECK-NEXT: true
  let bo4 = bo3
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)) != 0)
  // CHECK-NEXT: false
  _fixLifetime(bo3)
  _fixLifetime(bo4)
}
// CHECK-NEXT: deallocated
// CHECK-NEXT: deallocated

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

  println(nonPointerBits(bo) == NATIVE_FLAG_BIT)
  // CHECK-NEXT: true
  
  var bo3 = Builtin.castToBridgeObject(C(), NATIVE_FLAG_BIT.value)
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)) != 0)
  // CHECK-NEXT: true
  let bo4 = bo3
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)) != 0)
  // CHECK-NEXT: false
  _fixLifetime(bo3)
  _fixLifetime(bo4)
}
// CHECK-NEXT: deallocated
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
  
  println(nonPointerBits(bo) == SOME_SPARE_BIT)
  // CHECK-NEXT: true
  
  var bo3 = Builtin.castToBridgeObject(C(), SOME_SPARE_BIT.value)
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)) != 0)
  // CHECK-NEXT: true
  let bo4 = bo3
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)) != 0)
  // CHECK-NEXT: false
  _fixLifetime(bo3)
  _fixLifetime(bo4)
}
// CHECK-NEXT: deallocated
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

  var bo3 = Builtin.castToBridgeObject(NSNumber(integer: 22), 0.value)
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)) != 0)
  // CHECK-NEXT: false
  _fixLifetime(bo3)
}

var unTaggedString: NSString {
  return NSString(format: "A long string that won't fit in a tagged pointer")  
}

// Try with an un-tagged pointer. 
if true {
  let x = unTaggedString
  let bo = Builtin.castToBridgeObject(x, SOME_SPARE_BIT.value)
  let bo2 = bo
  let x1: NSString = Builtin.castReferenceFromBridgeObject(bo)
  let x2: NSString = Builtin.castReferenceFromBridgeObject(bo2)
  // CHECK-NEXT: true
  println(x === x1)
  // CHECK-NEXT: true
  println(x === x2)
  
  println(nonPointerBits(bo) == SOME_SPARE_BIT)
  // CHECK-NEXT: true
  
  var bo3 = Builtin.castToBridgeObject(unTaggedString, 0.value)
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)) != 0)
  // CHECK-NEXT: false
  _fixLifetime(bo3)
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

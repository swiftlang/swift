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
let NATIVE_SPARE_BITS: UInt = 0x0000_0003
let OBJC_TAGGED_POINTER_BITS: UInt = 0

#elseif arch(x86_64)

// We have ObjC tagged pointers in the lowest and highest bit
let NATIVE_SPARE_BITS: UInt = 0x7F00_0000_0000_0006
let OBJC_TAGGED_POINTER_BITS: UInt = 0x8000_0000_0000_0001

#elseif arch(arm64)

// We have ObjC tagged pointers in the highest bit
let NATIVE_SPARE_BITS: UInt = 0x7F00_0000_0000_0007
let OBJC_TAGGED_POINTER_BITS: UInt = 0x8000_0000_0000_0000

#endif

func bitPattern(x: Builtin.BridgeObject) -> UInt {
  return UInt(Builtin.castBitPatternFromBridgeObject(x))
}

func nonPointerBits(x: Builtin.BridgeObject) -> UInt {
  return bitPattern(x) & NATIVE_SPARE_BITS
}

// Try without any bits set.
if true {
  let x = C()
  let bo = Builtin.castToBridgeObject(x, 0._builtinWordValue)
  let bo2 = bo
  let x1: C = Builtin.castReferenceFromBridgeObject(bo)
  let x2: C = Builtin.castReferenceFromBridgeObject(bo2)
  // CHECK:      true
  println(x === x1)
  // CHECK-NEXT: true
  println(x === x2)

  println(nonPointerBits(bo) == 0)
  // CHECK-NEXT: true
  
  var bo3 = Builtin.castToBridgeObject(C(), 0._builtinWordValue)
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)))
  // CHECK-NEXT: true
  let bo4 = bo3
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)))
  // CHECK-NEXT: false
  _fixLifetime(bo3)
  _fixLifetime(bo4)
}
// CHECK-NEXT: deallocated
// CHECK-NEXT: deallocated

// Try with all spare bits set.
if true {
  let x = C()
  let bo = Builtin.castToBridgeObject(x, NATIVE_SPARE_BITS._builtinWordValue)

  let bo2 = bo
  let x1: C = Builtin.castReferenceFromBridgeObject(bo)
  let x2: C = Builtin.castReferenceFromBridgeObject(bo2)
  // CHECK-NEXT: true
  println(x === x1)
  // CHECK-NEXT: true
  println(x === x2)
  
  println(nonPointerBits(bo) == NATIVE_SPARE_BITS)
  // CHECK-NEXT: true
  
  var bo3 = Builtin.castToBridgeObject(C(), NATIVE_SPARE_BITS._builtinWordValue)
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)))
  // CHECK-NEXT: true
  let bo4 = bo3
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)))
  // CHECK-NEXT: false
  _fixLifetime(bo3)
  _fixLifetime(bo4)
}
// CHECK-NEXT: deallocated
// CHECK-NEXT: deallocated

#if os(OSX) || os(iOS)

import Foundation

func nonNativeBridgeObject(o: AnyObject) -> Builtin.BridgeObject {
  let tagged = ((Builtin.reinterpretCast(o) as UInt) & OBJC_TAGGED_POINTER_BITS) != 0
  return Builtin.castToBridgeObject(
    o, tagged ? 0._builtinWordValue : NATIVE_SPARE_BITS._builtinWordValue)
}

// Try with a (probably) tagged pointer. No bits may be masked into a
// non-native object.
if true {
  let x = NSNumber(integer: 22)
  let bo = nonNativeBridgeObject(x)
  let bo2 = bo
  let x1: NSNumber = Builtin.castReferenceFromBridgeObject(bo)
  let x2: NSNumber = Builtin.castReferenceFromBridgeObject(bo2)
  // CHECK-NEXT: true
  println(x === x1)
  // CHECK-NEXT: true
  println(x === x2)

  var bo3 = nonNativeBridgeObject(NSNumber(integer: 22))
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)))
  // CHECK-NEXT: false
  _fixLifetime(bo3)
}

var unTaggedString: NSString {
  return NSString(format: "A long string that won't fit in a tagged pointer")  
}

// Try with an un-tagged pointer. 
if true {
  let x = unTaggedString
  let bo = nonNativeBridgeObject(x)
  let bo2 = bo
  let x1: NSString = Builtin.castReferenceFromBridgeObject(bo)
  let x2: NSString = Builtin.castReferenceFromBridgeObject(bo2)
  // CHECK-NEXT: true
  println(x === x1)
  // CHECK-NEXT: true
  println(x === x2)
  
  var bo3 = nonNativeBridgeObject(unTaggedString)
  println(
    _swift_isUniquelyReferencedNonObjC_nonNull_bridgeObject(
      bitPattern(bo3)))
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

  bo = Builtin.castToBridgeObject(C(), 0._builtinWordValue)
  // CHECK-NEXT: Some
  hitOptionalSpecifically(bo)
  // CHECK-NEXT: Some
  hitOptionalGenerically(bo)
}

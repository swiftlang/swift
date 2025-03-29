// RUN: %target-run-simple-swift(-Onone -parse-stdlib -Xfrontend -enable-copy-propagation -target %target-swift-5.6-abi-triple) | %FileCheck %s --check-prefixes=CHECK,CHECK-DBG,CHECK-%target-ptrsize
// RUN: %target-run-simple-swift(-O -parse-stdlib -Xfrontend -enable-copy-propagation -target %target-swift-5.6-abi-triple) | %FileCheck %s --check-prefixes=CHECK,CHECK-OPT,CHECK-%target-ptrsize

// REQUIRES: executable_test
// REQUIRES: objc_interop
// rdar://124700033
// UNSUPPORTED: OS=xros

// FIXME: rdar://problem/19648117 Needs splitting objc parts out

import Swift
import SwiftShims

class C {
  deinit { print("deallocated") }
}

#if _pointerBitWidth(_32)

// We have no ObjC tagged pointers, and two low spare bits due to alignment.
let NATIVE_SPARE_BITS: UInt = 0x0000_0003
let OBJC_TAGGED_POINTER_BITS: UInt = 0

#elseif arch(x86_64)

// We have ObjC tagged pointers in the lowest and highest bit
let NATIVE_SPARE_BITS: UInt = 0x7F00_0000_0000_0006
let OBJC_TAGGED_POINTER_BITS: UInt = 0x8000_0000_0000_0001

#elseif arch(arm64)

// We have ObjC tagged pointers in the highest bit
let NATIVE_SPARE_BITS: UInt = 0x7000_0000_0000_0007
let OBJC_TAGGED_POINTER_BITS: UInt = 0x8000_0000_0000_0000

#elseif arch(powerpc64) || arch(powerpc64le)

// We have no ObjC tagged pointers, and three low spare bits due to alignment.
let NATIVE_SPARE_BITS: UInt = 0x0000_0000_0000_0007
let OBJC_TAGGED_POINTER_BITS: UInt = 0

#elseif arch(s390x)

// We have no ObjC tagged pointers, and three low spare bits due to alignment.
let NATIVE_SPARE_BITS: UInt = 0x0000_0000_0000_0007
let OBJC_TAGGED_POINTER_BITS: UInt = 0

#elseif arch(riscv64)

// We have no ObjC tagged pointers, and three low spare bits due to alignment.
let NATIVE_SPARE_BITS: UInt = 0x0000_0000_0000_0007
let OBJC_TAGGED_POINTER_BITS: UInt = 0

#endif

func bitPattern(_ x: Builtin.BridgeObject) -> UInt {
  return UInt(Builtin.castBitPatternFromBridgeObject(x))
}

func nonPointerBits(_ x: Builtin.BridgeObject) -> UInt {
  return bitPattern(x) & NATIVE_SPARE_BITS
}

// Try without any bits set.
if true {
  print("No bits set") // CHECK: No bits set
  let x = C()
  let bo = Builtin.castToBridgeObject(x, 0._builtinWordValue)
  let bo2 = bo
  let x1: C = Builtin.castReferenceFromBridgeObject(bo)
  let x2: C = Builtin.castReferenceFromBridgeObject(bo2)
  // CHECK:      true
  print(x === x1)
  // CHECK-NEXT: true
  print(x === x2)

  print(nonPointerBits(bo) == 0)
  // CHECK-NEXT: true
  
  var bo3 = Builtin.castToBridgeObject(C(), 0._builtinWordValue)
  print(Bool(_builtinBooleanLiteral: Builtin.isUnique(&bo3)))
  // CHECK-NEXT: true
  let bo4 = bo3
  print(Bool(_builtinBooleanLiteral: Builtin.isUnique(&bo3)))
  // CHECK-NEXT: false
  _fixLifetime(bo3)
  _fixLifetime(bo4)
}
// CHECK-NEXT: deallocated
// CHECK-NEXT: deallocated

// Try with all spare bits set.
if true {
  print("All spare bits set") // CHECK: All spare bits set
  let x = C()
  let bo = Builtin.castToBridgeObject(x, NATIVE_SPARE_BITS._builtinWordValue)

  let bo2 = bo
  let x1: C = Builtin.castReferenceFromBridgeObject(bo)
  let x2: C = Builtin.castReferenceFromBridgeObject(bo2)
  // CHECK-NEXT: true
  print(x === x1)
  // CHECK-NEXT: true
  print(x === x2)
  
  print(nonPointerBits(bo) == NATIVE_SPARE_BITS)
  // CHECK-NEXT: true
  
  var bo3 = Builtin.castToBridgeObject(C(), NATIVE_SPARE_BITS._builtinWordValue)
  print(Bool(_builtinBooleanLiteral: Builtin.isUnique(&bo3)))
  // CHECK-NEXT: true
  let bo4 = bo3
  print(Bool(_builtinBooleanLiteral: Builtin.isUnique(&bo3)))
  // CHECK-NEXT: false
  _fixLifetime(bo3)
  _fixLifetime(bo4)
}
// CHECK-NEXT: deallocated
// CHECK-NEXT: deallocated


import Foundation

func nonNativeBridgeObject(_ o: AnyObject) -> Builtin.BridgeObject {
  let tagged = ((Builtin.reinterpretCast(o) as UInt) & OBJC_TAGGED_POINTER_BITS) != 0
  return Builtin.castToBridgeObject(
    o, tagged ? 0._builtinWordValue : NATIVE_SPARE_BITS._builtinWordValue)
}

// Try with a (probably) tagged pointer. No bits may be masked into a
// non-native object.
if true {
  print("Tagged pointer") // CHECK: Tagged pointer
  let x = NSNumber(value: 22)
  let bo = nonNativeBridgeObject(x)
  let bo2 = bo
  let x1: NSNumber = Builtin.castReferenceFromBridgeObject(bo)
  let x2: NSNumber = Builtin.castReferenceFromBridgeObject(bo2)
  // CHECK-NEXT: true
  print(x === x1)
  // CHECK-NEXT: true
  print(x === x2)

  var bo3 = nonNativeBridgeObject(NSNumber(value: 22))
  print(Bool(_builtinBooleanLiteral: Builtin.isUnique(&bo3)))
  // On 64-bit*, this will be an objc tagged pointer.  On 32-bit, there are no
  // objc tagged pointers.  If it's an objc tagged pointer, it will never be
  // unique. Otherwise, it will be unique--subject to any intering of NSNumbers
  // that Foundation does.
  // * Whether it's a tagged pointer doesn't actually depend on bit width--see
  //   OBJC_TAGGED_POINTER_BITS for the various architectures at the top of the
  //   file.  If those other architectures ever start being used in CI, this
  //   test will need more involved updating.
  // CHECK-32-NEXT: true
  // CHECK-64-NEXT: false
  _fixLifetime(bo3)
}

var unTaggedString: NSString {
  return NSString(format: "A long string that won't fit in a tagged pointer")  
}

// Try with an un-tagged pointer. 
if true {
  print("Untagged pointer") // CHECK: Untagged pointer
  let x = unTaggedString
  let bo = nonNativeBridgeObject(x)
  let bo2 = bo
  let x1: NSString = Builtin.castReferenceFromBridgeObject(bo)
  let x2: NSString = Builtin.castReferenceFromBridgeObject(bo2)
  // CHECK-NEXT: true
  print(x === x1)
  // CHECK-NEXT: true
  print(x === x2)
  
  var bo3 = nonNativeBridgeObject(unTaggedString)
  print(Bool(_builtinBooleanLiteral: Builtin.isUnique(&bo3)))
  // CHECK-NEXT: true
  _fixLifetime(bo3)
}


func hitOptionalGenerically<T>(_ x: T?) {
  switch x {
  case .some:
    print("some")
  case .none:
    print("none")
  }
}

func hitOptionalSpecifically(_ x: Builtin.BridgeObject?) {
  switch x {
  case .some:
    print("some")
  case .none:
    print("none")
  }
}

if true {
  print("BridgeObject") // CHECK: BridgeObject

  // CHECK-NEXT: true
  print(MemoryLayout<Optional<Builtin.BridgeObject>>.size
            == MemoryLayout<Builtin.BridgeObject>.size)

  var bo: Builtin.BridgeObject?

  // CHECK-NEXT: none
  hitOptionalSpecifically(bo)
  // CHECK-NEXT: none
  hitOptionalGenerically(bo)

  bo = Builtin.castToBridgeObject(C(), 0._builtinWordValue)
  // CHECK-NEXT: some
  hitOptionalSpecifically(bo)
  // CHECK: some
  hitOptionalGenerically(bo)
}

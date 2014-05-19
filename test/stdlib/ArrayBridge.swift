//===--- ArrayBridge.swift - Tests of Array casting and bridging ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: xcrun -sdk %target-sdk-name clang++ -arch %target-cpu %S/Inputs/ArrayBridge/ArrayBridge.m -c -o %t/ArrayBridgeObjC.o -g
// RUN: %target-build-swift %s -I %S/Inputs/ArrayBridge/ -Xlinker %t/ArrayBridgeObjC.o -o %t/ArrayBridge

// RUN: %target-run %t/ArrayBridge > %t.txt
// RUN: FileCheck %s < %t.txt

// CHECK: testing...
println("testing...")

import Foundation
import ArrayBridgeObjC

var trackedCount = 0
var nextTrackedSerialNumber = 0

class Tracked : ForwardIndex, Printable {
  init(_ value: Int) {
    ++trackedCount
    serialNumber = ++nextTrackedSerialNumber
    self.value = value
  }
  
  deinit {
    assert(serialNumber > 0, "double destruction!")
    --trackedCount
    serialNumber = -serialNumber
  }

  var description: String {
    assert(serialNumber > 0, "dead Tracked!")
    return "Base#\(serialNumber)(\(value))"
  }

  func succ() -> Tracked {
    return Tracked(self.value.succ())
  }

  var value: Int
  var serialNumber: Int
}

func == (x: Tracked, y: Tracked) -> Bool {
  return x.value == y.value
}

typealias Base = Tracked
class Derived : Base, Printable {
  override var description: String {
    assert(serialNumber > 0, "dead Tracked!")
    return "Derived#\(serialNumber)(\(value))"
  }
}

class BridgedObjC : Base, Printable {
  override var description: String {
    assert(serialNumber > 0, "dead Tracked!")
    return "BridgedObjC#\(serialNumber)(\(value))"
  }
}

struct BridgedSwift : Printable, _BridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return BridgedObjC.self
  }
  
  func bridgeToObjectiveC() -> BridgedObjC {
    return BridgedObjC(trak.value)
  }

  static func bridgeFromObjectiveC(x: BridgedObjC) -> BridgedSwift? {
    return x.value >= 0 ? BridgedSwift(x.value) : nil
  }
  
  var description: String {
    assert(trak.serialNumber > 0, "dead Tracked!")
    return "BridgedSwift#\(trak.serialNumber)(\(trak.value))"
  }

  init(_ value: Int) {
    self.trak = Tracked(value)
  }
  
  func succ() -> BridgedSwift {
    return BridgedSwift(trak.value.succ())
  }

  var trak: Tracked
}


//===--- Bridged Verbatim -------------------------------------------------===//
// Base is "bridged verbatim"
//===----------------------------------------------------------------------===//

func testBridgedVerbatim() {
  let bases: Base[] = [Base(100), Base(200), Base(300)]

  //===--- Implicit conversion to/from NSArray ------------------------------===//

  // CHECK-NEXT: Base#1(100)
  let basesConvertedToNSArray: NSArray = bases
  println(basesConvertedToNSArray.objectAtIndex(0) as Base)

  // Create an ordinary NSArray, not a native one
  let nsArrayOfBase: NSArray = NSArray(object: Base(42))

  // NSArray converts implicitly to AnyObject[]...
  let nsArrayOfBaseConvertedToAnyObjectArray: AnyObject[] = nsArrayOfBase

  // Capture the representation of the first element
  // CHECK-NEXT: [[base42:Base.*42]]
  println(nsArrayOfBase.objectAtIndex(0) as Base)

  // ...with the same elements
  // CHECK-NEXT: [[base42]]
  println((nsArrayOfBaseConvertedToAnyObjectArray[0] as Base)!)

  //===--- Up- and Down-casts -----------------------------------------------===//
  let derived: Derived[] = [Derived(11), Derived(22)]
  // CHECK-NEXT: [[derived0:\[Derived#[0-9]+\(11\), Derived#[0-9]+\(22\)\]{1}]]
  println(derived)

  // upcast is implicit
  let derivedAsBases: Base[] = derived

  // CHECK-NEXT: [[derived0]]
  println(derivedAsBases)

  // Arrays are logically distinct after upcast
  derived[0] = Derived(33)
  
  // CHECK-NEXT: {{\[Derived#[0-9]+\(33\), Derived#[0-9]+\(22\)]}}
  println(derived)
  // CHECK-NEXT: [[derived0]]
  println(derivedAsBases)

  // CHECK-NEXT: [[derived0]]
  if let roundTripDerived = derivedAsBases as Derived[] {
    println(roundTripDerived)
  }
  else {
    println("roundTripDerived upcast failed")
  }

  // CHECK-NEXT: [[derived2:\[Derived#[0-9]+\(44\), Derived#[0-9]+\(55\)\]{1}]]
  let derivedInBaseBuffer: Base[] = [Derived(44), Derived(55)]
  println(derivedInBaseBuffer)
  
  // CHECK-NEXT: Explicit downcast-ability is based on element type, not buffer type
  if let downcastBaseBuffer = derivedInBaseBuffer as Derived[] {
    println("Explicit downcast-ability is based on element type, not buffer type")
  }
  else {
    println("Unexpected downcast failure")
  }

  // We can up-cast to array of AnyObject
  // CHECK-NEXT: [[derived2]]
  let derivedAsAnyObjectArray: AnyObject[] = derivedInBaseBuffer
  println(derivedAsAnyObjectArray)

  // CHECK-NEXT: downcastBackToBase = [[derived2]]
  if let downcastBackToBase = derivedAsAnyObjectArray as Base[] {
    println("downcastBackToBase = \(downcastBackToBase)")
  }
  else {
    println("downcastBackToBase failed")
  }

  // CHECK-NEXT: downcastBackToDerived = [[derived2]]
  if let downcastBackToDerived = derivedAsAnyObjectArray as Derived[] {
    println("downcastBackToDerived = \(downcastBackToDerived)")
  }
  else {
    println("downcastBackToDerived failed")
  }
}
testBridgedVerbatim()

//===--- Explicitly Bridged -----------------------------------------------===//
// BridgedSwift conforms to _BridgedToObjectiveC
//===----------------------------------------------------------------------===//
func testExplicitlyBridged() {
  // CHECK-LABEL: testExplicitlyBridged()
  println("testExplicitlyBridged()")

  let bridgedSwifts = [BridgedSwift(42), BridgedSwift(17)]
  
  let bridgedSwiftsAsNSArray: NSArray = bridgedSwifts
  // CHECK-NEXT: [BridgedObjC#{{[0-9]+}}(42), BridgedObjC#{{[0-9]+}}(17)]
  println("bridgedSwiftsAsNSArray = \(bridgedSwiftsAsNSArray as AnyObject[]))")

  // Make sure we can bridge back.
  let roundTripBridgedSwifts
    = BridgedSwift[].bridgeFromObjectiveC(bridgedSwiftsAsNSArray)!
  // CHECK-NEXT-NOT: [BridgedSwift#[[id00]](42), BridgedSwift#[[id01]](17)]
  // CHECK-NEXT: [BridgedSwift#[[id10:[0-9]+]](42), BridgedSwift#[[id11:[0-9]+]](17)]
  println("roundTripBridgedSwifts = \(roundTripBridgedSwifts))")

  // Make a real Cocoa NSArray of these...
  let cocoaBridgedSwifts = NSArray(array: bridgedSwiftsAsNSArray)

  // ...and bridge *that* back
  let bridgedBackSwifts
    = BridgedSwift[].bridgeFromObjectiveC(cocoaBridgedSwifts)!
  // CHECK-NEXT-NOT: [BridgedSwift#[[id00]](42), BridgedSwift#[[id01]](17)]
  // CHECK-NEXT-NOT: [BridgedSwift#[[id10]](42), BridgedSwift#[[id11]](17)]
  // CHECK-NEXT: [BridgedSwift#{{[0-9]+}}(42), BridgedSwift#{{[0-9]+}}(17)]
  println("bridgedBackSwifts      = \(bridgedBackSwifts)")
  
  // all: verbatim,  not, and doesn't bridge
  // implicit conversions to/from NSArray 
  // Base[] -> Derived[] and Derived[] -> Base[] where Base can be AnyObject
  // defining @objc method taking T[] and returning T[]

  // Up-casts.
  let bridgedSwiftsAsBridgedObjCs: BridgedObjC[] = bridgedSwifts
  // CHECK-NEXT: BridgedObjC#[[ID0:[0-9]+]](42)
  println(bridgedSwiftsAsBridgedObjCs[0])
  // CHECK-NEXT: BridgedObjC#[[ID1:[0-9]+]](17)
  println(bridgedSwiftsAsBridgedObjCs[1])

  let bridgedSwiftsAsBases: Base[] = bridgedSwifts
  // CHECK-NEXT: BridgedObjC#[[ID0:[0-9]+]](42)
  println(bridgedSwiftsAsBases[0])
  // CHECK-NEXT: BridgedObjC#[[ID1:[0-9]+]](17)
  println(bridgedSwiftsAsBases[1])

  let bridgedSwiftsAsAnyObjects: AnyObject[] = bridgedSwifts
  // CHECK-NEXT: BridgedObjC#[[ID0:[0-9]+]](42)
  println(bridgedSwiftsAsAnyObjects[0])
  // CHECK-NEXT: BridgedObjC#[[ID1:[0-9]+]](17)
  println(bridgedSwiftsAsAnyObjects[1])

  // Downcasts of non-verbatim bridged value types to objects.
  if true {
    let downcasted = bridgedSwifts as BridgedObjC[]
    // CHECK-NEXT: BridgedObjC#[[ID0:[0-9]+]](42)
    println(downcasted[0])
    // CHECK-NEXT: BridgedObjC#[[ID1:[0-9]+]](17)
    println(downcasted[1])
  }

  if true {
    let downcasted = bridgedSwifts as Base[]
    // CHECK-NEXT: BridgedObjC#[[ID0:[0-9]+]](42)
    println(downcasted[0])
    // CHECK-NEXT: BridgedObjC#[[ID1:[0-9]+]](17)
    println(downcasted[1])
  }

  if true {
    let downcasted = bridgedSwifts as AnyObject[]
    // CHECK-NEXT: BridgedObjC#[[ID0:[0-9]+]](42)
    println(downcasted[0])
    // CHECK-NEXT: BridgedObjC#[[ID1:[0-9]+]](17)
    println(downcasted[1])
  }

  // Downcasts of up-casted arrays.
  if let downcasted = bridgedSwiftsAsAnyObjects as BridgedObjC[] {
    // CHECK-NEXT: BridgedObjC#[[ID0:[0-9]+]](42)
    println(downcasted[0])
    // CHECK-NEXT: BridgedObjC#[[ID1:[0-9]+]](17)
    println(downcasted[1])
  } else {
    println("Could not downcast AnyObject[] to BridgedObjC[]?")
  }

  if let downcasted = bridgedSwiftsAsAnyObjects as Base[] {
    // CHECK-NEXT: BridgedObjC#[[ID0:[0-9]+]](42)
    println(downcasted[0])
    // CHECK-NEXT: BridgedObjC#[[ID1:[0-9]+]](17)
    println(downcasted[1])
  } else {
    println("Could not downcast AnyObject[] to Base[]?")
  }

  // Downcast of Cocoa array to an array of classes.
  let wrappedCocoaBridgedSwifts: AnyObject[] = cocoaBridgedSwifts
  if let downcasted = wrappedCocoaBridgedSwifts as BridgedObjC[] {
    // CHECK-NEXT: BridgedObjC#[[ID0:[0-9]+]](42)
    println(downcasted[0])
    // CHECK-NEXT: BridgedObjC#[[ID1:[0-9]+]](17)
    println(downcasted[1])
  } else {
    println("Could not downcast AnyObject[] to BridgedObjC[]?")
  }

  // Downcast of Cocoa array to an array of values.
  if let downcasted = wrappedCocoaBridgedSwifts as BridgedSwift[] {
    // CHECK-NEXT: BridgedSwift#[[ID0:[0-9]+]](42)
    println(downcasted[0])
    // CHECK-NEXT: BridgedSwift#[[ID1:[0-9]+]](17)
    println(downcasted[1])
  } else {
    println("Could not downcast AnyObject[] to BridgedSwift[]?")
  }

  // Downcast of Cocoa array to an array of strings (which should fail)
  // CHECK-NEXT: Could not downcast AnyObject[] to String[]
  if let downcasted = wrappedCocoaBridgedSwifts as String[] {
    println("Shouldn't be able to downcast to an array of strings")
  } else {
    println("Could not downcast AnyObject[] to String[]")
  }

  // Downcast from an implicitly unwrapped optional array of AnyObjects.
  var wrappedCocoaBridgedSwiftsIUO: AnyObject[]! = wrappedCocoaBridgedSwifts
    if let downcasted = wrappedCocoaBridgedSwiftsIUO as BridgedSwift[] {
    // CHECK-NEXT: BridgedSwift#[[ID0:[0-9]+]](42)
    println(downcasted[0])
    // CHECK-NEXT: BridgedSwift#[[ID1:[0-9]+]](17)
    println(downcasted[1])
  } else {
    println("Could not downcast AnyObject[]! to BridgedSwift[]")
  }

  // Downcast from a nil implicitly unwrapped optional array of AnyObjects.
  wrappedCocoaBridgedSwiftsIUO = nil
  if let downcasted = wrappedCocoaBridgedSwiftsIUO as BridgedSwift[] {
    println("Cannot downcast from a nil array!")
  } else {
    // CHECK-NEXT: Correctly rejected downcast of nil array
    println("Correctly rejected downcast of nil array")
  }

  // Downcast from an optional array of AnyObjects.
  var wrappedCocoaBridgedSwiftsOpt: AnyObject[]? = wrappedCocoaBridgedSwifts
    if let downcasted = wrappedCocoaBridgedSwiftsOpt as BridgedSwift[] {
    // CHECK-NEXT: BridgedSwift#[[ID0:[0-9]+]](42)
    println(downcasted[0])
    // CHECK-NEXT: BridgedSwift#[[ID1:[0-9]+]](17)
    println(downcasted[1])
  } else {
    println("Could not downcast AnyObject[]! to BridgedSwift[]")
  }

  // Downcast from a nil optional array of AnyObjects.
  wrappedCocoaBridgedSwiftsOpt = nil
  if let downcasted = wrappedCocoaBridgedSwiftsOpt as BridgedSwift[] {
    println("Cannot downcast from a nil array!")
  } else {
    // CHECK-NEXT: Correctly rejected downcast of nil array
    println("Correctly rejected downcast of nil array")
  }
}
testExplicitlyBridged()

//===--- Non-bridging -----------------------------------------------------===//
// X is not bridged to Objective-C
//===----------------------------------------------------------------------===//

struct X {}

/*
let x: NSArray = arrayAsID(bases)!

println(x.objectAtIndex(0) as Base)
*/



// CHECK-NEXT: done.
println("done.")

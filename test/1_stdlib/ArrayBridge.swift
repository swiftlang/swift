//===--- ArrayBridge.swift - Tests of Array casting and bridging ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-clang %S/Inputs/ArrayBridge/ArrayBridge.m -c -o %t/ArrayBridgeObjC.o -g
// RUN: %target-build-swift %s -I %S/Inputs/ArrayBridge/ -Xlinker %t/ArrayBridgeObjC.o -o %t/ArrayBridge

// RUN: %target-run %t/ArrayBridge > %t.txt
// RUN: FileCheck %s < %t.txt
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import ArrayBridgeObjC
import StdlibUnittest
let tests = TestSuite("ArrayBridge")

var trackedCount = 0
var nextBaseSerialNumber = 0

@objc protocol Fooable {
  func foo()
}

@objc protocol Barable {
  func bar()
}

@objc protocol Bazable {
  func baz()
}

/// A type that will be bridged verbatim to Objective-C
class Base : NSObject, Fooable {
  func foo() { }

  required init(_ value: Int) {
    trackedCount += 1
    nextBaseSerialNumber += 1
    serialNumber = nextBaseSerialNumber
    self.value = value
  }
  
  deinit {
    assert(serialNumber > 0, "double destruction!")
    trackedCount -= 1
    serialNumber = -serialNumber
  }

  override var description: String {
    assert(serialNumber > 0, "dead Base!")
    return "Base#\(serialNumber)(\(value))"
  }

  func successor() -> Self {
    return self.dynamicType.init(self.value + 1)
  }

  override func isEqual(_ other: AnyObject?) -> Bool {
    return (other as? Base)?.value == self.value
  }
  
  var value: Int
  var serialNumber: Int
}

class Derived : Base, Barable {
  func bar() { }
  override var description: String {
    assert(serialNumber > 0, "dead Base!")
    return "Derived#\(serialNumber)(\(value))"
  }
}

var bridgeFromOperationCount = 0
var bridgeToOperationCount = 0

/// A value type that's bridged using _ObjectiveCBridgeable
struct BridgedSwift : CustomStringConvertible, _ObjectiveCBridgeable {
  func _bridgeToObjectiveC() -> Derived {
    bridgeToOperationCount += 1
    return Derived(trak.value)
  }

  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }

  static func _forceBridgeFromObjectiveC(
    _ x: Derived,
    result: inout BridgedSwift?
  ) {
    assert(x.value >= 0, "not bridged")
    bridgeFromOperationCount += 1
    result = BridgedSwift(x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    _ x: Derived,
    result: inout BridgedSwift?
  ) -> Bool {
    if x.value >= 0 {
      result = BridgedSwift(x.value)
      return true
    }

    result = nil
    return false
  }

  static func _unconditionallyBridgeFromObjectiveC(_ source: Derived?)
      -> BridgedSwift {
    var result: BridgedSwift? = nil
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }

  var description: String {
    assert(trak.serialNumber > 0, "dead Base!")
    return "BridgedSwift#\(trak.serialNumber)(\(trak.value))"
  }

  init(_ value: Int) {
    self.trak = Base(value)
  }
  
  func successor() -> BridgedSwift {
    return BridgedSwift(trak.value + 1)
  }

  static func resetStats() {
    bridgeFromOperationCount = 0
    bridgeToOperationCount = 0
  }
  
  var trak: Base
}

// A class used to test various Objective-C thunks.
class Thunks : NSObject {
  func createDerived(_ value: Int) -> AnyObject {
    return Derived(value)
  }
  
  @objc func acceptDerivedArray(
    _ x: [Derived],
    expecting expected: NSArray
  )  {
    expectEqual(x.count, expected.count)
    for i in 0..<x.count {
      expectTrue(x[i] === expected[i])
    }
  }

  @objc func produceDerivedArray(
    _ expectations: NSMutableArray
  ) -> [Derived] {
    var array: [Derived] = []
    for i in 0..<5 {
      array.append(Derived(i))
      expectations.add(array[i])
    }
    return array
  }

  @objc func checkProducedDerivedArray(
    _ produced: NSArray, expecting expected: NSArray
  ) {
    expectEqual(produced.count, expected.count)
    for i in 0..<produced.count {
      expectTrue(produced[i] === expected[i])
    }
  }
  
  @objc func acceptBridgedSwiftArray(_ raw: NSArray) -> AnyObject {
    let x = raw as! [BridgedSwift]
    return Box(x)
  }

  @objc func produceBridgedSwiftArray(_ numItems: Int) -> NSArray {
    var array: [BridgedSwift] = []
    for i in 0..<numItems {
      array.append(BridgedSwift(i))
    }
    return array as NSArray
  }
}


//===--- Bridged Verbatim -------------------------------------------------===//
// Base is "bridged verbatim"
//===----------------------------------------------------------------------===//

tests.test("testBridgedVerbatim") {
  nextBaseSerialNumber = 0
  let bases: [Base] = [Base(100), Base(200), Base(300)]

  //===--- Implicit conversion to/from NSArray ------------------------------===//

  let basesConvertedToNSArray = bases as NSArray
  expectEqual(
    "Base#1(100)",
    String(basesConvertedToNSArray.object(at: 0) as! Base))

  // Create an ordinary NSArray, not a native one
  let nsArrayOfBase: NSArray = NSArray(object: Base(42))

  // NSArray can be unconditionally cast to [AnyObject]...
  let nsArrayOfBaseConvertedToAnyObjectArray = nsArrayOfBase as [AnyObject]

  // Capture the representation of the first element
  let base42: ObjectIdentifier
  do {
    let b = nsArrayOfBase.object(at: 0) as! Base
    expectEqual(42, b.value)
    base42 = ObjectIdentifier(b)
  }

  // ...with the same elements
  expectEqual(
    base42,
    ObjectIdentifier(nsArrayOfBaseConvertedToAnyObjectArray[0] as! Base))

  // Verify that NSArray class methods are inherited by a Swift bridging class.
  let className = String(reflecting: basesConvertedToNSArray.dynamicType)
  expectTrue(className.hasPrefix("Swift._ContiguousArrayStorage"))
  expectTrue(basesConvertedToNSArray.dynamicType.supportsSecureCoding)

  //===--- Up- and Down-casts -----------------------------------------------===//
  var derived: [Derived] = [Derived(11), Derived(22)]
  let derived0 = derived

  // upcast is implicit
  let derivedAsBases: [Base] = derived
  expectEqual(derived.count, derivedAsBases.count)
  for (x, y) in zip(derived, derivedAsBases) {
    expectTrue(x === y)
  }

  // Arrays are logically distinct after upcast
  derived[0] = Derived(33)

  expectEqual([Derived(33), Derived(22)], derived)
  expectEqual([Derived(11), Derived(22)], derivedAsBases)

  expectEqual(derived0, derivedAsBases as! [Derived])

  let derivedInBaseBuffer: [Base] = [Derived(44), Derived(55)]
  let derived2 = derivedInBaseBuffer
  
  // Explicit downcast-ability is based on element type, not buffer type
  expectNotEmpty(derivedInBaseBuffer as? [Derived])

  // We can up-cast to array of AnyObject
  let derivedAsAnyObjectArray: [AnyObject] = derivedInBaseBuffer
  expectEqual(derived2, derivedAsAnyObjectArray.map { $0 as! Base })

  let downcastBackToBase = derivedAsAnyObjectArray as? [Base]
  expectNotEmpty(downcastBackToBase)

  if let downcastBackToDerived = expectNotEmpty(derivedAsAnyObjectArray as? [Derived]) {
    expectEqual(derived2, downcastBackToDerived)
  }

  if let downcastToProtocols = expectNotEmpty(derivedAsAnyObjectArray as? [Fooable]) {
    expectEqual(derived2, downcastToProtocols.map { $0 as! Derived })
  }

  if let downcastToProtocols = expectNotEmpty(derivedAsAnyObjectArray as? [Barable]) {
    expectEqual(derived2, downcastToProtocols.map { $0 as! Derived })
  }

  if let downcastToProtocols = expectNotEmpty(derivedAsAnyObjectArray as? [protocol<Barable, Fooable>]) {
    expectEqual(derived2, downcastToProtocols.map { $0 as! Derived })
  }

  expectEmpty(derivedAsAnyObjectArray as? [protocol<Barable, Bazable>])
}

tests.test("doTestDerived") {
  testDerived(Thunks())
}

//===--- Explicitly Bridged -----------------------------------------------===//
// BridgedSwift conforms to _ObjectiveCBridgeable
//===----------------------------------------------------------------------===//
func testExplicitlyBridged() {
  // CHECK-LABEL: testExplicitlyBridged()
  print("testExplicitlyBridged()")

  let bridgedSwifts = [BridgedSwift(42), BridgedSwift(17)]
  
  let bridgedSwiftsAsNSArray = bridgedSwifts as NSArray
  // CHECK-NEXT: [Derived#{{[0-9]+}}(42), Derived#{{[0-9]+}}(17)]
  print("bridgedSwiftsAsNSArray = \(bridgedSwiftsAsNSArray as [AnyObject]))")

  // Make sure we can bridge back.
  let roundTripBridgedSwifts
    = Swift._forceBridgeFromObjectiveC(bridgedSwiftsAsNSArray, 
                                       [BridgedSwift].self)
  // CHECK-NOT: [BridgedSwift#[[id00]](42), BridgedSwift#[[id01]](17)]
  // CHECK-NEXT: [BridgedSwift#[[id10:[0-9]+]](42), BridgedSwift#[[id11:[0-9]+]](17)]
  print("roundTripBridgedSwifts = \(roundTripBridgedSwifts))")

  // Make a real Cocoa NSArray of these...
  let cocoaBridgedSwifts = NSArray(array: bridgedSwiftsAsNSArray as [AnyObject])

  // ...and bridge *that* back
  let bridgedBackSwifts
    = Swift._forceBridgeFromObjectiveC(cocoaBridgedSwifts, [BridgedSwift].self)
  // CHECK-NOT: [BridgedSwift#[[id00]](42), BridgedSwift#[[id01]](17)]
  // CHECK-NOT: [BridgedSwift#[[id10]](42), BridgedSwift#[[id11]](17)]
  // CHECK-NEXT: [BridgedSwift#{{[0-9]+}}(42), BridgedSwift#{{[0-9]+}}(17)]
  print("bridgedBackSwifts      = \(bridgedBackSwifts)")
  
  // all: verbatim, not, and doesn't bridge
  // implicit conversions to/from NSArray 
  // [Base] -> [Derived] and [Derived] -> [Base] where Base can be AnyObject
  // defining @objc method taking [T] and returning [T]

  // Up-casts.
  let bridgedSwiftsAsDeriveds: [Derived] = bridgedSwifts
  // CHECK-NEXT: Derived#[[ID0:[0-9]+]](42)
  print(bridgedSwiftsAsDeriveds[0])
  // CHECK-NEXT: Derived#[[ID1:[0-9]+]](17)
  print(bridgedSwiftsAsDeriveds[1])

  let bridgedSwiftsAsBases: [Base] = bridgedSwifts
  // CHECK-NEXT: Derived#[[ID0:[0-9]+]](42)
  print(bridgedSwiftsAsBases[0])
  // CHECK-NEXT: Derived#[[ID1:[0-9]+]](17)
  print(bridgedSwiftsAsBases[1])

  let bridgedSwiftsAsAnyObjects: [AnyObject] = bridgedSwifts
  // CHECK-NEXT: Derived#[[ID0:[0-9]+]](42)
  print(bridgedSwiftsAsAnyObjects[0])
  // CHECK-NEXT: Derived#[[ID1:[0-9]+]](17)
  print(bridgedSwiftsAsAnyObjects[1])

  // Downcasts of non-verbatim bridged value types to objects.
  do {
    let downcasted = bridgedSwifts as [Derived]
    // CHECK-NEXT: Derived#[[ID0:[0-9]+]](42)
    print(downcasted[0])
    // CHECK-NEXT: Derived#[[ID1:[0-9]+]](17)
    print(downcasted[1])
  }

  do {
    let downcasted = bridgedSwifts as [Base]
    // CHECK-NEXT: Derived#[[ID0:[0-9]+]](42)
    print(downcasted[0])
    // CHECK-NEXT: Derived#[[ID1:[0-9]+]](17)
    print(downcasted[1])
  }

  do {
    let downcasted = bridgedSwifts as [AnyObject]
    // CHECK-NEXT: Derived#[[ID0:[0-9]+]](42)
    print(downcasted[0])
    // CHECK-NEXT: Derived#[[ID1:[0-9]+]](17)
    print(downcasted[1])
  }

  // Downcasts of up-casted arrays.
  if let downcasted = bridgedSwiftsAsAnyObjects as? [Derived] {
    // CHECK-NEXT: Derived#[[ID0:[0-9]+]](42)
    print(downcasted[0])
    // CHECK-NEXT: Derived#[[ID1:[0-9]+]](17)
    print(downcasted[1])
  } else {
    print("Could not downcast [AnyObject] to [Derived]?")
  }

  if let downcasted = bridgedSwiftsAsAnyObjects as? [Base] {
    // CHECK-NEXT: Derived#[[ID0:[0-9]+]](42)
    print(downcasted[0])
    // CHECK-NEXT: Derived#[[ID1:[0-9]+]](17)
    print(downcasted[1])
  } else {
    print("Could not downcast [AnyObject] to [Base]?")
  }

  // Downcast of Cocoa array to an array of classes.
  let wrappedCocoaBridgedSwifts = cocoaBridgedSwifts as [AnyObject]
  if let downcasted = wrappedCocoaBridgedSwifts as? [Derived] {
    // CHECK-NEXT: Derived#[[ID0:[0-9]+]](42)
    print(downcasted[0])
    // CHECK-NEXT: Derived#[[ID1:[0-9]+]](17)
    print(downcasted[1])
  } else {
    print("Could not downcast [AnyObject] to [Derived]?")
  }

  // Downcast of Cocoa array to an array of values.
  if let downcasted = wrappedCocoaBridgedSwifts as? [BridgedSwift] {
    // CHECK-NEXT: BridgedSwift#[[ID0:[0-9]+]](42)
    print(downcasted[0])
    // CHECK-NEXT: BridgedSwift#[[ID1:[0-9]+]](17)
    print(downcasted[1])
  } else {
    print("Could not downcast [AnyObject] to [BridgedSwift]?")
  }

  // Downcast of Cocoa array to an array of strings (which should fail)
  // CHECK-NEXT: Could not downcast [AnyObject] to [String]
  if let _ = wrappedCocoaBridgedSwifts as? [String] {
    print("Shouldn't be able to downcast to an array of strings")
  } else {
    print("Could not downcast [AnyObject] to [String]")
  }

  // Downcast from an implicitly unwrapped optional array of AnyObjects.
  var wrappedCocoaBridgedSwiftsIUO: [AnyObject]! = wrappedCocoaBridgedSwifts
    if let downcasted = wrappedCocoaBridgedSwiftsIUO as? [BridgedSwift] {
    // CHECK-NEXT: BridgedSwift#[[ID0:[0-9]+]](42)
    print(downcasted[0])
    // CHECK-NEXT: BridgedSwift#[[ID1:[0-9]+]](17)
    print(downcasted[1])
  } else {
    print("Could not downcast [AnyObject]! to [BridgedSwift]")
  }

  // Downcast from a nil implicitly unwrapped optional array of AnyObjects.
  wrappedCocoaBridgedSwiftsIUO = nil
  if let _ = wrappedCocoaBridgedSwiftsIUO as? [BridgedSwift] {
    print("Cannot downcast from a nil array!")
  } else {
    // CHECK-NEXT: Correctly rejected downcast of nil array
    print("Correctly rejected downcast of nil array")
  }

  // Downcast from an optional array of AnyObjects.
  var wrappedCocoaBridgedSwiftsOpt: [AnyObject]? = wrappedCocoaBridgedSwifts
    if let downcasted = wrappedCocoaBridgedSwiftsOpt as? [BridgedSwift] {
    // CHECK-NEXT: BridgedSwift#[[ID0:[0-9]+]](42)
    print(downcasted[0])
    // CHECK-NEXT: BridgedSwift#[[ID1:[0-9]+]](17)
    print(downcasted[1])
  } else {
    print("Could not downcast [AnyObject]! to [BridgedSwift]")
  }

  // Downcast from a nil optional array of AnyObjects.
  wrappedCocoaBridgedSwiftsOpt = nil
  if let _ = wrappedCocoaBridgedSwiftsOpt as? [BridgedSwift] {
    print("Cannot downcast from a nil array!")
  } else {
    // CHECK-NEXT: Correctly rejected downcast of nil array
    print("Correctly rejected downcast of nil array")
  }
}
testExplicitlyBridged()

tests.test("testExplicitlyBridged") {
  testBridgedSwift(Thunks())
}

tests.test("testRoundTrip") {
  class Test : NSObject {
    @objc dynamic func call(_ array: NSArray) -> NSArray {

      let result = array as! [BridgedSwift]
      expectEqual(0, bridgeFromOperationCount)
      expectEqual(0, bridgeToOperationCount)

      // Clear out the stats before returning array
      BridgedSwift.resetStats()
      return result as NSArray
    }
  }
  
  let test = Test()
  
  let array = [
    BridgedSwift(10), BridgedSwift(20), BridgedSwift(30),
    BridgedSwift(40), BridgedSwift(50) ]
  
  BridgedSwift.resetStats()
  _ = test.call(array as NSArray)
  
  expectEqual(0, bridgeFromOperationCount)
  expectEqual(0, bridgeToOperationCount)
}

//===--- Non-bridging -----------------------------------------------------===//
// X is not bridged to Objective-C
//===----------------------------------------------------------------------===//

struct X {}

/*
let x: NSArray = arrayAsID(bases)!

print(x.objectAt(0) as Base)
*/

func testMutableArray() {
  let m = NSMutableArray(array: ["fu", "bar", "buzz"])
  let a = m as NSArray as! [NSString]
  print(a) // CHECK-NEXT: [fu, bar, buzz]
  m.add("goop")
  print(a) // CHECK-NEXT: [fu, bar, buzz]
}
testMutableArray()

runAllTests()

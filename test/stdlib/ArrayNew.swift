// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// FIXME: -fobjc-abi-version=2 is a band-aid fix for for rdar://16946936
//
// RUN: cp %s %t/main.swift
// RUN: xcrun -sdk %target-sdk-name clang++ -fobjc-arc -fobjc-abi-version=2 -arch %target-cpu %S/Inputs/SlurpFastEnumeration/SlurpFastEnumeration.m -c -o %t/SlurpFastEnumeration.o
// RUN: %target-build-swift %S/Inputs/DictionaryKeyValueTypes.swift %t/main.swift -I %S/Inputs/SlurpFastEnumeration/ -Xlinker %t/SlurpFastEnumeration.o -o %t/Array -Xfrontend -disable-access-control
//
// RUN: %target-run %t/Array

import Darwin
import StdlibUnittest
import Foundation

func isNativeNSArray(d: NSArray) -> Bool {
  var className: NSString = NSStringFromClass(d.dynamicType)
  return className.rangeOfString("_ContiguousArrayStorage").length > 0
}

var ArrayTestSuite = TestSuite("Array")

ArrayTestSuite.test("sizeof") {
  var a = [ 10, 20, 30 ]
#if arch(i386) || arch(arm)
  expectEqual(4, sizeofValue(a))
#else
  expectEqual(8, sizeofValue(a))
#endif
}

ArrayTestSuite.test("valueDestruction") {
  var a = [TestValueTy]()
  for i in 100...110 {
    a.append(TestValueTy(i))
  }
}

//===----------------------------------------------------------------------===//
// Native array tests
// FIXME: incomplete.
//===----------------------------------------------------------------------===//

ArrayTestSuite.test("Native/count/empty") {
  let a = [TestValueTy]()
  expectEqual(0, a.count)
}

ArrayTestSuite.test("Native/count") {
  let a = [ TestValueTy(10), TestValueTy(20), TestValueTy(30) ]
  expectEqual(3, a.count)
}

ArrayTestSuite.test("Native/isEmpty/empty") {
  let a = [TestValueTy]()
  expectTrue(a.isEmpty)
}

ArrayTestSuite.test("Native/isEmpty") {
  let a = [ TestValueTy(10), TestValueTy(20), TestValueTy(30) ]
  expectFalse(a.isEmpty)
}

//===----------------------------------------------------------------------===//
// NSArray -> Array bridging tests
// FIXME: incomplete.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Array -> NSArray bridging tests
//
// Key and Value are bridged verbatim.
//
// FIXME: incomplete.
//===----------------------------------------------------------------------===//

func getBridgedNSArrayOfRefTypeVerbatimBridged(
  numElements: Int = 3,
  capacity: Int? = nil
) -> NSArray {
  assert(_isBridgedVerbatimToObjectiveC(TestObjCValueTy.self))

  var a = [TestObjCValueTy]()
  if let requestedCapacity = capacity {
    a.reserveCapacity(requestedCapacity)
  }
  for i in 1..<(numElements + 1) {
    a.append(TestObjCValueTy(i * 10))
  }

  let bridged = _convertArrayToNSArray(a)
  assert(isNativeNSArray(bridged))

  return bridged
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/count/empty") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 0)
  expectEqual(0, a.count)
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/count") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged()
  expectEqual(3, a.count)
}

/*
Disabled because of:
<rdar://problem/18336202> Array allows out-of-bounds access through
objectAtIndex() on the bridged NSArray

for index in [ -100, -1, 0, 1, 100 ] {
  ArrayTestSuite.test(
    "BridgedToObjC/Verbatim/objectAtIndex/empty/trap/\(index)") {
    let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 0)
    expectCrashLater()
    a.objectAtIndex(index)
  }
}

for index in [ -100, -1, 3, 4, 100 ] {
  ArrayTestSuite.test("BridgedToObjC/Verbatim/objectAtIndex/trap/\(index)") {
    let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 3)
    expectCrashLater()
    a.objectAtIndex(index)
  }
}
*/

ArrayTestSuite.test("BridgedToObjC/Verbatim/objectAtIndex") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 3)

  var v: AnyObject = a.objectAtIndex(0)
  expectEqual(10, (v as TestObjCValueTy).value)
  let idValue0 = unsafeBitCast(v, UWord.self)

  v = a.objectAtIndex(1)
  expectEqual(20, (v as TestObjCValueTy).value)
  let idValue1 = unsafeBitCast(v, UWord.self)

  v = a.objectAtIndex(2)
  expectEqual(30, (v as TestObjCValueTy).value)
  let idValue2 = unsafeBitCast(v, UWord.self)

  /*
  FIXME: delayed bridging for Swift.Array
  for i in 0..<3 {
    expectEqual(idValue0, unsafeBitCast(a.objectAtIndex(0), UWord.self))
    expectEqual(idValue1, unsafeBitCast(a.objectAtIndex(1), UWord.self))
    expectEqual(idValue2, unsafeBitCast(a.objectAtIndex(2), UWord.self))
  }
  */
}

for indexRange in [ 0..<4, -2..<(-1), -1..<2, 2..<4, 4..<5 ] {
  ArrayTestSuite.test("BridgedToObjC/Verbatim/getObjects/trap/\(indexRange)")
    .crashOutputMatches("Array index out of range")
    .code {
    let a = getBridgedNSArrayOfRefTypeVerbatimBridged(
      numElements: 3, capacity: 16)
    let buffer = UnsafeMutablePointer<AnyObject>.alloc(16)
    a.getObjects(
      AutoreleasingUnsafeMutablePointer(buffer), range: NSRange(0..<3))
    expectCrashLater()
    a.getObjects(
      AutoreleasingUnsafeMutablePointer(buffer), range: NSRange(indexRange))
  }
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/getObjects") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 3)
  let buffer = UnsafeMutablePointer<AnyObject>.alloc(16)
  a.getObjects(
    AutoreleasingUnsafeMutablePointer(buffer), range: NSRange(0..<3))

  var v: AnyObject = buffer[0]
  expectEqual(10, (v as TestObjCValueTy).value)
  let idValue0 = unsafeBitCast(v, UWord.self)

  v = buffer[1]
  expectEqual(20, (v as TestObjCValueTy).value)
  let idValue1 = unsafeBitCast(v, UWord.self)

  v = buffer[2]
  expectEqual(30, (v as TestObjCValueTy).value)
  let idValue2 = unsafeBitCast(v, UWord.self)

  for i in 0..<3 {
    expectEqual(idValue0, unsafeBitCast(a.objectAtIndex(0), UWord.self))
    expectEqual(idValue1, unsafeBitCast(a.objectAtIndex(1), UWord.self))
    expectEqual(idValue2, unsafeBitCast(a.objectAtIndex(2), UWord.self))
  }

  buffer.dealloc(3)
  _fixLifetime(a)
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/copyWithZone") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 3)
  let copy: AnyObject = a.copyWithZone(nil)
  expectEqual(unsafeBitCast(a, UWord.self), unsafeBitCast(copy, UWord.self))
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/FastEnumeration/UseFromSwift/Empty") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 0)

  checkArrayFastEnumerationFromSwift(
    [], a, { a },
    { ($0 as TestObjCValueTy).value })
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/FastEnumeration/UseFromSwift/3") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 3)

  checkArrayFastEnumerationFromSwift(
    [ 10, 20, 30 ],
    a, { a },
    { ($0 as TestObjCValueTy).value })
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/FastEnumeration/UseFromSwift/7") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 7)

  checkArrayFastEnumerationFromSwift(
    [ 10, 20, 30, 40, 50, 60, 70 ],
    a, { a },
    { ($0 as TestObjCValueTy).value })
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/FastEnumeration/UseFromObjC/Empty") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 0)

  checkArrayFastEnumerationFromObjC(
    [], a, { a },
    { ($0 as TestObjCValueTy).value })
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/FastEnumeration/UseFromObjC") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 3)

  checkArrayFastEnumerationFromObjC(
    [ 10, 20, 30 ],
    a, { a },
    { ($0 as TestObjCValueTy).value })
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/ObjectEnumerator/FastEnumeration/UseFromSwift/Empty") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 0)

  checkArrayFastEnumerationFromSwift(
    [], a, { a.objectEnumerator() },
    { ($0 as TestObjCValueTy).value })
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/ObjectEnumerator/FastEnumeration/UseFromSwift") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 3)

  checkArrayFastEnumerationFromSwift(
    [ 10, 20, 30 ],
    a, { a.objectEnumerator() },
    { ($0 as TestObjCValueTy).value })
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/ObjectEnumerator/FastEnumeration/UseFromSwift/Partial") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 9)

  checkArrayEnumeratorPartialFastEnumerationFromSwift(
    [ 10, 20, 30, 40, 50, 60, 70, 80, 90 ],
    a, maxFastEnumerationItems: 5,
    { ($0 as TestObjCValueTy).value })
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/ObjectEnumerator/FastEnumeration/UseFromObjC") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 3)

  checkArrayFastEnumerationFromObjC(
    [ 10, 20, 30 ],
    a, { a.objectEnumerator() },
    { ($0 as TestObjCValueTy).value })
}

//===----------------------------------------------------------------------===//
// Array -> NSArray bridging tests
//
// Key and Value are bridged non-verbatim.
//
// FIXME: incomplete.
//===----------------------------------------------------------------------===//

func getBridgedNSArrayOfValueTypeCustomBridged(
  numElements: Int = 3,
  capacity: Int? = nil
) -> NSArray {
  assert(!_isBridgedVerbatimToObjectiveC(TestBridgedValueTy.self))

  var a = [TestBridgedValueTy]()
  if let requestedCapacity = capacity {
    a.reserveCapacity(requestedCapacity)
  }
  for i in 1..<(numElements + 1) {
    a.append(TestBridgedValueTy(i * 10))
  }

  let bridged = _convertArrayToNSArray(a)
  assert(isNativeNSArray(bridged))

  return bridged
}

ArrayTestSuite.test("BridgedToObjC/Custom/count/empty") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 0)
  expectEqual(0, a.count)
}

ArrayTestSuite.test("BridgedToObjC/Custom/count") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged()
  expectEqual(3, a.count)
}

for index in [ -100, -1, 0, 1, 100 ] {
  ArrayTestSuite.test(
    "BridgedToObjC/Custom/objectAtIndex/empty/trap/\(index)") {
    let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 0)
    expectCrashLater()
    a.objectAtIndex(index)
  }
}

for index in [ -100, -1, 3, 4, 100 ] {
  ArrayTestSuite.test("BridgedToObjC/Custom/objectAtIndex/trap/\(index)") {
    let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)
    expectCrashLater()
    a.objectAtIndex(index)
  }
}

ArrayTestSuite.test("BridgedToObjC/Custom/objectAtIndex") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)

  var v: AnyObject = a.objectAtIndex(0)
  expectEqual(10, (v as TestObjCValueTy).value)
  let idValue0 = unsafeBitCast(v, UWord.self)

  v = a.objectAtIndex(1)
  expectEqual(20, (v as TestObjCValueTy).value)
  let idValue1 = unsafeBitCast(v, UWord.self)

  v = a.objectAtIndex(2)
  expectEqual(30, (v as TestObjCValueTy).value)
  let idValue2 = unsafeBitCast(v, UWord.self)

  /*
  FIXME: delayed bridging for Swift.Array
  for i in 0..<3 {
    expectEqual(idValue0, unsafeBitCast(a.objectAtIndex(0), UWord.self))
    expectEqual(idValue1, unsafeBitCast(a.objectAtIndex(1), UWord.self))
    expectEqual(idValue2, unsafeBitCast(a.objectAtIndex(2), UWord.self))
  }
  */
}

for indexRange in [ 0..<4, -2..<(-1), -1..<2, 2..<4, 4..<5 ] {
  ArrayTestSuite.test("BridgedToObjC/Custom/getObjects/trap/\(indexRange)")
    .crashOutputMatches("Array index out of range")
    .code {
    let a = getBridgedNSArrayOfValueTypeCustomBridged(
      numElements: 3, capacity: 16)
    let buffer = UnsafeMutablePointer<AnyObject>.alloc(16)
    a.getObjects(
      AutoreleasingUnsafeMutablePointer(buffer), range: NSRange(0..<3))
    expectCrashLater()
    a.getObjects(
      AutoreleasingUnsafeMutablePointer(buffer), range: NSRange(indexRange))
  }
}

ArrayTestSuite.test("BridgedToObjC/Custom/getObjects") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)
  let buffer = UnsafeMutablePointer<AnyObject>.alloc(16)
  a.getObjects(
    AutoreleasingUnsafeMutablePointer(buffer), range: NSRange(0..<3))

  var v: AnyObject = buffer[0]
  expectEqual(10, (v as TestObjCValueTy).value)
  let idValue0 = unsafeBitCast(v, UWord.self)

  v = buffer[1]
  expectEqual(20, (v as TestObjCValueTy).value)
  let idValue1 = unsafeBitCast(v, UWord.self)

  v = buffer[2]
  expectEqual(30, (v as TestObjCValueTy).value)
  let idValue2 = unsafeBitCast(v, UWord.self)

  /*
  FIXME: delayed bridging for Swift.Array
  for i in 0..<3 {
    expectEqual(idValue0, unsafeBitCast(a.objectAtIndex(0), UWord.self))
    expectEqual(idValue1, unsafeBitCast(a.objectAtIndex(1), UWord.self))
    expectEqual(idValue2, unsafeBitCast(a.objectAtIndex(2), UWord.self))
  }
  */

  buffer.dealloc(3)
  _fixLifetime(a)

  expectAutoreleasedValues(opt: 3, unopt: 3)
}

ArrayTestSuite.test("BridgedToObjC/Custom/copyWithZone") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)
  let copy: AnyObject = a.copyWithZone(nil)
  expectEqual(unsafeBitCast(a, UWord.self), unsafeBitCast(copy, UWord.self))
}

ArrayTestSuite.test("BridgedToObjC/Custom/FastEnumeration/UseFromSwift/Empty") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 0)

  checkArrayFastEnumerationFromSwift(
    [], a, { a },
    { ($0 as TestObjCValueTy).value })
}

ArrayTestSuite.test("BridgedToObjC/Custom/FastEnumeration/UseFromSwift/3") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)

  checkArrayFastEnumerationFromSwift(
    [ 10, 20, 30 ],
    a, { a },
    { ($0 as TestObjCValueTy).value })

  expectAutoreleasedValues(opt: 9, unopt: 9)
}

ArrayTestSuite.test("BridgedToObjC/Custom/FastEnumeration/UseFromSwift/7") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 7)

  checkArrayFastEnumerationFromSwift(
    [ 10, 20, 30, 40, 50, 60, 70 ],
    a, { a },
    { ($0 as TestObjCValueTy).value })

  expectAutoreleasedValues(opt: 21, unopt: 21)
}

ArrayTestSuite.test("BridgedToObjC/Custom/FastEnumeration/UseFromObjC/Empty") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 0)

  checkArrayFastEnumerationFromObjC(
    [], a, { a },
    { ($0 as TestObjCValueTy).value })
}

ArrayTestSuite.test("BridgedToObjC/Custom/FastEnumeration/UseFromObjC") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)

  checkArrayFastEnumerationFromObjC(
    [ 10, 20, 30 ],
    a, { a },
    { ($0 as TestObjCValueTy).value })

  expectAutoreleasedValues(opt: 9, unopt: 9)
}

ArrayTestSuite.test("BridgedToObjC/Custom/ObjectEnumerator/FastEnumeration/UseFromSwift/Empty") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 0)

  checkArrayFastEnumerationFromSwift(
    [], a, { a.objectEnumerator() },
    { ($0 as TestObjCValueTy).value })
}

ArrayTestSuite.test("BridgedToObjC/Custom/ObjectEnumerator/FastEnumeration/UseFromSwift") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)

  checkArrayFastEnumerationFromSwift(
    [ 10, 20, 30 ],
    a, { a.objectEnumerator() },
    { ($0 as TestObjCValueTy).value })

  expectAutoreleasedValues(opt: 9, unopt: 9)
}

ArrayTestSuite.test("BridgedToObjC/Custom/ObjectEnumerator/FastEnumeration/UseFromSwift/Partial") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 9)

  checkArrayEnumeratorPartialFastEnumerationFromSwift(
    [ 10, 20, 30, 40, 50, 60, 70, 80, 90 ],
    a, maxFastEnumerationItems: 5,
    { ($0 as TestObjCValueTy).value })

  expectAutoreleasedValues(opt: 27, unopt: 27)
}

ArrayTestSuite.test("BridgedToObjC/Custom/ObjectEnumerator/FastEnumeration/UseFromObjC") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)

  checkArrayFastEnumerationFromObjC(
    [ 10, 20, 30 ],
    a, { a.objectEnumerator() },
    { ($0 as TestObjCValueTy).value })

  expectAutoreleasedValues(opt: 9, unopt: 9)
}

ArrayTestSuite.setUp {
  resetLeaksOfDictionaryKeysValues()
}

ArrayTestSuite.tearDown {
  expectNoLeaksOfDictionaryKeysValues()
}

runAllTests()


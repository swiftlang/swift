// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// FIXME: -fobjc-abi-version=2 is a band-aid fix for for rdar://16946936
//
// RUN: ln -s  %s %t/main.swift
// RUN: xcrun -sdk %target-sdk-name clang++ -fobjc-arc -fobjc-abi-version=2 -arch %target-cpu %S/Inputs/SlurpFastEnumeration/SlurpFastEnumeration.m -c -o %t/SlurpFastEnumeration.o
// RUN: %target-build-swift %S/Inputs/DictionaryKeyValueTypes.swift %t/main.swift -I %S/Inputs/SlurpFastEnumeration/ -Xlinker %t/SlurpFastEnumeration.o -o %t/Array -Xfrontend -disable-access-control
// RUN: %target-run %t/Array


import Darwin
import StdlibUnittest
import Foundation

var ArrayTestSuite = StdlibUnittest.TestSuite("Array")

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

ArrayTestSuite.test("BridgedToObjC/Verbatim/count/empty") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 0)
  expectEqual(0, a.count)
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/count") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged()
  expectEqual(3, a.count)
}

for index in [ -100, -1, 0, 1, 100 ] {
  ArrayTestSuite.test(
    "BridgedToObjC/Verbatim/objectAtIndex/empty/trap/\(index)")
    .crashOutputMatches("Array index out of range")
    .code {
    let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 0)
    expectCrashLater()
    a.objectAtIndex(index)
  }
}

for index in [ -100, -1, 3, 4, 100 ] {
  ArrayTestSuite.test("BridgedToObjC/Verbatim/objectAtIndex/trap/\(index)")
    .crashOutputMatches("Array index out of range")
    .code {
    let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 3)
    expectCrashLater()
    a.objectAtIndex(index)
  }
}

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

  for i in 0..<3 {
    expectEqual(idValue0, unsafeBitCast(a.objectAtIndex(0), UWord.self))
    expectEqual(idValue1, unsafeBitCast(a.objectAtIndex(1), UWord.self))
    expectEqual(idValue2, unsafeBitCast(a.objectAtIndex(2), UWord.self))
  }

  expectAutoreleasedKeysAndValues(unopt: (0, 3))
}

for indexRange in [
  -2..<(-2), 1..<1,
  0..<4, -2..<(-1), -1..<2, 0..<1, 2..<4, 4..<5
] as [Range<Int>] {
  ArrayTestSuite.test("BridgedToObjC/Verbatim/getObjects/empty/trap/\(indexRange)")
    .crashOutputMatches("Array index out of range")
    .code {
    let a = getBridgedNSArrayOfRefTypeVerbatimBridged(
      numElements: 0, capacity: 16)
    let buffer = UnsafeMutablePointer<AnyObject>.alloc(16)
    a.getObjects(
      AutoreleasingUnsafeMutablePointer(buffer), range: NSRange(0..<0))
    expectCrashLater()
    a.getObjects(
      AutoreleasingUnsafeMutablePointer(buffer), range: NSRange(indexRange))
  }
}

for indexRange in [ 0..<4, -2..<(-1), -1..<2, 2..<4, 4..<5 ] as [Range<Int>] {
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

  expectAutoreleasedKeysAndValues(unopt: (0, 3))
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

ArrayTestSuite.test("BridgedToObjC/Verbatim/BridgeBack/Reallocate") {
  let a = getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 3)

  var v: AnyObject = a[0]
  expectEqual(10, (v as TestObjCValueTy).value)
  let idValue0 = unsafeBitCast(v, UWord.self)

  v = a[1]
  expectEqual(20, (v as TestObjCValueTy).value)
  let idValue1 = unsafeBitCast(v, UWord.self)

  v = a[2]
  expectEqual(30, (v as TestObjCValueTy).value)
  let idValue2 = unsafeBitCast(v, UWord.self)

  // Bridge back to native array.
  var native: [TestObjCValueTy] = _convertNSArrayToArray(a)
  native[0] = TestObjCValueTy(110)
  native[1] = TestObjCValueTy(120)
  native[2] = TestObjCValueTy(130)
  native.append(TestObjCValueTy(140))

  // Ensure that the compiler does not elide mutation of the native array.
  _blackHole(native)

  // Check that mutating the native array did not affect the bridged array.
  expectEqual(3, a.count)
  expectEqual(idValue0, unsafeBitCast(a.objectAtIndex(0), UWord.self))
  expectEqual(idValue1, unsafeBitCast(a.objectAtIndex(1), UWord.self))
  expectEqual(idValue2, unsafeBitCast(a.objectAtIndex(2), UWord.self))

  expectAutoreleasedKeysAndValues(unopt: (0, 3))
}

ArrayTestSuite.test("BridgedToObjC/Verbatim/BridgeBack/Adopt") {
  // Bridge back to native array.
  var native: [TestObjCValueTy] = _convertNSArrayToArray(
    getBridgedNSArrayOfRefTypeVerbatimBridged(numElements: 3))
  let identity1 = unsafeBitCast(native, UWord.self)

  // Mutate elements, but don't change length.
  native[0] = TestObjCValueTy(110)
  native[1] = TestObjCValueTy(120)
  native[2] = TestObjCValueTy(130)

  // Expect no reallocations.
  expectEqual(identity1, unsafeBitCast(native, UWord.self))
}

//===----------------------------------------------------------------------===//
// Array -> NSArray bridging tests
//
// Key and Value are bridged non-verbatim.
//
// FIXME: incomplete.
//===----------------------------------------------------------------------===//

ArrayTestSuite.test("BridgedToObjC/Custom/count/empty") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 0)
  expectEqual(0, a.count)

  expectEqual(0, TestBridgedValueTy.bridgeOperations)
}

ArrayTestSuite.test("BridgedToObjC/Custom/count") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged()
  expectEqual(3, a.count)

  expectEqual(0, TestBridgedValueTy.bridgeOperations)
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

  for i in 0..<3 {
    expectEqual(idValue0, unsafeBitCast(a.objectAtIndex(0), UWord.self))
    expectEqual(idValue1, unsafeBitCast(a.objectAtIndex(1), UWord.self))
    expectEqual(idValue2, unsafeBitCast(a.objectAtIndex(2), UWord.self))
  }

  expectEqual(3, TestBridgedValueTy.bridgeOperations)
  expectAutoreleasedKeysAndValues(unopt: (0, 3))
}

for indexRange in [
  -2..<(-2), 1..<1,
  0..<4, -2..<(-1), -1..<2, 0..<1, 2..<4, 4..<5
] as [Range<Int>] {
  ArrayTestSuite.test("BridgedToObjC/Custom/getObjects/empty/trap/\(indexRange)")
    .crashOutputMatches("Array index out of range")
    .code {
    let a = getBridgedNSArrayOfValueTypeCustomBridged(
      numElements: 0, capacity: 16)
    let buffer = UnsafeMutablePointer<AnyObject>.alloc(16)
    a.getObjects(
      AutoreleasingUnsafeMutablePointer(buffer), range: NSRange(0..<0))
    expectCrashLater()
    a.getObjects(
      AutoreleasingUnsafeMutablePointer(buffer), range: NSRange(indexRange))
  }
}

for indexRange in [ 0..<4, -2..<(-1), -1..<2, 2..<4, 4..<5 ] as [Range<Int>] {
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

  for i in 0..<3 {
    expectEqual(idValue0, unsafeBitCast(a.objectAtIndex(0), UWord.self))
    expectEqual(idValue1, unsafeBitCast(a.objectAtIndex(1), UWord.self))
    expectEqual(idValue2, unsafeBitCast(a.objectAtIndex(2), UWord.self))
  }

  buffer.dealloc(3)
  _fixLifetime(a)

  expectEqual(3, TestBridgedValueTy.bridgeOperations)
  expectAutoreleasedKeysAndValues(unopt: (0, 3))
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

  expectEqual(0, TestBridgedValueTy.bridgeOperations)
}

ArrayTestSuite.test("BridgedToObjC/Custom/FastEnumeration/UseFromSwift/3") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)

  checkArrayFastEnumerationFromSwift(
    [ 10, 20, 30 ],
    a, { a },
    { ($0 as TestObjCValueTy).value })

  expectEqual(3, TestBridgedValueTy.bridgeOperations)
}

ArrayTestSuite.test("BridgedToObjC/Custom/FastEnumeration/UseFromSwift/7") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 7)

  checkArrayFastEnumerationFromSwift(
    [ 10, 20, 30, 40, 50, 60, 70 ],
    a, { a },
    { ($0 as TestObjCValueTy).value })

  expectEqual(7, TestBridgedValueTy.bridgeOperations)
}

ArrayTestSuite.test("BridgedToObjC/Custom/FastEnumeration/UseFromObjC/Empty") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 0)

  checkArrayFastEnumerationFromObjC(
    [], a, { a },
    { ($0 as TestObjCValueTy).value })

  expectEqual(0, TestBridgedValueTy.bridgeOperations)
}

ArrayTestSuite.test("BridgedToObjC/Custom/FastEnumeration/UseFromObjC") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)

  checkArrayFastEnumerationFromObjC(
    [ 10, 20, 30 ],
    a, { a },
    { ($0 as TestObjCValueTy).value })

  expectEqual(3, TestBridgedValueTy.bridgeOperations)
}

ArrayTestSuite.test("BridgedToObjC/Custom/ObjectEnumerator/FastEnumeration/UseFromSwift/Empty") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 0)

  checkArrayFastEnumerationFromSwift(
    [], a, { a.objectEnumerator() },
    { ($0 as TestObjCValueTy).value })

  expectEqual(0, TestBridgedValueTy.bridgeOperations)
}

ArrayTestSuite.test("BridgedToObjC/Custom/ObjectEnumerator/FastEnumeration/UseFromSwift") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)

  checkArrayFastEnumerationFromSwift(
    [ 10, 20, 30 ],
    a, { a.objectEnumerator() },
    { ($0 as TestObjCValueTy).value })

  expectEqual(3, TestBridgedValueTy.bridgeOperations)
}

ArrayTestSuite.test("BridgedToObjC/Custom/ObjectEnumerator/FastEnumeration/UseFromSwift/Partial") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 9)

  checkArrayEnumeratorPartialFastEnumerationFromSwift(
    [ 10, 20, 30, 40, 50, 60, 70, 80, 90 ],
    a, maxFastEnumerationItems: 5,
    { ($0 as TestObjCValueTy).value })

  expectEqual(9, TestBridgedValueTy.bridgeOperations)
}

ArrayTestSuite.test("BridgedToObjC/Custom/ObjectEnumerator/FastEnumeration/UseFromObjC") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)

  checkArrayFastEnumerationFromObjC(
    [ 10, 20, 30 ],
    a, { a.objectEnumerator() },
    { ($0 as TestObjCValueTy).value })

  expectEqual(3, TestBridgedValueTy.bridgeOperations)
}

ArrayTestSuite.test("BridgedToObjC/Custom/BridgeBack/Cast") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)

  var v: AnyObject = a[0]
  expectEqual(10, (v as TestObjCValueTy).value)
  let idValue0 = unsafeBitCast(v, UWord.self)

  v = a[1]
  expectEqual(20, (v as TestObjCValueTy).value)
  let idValue1 = unsafeBitCast(v, UWord.self)

  v = a[2]
  expectEqual(30, (v as TestObjCValueTy).value)
  let idValue2 = unsafeBitCast(v, UWord.self)

  // Bridge back to native array with a cast.
  var native: [TestObjCValueTy] = _convertNSArrayToArray(a)
  native[0] = TestObjCValueTy(110)
  native[1] = TestObjCValueTy(120)
  native[2] = TestObjCValueTy(130)
  native.append(TestObjCValueTy(140))

  // Ensure that the compiler does not elide mutation of the native array.
  _blackHole(native)

  // Check that mutating the native array did not affect the bridged array.
  expectEqual(3, a.count)
  expectEqual(idValue0, unsafeBitCast(a.objectAtIndex(0), UWord.self))
  expectEqual(idValue1, unsafeBitCast(a.objectAtIndex(1), UWord.self))
  expectEqual(idValue2, unsafeBitCast(a.objectAtIndex(2), UWord.self))

  expectAutoreleasedKeysAndValues(unopt: (0, 3))
}

ArrayTestSuite.test("BridgedToObjC/Custom/BridgeBack/Reallocate") {
  let a = getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3)

  var v: AnyObject = a[0]
  expectEqual(10, (v as TestObjCValueTy).value)
  let idValue0 = unsafeBitCast(v, UWord.self)

  v = a[1]
  expectEqual(20, (v as TestObjCValueTy).value)
  let idValue1 = unsafeBitCast(v, UWord.self)

  v = a[2]
  expectEqual(30, (v as TestObjCValueTy).value)
  let idValue2 = unsafeBitCast(v, UWord.self)

  // Bridge back to native array.
  var native: [TestBridgedValueTy] = _convertNSArrayToArray(a)
  native[0] = TestBridgedValueTy(110)
  native[1] = TestBridgedValueTy(120)
  native[2] = TestBridgedValueTy(130)
  native.append(TestBridgedValueTy(140))

  // Ensure that the compiler does not elide mutation of the native array.
  _blackHole(native)

  // Check that mutating the native array did not affect the bridged array.
  expectEqual(3, a.count)
  expectEqual(idValue0, unsafeBitCast(a.objectAtIndex(0), UWord.self))
  expectEqual(idValue1, unsafeBitCast(a.objectAtIndex(1), UWord.self))
  expectEqual(idValue2, unsafeBitCast(a.objectAtIndex(2), UWord.self))

  expectAutoreleasedKeysAndValues(unopt: (0, 3))
}

ArrayTestSuite.test("BridgedToObjC/Custom/BridgeBack/Adopt") {
  // Bridge back to native array.
  var native: [TestBridgedValueTy] = _convertNSArrayToArray(
    getBridgedNSArrayOfValueTypeCustomBridged(numElements: 3))
  let identity1 = unsafeBitCast(native, UWord.self)

  // Mutate elements, but don't change length.
  native[0] = TestBridgedValueTy(110)
  native[1] = TestBridgedValueTy(120)
  native[2] = TestBridgedValueTy(130)

  // Expect no reallocations.
  expectEqual(identity1, unsafeBitCast(native, UWord.self))
}

let evilBoundsError = "EvilCollection: index out of range"

final class EvilCollection : CollectionType {
  init(_ growth: Int, boundsChecked: Bool) {
    self.growth = growth
    self.boundsChecked = boundsChecked
  }
  
  var growth: Int
  var count: Int = 20
  var boundsChecked: Bool
  
  var startIndex : Int {
    count += growth
    return 0
  }
  
  var endIndex : Int {
    return count
  }
  
  subscript(i: Int) -> TestValueTy {
    if boundsChecked {
      precondition(i >= 0 && i < count, evilBoundsError)
    }
    return TestValueTy(i)
  }

  func generate() -> IndexingGenerator<EvilCollection> {
    return IndexingGenerator(self)
  }
}

for (step, evilBoundsCheck) in [ (1, true), (-1, false), (-1, true) ] {
  
  let message = step < 0 && evilBoundsCheck
    ? evilBoundsError
    : "invalid CollectionType: count differed in successive traversals"

  let natureOfEvil = step > 0 ? "Growth" : "Shrinkage"
  let boundsChecked = evilBoundsCheck ? "BoundsChecked" : "NoBoundsCheck"
  let testPrefix = "MemorySafety/\(boundsChecked)/Evil\(natureOfEvil)"
  
  let t = ArrayTestSuite.test("\(testPrefix)/Infrastructure")
  (evilBoundsCheck ? t.crashOutputMatches(evilBoundsError) : t).code {
    let evil = EvilCollection(step, boundsChecked: evilBoundsCheck)
    let count0 = countElements(evil)
    let count1 = countElements(evil)
    expectNotEqual(count0, count1)
    if step > 0 {
      expectLE(count0, count1)
    }
    else {
      expectGE(count0, count1)
    }
    if evilBoundsCheck {
      expectCrashLater()
    }
    let x = evil[-1]
  }
  
  
  ArrayTestSuite.test("\(testPrefix)/Construction")
  .crashOutputMatches(message)
  .code {
    let evil = EvilCollection(step, boundsChecked: evilBoundsCheck)
    expectCrashLater()
    let a = Array(evil)
  }

  for (op, rangeMax) in ["Grow":0, "Shrink":200] {
    ArrayTestSuite.test("\(testPrefix)/replaceRange/\(op)Unique")
    .crashOutputMatches(message)
    .code {
      let evil = EvilCollection(step, boundsChecked: evilBoundsCheck)
      var a = Array(lazy(0..<200).map { TestValueTy($0) })
      expectCrashLater()
      a.replaceRange(0..<rangeMax, with: evil)
    }

    ArrayTestSuite.test("\(testPrefix)/replaceRange/\(op)NonUnique")
    .crashOutputMatches(message)
    .code {
      let evil = EvilCollection(step, boundsChecked: evilBoundsCheck)
      var a = Array(lazy(0..<200).map { TestValueTy($0) })
      var b = a
      expectCrashLater()
      a.replaceRange(0..<rangeMax, with: evil)
    }
  }
}

ArrayTestSuite.setUp {
  resetLeaksOfDictionaryKeysValues()
  TestBridgedValueTy.bridgeOperations = 0
}

ArrayTestSuite.tearDown {
  expectNoLeaksOfDictionaryKeysValues()
}

runAllTests()


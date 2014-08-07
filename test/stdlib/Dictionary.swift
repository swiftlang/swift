// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// FIXME: -fobjc-abi-version=2 is a band-aid fix for for rdar://16946936
//
// RUN: xcrun -sdk %target-sdk-name clang++ -fobjc-abi-version=2 -arch %target-cpu %S/Inputs/SlurpFastEnumeration/SlurpFastEnumeration.m -c -o %t/SlurpFastEnumeration.o
// RUN: %target-build-swift %s -I %S/Inputs/SlurpFastEnumeration/ -Xlinker %t/SlurpFastEnumeration.o -o %t/Dictionary -Xfrontend -disable-access-control
//
// RUN: %target-run %t/Dictionary

import Darwin
import StdlibUnittest
import Foundation

var DictionaryTestCase = TestCase("Dictionary")

//===---
// Utilities.
//===---

var keyCount = 0
var keySerial = 0

// A wrapper class that can help us track allocations and find issues with
// object lifetime.
class TestKeyTy : Equatable, Hashable, Printable {
  init(_ value: Int) {
    ++keyCount
    serial = ++keySerial
    self.value = value
    self._hashValue = value
  }

  convenience init(value: Int, hashValue: Int) {
    self.init(value)
    self._hashValue = hashValue
  }

  deinit {
    assert(serial > 0, "double destruction")
    --keyCount
    serial = -serial
  }

  var description: String {
    assert(serial > 0, "dead TestKeyTy")
    return value.description
  }

  var hashValue: Int {
    return _hashValue
  }

  var value: Int
  var _hashValue: Int
  var serial: Int
}

func == (lhs: TestKeyTy, rhs: TestKeyTy) -> Bool {
  return lhs.value == rhs.value
}

var valueCount = 0
var valueSerial = 0

class TestValueTy : Printable {
  init(_ value: Int) {
    ++valueCount
    serial = ++valueSerial
    self.value = value
  }

  deinit {
    assert(serial > 0, "double destruction")
    --valueCount
    serial = -serial
  }

  var description: String {
    assert(serial > 0, "dead TestValueTy")
    return value.description
  }

  var value: Int
  var serial: Int
}

class TestEquatableValueTy : Equatable, Printable {
  init(_ value: Int) {
    ++valueCount
    serial = ++valueSerial
    self.value = value
  }

  deinit {
    assert(serial > 0, "double destruction")
    --valueCount
    serial = -serial
  }

  var description: String {
    assert(serial > 0, "dead TestEquatableValueTy")
    return value.description
  }

  var value: Int
  var serial: Int
}

func == (lhs: TestEquatableValueTy, rhs: TestEquatableValueTy) -> Bool {
  return lhs.value == rhs.value
}

func acceptsAnyDictionary<KeyTy : Hashable, ValueTy>(
    d: Dictionary<KeyTy, ValueTy>) {
}

func isNativeDictionary<KeyTy : Hashable, ValueTy>(
    d: Dictionary<KeyTy, ValueTy>) -> Bool {
  switch d._variantStorage {
  case .Native:
    return true
  case .Cocoa:
    return false
  }
}

func isCocoaDictionary<KeyTy : Hashable, ValueTy>(
    d: Dictionary<KeyTy, ValueTy>) -> Bool {
  return !isNativeDictionary(d)
}

// Compare two arrays as sets.
func equalsUnordered(lhs: Array<(Int, Int)>, rhs: Array<(Int, Int)>) -> Bool {
  func comparePair(lhs: (Int, Int), rhs: (Int, Int)) -> Bool {
    return lexicographicalCompare([ lhs.0, lhs.1 ], [ rhs.0, rhs.1 ])
  }
  return equal(sorted(lhs, comparePair), sorted(rhs, comparePair)) {
    $0.0 == $1.0 && $0.1 == $1.1
  }
}

func equalsUnordered(lhs: Array<Int>, rhs: Array<Int>) -> Bool {
  return equal(sorted(lhs), sorted(rhs))
}

//===---
// Tests.
//===---

DictionaryTestCase.test("sizeof") {
  var dict = [ 1: "meow", 2: "meow" ]
#if arch(i386) || arch(arm)
  expectEqual(4, sizeofValue(dict))
#else
  expectEqual(8, sizeofValue(dict))
#endif
}

DictionaryTestCase.test("valueDestruction") {
  var d1 = Dictionary<Int, TestValueTy>()
  for i in 100...110 {
    d1[i] = TestValueTy(i)
  }

  var d2 = Dictionary<TestKeyTy, TestValueTy>()
  for i in 100...110 {
    d2[TestKeyTy(i)] = TestValueTy(i)
  }
}

DictionaryTestCase.test("COW.Smoke") {
  var d1 = Dictionary<TestKeyTy, TestValueTy>(minimumCapacity: 10)
  var identity1 = unsafeBitCast(d1, Word.self)

  d1[TestKeyTy(10)] = TestValueTy(1010)
  d1[TestKeyTy(20)] = TestValueTy(1020)
  d1[TestKeyTy(30)] = TestValueTy(1030)

  var d2 = d1
  acceptsAnyDictionary(d2)
  assert(identity1 == unsafeBitCast(d2, Word.self))

  d2[TestKeyTy(40)] = TestValueTy(2040)
  assert(identity1 != unsafeBitCast(d2, Word.self))

  d1[TestKeyTy(50)] = TestValueTy(1050)
  assert(identity1 == unsafeBitCast(d1, Word.self))

  // Keep variables alive.
  acceptsAnyDictionary(d1)
  acceptsAnyDictionary(d2)
}

func getCOWFastDictionary() -> Dictionary<Int, Int> {
  var d = Dictionary<Int, Int>(minimumCapacity: 10)
  d[10] = 1010
  d[20] = 1020
  d[30] = 1030
  return d
}

func getCOWSlowDictionary() -> Dictionary<TestKeyTy, TestValueTy> {
  var d = Dictionary<TestKeyTy, TestValueTy>(minimumCapacity: 10)
  d[TestKeyTy(10)] = TestValueTy(1010)
  d[TestKeyTy(20)] = TestValueTy(1020)
  d[TestKeyTy(30)] = TestValueTy(1030)
  return d
}

func getCOWSlowEquatableDictionary()
    -> Dictionary<TestKeyTy, TestEquatableValueTy> {
  var d = Dictionary<TestKeyTy, TestEquatableValueTy>(minimumCapacity: 10)
  d[TestKeyTy(10)] = TestEquatableValueTy(1010)
  d[TestKeyTy(20)] = TestEquatableValueTy(1020)
  d[TestKeyTy(30)] = TestEquatableValueTy(1030)
  return d
}


DictionaryTestCase.test("COW.Fast.IndexesDontAffectUniquenessCheck") {
  var d = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d, Word.self)

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex != endIndex)
  assert(startIndex < endIndex)
  assert(startIndex <= endIndex)
  assert(!(startIndex >= endIndex))
  assert(!(startIndex > endIndex))

  assert(identity1 == unsafeBitCast(d, Word.self))

  d[40] = 2040
  assert(identity1 == unsafeBitCast(d, Word.self))

  // Keep indexes alive during the calls above.
  withExtendedLifetime(startIndex) { () }
  withExtendedLifetime(endIndex) { () }
}

DictionaryTestCase.test("COW.Slow.IndexesDontAffectUniquenessCheck") {
  var d = getCOWSlowDictionary()
  var identity1 = unsafeBitCast(d, Word.self)

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex != endIndex)
  assert(startIndex < endIndex)
  assert(startIndex <= endIndex)
  assert(!(startIndex >= endIndex))
  assert(!(startIndex > endIndex))
  assert(identity1 == unsafeBitCast(d, Word.self))

  d[TestKeyTy(40)] = TestValueTy(2040)
  assert(identity1 == unsafeBitCast(d, Word.self))

  // Keep indexes alive during the calls above.
  withExtendedLifetime(startIndex) { () }
  withExtendedLifetime(endIndex) { () }
}


DictionaryTestCase.test("COW.Fast.SubscriptWithIndexDoesNotReallocate") {
  var d = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d, Word.self)

  var startIndex = d.startIndex
  let empty = startIndex == d.endIndex
  assert((d.startIndex < d.endIndex) == !empty)
  assert(d.startIndex <= d.endIndex)
  assert((d.startIndex >= d.endIndex) == empty)
  assert(!(d.startIndex > d.endIndex))
  assert(identity1 == unsafeBitCast(d, Word.self))

  assert(d[startIndex].1 != 0)
  assert(identity1 == unsafeBitCast(d, Word.self))
}

DictionaryTestCase.test("COW.Slow.SubscriptWithIndexDoesNotReallocate") {
  var d = getCOWSlowDictionary()
  var identity1 = unsafeBitCast(d, Word.self)

  var startIndex = d.startIndex
  let empty = startIndex == d.endIndex
  assert((d.startIndex < d.endIndex) == !empty)
  assert(d.startIndex <= d.endIndex)
  assert((d.startIndex >= d.endIndex) == empty)
  assert(!(d.startIndex > d.endIndex))
  assert(identity1 == unsafeBitCast(d, Word.self))

  assert(d[startIndex].1.value != 0)
  assert(identity1 == unsafeBitCast(d, Word.self))
}


DictionaryTestCase.test("COW.Fast.SubscriptWithKeyDoesNotReallocate") {
  var d = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d, Word.self)

  assert(d[10]! == 1010)
  assert(identity1 == unsafeBitCast(d, Word.self))

  // Insert a new key-value pair.
  d[40] = 2040
  assert(identity1 == unsafeBitCast(d, Word.self))
  assert(d.count == 4)
  assert(d[10]! == 1010)
  assert(d[20]! == 1020)
  assert(d[30]! == 1030)
  assert(d[40]! == 2040)

  // Overwrite a value in existing binding.
  d[10] = 2010
  assert(identity1 == unsafeBitCast(d, Word.self))
  assert(d.count == 4)
  assert(d[10]! == 2010)
  assert(d[20]! == 1020)
  assert(d[30]! == 1030)
  assert(d[40]! == 2040)

  // Delete an existing key.
  d[10] = nil
  assert(identity1 == unsafeBitCast(d, Word.self))
  assert(d.count == 3)
  assert(d[20]! == 1020)
  assert(d[30]! == 1030)
  assert(d[40]! == 2040)

  // Try to delete a key that does not exist.
  d[42] = nil
  assert(identity1 == unsafeBitCast(d, Word.self))
  assert(d.count == 3)
  assert(d[20]! == 1020)
  assert(d[30]! == 1030)
  assert(d[40]! == 2040)
}

DictionaryTestCase.test("COW.Slow.SubscriptWithKeyDoesNotReallocate") {
  var d = getCOWSlowDictionary()
  var identity1 = unsafeBitCast(d, Word.self)

  assert(d[TestKeyTy(10)]!.value == 1010)
  assert(identity1 == unsafeBitCast(d, Word.self))

  // Insert a new key-value pair.
  d[TestKeyTy(40)] = TestValueTy(2040)
  assert(identity1 == unsafeBitCast(d, Word.self))
  assert(d.count == 4)
  assert(d[TestKeyTy(10)]!.value == 1010)
  assert(d[TestKeyTy(20)]!.value == 1020)
  assert(d[TestKeyTy(30)]!.value == 1030)
  assert(d[TestKeyTy(40)]!.value == 2040)

  // Overwrite a value in existing binding.
  d[TestKeyTy(10)] = TestValueTy(2010)
  assert(identity1 == unsafeBitCast(d, Word.self))
  assert(d.count == 4)
  assert(d[TestKeyTy(10)]!.value == 2010)
  assert(d[TestKeyTy(20)]!.value == 1020)
  assert(d[TestKeyTy(30)]!.value == 1030)
  assert(d[TestKeyTy(40)]!.value == 2040)

  // Delete an existing key.
  d[TestKeyTy(10)] = nil
  assert(identity1 == unsafeBitCast(d, Word.self))
  assert(d.count == 3)
  assert(d[TestKeyTy(20)]!.value == 1020)
  assert(d[TestKeyTy(30)]!.value == 1030)
  assert(d[TestKeyTy(40)]!.value == 2040)

  // Try to delete a key that does not exist.
  d[TestKeyTy(42)] = nil
  assert(identity1 == unsafeBitCast(d, Word.self))
  assert(d.count == 3)
  assert(d[TestKeyTy(20)]!.value == 1020)
  assert(d[TestKeyTy(30)]!.value == 1030)
  assert(d[TestKeyTy(40)]!.value == 2040)
}


DictionaryTestCase.test("COW.Fast.UpdateValueForKeyDoesNotReallocate") {
  if true {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    // Insert a new key-value pair.
    assert(d1.updateValue(2040, forKey: 40) == .None)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(d1[40]! == 2040)

    // Overwrite a value in existing binding.
    assert(d1.updateValue(2010, forKey: 10)! == 1010)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(d1[10]! == 2010)
  }

  if true {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    var d2 = d1
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 == unsafeBitCast(d2, Word.self))

    // Insert a new key-value pair.
    d2.updateValue(2040, forKey: 40)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 != unsafeBitCast(d2, Word.self))

    assert(d1.count == 3)
    assert(d1[10]! == 1010)
    assert(d1[20]! == 1020)
    assert(d1[30]! == 1030)
    assert(d1[40] == .None)

    assert(d2.count == 4)
    assert(d2[10]! == 1010)
    assert(d2[20]! == 1020)
    assert(d2[30]! == 1030)
    assert(d2[40]! == 2040)

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }

  if true {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    var d2 = d1
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 == unsafeBitCast(d2, Word.self))

    // Overwrite a value in existing binding.
    d2.updateValue(2010, forKey: 10)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 != unsafeBitCast(d2, Word.self))

    assert(d1.count == 3)
    assert(d1[10]! == 1010)
    assert(d1[20]! == 1020)
    assert(d1[30]! == 1030)

    assert(d2.count == 3)
    assert(d2[10]! == 2010)
    assert(d2[20]! == 1020)
    assert(d2[30]! == 1030)

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }
}

DictionaryTestCase.test("COW.Slow.AddDoesNotReallocate") {
  if true {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    // Insert a new key-value pair.
    assert(d1.updateValue(TestValueTy(2040), forKey: TestKeyTy(40)) == nil)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(d1.count == 4)
    assert(d1[TestKeyTy(40)]!.value == 2040)

    // Overwrite a value in existing binding.
    assert(d1.updateValue(TestValueTy(2010), forKey: TestKeyTy(10))!.value == 1010)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(d1.count == 4)
    assert(d1[TestKeyTy(10)]!.value == 2010)
  }

  if true {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    var d2 = d1
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 == unsafeBitCast(d2, Word.self))

    // Insert a new key-value pair.
    d2.updateValue(TestValueTy(2040), forKey: TestKeyTy(40))
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 != unsafeBitCast(d2, Word.self))

    assert(d1.count == 3)
    assert(d1[TestKeyTy(10)]!.value == 1010)
    assert(d1[TestKeyTy(20)]!.value == 1020)
    assert(d1[TestKeyTy(30)]!.value == 1030)
    assert(d1[TestKeyTy(40)] == nil)

    assert(d2.count == 4)
    assert(d2[TestKeyTy(10)]!.value == 1010)
    assert(d2[TestKeyTy(20)]!.value == 1020)
    assert(d2[TestKeyTy(30)]!.value == 1030)
    assert(d2[TestKeyTy(40)]!.value == 2040)

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }

  if true {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    var d2 = d1
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 == unsafeBitCast(d2, Word.self))

    // Overwrite a value in existing binding.
    d2.updateValue(TestValueTy(2010), forKey: TestKeyTy(10))
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 != unsafeBitCast(d2, Word.self))

    assert(d1.count == 3)
    assert(d1[TestKeyTy(10)]!.value == 1010)
    assert(d1[TestKeyTy(20)]!.value == 1020)
    assert(d1[TestKeyTy(30)]!.value == 1030)

    assert(d2.count == 3)
    assert(d2[TestKeyTy(10)]!.value == 2010)
    assert(d2[TestKeyTy(20)]!.value == 1020)
    assert(d2[TestKeyTy(30)]!.value == 1030)

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }
}


DictionaryTestCase.test("COW.Fast.IndexForKeyDoesNotReallocate") {
  var d = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d, Word.self)

  // Find an existing key.
  if true {
    var foundIndex1 = d.indexForKey(10)!
    assert(identity1 == unsafeBitCast(d, Word.self))

    var foundIndex2 = d.indexForKey(10)!
    assert(foundIndex1 == foundIndex2)

    assert(d[foundIndex1].0 == 10)
    assert(d[foundIndex1].1 == 1010)
    assert(identity1 == unsafeBitCast(d, Word.self))
  }

  // Try to find a key that is not present.
  if true {
    var foundIndex1 = d.indexForKey(1111)
    assert(foundIndex1 == nil)
    assert(identity1 == unsafeBitCast(d, Word.self))
  }
}

DictionaryTestCase.test("COW.Slow.IndexForKeyDoesNotReallocate") {
  var d = getCOWSlowDictionary()
  var identity1 = unsafeBitCast(d, Word.self)

  // Find an existing key.
  if true {
    var foundIndex1 = d.indexForKey(TestKeyTy(10))!
    assert(identity1 == unsafeBitCast(d, Word.self))

    var foundIndex2 = d.indexForKey(TestKeyTy(10))!
    assert(foundIndex1 == foundIndex2)

    assert(d[foundIndex1].0 == TestKeyTy(10))
    assert(d[foundIndex1].1.value == 1010)
    assert(identity1 == unsafeBitCast(d, Word.self))
  }

  // Try to find a key that is not present.
  if true {
    var foundIndex1 = d.indexForKey(TestKeyTy(1111))
    assert(foundIndex1 == nil)
    assert(identity1 == unsafeBitCast(d, Word.self))
  }
}


DictionaryTestCase.test("COW.Fast.RemoveAtIndexDoesNotReallocate") {
  if true {
    var d = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d, Word.self)

    let foundIndex1 = d.indexForKey(10)!
    assert(identity1 == unsafeBitCast(d, Word.self))

    assert(d[foundIndex1].0 == 10)
    assert(d[foundIndex1].1 == 1010)

    d.removeAtIndex(foundIndex1)
    assert(identity1 == unsafeBitCast(d, Word.self))
    assert(d.indexForKey(10) == nil)
  }

  if true {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    var d2 = d1
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 == unsafeBitCast(d2, Word.self))

    var foundIndex1 = d2.indexForKey(10)!
    assert(d2[foundIndex1].0 == 10)
    assert(d2[foundIndex1].1 == 1010)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 == unsafeBitCast(d2, Word.self))

    d2.removeAtIndex(foundIndex1)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 != unsafeBitCast(d2, Word.self))
    assert(d2.indexForKey(10) == nil)
  }
}

DictionaryTestCase.test("COW.Slow.RemoveAtIndexDoesNotReallocate") {
  if true {
    var d = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d, Word.self)

    var foundIndex1 = d.indexForKey(TestKeyTy(10))!
    assert(identity1 == unsafeBitCast(d, Word.self))

    assert(d[foundIndex1].0 == TestKeyTy(10))
    assert(d[foundIndex1].1.value == 1010)

    d.removeAtIndex(foundIndex1)
    assert(identity1 == unsafeBitCast(d, Word.self))
    assert(d.indexForKey(TestKeyTy(10)) == nil)
  }

  if true {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    var d2 = d1
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 == unsafeBitCast(d2, Word.self))

    var foundIndex1 = d2.indexForKey(TestKeyTy(10))!
    assert(d2[foundIndex1].0 == TestKeyTy(10))
    assert(d2[foundIndex1].1.value == 1010)

    d2.removeAtIndex(foundIndex1)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 != unsafeBitCast(d2, Word.self))
    assert(d2.indexForKey(TestKeyTy(10)) == nil)
  }
}


DictionaryTestCase.test("COW.Fast.RemoveValueForKeyDoesNotReallocate") {
  if true {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    var deleted = d1.removeValueForKey(0)
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d1, Word.self))

    deleted = d1.removeValueForKey(10)
    assert(deleted! == 1010)
    assert(identity1 == unsafeBitCast(d1, Word.self))

    // Keep variables alive.
    acceptsAnyDictionary(d1)
  }

  if true {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    var d2 = d1
    var deleted = d2.removeValueForKey(0)
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 == unsafeBitCast(d2, Word.self))

    deleted = d2.removeValueForKey(10)
    assert(deleted! == 1010)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 != unsafeBitCast(d2, Word.self))

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }
}

DictionaryTestCase.test("COW.Slow.RemoveValueForKeyDoesNotReallocate") {
  if true {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    var deleted = d1.removeValueForKey(TestKeyTy(0))
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d1, Word.self))

    deleted = d1.removeValueForKey(TestKeyTy(10))
    assert(deleted!.value == 1010)
    assert(identity1 == unsafeBitCast(d1, Word.self))

    // Keep variables alive.
    acceptsAnyDictionary(d1)
  }

  if true {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    var d2 = d1
    var deleted = d2.removeValueForKey(TestKeyTy(0))
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 == unsafeBitCast(d2, Word.self))

    deleted = d2.removeValueForKey(TestKeyTy(10))
    assert(deleted!.value == 1010)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 != unsafeBitCast(d2, Word.self))

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }
}


DictionaryTestCase.test("COW.Fast.RemoveAllDoesNotReallocate") {
  if true {
    var d = getCOWFastDictionary()
    let originalCapacity = d._variantStorage.native.capacity
    assert(d.count == 3)
    assert(d[10]! == 1010)

    d.removeAll()
    // We can not assert that identity changed, since the new buffer of smaller
    // size can be allocated at the same address as the old one.
    var identity1 = unsafeBitCast(d, Word.self)
    assert(d._variantStorage.native.capacity < originalCapacity)
    assert(d.count == 0)
    assert(d[10] == nil)

    d.removeAll()
    assert(identity1 == unsafeBitCast(d, Word.self))
    assert(d.count == 0)
    assert(d[10] == nil)
  }

  if true {
    var d = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d, Word.self)
    let originalCapacity = d._variantStorage.native.capacity
    assert(d.count == 3)
    assert(d[10]! == 1010)

    d.removeAll(keepCapacity: true)
    assert(identity1 == unsafeBitCast(d, Word.self))
    assert(d._variantStorage.native.capacity == originalCapacity)
    assert(d.count == 0)
    assert(d[10] == nil)

    d.removeAll(keepCapacity: true)
    assert(identity1 == unsafeBitCast(d, Word.self))
    assert(d._variantStorage.native.capacity == originalCapacity)
    assert(d.count == 0)
    assert(d[10] == nil)
  }

  if true {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)
    assert(d1.count == 3)
    assert(d1[10]! == 1010)

    var d2 = d1
    d2.removeAll()
    var identity2 = unsafeBitCast(d2, Word.self)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[10]! == 1010)
    assert(d2.count == 0)
    assert(d2[10] == nil)

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }

  if true {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)
    let originalCapacity = d1._variantStorage.native.capacity
    assert(d1.count == 3)
    assert(d1[10] == 1010)

    var d2 = d1
    d2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(d2, Word.self)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[10]! == 1010)
    assert(d2._variantStorage.native.capacity == originalCapacity)
    assert(d2.count == 0)
    assert(d2[10] == nil)

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }
}

DictionaryTestCase.test("COW.Slow.RemoveAllDoesNotReallocate") {
  if true {
    var d = getCOWSlowDictionary()
    let originalCapacity = d._variantStorage.native.capacity
    assert(d.count == 3)
    assert(d[TestKeyTy(10)]!.value == 1010)

    d.removeAll()
    // We can not assert that identity changed, since the new buffer of smaller
    // size can be allocated at the same address as the old one.
    var identity1 = unsafeBitCast(d, Word.self)
    assert(d._variantStorage.native.capacity < originalCapacity)
    assert(d.count == 0)
    assert(d[TestKeyTy(10)] == nil)

    d.removeAll()
    assert(identity1 == unsafeBitCast(d, Word.self))
    assert(d.count == 0)
    assert(d[TestKeyTy(10)] == nil)
  }

  if true {
    var d = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d, Word.self)
    let originalCapacity = d._variantStorage.native.capacity
    assert(d.count == 3)
    assert(d[TestKeyTy(10)]!.value == 1010)

    d.removeAll(keepCapacity: true)
    assert(identity1 == unsafeBitCast(d, Word.self))
    assert(d._variantStorage.native.capacity == originalCapacity)
    assert(d.count == 0)
    assert(d[TestKeyTy(10)] == nil)

    d.removeAll(keepCapacity: true)
    assert(identity1 == unsafeBitCast(d, Word.self))
    assert(d._variantStorage.native.capacity == originalCapacity)
    assert(d.count == 0)
    assert(d[TestKeyTy(10)] == nil)
  }

  if true {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)
    assert(d1.count == 3)
    assert(d1[TestKeyTy(10)]!.value == 1010)

    var d2 = d1
    d2.removeAll()
    var identity2 = unsafeBitCast(d2, Word.self)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[TestKeyTy(10)]!.value == 1010)
    assert(d2.count == 0)
    assert(d2[TestKeyTy(10)] == nil)

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }

  if true {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)
    let originalCapacity = d1._variantStorage.native.capacity
    assert(d1.count == 3)
    assert(d1[TestKeyTy(10)]!.value == 1010)

    var d2 = d1
    d2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(d2, Word.self)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[TestKeyTy(10)]!.value == 1010)
    assert(d2._variantStorage.native.capacity == originalCapacity)
    assert(d2.count == 0)
    assert(d2[TestKeyTy(10)] == nil)

    // Keep variables alive.
    acceptsAnyDictionary(d1)
    acceptsAnyDictionary(d2)
  }
}


DictionaryTestCase.test("COW.Fast.CountDoesNotReallocate") {
  var d = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d, Word.self)

  assert(d.count == 3)
  assert(identity1 == unsafeBitCast(d, Word.self))
}

DictionaryTestCase.test("COW.Slow.CountDoesNotReallocate") {
  var d = getCOWSlowDictionary()
  var identity1 = unsafeBitCast(d, Word.self)

  assert(d.count == 3)
  assert(identity1 == unsafeBitCast(d, Word.self))
}


DictionaryTestCase.test("COW.Fast.GenerateDoesNotReallocate") {
  var d = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d, Word.self)

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value) = gen.next() {
    pairs += [(key, value)]
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  assert(identity1 == unsafeBitCast(d, Word.self))
}

DictionaryTestCase.test("COW.Slow.GenerateDoesNotReallocate") {
  var d = getCOWSlowDictionary()
  var identity1 = unsafeBitCast(d, Word.self)

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value) = gen.next() {
    // FIXME: This doesn't work (<rdar://problem/17751308> Can't +=
    // with array literal of pairs)
    // pairs += [(key.value, value.value)]

    // FIXME: This doesn't work (<rdar://problem/17750582> generics over tuples)
    // pairs.append((key.value, value.value))

    // FIXME: This doesn't work (<rdar://problem/17751359>)
    // pairs.append(key.value, value.value)

    let kv = (key.value, value.value)
    pairs += [kv]
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  assert(identity1 == unsafeBitCast(d, Word.self))
}


DictionaryTestCase.test("COW.Fast.EqualityTestDoesNotReallocate") {
  var d1 = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d1, Word.self)

  var d2 = getCOWFastDictionary()
  var identity2 = unsafeBitCast(d2, Word.self)

  assert(d1 == d2)
  assert(identity1 == unsafeBitCast(d1, Word.self))
  assert(identity2 == unsafeBitCast(d2, Word.self))

  d2[40] = 2040
  assert(d1 != d2)
  assert(identity1 == unsafeBitCast(d1, Word.self))
  assert(identity2 == unsafeBitCast(d2, Word.self))
}

DictionaryTestCase.test("COW.Slow.EqualityTestDoesNotReallocate") {
  var d1 = getCOWSlowEquatableDictionary()
  var identity1 = unsafeBitCast(d1, Word.self)

  var d2 = getCOWSlowEquatableDictionary()
  var identity2 = unsafeBitCast(d2, Word.self)

  assert(d1 == d2)
  assert(identity1 == unsafeBitCast(d1, Word.self))
  assert(identity2 == unsafeBitCast(d2, Word.self))

  d2[TestKeyTy(40)] = TestEquatableValueTy(2040)
  assert(d1 != d2)
  assert(identity1 == unsafeBitCast(d1, Word.self))
  assert(identity2 == unsafeBitCast(d2, Word.self))
}


//===---
// Native dictionary tests.
//===---

func helperDeleteThree(k1: TestKeyTy, k2: TestKeyTy, k3: TestKeyTy) {
  var d1 = Dictionary<TestKeyTy, TestValueTy>(minimumCapacity: 10)

  d1[k1] = TestValueTy(1010)
  d1[k2] = TestValueTy(1020)
  d1[k3] = TestValueTy(1030)

  assert(d1[k1]!.value == 1010)
  assert(d1[k2]!.value == 1020)
  assert(d1[k3]!.value == 1030)

  d1[k1] = nil
  assert(d1[k2]!.value == 1020)
  assert(d1[k3]!.value == 1030)

  d1[k2] = nil
  assert(d1[k3]!.value == 1030)

  d1[k3] = nil
  assert(d1.count == 0)
}

DictionaryTestCase.test("deleteChainCollision") {
  var k1 = TestKeyTy(value: 10, hashValue: 0)
  var k2 = TestKeyTy(value: 20, hashValue: 0)
  var k3 = TestKeyTy(value: 30, hashValue: 0)

  helperDeleteThree(k1, k2, k3)
}

DictionaryTestCase.test("deleteChainNoCollision") {
  var k1 = TestKeyTy(value: 10, hashValue: 0)
  var k2 = TestKeyTy(value: 20, hashValue: 1)
  var k3 = TestKeyTy(value: 30, hashValue: 2)

  helperDeleteThree(k1, k2, k3)
}

DictionaryTestCase.test("deleteChainCollision2") {
  var k1_0 = TestKeyTy(value: 10, hashValue: 0)
  var k2_0 = TestKeyTy(value: 20, hashValue: 0)
  var k3_2 = TestKeyTy(value: 30, hashValue: 2)
  var k4_0 = TestKeyTy(value: 40, hashValue: 0)
  var k5_2 = TestKeyTy(value: 50, hashValue: 2)
  var k6_0 = TestKeyTy(value: 60, hashValue: 0)

  var d = Dictionary<TestKeyTy, TestValueTy>(minimumCapacity: 10)

  d[k1_0] = TestValueTy(1010) // in bucket 0
  d[k2_0] = TestValueTy(1020) // in bucket 1
  d[k3_2] = TestValueTy(1030) // in bucket 2
  d[k4_0] = TestValueTy(1040) // in bucket 3
  d[k5_2] = TestValueTy(1050) // in bucket 4
  d[k6_0] = TestValueTy(1060) // in bucket 5

  d[k3_2] = nil

  assert(d[k1_0]!.value == 1010)
  assert(d[k2_0]!.value == 1020)
  assert(d[k3_2] == nil)
  assert(d[k4_0]!.value == 1040)
  assert(d[k5_2]!.value == 1050)
  assert(d[k6_0]!.value == 1060)
}

func uniformRandom(max: Int) -> Int {
  // FIXME: this is not uniform.
  return random() % max
}

func pickRandom<T>(a: [T]) -> T {
  return a[uniformRandom(a.count)]
}

DictionaryTestCase.test("deleteChainCollisionRandomized") {
  let timeNow = CUnsignedInt(time(nil))
  println("time is \(timeNow)")
  srandom(timeNow)

  func check(d: Dictionary<TestKeyTy, TestValueTy>) {
    var keys = Array(d.keys)
    for i in 0..<keys.count {
      for j in 0..<i {
        assert(keys[i] != keys[j])
      }
    }

    for k in keys {
      assert(d[k] != nil)
    }
  }

  var collisionChainsChoices = Array(1...8)
  var chainOverlapChoices = Array(0...5)

  var collisionChains = pickRandom(collisionChainsChoices)
  var chainOverlap = pickRandom(chainOverlapChoices)
  println("chose parameters: collisionChains=\(collisionChains) chainLength=\(chainOverlap)")

  let chainLength = 7

  var knownKeys: [TestKeyTy] = []
  func getKey(value: Int) -> TestKeyTy {
    for k in knownKeys {
      if k.value == value {
        return k
      }
    }
    let hashValue = uniformRandom(chainLength - chainOverlap) * collisionChains
    let k = TestKeyTy(value: value, hashValue: hashValue)
    knownKeys += [k]
    return k
  }

  var d = Dictionary<TestKeyTy, TestValueTy>(minimumCapacity: 30)
  for i in 1..<300 {
    let key = getKey(uniformRandom(collisionChains * chainLength))
    if uniformRandom(chainLength * 2) == 0 {
      d[key] = nil
    } else {
      d[key] = TestValueTy(key.value * 10)
    }
    check(d)
  }
}

DictionaryTestCase.test("convertFromDictionaryLiteral") {
  if true {
    var empty = Dictionary<Int, Int>.convertFromDictionaryLiteral()
    assert(empty.count == 0)
    assert(empty[1111] == nil)
  }
  if true {
    var d = Dictionary.convertFromDictionaryLiteral((10, 1010))
    assert(d.count == 1)
    assert(d[10]! == 1010)
    assert(d[1111] == nil)
  }
  if true {
    var d = Dictionary.convertFromDictionaryLiteral(
        (10, 1010), (20, 1020))
    assert(d.count == 2)
    assert(d[10]! == 1010)
    assert(d[20]! == 1020)
    assert(d[1111] == nil)
  }
  if true {
    var d = Dictionary.convertFromDictionaryLiteral(
        (10, 1010), (20, 1020), (30, 1030))
    assert(d.count == 3)
    assert(d[10]! == 1010)
    assert(d[20]! == 1020)
    assert(d[30]! == 1030)
    assert(d[1111] == nil)
  }
  if true {
    var d = Dictionary.convertFromDictionaryLiteral(
        (10, 1010), (20, 1020), (30, 1030), (40, 1040))
    assert(d.count == 4)
    assert(d[10]! == 1010)
    assert(d[20]! == 1020)
    assert(d[30]! == 1030)
    assert(d[40]! == 1040)
    assert(d[1111] == nil)
  }
  if true {
    var d: Dictionary<Int, Int> = [ 10: 1010, 20: 1020, 30: 1030 ]
    assert(d.count == 3)
    assert(d[10]! == 1010)
    assert(d[20]! == 1020)
    assert(d[30]! == 1030)
  }
}

//===---
// NSDictionary -> Dictionary bridging tests.
//===---

func isNativeNSDictionary(d: NSDictionary) -> Bool {
  var className: NSString = NSStringFromClass(d.dynamicType)
  return className.rangeOfString("_NativeDictionaryStorageOwner").length > 0
}

func isCocoaNSDictionary(d: NSDictionary) -> Bool {
  var className: NSString = NSStringFromClass(d.dynamicType)
  return className.rangeOfString("NSDictionary").length > 0 ||
    className.rangeOfString("NSCFDictionary").length > 0
}

var _objcKeyCount = 0
var _objcKeySerial = 0

class TestObjCKeyTy : NSObject, NSCopying, Printable {
  class var objectCount: Int {
    get {
      return _objcKeyCount
    }
    set {
      _objcKeyCount = newValue
    }
  }

  init(_ value: Int) {
    ++TestObjCKeyTy.objectCount
    serial = ++_objcKeySerial
    self.value = value
    self._hashValue = value
  }

  convenience init(value: Int, hashValue: Int) {
    self.init(value)
    self._hashValue = hashValue
  }

  deinit {
    assert(serial > 0, "double destruction")
    --TestObjCKeyTy.objectCount
    serial = -serial
  }

  @objc
  func copyWithZone(zone: NSZone) -> AnyObject {
    return TestObjCKeyTy(value)
  }

  override var description: String {
    assert(serial > 0, "dead TestObjCKeyTy")
    return value.description
  }

  override func isEqual(object: AnyObject!) -> Bool {
    if let other: AnyObject = object {
      if let otherObjcKey = other as? TestObjCKeyTy {
        return self.value == otherObjcKey.value
      }
    }
    return false
  }

  override var hash : Int {
    return _hashValue
  }

  func _bridgeToObjectiveC() -> TestObjCKeyTy {
    return self
  }

  var value: Int
  var _hashValue: Int
  var serial: Int
}

var _objcValueCount = 0
var _objcValueSerial = 0

class TestObjCValueTy : NSObject, Printable {
  class var objectCount: Int {
    get {
      return _objcValueCount
    }
    set {
      _objcValueCount = newValue
    }
  }

  init(_ value: Int) {
    ++TestObjCValueTy.objectCount
    serial = ++_objcValueSerial
    self.value = value
  }

  deinit {
    assert(serial > 0, "double destruction")
    --TestObjCValueTy.objectCount
    serial = -serial
  }

  override var description: String {
    assert(serial > 0, "dead TestObjCValueTy")
    return value.description
  }

  var value: Int
  var serial: Int
}

class TestObjCEquatableValueTy : NSObject, Equatable, Printable {
  init(_ value: Int) {
    ++valueCount
    serial = ++valueSerial
    self.value = value
  }

  deinit {
    assert(serial > 0, "double destruction")
    --valueCount
    serial = -serial
  }

  override func isEqual(object: AnyObject!) -> Bool {
    if let other: AnyObject = object {
      if let otherObjcKey = other as? TestObjCEquatableValueTy {
        return self.value == otherObjcKey.value
      }
    }
    return false
  }

  override var description: String {
    assert(serial > 0, "dead TestObjCValueTy")
    return value.description
  }

  var value: Int
  var serial: Int
}

func == (lhs: TestObjCEquatableValueTy, rhs: TestObjCEquatableValueTy) -> Bool {
  return lhs.value == rhs.value
}

var bridgedKeyCount = 0
var bridgedKeySerial = 0
var _bridgedKeyBridgeOperations = 0

struct TestBridgedKeyTy
  : Equatable, Hashable, Printable, _ObjectiveCBridgeable {
  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }

  static var bridgeOperations: Int {
    get {
      return _bridgedKeyBridgeOperations
    }
    set {
      _bridgedKeyBridgeOperations = newValue
    }
  }

  init(_ value: Int) {
    ++bridgedKeyCount
    serial = ++bridgedKeySerial
    self.value = value
    self._hashValue = value
  }

  var description: String {
    assert(serial > 0, "dead TestBridgedKeyTy")
    return value.description
  }

  var hashValue: Int {
    return _hashValue
  }

  static func _getObjectiveCType() -> Any.Type {
    return TestObjCKeyTy.self
  }

  func _bridgeToObjectiveC() -> TestObjCKeyTy {
    TestBridgedKeyTy.bridgeOperations++
    return TestObjCKeyTy(value)
  }

  static func _forceBridgeFromObjectiveC(
    x: TestObjCKeyTy,
    inout result: TestBridgedKeyTy?
  ) {
    TestBridgedKeyTy.bridgeOperations++
    result = TestBridgedKeyTy(x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    x: TestObjCKeyTy,
    inout result: TestBridgedKeyTy?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  var value: Int
  var _hashValue: Int
  var serial: Int
}

func == (lhs: TestBridgedKeyTy, rhs: TestBridgedKeyTy) -> Bool {
  return lhs.value == rhs.value
}

var bridgedValueCount = 0
var bridgedValueSerial = 0
var _bridgedValueBridgeOperations = 0

struct TestBridgedValueTy : Printable, _ObjectiveCBridgeable {
  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }

  static var bridgeOperations: Int {
    get {
      return _bridgedValueBridgeOperations
    }
    set {
      _bridgedValueBridgeOperations = newValue
    }
  }

  init(_ value: Int) {
    ++bridgedValueCount
    serial = ++bridgedValueSerial
    self.value = value
  }

  var description: String {
    assert(serial > 0, "dead TestBridgedValueTy")
    return value.description
  }

  static func _getObjectiveCType() -> Any.Type {
    return TestObjCValueTy.self
  }

  func _bridgeToObjectiveC() -> TestObjCValueTy {
    TestBridgedValueTy.bridgeOperations++
    return TestObjCValueTy(value)
  }

  static func _forceBridgeFromObjectiveC(
    x: TestObjCValueTy,
    inout result: TestBridgedValueTy?
  ) {
    TestBridgedValueTy.bridgeOperations++
    result = TestBridgedValueTy(x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    x: TestObjCValueTy,
    inout result: TestBridgedValueTy?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  var value: Int
  var serial: Int
}

struct TestBridgedEquatableValueTy
  : Equatable, Printable, _ObjectiveCBridgeable {

  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }

  init(_ value: Int) {
    ++bridgedValueCount
    serial = ++bridgedValueSerial
    self.value = value
  }

  var description: String {
    assert(serial > 0, "dead TestBridgedValueTy")
    return value.description
  }

  static func _getObjectiveCType() -> Any.Type {
    return TestObjCValueTy.self
  }

  func _bridgeToObjectiveC() -> TestObjCEquatableValueTy {
    return TestObjCEquatableValueTy(value)
  }

  static func _forceBridgeFromObjectiveC(
    x: TestObjCEquatableValueTy,
    inout result: TestBridgedEquatableValueTy?
  ) {
    result = TestBridgedEquatableValueTy(x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    x: TestObjCEquatableValueTy,
    inout result: TestBridgedEquatableValueTy?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  var value: Int
  var serial: Int
}

func == (lhs: TestBridgedEquatableValueTy, rhs: TestBridgedEquatableValueTy) -> Bool {
  return lhs.value == rhs.value
}

func slurpFastEnumeration(
    d: NSDictionary, fe: NSFastEnumeration
) -> Array<(Int, Int)> {
  var state = NSFastEnumerationState()

  let stackBufLength = 3
  var stackBuf = HeapBuffer<(), AnyObject?>(
      HeapBufferStorage<(), AnyObject?>.self, (), stackBufLength)

  var pairs = Array<(Int, Int)>()
  while true {
    var returnedCount = fe.countByEnumeratingWithState(
        &state, objects: AutoreleasingUnsafeMutablePointer(stackBuf.baseAddress),
        count: stackBufLength)
    assert(state.state != 0)
    assert(state.mutationsPtr != .null())
    if returnedCount == 0 {
      break
    }
    for i in 0..<returnedCount {
      let key: AnyObject = state.itemsPtr[i]!
      let value: AnyObject = d.objectForKey(key)
      let kv = ((key as TestObjCKeyTy).value, (value as TestObjCValueTy).value)
      pairs += [kv]
    }
  }

  for i in 0..<3 {
    let returnedCount = fe.countByEnumeratingWithState(
        &state, objects: AutoreleasingUnsafeMutablePointer(stackBuf.baseAddress),
        count: stackBufLength)
    assert(returnedCount == 0)
  }

  return pairs
}

import SlurpFastEnumeration

func slurpFastEnumerationFromObjC(
    d: NSDictionary, fe: NSFastEnumeration
) -> Array<(Int, Int)> {
  var objcPairs = NSMutableArray()
  slurpFastEnumerationFromObjCImpl(d, fe, objcPairs)
  var pairs = Array<(Int, Int)>()
  for pairAnyObject: AnyObject in objcPairs {
    let pair = pairAnyObject as NSArray
    let key = (pair[0] as TestObjCKeyTy).value
    let value = (pair[1] as TestObjCValueTy).value
    pairs += [(key, value)]
  }
  return pairs
}

func getAsNSDictionary(d: Dictionary<Int, Int>) -> NSDictionary {
  let keys = NSMutableArray()
  let values = NSMutableArray()
  for (k, v) in d {
    keys.addObject(TestObjCKeyTy(k))
    values.addObject(TestObjCValueTy(v))
  }
  // Return an `NSMutableDictionary` to make sure that it has a unique
  // pointer identity.
  return NSMutableDictionary(objects: values, forKeys: keys)
}

func getAsEquatableNSDictionary(d: Dictionary<Int, Int>) -> NSDictionary {
  let keys = NSMutableArray()
  let values = NSMutableArray()
  for (k, v) in d {
    keys.addObject(TestObjCKeyTy(k))
    values.addObject(TestObjCEquatableValueTy(v))
  }
  // Return an `NSMutableDictionary` to make sure that it has a unique
  // pointer identity.
  return NSMutableDictionary(objects: values, forKeys: keys)
}

func getAsNSMutableDictionary(d: Dictionary<Int, Int>) -> NSMutableDictionary {
  let keys = NSMutableArray()
  let values = NSMutableArray()
  for (k, v) in d {
    keys.addObject(TestObjCKeyTy(k))
    values.addObject(TestObjCValueTy(v))
  }
  return NSMutableDictionary(objects: values, forKeys: keys)
}

func getBridgedVerbatimDictionary() -> Dictionary<NSObject, AnyObject> {
  var nsd = getAsNSDictionary([ 10: 1010, 20: 1020, 30: 1030 ])
  return _convertNSDictionaryToDictionary(nsd)
}

func getBridgedVerbatimDictionary(d: Dictionary<Int, Int>) -> Dictionary<NSObject, AnyObject> {
  var nsd = getAsNSDictionary(d)
  return _convertNSDictionaryToDictionary(nsd)
}

func getBridgedVerbatimDictionaryAndNSMutableDictionary()
    -> (Dictionary<NSObject, AnyObject>, NSMutableDictionary) {
  var nsd = getAsNSMutableDictionary([ 10: 1010, 20: 1020, 30: 1030 ])
  return (_convertNSDictionaryToDictionary(nsd), nsd)
}

func getBridgedNonverbatimDictionary() -> Dictionary<TestBridgedKeyTy, TestBridgedValueTy> {
  var nsd = getAsNSDictionary([ 10: 1010, 20: 1020, 30: 1030 ])
  return Swift._forceBridgeFromObjectiveC(nsd, Dictionary.self)
}

func getBridgedNonverbatimDictionary(d: Dictionary<Int, Int>) -> Dictionary<TestBridgedKeyTy, TestBridgedValueTy> {
  var nsd = getAsNSDictionary(d)
  return Swift._forceBridgeFromObjectiveC(nsd, Dictionary.self)
}

func getBridgedNonverbatimDictionaryAndNSMutableDictionary()
    -> (Dictionary<TestBridgedKeyTy, TestBridgedValueTy>, NSMutableDictionary) {
  var nsd = getAsNSMutableDictionary([ 10: 1010, 20: 1020, 30: 1030 ])
  return (Swift._forceBridgeFromObjectiveC(nsd, Dictionary.self), nsd)
}

func getBridgedVerbatimEquatableDictionary(d: Dictionary<Int, Int>) -> Dictionary<NSObject, TestObjCEquatableValueTy> {
  var nsd = getAsEquatableNSDictionary(d)
  return _convertNSDictionaryToDictionary(nsd)
}

func getBridgedNonverbatimEquatableDictionary(d: Dictionary<Int, Int>) -> Dictionary<TestBridgedKeyTy, TestBridgedEquatableValueTy> {
  var nsd = getAsEquatableNSDictionary(d)
  return Swift._forceBridgeFromObjectiveC(nsd, Dictionary.self)
}

func getHugeBridgedVerbatimDictionaryHelper() -> NSDictionary {
  let keys = NSMutableArray()
  let values = NSMutableArray()
  for i in 1...32 {
    keys.addObject(TestObjCKeyTy(i))
    values.addObject(TestObjCValueTy(1000 + i))
  }

  return NSDictionary(objects: values, forKeys: keys)
}

func getHugeBridgedVerbatimDictionary() -> Dictionary<NSObject, AnyObject> {
  var nsd = getHugeBridgedVerbatimDictionaryHelper()
  return _convertNSDictionaryToDictionary(nsd)
}

func getHugeBridgedNonverbatimDictionary() -> Dictionary<TestBridgedKeyTy, TestBridgedValueTy> {
  var nsd = getHugeBridgedVerbatimDictionaryHelper()
  return Swift._forceBridgeFromObjectiveC(nsd, Dictionary.self)
}

/// A mock dictionary that stores its keys and values in parallel arrays, which
/// allows it to return inner pointers to the keys array in fast enumeration.
@objc
class ParallelArrayDictionary : NSDictionary {
  struct Keys {
    var key0: AnyObject = TestObjCKeyTy(10)
    var key1: AnyObject = TestObjCKeyTy(20)
    var key2: AnyObject = TestObjCKeyTy(30)
    var key3: AnyObject = TestObjCKeyTy(40)
  }
  var keys = [ Keys() ]
  var value: AnyObject = TestObjCValueTy(1111)

  override init() {
    super.init()
  }

  required init(coder aDecoder: NSCoder) {
    fatalError("init(coder:) not implemented by ParallelArrayDictionary")
  }

  @objc override func copyWithZone(zone: NSZone) -> AnyObject {
    // Ensure that copying this dictionary does not produce a CoreFoundation
    // object.
    return self
  }

  override func countByEnumeratingWithState(
      state: UnsafeMutablePointer<NSFastEnumerationState>,
      objects: AutoreleasingUnsafeMutablePointer<AnyObject?>, count: Int) -> Int {
    var theState = state.memory
    if theState.state == 0 {
      theState.state = 1
      theState.itemsPtr = AutoreleasingUnsafeMutablePointer(keys._baseAddressIfContiguous)
      theState.mutationsPtr = _fastEnumerationStorageMutationsPtr
      state.memory = theState
      return 4
    }
    return 0
  }

  override func objectForKey(aKey: AnyObject?) -> AnyObject? {
    return value
  }

  override var count: Int {
    return 4
  }
}

func getParallelArrayBridgedVerbatimDictionary() -> Dictionary<NSObject, AnyObject> {
  var nsd: NSDictionary = ParallelArrayDictionary()
  return _convertNSDictionaryToDictionary(nsd)
}

func getParallelArrayBridgedNonverbatimDictionary() -> Dictionary<TestBridgedKeyTy, TestBridgedValueTy> {
  var nsd: NSDictionary = ParallelArrayDictionary()
  return Swift._forceBridgeFromObjectiveC(nsd, Dictionary.self)
}

DictionaryTestCase.test("BridgedFromObjC.Verbatim.DictionaryIsCopied") {
  var (d, nsd) = getBridgedVerbatimDictionaryAndNSMutableDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isCocoaDictionary(d))

  // Find an existing key.
  if true {
    var kv = d[d.indexForKey(TestObjCKeyTy(10))!]
    assert(kv.0 == TestObjCKeyTy(10))
    assert(kv.1.value == 1010)
  }

  // Delete the key from the NSMutableDictionary.
  assert(nsd[TestObjCKeyTy(10)] != nil)
  nsd.removeObjectForKey(TestObjCKeyTy(10))
  assert(nsd[TestObjCKeyTy(10)] == nil)

  // Find an existing key, again.
  if true {
    var kv = d[d.indexForKey(TestObjCKeyTy(10))!]
    assert(kv.0 == TestObjCKeyTy(10))
    assert(kv.1.value == 1010)
  }
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.DictionaryIsCopied") {
  var (d, nsd) = getBridgedNonverbatimDictionaryAndNSMutableDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isNativeDictionary(d))

  // Find an existing key.
  if true {
    var kv = d[d.indexForKey(TestBridgedKeyTy(10))!]
    assert(kv.0 == TestBridgedKeyTy(10))
    assert(kv.1.value == 1010)
  }

  // Delete the key from the NSMutableDictionary.
  assert(nsd[TestBridgedKeyTy(10)] != nil)
  nsd.removeObjectForKey(TestBridgedKeyTy(10))
  assert(nsd[TestBridgedKeyTy(10)] == nil)

  // Find an existing key, again.
  if true {
    var kv = d[d.indexForKey(TestBridgedKeyTy(10))!]
    assert(kv.0 == TestBridgedKeyTy(10))
    assert(kv.1.value == 1010)
  }
}


DictionaryTestCase.test("BridgedFromObjC.Verbatim.IndexForKey") {
  var d = getBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isCocoaDictionary(d))

  // Find an existing key.
  if true {
    var kv = d[d.indexForKey(TestObjCKeyTy(10))!]
    assert(kv.0 == TestObjCKeyTy(10))
    assert(kv.1.value == 1010)

    kv = d[d.indexForKey(TestObjCKeyTy(20))!]
    assert(kv.0 == TestObjCKeyTy(20))
    assert(kv.1.value == 1020)

    kv = d[d.indexForKey(TestObjCKeyTy(30))!]
    assert(kv.0 == TestObjCKeyTy(30))
    assert(kv.1.value == 1030)
  }

  // Try to find a key that does not exist.
  assert(d.indexForKey(TestObjCKeyTy(40)) == nil)
  assert(identity1 == unsafeBitCast(d, Word.self))
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.IndexForKey") {
  var d = getBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isNativeDictionary(d))

  // Find an existing key.
  if true {
    var kv = d[d.indexForKey(TestBridgedKeyTy(10))!]
    assert(kv.0 == TestBridgedKeyTy(10))
    assert(kv.1.value == 1010)

    kv = d[d.indexForKey(TestBridgedKeyTy(20))!]
    assert(kv.0 == TestBridgedKeyTy(20))
    assert(kv.1.value == 1020)

    kv = d[d.indexForKey(TestBridgedKeyTy(30))!]
    assert(kv.0 == TestBridgedKeyTy(30))
    assert(kv.1.value == 1030)
  }

  // Try to find a key that does not exist.
  assert(d.indexForKey(TestBridgedKeyTy(40)) == nil)
  assert(identity1 == unsafeBitCast(d, Word.self))
}

DictionaryTestCase.test("BridgedFromObjC.Verbatim.SubscriptWithIndex") {
  var d = getBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isCocoaDictionary(d))

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex != endIndex)
  assert(startIndex < endIndex)
  assert(startIndex <= endIndex)
  assert(!(startIndex >= endIndex))
  assert(!(startIndex > endIndex))
  assert(identity1 == unsafeBitCast(d, Word.self))

  var pairs = Array<(Int, Int)>()
  for var i = startIndex; i != endIndex; ++i {
    var (key, value: AnyObject) = d[i]
    let kv = ((key as TestObjCKeyTy).value, (value as TestObjCValueTy).value)
    pairs += [kv]
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  assert(identity1 == unsafeBitCast(d, Word.self))

  // Keep indexes alive during the calls above.
  withExtendedLifetime(startIndex) { () }
  withExtendedLifetime(endIndex) { () }
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.SubscriptWithIndex") {
  var d = getBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isNativeDictionary(d))

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex != endIndex)
  assert(startIndex < endIndex)
  assert(startIndex <= endIndex)
  assert(!(startIndex >= endIndex))
  assert(!(startIndex > endIndex))
  assert(identity1 == unsafeBitCast(d, Word.self))

  var pairs = Array<(Int, Int)>()
  for var i = startIndex; i != endIndex; ++i {
    var (key, value) = d[i]
    let kv = (key.value, value.value)
    pairs += [kv]
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  assert(identity1 == unsafeBitCast(d, Word.self))

  // Keep indexes alive during the calls above.
  withExtendedLifetime(startIndex) { () }
  withExtendedLifetime(endIndex) { () }
}

DictionaryTestCase.test("BridgedFromObjC.Verbatim.SubscriptWithIndex_Empty") {
  var d = getBridgedVerbatimDictionary([:])
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isCocoaDictionary(d))

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex == endIndex)
  assert(!(startIndex < endIndex))
  assert(startIndex <= endIndex)
  assert(startIndex >= endIndex)
  assert(!(startIndex > endIndex))
  assert(identity1 == unsafeBitCast(d, Word.self))

  // Keep indexes alive during the calls above.
  withExtendedLifetime(startIndex) { () }
  withExtendedLifetime(endIndex) { () }
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.SubscriptWithIndex_Empty") {
  var d = getBridgedNonverbatimDictionary([:])
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isNativeDictionary(d))

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex == endIndex)
  assert(!(startIndex < endIndex))
  assert(startIndex <= endIndex)
  assert(startIndex >= endIndex)
  assert(!(startIndex > endIndex))
  assert(identity1 == unsafeBitCast(d, Word.self))

  // Keep indexes alive during the calls above.
  withExtendedLifetime(startIndex) { () }
  withExtendedLifetime(endIndex) { () }
}

DictionaryTestCase.test("BridgedFromObjC.Verbatim.SubscriptWithKey") {
  var d = getBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isCocoaDictionary(d))

  // Read existing key-value pairs.
  var v = d[TestObjCKeyTy(10)] as TestObjCValueTy
  assert(v.value == 1010)

  v = d[TestObjCKeyTy(20)] as TestObjCValueTy
  assert(v.value == 1020)

  v = d[TestObjCKeyTy(30)] as TestObjCValueTy
  assert(v.value == 1030)

  assert(identity1 == unsafeBitCast(d, Word.self))

  // Insert a new key-value pair.
  d[TestObjCKeyTy(40)] = TestObjCValueTy(2040)
  var identity2 = unsafeBitCast(d, Word.self)
  assert(identity1 != identity2)
  assert(isNativeDictionary(d))
  assert(d.count == 4)

  v = d[TestObjCKeyTy(10)] as TestObjCValueTy
  assert(v.value == 1010)

  v = d[TestObjCKeyTy(20)] as TestObjCValueTy
  assert(v.value == 1020)

  v = d[TestObjCKeyTy(30)] as TestObjCValueTy
  assert(v.value == 1030)

  v = d[TestObjCKeyTy(40)] as TestObjCValueTy
  assert(v.value == 2040)

  // Overwrite value in existing binding.
  d[TestObjCKeyTy(10)] = TestObjCValueTy(2010)
  assert(identity2 == unsafeBitCast(d, Word.self))
  assert(isNativeDictionary(d))
  assert(d.count == 4)

  v = d[TestObjCKeyTy(10)] as TestObjCValueTy
  assert(v.value == 2010)

  v = d[TestObjCKeyTy(20)] as TestObjCValueTy
  assert(v.value == 1020)

  v = d[TestObjCKeyTy(30)] as TestObjCValueTy
  assert(v.value == 1030)

  v = d[TestObjCKeyTy(40)] as TestObjCValueTy
  assert(v.value == 2040)
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.SubscriptWithKey") {
  var d = getBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isNativeDictionary(d))

  // Read existing key-value pairs.
  var v = d[TestBridgedKeyTy(10)]
  assert(v!.value == 1010)

  v = d[TestBridgedKeyTy(20)]
  assert(v!.value == 1020)

  v = d[TestBridgedKeyTy(30)]
  assert(v!.value == 1030)

  assert(identity1 == unsafeBitCast(d, Word.self))

  // Insert a new key-value pair.
  d[TestBridgedKeyTy(40)] = TestBridgedValueTy(2040)
  var identity2 = unsafeBitCast(d, Word.self)
  assert(identity1 != identity2)
  assert(isNativeDictionary(d))
  assert(d.count == 4)

  v = d[TestBridgedKeyTy(10)]
  assert(v!.value == 1010)

  v = d[TestBridgedKeyTy(20)]
  assert(v!.value == 1020)

  v = d[TestBridgedKeyTy(30)]
  assert(v!.value == 1030)

  v = d[TestBridgedKeyTy(40)]
  assert(v!.value == 2040)

  // Overwrite value in existing binding.
  d[TestBridgedKeyTy(10)] = TestBridgedValueTy(2010)
  assert(identity2 == unsafeBitCast(d, Word.self))
  assert(isNativeDictionary(d))
  assert(d.count == 4)

  v = d[TestBridgedKeyTy(10)]
  assert(v!.value == 2010)

  v = d[TestBridgedKeyTy(20)]
  assert(v!.value == 1020)

  v = d[TestBridgedKeyTy(30)]
  assert(v!.value == 1030)

  v = d[TestBridgedKeyTy(40)]
  assert(v!.value == 2040)
}


DictionaryTestCase.test("BridgedFromObjC.Verbatim.UpdateValueForKey") {
  // Insert a new key-value pair.
  if true {
    var d = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d, Word.self)
    assert(isCocoaDictionary(d))

    var oldValue: AnyObject? =
        d.updateValue(TestObjCValueTy(2040), forKey: TestObjCKeyTy(40))
    assert(oldValue == nil)
    var identity2 = unsafeBitCast(d, Word.self)
    assert(identity1 != identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 4)

    assert(d[TestObjCKeyTy(10)]!.value == 1010)
    assert(d[TestObjCKeyTy(20)]!.value == 1020)
    assert(d[TestObjCKeyTy(30)]!.value == 1030)
    assert(d[TestObjCKeyTy(40)]!.value == 2040)
  }

  // Overwrite a value in existing binding.
  if true {
    var d = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d, Word.self)
    assert(isCocoaDictionary(d))

    var oldValue: AnyObject? =
        d.updateValue(TestObjCValueTy(2010), forKey: TestObjCKeyTy(10))
    assert((oldValue as TestObjCValueTy).value == 1010)

    var identity2 = unsafeBitCast(d, Word.self)
    assert(identity1 != identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 3)

    assert(d[TestObjCKeyTy(10)]!.value == 2010)
    assert(d[TestObjCKeyTy(20)]!.value == 1020)
    assert(d[TestObjCKeyTy(30)]!.value == 1030)
  }
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.UpdateValueForKey") {
  // Insert a new key-value pair.
  if true {
    var d = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d, Word.self)
    assert(isNativeDictionary(d))

    var oldValue =
        d.updateValue(TestBridgedValueTy(2040), forKey: TestBridgedKeyTy(40))
    assert(oldValue == nil)
    var identity2 = unsafeBitCast(d, Word.self)
    assert(identity1 != identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 4)

    assert(d[TestBridgedKeyTy(10)]!.value == 1010)
    assert(d[TestBridgedKeyTy(20)]!.value == 1020)
    assert(d[TestBridgedKeyTy(30)]!.value == 1030)
    assert(d[TestBridgedKeyTy(40)]!.value == 2040)
  }

  // Overwrite a value in existing binding.
  if true {
    var d = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d, Word.self)
    assert(isNativeDictionary(d))

    var oldValue =
        d.updateValue(TestBridgedValueTy(2010), forKey: TestBridgedKeyTy(10))!
    assert(oldValue.value == 1010)

    var identity2 = unsafeBitCast(d, Word.self)
    assert(identity1 == identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 3)

    assert(d[TestBridgedKeyTy(10)]!.value == 2010)
    assert(d[TestBridgedKeyTy(20)]!.value == 1020)
    assert(d[TestBridgedKeyTy(30)]!.value == 1030)
  }
}


DictionaryTestCase.test("BridgedFromObjC.Verbatim.RemoveAtIndex") {
  var d = getBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isCocoaDictionary(d))

  let foundIndex1 = d.indexForKey(TestObjCKeyTy(10))!
  assert(d[foundIndex1].0 == TestObjCKeyTy(10))
  assert(d[foundIndex1].1.value == 1010)
  assert(identity1 == unsafeBitCast(d, Word.self))

  d.removeAtIndex(foundIndex1)
  assert(identity1 != unsafeBitCast(d, Word.self))
  assert(isNativeDictionary(d))
  assert(d.count == 2)
  assert(d.indexForKey(TestObjCKeyTy(10)) == nil)
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.RemoveAtIndex") {
  var d = getBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isNativeDictionary(d))

  let foundIndex1 = d.indexForKey(TestBridgedKeyTy(10))!
  assert(d[foundIndex1].0 == TestBridgedKeyTy(10))
  assert(d[foundIndex1].1.value == 1010)
  assert(identity1 == unsafeBitCast(d, Word.self))

  d.removeAtIndex(foundIndex1)
  assert(identity1 == unsafeBitCast(d, Word.self))
  assert(isNativeDictionary(d))
  assert(d.count == 2)
  assert(d.indexForKey(TestBridgedKeyTy(10)) == nil)
}


DictionaryTestCase.test("BridgedFromObjC.Verbatim.RemoveValueForKey") {
  if true {
    var d = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d, Word.self)
    assert(isCocoaDictionary(d))

    var deleted: AnyObject? = d.removeValueForKey(TestObjCKeyTy(0))
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d, Word.self))
    assert(isCocoaDictionary(d))

    deleted = d.removeValueForKey(TestObjCKeyTy(10))
    assert(deleted!.value == 1010)
    var identity2 = unsafeBitCast(d, Word.self)
    assert(identity1 != identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 2)

    assert(d[TestObjCKeyTy(10)] == nil)
    assert(d[TestObjCKeyTy(20)]!.value == 1020)
    assert(d[TestObjCKeyTy(30)]!.value == 1030)
    assert(identity2 == unsafeBitCast(d, Word.self))
  }

  if true {
    var d1 = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    var d2 = d1
    assert(isCocoaDictionary(d1))
    assert(isCocoaDictionary(d2))

    var deleted: AnyObject? = d2.removeValueForKey(TestObjCKeyTy(0))
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 == unsafeBitCast(d2, Word.self))
    assert(isCocoaDictionary(d1))
    assert(isCocoaDictionary(d2))

    deleted = d2.removeValueForKey(TestObjCKeyTy(10))
    assert(deleted!.value == 1010)
    var identity2 = unsafeBitCast(d2, Word.self)
    assert(identity1 != identity2)
    assert(isCocoaDictionary(d1))
    assert(isNativeDictionary(d2))
    assert(d2.count == 2)

    assert(d1[TestObjCKeyTy(10)]!.value == 1010)
    assert(d1[TestObjCKeyTy(20)]!.value == 1020)
    assert(d1[TestObjCKeyTy(30)]!.value == 1030)
    assert(identity1 == unsafeBitCast(d1, Word.self))

    assert(d2[TestObjCKeyTy(10)] == nil)
    assert(d2[TestObjCKeyTy(20)]!.value == 1020)
    assert(d2[TestObjCKeyTy(30)]!.value == 1030)
    assert(identity2 == unsafeBitCast(d2, Word.self))
  }
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.RemoveValueForKey") {
  if true {
    var d = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d, Word.self)
    assert(isNativeDictionary(d))

    var deleted = d.removeValueForKey(TestBridgedKeyTy(0))
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d, Word.self))
    assert(isNativeDictionary(d))

    deleted = d.removeValueForKey(TestBridgedKeyTy(10))
    assert(deleted!.value == 1010)
    var identity2 = unsafeBitCast(d, Word.self)
    assert(identity1 == identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 2)

    assert(d[TestBridgedKeyTy(10)] == nil)
    assert(d[TestBridgedKeyTy(20)]!.value == 1020)
    assert(d[TestBridgedKeyTy(30)]!.value == 1030)
    assert(identity2 == unsafeBitCast(d, Word.self))
  }

  if true {
    var d1 = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)

    var d2 = d1
    assert(isNativeDictionary(d1))
    assert(isNativeDictionary(d2))

    var deleted = d2.removeValueForKey(TestBridgedKeyTy(0))
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity1 == unsafeBitCast(d2, Word.self))
    assert(isNativeDictionary(d1))
    assert(isNativeDictionary(d2))

    deleted = d2.removeValueForKey(TestBridgedKeyTy(10))
    assert(deleted!.value == 1010)
    var identity2 = unsafeBitCast(d2, Word.self)
    assert(identity1 != identity2)
    assert(isNativeDictionary(d1))
    assert(isNativeDictionary(d2))
    assert(d2.count == 2)

    assert(d1[TestBridgedKeyTy(10)]!.value == 1010)
    assert(d1[TestBridgedKeyTy(20)]!.value == 1020)
    assert(d1[TestBridgedKeyTy(30)]!.value == 1030)
    assert(identity1 == unsafeBitCast(d1, Word.self))

    assert(d2[TestBridgedKeyTy(10)] == nil)
    assert(d2[TestBridgedKeyTy(20)]!.value == 1020)
    assert(d2[TestBridgedKeyTy(30)]!.value == 1030)
    assert(identity2 == unsafeBitCast(d2, Word.self))
  }
}


DictionaryTestCase.test("BridgedFromObjC.Verbatim.RemoveAll") {
  if true {
    var d = getBridgedVerbatimDictionary([:])
    var identity1 = unsafeBitCast(d, Word.self)
    assert(isCocoaDictionary(d))
    assert(d.count == 0)

    d.removeAll()
    assert(identity1 == unsafeBitCast(d, Word.self))
    assert(d.count == 0)
  }

  if true {
    var d = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d, Word.self)
    assert(isCocoaDictionary(d))
    let originalCapacity = d.count
    assert(d.count == 3)
    assert(d[TestObjCKeyTy(10)]!.value == 1010)

    d.removeAll()
    assert(identity1 != unsafeBitCast(d, Word.self))
    assert(d._variantStorage.native.capacity < originalCapacity)
    assert(d.count == 0)
    assert(d[TestObjCKeyTy(10)] == nil)
  }

  if true {
    var d = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d, Word.self)
    assert(isCocoaDictionary(d))
    let originalCapacity = d.count
    assert(d.count == 3)
    assert(d[TestObjCKeyTy(10)]!.value == 1010)

    d.removeAll(keepCapacity: true)
    assert(identity1 != unsafeBitCast(d, Word.self))
    assert(d._variantStorage.native.capacity >= originalCapacity)
    assert(d.count == 0)
    assert(d[TestObjCKeyTy(10)] == nil)
  }

  if true {
    var d1 = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)
    assert(isCocoaDictionary(d1))
    let originalCapacity = d1.count
    assert(d1.count == 3)
    assert(d1[TestObjCKeyTy(10)]!.value == 1010)

    var d2 = d1
    d2.removeAll()
    var identity2 = unsafeBitCast(d2, Word.self)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[TestObjCKeyTy(10)]!.value == 1010)
    assert(d2._variantStorage.native.capacity < originalCapacity)
    assert(d2.count == 0)
    assert(d2[TestObjCKeyTy(10)] == nil)
  }

  if true {
    var d1 = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)
    assert(isCocoaDictionary(d1))
    let originalCapacity = d1.count
    assert(d1.count == 3)
    assert(d1[TestObjCKeyTy(10)]!.value == 1010)

    var d2 = d1
    d2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(d2, Word.self)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[TestObjCKeyTy(10)]!.value == 1010)
    assert(d2._variantStorage.native.capacity >= originalCapacity)
    assert(d2.count == 0)
    assert(d2[TestObjCKeyTy(10)] == nil)
  }
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.RemoveAll") {
  if true {
    var d = getBridgedNonverbatimDictionary([:])
    var identity1 = unsafeBitCast(d, Word.self)
    assert(isNativeDictionary(d))
    assert(d.count == 0)

    d.removeAll()
    assert(identity1 == unsafeBitCast(d, Word.self))
    assert(d.count == 0)
  }

  if true {
    var d = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d, Word.self)
    assert(isNativeDictionary(d))
    let originalCapacity = d.count
    assert(d.count == 3)
    assert(d[TestBridgedKeyTy(10)]!.value == 1010)

    d.removeAll()
    assert(identity1 != unsafeBitCast(d, Word.self))
    assert(d._variantStorage.native.capacity < originalCapacity)
    assert(d.count == 0)
    assert(d[TestBridgedKeyTy(10)] == nil)
  }

  if true {
    var d = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d, Word.self)
    assert(isNativeDictionary(d))
    let originalCapacity = d.count
    assert(d.count == 3)
    assert(d[TestBridgedKeyTy(10)]!.value == 1010)

    d.removeAll(keepCapacity: true)
    assert(identity1 == unsafeBitCast(d, Word.self))
    assert(d._variantStorage.native.capacity >= originalCapacity)
    assert(d.count == 0)
    assert(d[TestBridgedKeyTy(10)] == nil)
  }

  if true {
    var d1 = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)
    assert(isNativeDictionary(d1))
    let originalCapacity = d1.count
    assert(d1.count == 3)
    assert(d1[TestBridgedKeyTy(10)]!.value == 1010)

    var d2 = d1
    d2.removeAll()
    var identity2 = unsafeBitCast(d2, Word.self)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[TestBridgedKeyTy(10)]!.value == 1010)
    assert(d2._variantStorage.native.capacity < originalCapacity)
    assert(d2.count == 0)
    assert(d2[TestBridgedKeyTy(10)] == nil)
  }

  if true {
    var d1 = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d1, Word.self)
    assert(isNativeDictionary(d1))
    let originalCapacity = d1.count
    assert(d1.count == 3)
    assert(d1[TestBridgedKeyTy(10)]!.value == 1010)

    var d2 = d1
    d2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(d2, Word.self)
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[TestBridgedKeyTy(10)]!.value == 1010)
    assert(d2._variantStorage.native.capacity >= originalCapacity)
    assert(d2.count == 0)
    assert(d2[TestBridgedKeyTy(10)] == nil)
  }
}


DictionaryTestCase.test("BridgedFromObjC.Verbatim.Count") {
  var d = getBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isCocoaDictionary(d))

  assert(d.count == 3)
  assert(identity1 == unsafeBitCast(d, Word.self))
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.Count") {
  var d = getBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isNativeDictionary(d))

  assert(d.count == 3)
  assert(identity1 == unsafeBitCast(d, Word.self))
}


DictionaryTestCase.test("BridgedFromObjC.Verbatim.Generate") {
  var d = getBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isCocoaDictionary(d))

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value: AnyObject) = gen.next() {
    let kv = ((key as TestObjCKeyTy).value, (value as TestObjCValueTy).value)
    pairs.append(kv)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(identity1 == unsafeBitCast(d, Word.self))
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.Generate") {
  var d = getBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isNativeDictionary(d))

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value) = gen.next() {
    let kv = (key.value, value.value)
    pairs.append(kv)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(identity1 == unsafeBitCast(d, Word.self))
}

DictionaryTestCase.test("BridgedFromObjC.Verbatim.Generate_Empty") {
  var d = getBridgedVerbatimDictionary([:])
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isCocoaDictionary(d))

  var gen = d.generate()
  // Can not write code below because of
  // <rdar://problem/16811736> Optional tuples are broken as optionals regarding == comparison
  // assert(gen.next() == .None)
  assert(gen.next() == nil)
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(identity1 == unsafeBitCast(d, Word.self))
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.Generate_Empty") {
  var d = getBridgedNonverbatimDictionary([:])
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isNativeDictionary(d))

  var gen = d.generate()
  // Can not write code below because of
  // <rdar://problem/16811736> Optional tuples are broken as optionals regarding == comparison
  // assert(gen.next() == .None)
  assert(gen.next() == nil)
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(identity1 == unsafeBitCast(d, Word.self))
}


DictionaryTestCase.test("BridgedFromObjC.Verbatim.Generate_Huge") {
  var d = getHugeBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isCocoaDictionary(d))

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value: AnyObject) = gen.next() {
    let kv = ((key as TestObjCKeyTy).value, (value as TestObjCValueTy).value)
    pairs.append(kv)
  }
  var expectedPairs = Array<(Int, Int)>()
  for i in 1...32 {
    expectedPairs += [(i, 1000 + i)]
  }
  assert(equalsUnordered(pairs, expectedPairs))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(identity1 == unsafeBitCast(d, Word.self))
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.Generate_Huge") {
  var d = getHugeBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isNativeDictionary(d))

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value) = gen.next() {
    let kv = (key.value, value.value)
    pairs.append(kv)
  }
  var expectedPairs = Array<(Int, Int)>()
  for i in 1...32 {
    expectedPairs += [(i, 1000 + i)]
  }
  assert(equalsUnordered(pairs, expectedPairs))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(identity1 == unsafeBitCast(d, Word.self))
}


DictionaryTestCase.test("BridgedFromObjC.Verbatim.Generate_ParallelArray") {
autoreleasepool {
  // Add an autorelease pool because ParallelArrayDictionary autoreleases
  // values in objectForKey.

  var d = getParallelArrayBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isCocoaDictionary(d))

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value: AnyObject) = gen.next() {
    let kv = ((key as TestObjCKeyTy).value, (value as TestObjCValueTy).value)
    pairs.append(kv)
  }
  var expectedPairs = [ (10, 1111), (20, 1111), (30, 1111), (40, 1111) ]
  assert(equalsUnordered(pairs, expectedPairs))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(identity1 == unsafeBitCast(d, Word.self))
}
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.Generate_ParallelArray") {
autoreleasepool {
  // Add an autorelease pool because ParallelArrayDictionary autoreleases
  // values in objectForKey.

  var d = getParallelArrayBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Word.self)
  assert(isNativeDictionary(d))

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value) = gen.next() {
    let kv = (key.value, value.value)
    pairs.append(kv)
  }
  var expectedPairs = [ (10, 1111), (20, 1111), (30, 1111), (40, 1111) ]
  assert(equalsUnordered(pairs, expectedPairs))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(identity1 == unsafeBitCast(d, Word.self))
}
}


DictionaryTestCase.test("BridgedFromObjC.Verbatim.EqualityTest_Empty") {
  var d1 = getBridgedVerbatimEquatableDictionary([:])
  var identity1 = unsafeBitCast(d1, Word.self)
  assert(isCocoaDictionary(d1))

  var d2 = getBridgedVerbatimEquatableDictionary([:])
  var identity2 = unsafeBitCast(d2, Word.self)
  assert(isCocoaDictionary(d2))
  assert(identity1 != identity2)

  assert(d1 == d2)
  assert(identity1 == unsafeBitCast(d1, Word.self))
  assert(identity2 == unsafeBitCast(d2, Word.self))

  d2[TestObjCKeyTy(10)] = TestObjCEquatableValueTy(2010)
  assert(isNativeDictionary(d2))
  assert(identity2 != unsafeBitCast(d2, Word.self))
  identity2 = unsafeBitCast(d2, Word.self)

  assert(d1 != d2)
  assert(identity1 == unsafeBitCast(d1, Word.self))
  assert(identity2 == unsafeBitCast(d2, Word.self))
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.EqualityTest_Empty") {
  var d1 = getBridgedNonverbatimEquatableDictionary([:])
  var identity1 = unsafeBitCast(d1, Word.self)
  assert(isNativeDictionary(d1))

  var d2 = getBridgedNonverbatimEquatableDictionary([:])
  var identity2 = unsafeBitCast(d2, Word.self)
  assert(isNativeDictionary(d2))
  assert(identity1 != identity2)

  assert(d1 == d2)
  assert(identity1 == unsafeBitCast(d1, Word.self))
  assert(identity2 == unsafeBitCast(d2, Word.self))

  d2[TestBridgedKeyTy(10)] = TestBridgedEquatableValueTy(2010)
  assert(isNativeDictionary(d2))
  assert(identity2 == unsafeBitCast(d2, Word.self))

  assert(d1 != d2)
  assert(identity1 == unsafeBitCast(d1, Word.self))
  assert(identity2 == unsafeBitCast(d2, Word.self))
}


DictionaryTestCase.test("BridgedFromObjC.Verbatim.EqualityTest_Small") {
  func helper(nd1: Dictionary<Int, Int>, nd2: Dictionary<Int, Int>, expectedEq: Bool) {
    var d1 = getBridgedVerbatimEquatableDictionary(nd1)
    var identity1 = unsafeBitCast(d1, Word.self)
    assert(isCocoaDictionary(d1))

    var d2 = getBridgedVerbatimEquatableDictionary(nd2)
    var identity2 = unsafeBitCast(d2, Word.self)
    assert(isCocoaDictionary(d2))

    if true {
      var eq1 = (d1 == d2)
      assert(eq1 == expectedEq)

      var eq2 = (d2 == d1)
      assert(eq2 == expectedEq)

      var neq1 = (d1 != d2)
      assert(neq1 != expectedEq)

      var neq2 = (d2 != d1)
      assert(neq2 != expectedEq)
    }
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity2 == unsafeBitCast(d2, Word.self))

    d2[TestObjCKeyTy(1111)] = TestObjCEquatableValueTy(1111)
    d2[TestObjCKeyTy(1111)] = nil
    assert(isNativeDictionary(d2))
    assert(identity2 != unsafeBitCast(d2, Word.self))
    identity2 = unsafeBitCast(d2, Word.self)

    if true {
      var eq1 = (d1 == d2)
      assert(eq1 == expectedEq)

      var eq2 = (d2 == d1)
      assert(eq2 == expectedEq)

      var neq1 = (d1 != d2)
      assert(neq1 != expectedEq)

      var neq2 = (d2 != d1)
      assert(neq2 != expectedEq)
    }
    assert(identity1 == unsafeBitCast(d1, Word.self))
    assert(identity2 == unsafeBitCast(d2, Word.self))
  }

  helper([:], [:], true)

  helper([ 10: 1010 ],
         [ 10: 1010 ],
         true)

  helper([ 10: 1010, 20: 1020 ],
         [ 10: 1010, 20: 1020 ],
         true)

  helper([ 10: 1010, 20: 1020, 30: 1030 ],
         [ 10: 1010, 20: 1020, 30: 1030 ],
         true)

  helper([ 10: 1010, 20: 1020, 30: 1030 ],
         [ 10: 1010, 20: 1020, 1111: 1030 ],
         false)

  helper([ 10: 1010, 20: 1020, 30: 1030 ],
         [ 10: 1010, 20: 1020, 30: 1111 ],
         false)

  helper([ 10: 1010, 20: 1020, 30: 1030 ],
         [ 10: 1010, 20: 1020 ],
         false)

  helper([ 10: 1010, 20: 1020, 30: 1030 ],
         [ 10: 1010 ],
         false)

  helper([ 10: 1010, 20: 1020, 30: 1030 ],
         [:],
         false)

  helper([ 10: 1010, 20: 1020, 30: 1030 ],
         [ 10: 1010, 20: 1020, 30: 1030, 40: 1040 ],
         false)
}


DictionaryTestCase.test("BridgedFromObjC.Verbatim.ArrayOfDictionaries") {
  var nsa = NSMutableArray()
  for i in 0..<3 {
    nsa.addObject(
        getAsNSDictionary([ 10: 1010 + i, 20: 1020 + i, 30: 1030 + i ]))
  }

  var a = nsa as [AnyObject] as [Dictionary<NSObject, AnyObject>]
  for i in 0..<3 {
    var d = a[i]
    var gen = d.generate()
    var pairs = Array<(Int, Int)>()
    while let (key, value: AnyObject) = gen.next() {
      let kv = ((key as TestObjCKeyTy).value, (value as TestObjCValueTy).value)
      pairs.append(kv)
    }
    var expectedPairs = [ (10, 1010 + i), (20, 1020 + i), (30, 1030 + i) ]
    assert(equalsUnordered(pairs, expectedPairs))
  }
}

DictionaryTestCase.test("BridgedFromObjC.Nonverbatim.ArrayOfDictionaries") {
  var nsa = NSMutableArray()
  for i in 0..<3 {
    nsa.addObject(
        getAsNSDictionary([ 10: 1010 + i, 20: 1020 + i, 30: 1030 + i ]))
  }

  var a = nsa as [AnyObject] as [Dictionary<TestBridgedKeyTy, TestBridgedValueTy>]
  for i in 0..<3 {
    var d = a[i]
    var gen = d.generate()
    var pairs = Array<(Int, Int)>()
    while let (key, value) = gen.next() {
      let kv = (key.value, value.value)
      pairs.append(kv)
    }
    var expectedPairs = [ (10, 1010 + i), (20, 1020 + i), (30, 1030 + i) ]
    assert(equalsUnordered(pairs, expectedPairs))
  }
}


//===---
// Dictionary -> NSDictionary bridging tests.
//
// Key and Value are bridged verbatim.
//===---

func getBridgedNSDictionaryOfRefTypesBridgedVerbatim() -> NSDictionary {
  assert(_isBridgedVerbatimToObjectiveC(TestObjCKeyTy.self))
  assert(_isBridgedVerbatimToObjectiveC(TestObjCValueTy.self))

  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  let bridged
  = unsafeBitCast(_convertDictionaryToNSDictionary(d), NSDictionary.self)

  assert(isNativeNSDictionary(bridged))

  return bridged
}

func getBridgedEmptyNSDictionary() -> NSDictionary {
  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>()

  let bridged
    = unsafeBitCast(_convertDictionaryToNSDictionary(d), NSDictionary.self)
  assert(isNativeNSDictionary(bridged))

  return bridged
}


DictionaryTestCase.test("BridgedToObjC_Count") {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  assert(d.count == 3)
}

DictionaryTestCase.test("BridgedToObjC.ObjectForKey") {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  assert(d.objectForKey(nil) == nil)

  var v: AnyObject? = d.objectForKey(TestObjCKeyTy(10))
  assert((v as TestObjCValueTy).value == 1010)

  v = d.objectForKey(TestObjCKeyTy(20))
  assert((v as TestObjCValueTy).value == 1020)

  v = d.objectForKey(TestObjCKeyTy(30))
  assert((v as TestObjCValueTy).value == 1030)

  assert(d.objectForKey(TestObjCKeyTy(40)) == nil)
}

DictionaryTestCase.test("BridgedToObjC.KeyEnumerator.NextObject") {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key: AnyObject = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)
    let kv = ((key as TestObjCKeyTy).value, (value as TestObjCValueTy).value)
    pairs.append(kv)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  assert(enumerator.nextObject() == nil)
  assert(enumerator.nextObject() == nil)
  assert(enumerator.nextObject() == nil)
}

DictionaryTestCase.test("BridgedToObjC.KeyEnumerator.NextObject_Empty") {
  let d = getBridgedEmptyNSDictionary()
  let enumerator = d.keyEnumerator()

  assert(enumerator.nextObject() == nil)
  assert(enumerator.nextObject() == nil)
  assert(enumerator.nextObject() == nil)
}

DictionaryTestCase.test("BridgedToObjC.KeyEnumerator.FastEnumeration") {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  var pairs = slurpFastEnumeration(d, d.keyEnumerator())
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  pairs = slurpFastEnumerationFromObjC(d, d.keyEnumerator())
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
}

DictionaryTestCase.test("BridgedToObjC.KeyEnumerator.FastEnumeration_Empty") {
  let d = getBridgedEmptyNSDictionary()

  var pairs = slurpFastEnumeration(d, d.keyEnumerator())
  assert(equalsUnordered(pairs, []))

  pairs = slurpFastEnumerationFromObjC(d, d.keyEnumerator())
  assert(equalsUnordered(pairs, []))
}

DictionaryTestCase.test("BridgedToObjC.FastEnumeration") {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  var pairs = slurpFastEnumeration(d, d)
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  pairs = slurpFastEnumerationFromObjC(d, d)
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
}

DictionaryTestCase.test("BridgedToObjC.FastEnumeration_Empty") {
  let d = getBridgedEmptyNSDictionary()

  var pairs = slurpFastEnumeration(d, d)
  assert(equalsUnordered(pairs, []))

  pairs = slurpFastEnumerationFromObjC(d, d)
  assert(equalsUnordered(pairs, []))
}


//===---
// Dictionary -> NSDictionary bridging tests.
//
// Key type and value type are bridged non-verbatim.
//===---

func getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged() -> NSDictionary {
  assert(!_isBridgedVerbatimToObjectiveC(TestBridgedKeyTy.self))
  assert(!_isBridgedVerbatimToObjectiveC(TestBridgedValueTy.self))

  var d = Dictionary<TestBridgedKeyTy, TestBridgedValueTy>()
  d[TestBridgedKeyTy(10)] = TestBridgedValueTy(1010)
  d[TestBridgedKeyTy(20)] = TestBridgedValueTy(1020)
  d[TestBridgedKeyTy(30)] = TestBridgedValueTy(1030)

  let bridged = _convertDictionaryToNSDictionary(d)
  assert(isNativeNSDictionary(bridged))

  return bridged
}

DictionaryTestCase.test("BridgedToObjC.KeyValue_ValueTypesCustomBridged") {
  let d = getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key: AnyObject = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)
    let kv = ((key as TestObjCKeyTy).value, (value as TestObjCValueTy).value)
    pairs.append(kv)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
}

func getBridgedNSDictionaryOfKey_ValueTypeCustomBridged() -> NSDictionary {
  assert(!_isBridgedVerbatimToObjectiveC(TestBridgedKeyTy.self))
  assert(_isBridgedVerbatimToObjectiveC(TestObjCValueTy.self))

  var d = Dictionary<TestBridgedKeyTy, TestObjCValueTy>()
  d[TestBridgedKeyTy(10)] = TestObjCValueTy(1010)
  d[TestBridgedKeyTy(20)] = TestObjCValueTy(1020)
  d[TestBridgedKeyTy(30)] = TestObjCValueTy(1030)

  let bridged = _convertDictionaryToNSDictionary(d)
  assert(isNativeNSDictionary(bridged))

  return bridged
}

DictionaryTestCase.test("BridgedToObjC.Key_ValueTypeCustomBridged") {
  let d = getBridgedNSDictionaryOfKey_ValueTypeCustomBridged()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key: AnyObject = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)
    let kv = ((key as TestObjCKeyTy).value, (value as TestObjCValueTy).value)
    pairs.append(kv)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
}

func getBridgedNSDictionaryOfValue_ValueTypeCustomBridged() -> NSDictionary {
  assert(_isBridgedVerbatimToObjectiveC(TestObjCKeyTy.self))
  assert(!_isBridgedVerbatimToObjectiveC(TestBridgedValueTy.self))

  var d = Dictionary<TestObjCKeyTy, TestBridgedValueTy>()
  d[TestObjCKeyTy(10)] = TestBridgedValueTy(1010)
  d[TestObjCKeyTy(20)] = TestBridgedValueTy(1020)
  d[TestObjCKeyTy(30)] = TestBridgedValueTy(1030)

  let bridged = _convertDictionaryToNSDictionary(d)
  assert(isNativeNSDictionary(bridged))

  return bridged
}

DictionaryTestCase.test("BridgedToObjC.Value_ValueTypeCustomBridged") {
  let d = getBridgedNSDictionaryOfValue_ValueTypeCustomBridged()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key: AnyObject = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)
    let kv = ((key as TestObjCKeyTy).value, (value as TestObjCValueTy).value)
    pairs.append(kv)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
}


//===---
// NSDictionary -> Dictionary -> NSDictionary bridging tests.
//===---

func getRoundtripBridgedNSDictionary() -> NSDictionary {
  let keys = NSMutableArray()
  keys.addObject(TestObjCKeyTy(10))
  keys.addObject(TestObjCKeyTy(20))
  keys.addObject(TestObjCKeyTy(30))

  let values = NSMutableArray()
  values.addObject(TestObjCValueTy(1010))
  values.addObject(TestObjCValueTy(1020))
  values.addObject(TestObjCValueTy(1030))

  var nsd = NSDictionary(objects: values, forKeys: keys)

  var d: Dictionary<NSObject, AnyObject> = _convertNSDictionaryToDictionary(nsd)

  let bridgedBack = _convertDictionaryToNSDictionary(d)
  assert(isCocoaNSDictionary(bridgedBack))
  // FIXME: this should be true.
  //assert(unsafeBitCast(nsd, Word.self) == unsafeBitCast(bridgedBack, Word.self))

  return bridgedBack
}

DictionaryTestCase.test("BridgingRoundtrip") {
  let d = getRoundtripBridgedNSDictionary()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key: AnyObject = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)
    let kv = ((key as TestObjCKeyTy).value, (value as TestObjCValueTy).value)
    pairs.append(kv)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
}

//===---
// NSDictionary -> Dictionary implicit conversion.
//===---

DictionaryTestCase.test("NSDictionaryToDictionaryCoversion") {
  let keys = NSMutableArray()
  keys.addObject(TestObjCKeyTy(10))
  keys.addObject(TestObjCKeyTy(20))
  keys.addObject(TestObjCKeyTy(30))

  let values = NSMutableArray()
  values.addObject(TestObjCValueTy(1010))
  values.addObject(TestObjCValueTy(1020))
  values.addObject(TestObjCValueTy(1030))

  let nsd = NSDictionary(objects: values, forKeys: keys)

  let d: Dictionary = nsd

  var pairs = Array<(Int, Int)>()
  for (key, value: AnyObject) in d {
    let kv = ((key as TestObjCKeyTy).value, (value as TestObjCValueTy).value)
    pairs.append(kv)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
}

DictionaryTestCase.test("DictionaryToNSDictionaryCoversion") {
  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)
  let nsd: NSDictionary = d

  var pairs = slurpFastEnumeration(d, d)
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
}

//===---
// Dictionary upcasts
//===---

DictionaryTestCase.test("DictionaryUpcastEntryPoint") {
  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  var dAsAnyObject: Dictionary<NSObject, AnyObject> = _dictionaryUpCast(d)

  assert(dAsAnyObject.count == 3)
  var v: AnyObject? = dAsAnyObject[TestObjCKeyTy(10)]
  assert((v! as TestObjCValueTy).value == 1010)

  v = dAsAnyObject[TestObjCKeyTy(20)]
  assert((v! as TestObjCValueTy).value == 1020)

  v = dAsAnyObject[TestObjCKeyTy(30)]
  assert((v! as TestObjCValueTy).value == 1030)
}

DictionaryTestCase.test("DictionaryUpcast") {
  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  var dAsAnyObject: Dictionary<NSObject, AnyObject> = d

  assert(dAsAnyObject.count == 3)
  var v: AnyObject? = dAsAnyObject[TestObjCKeyTy(10)]
  assert((v! as TestObjCValueTy).value == 1010)

  v = dAsAnyObject[TestObjCKeyTy(20)]
  assert((v! as TestObjCValueTy).value == 1020)

  v = dAsAnyObject[TestObjCKeyTy(30)]
  assert((v! as TestObjCValueTy).value == 1030)
}

DictionaryTestCase.test("DictionaryUpcastBridgedEntryPoint") {
  var d = Dictionary<TestBridgedKeyTy, TestBridgedValueTy>(minimumCapacity: 32)
  d[TestBridgedKeyTy(10)] = TestBridgedValueTy(1010)
  d[TestBridgedKeyTy(20)] = TestBridgedValueTy(1020)
  d[TestBridgedKeyTy(30)] = TestBridgedValueTy(1030)

  if true {
    var dOO: Dictionary<NSObject, AnyObject> = _dictionaryBridgeToObjectiveC(d)

    assert(dOO.count == 3)
    var v: AnyObject? = dOO[TestObjCKeyTy(10)]
    assert((v! as TestBridgedValueTy).value == 1010)

    v = dOO[TestObjCKeyTy(20)]
    assert((v! as TestBridgedValueTy).value == 1020)

    v = dOO[TestObjCKeyTy(30)]
    assert((v! as TestBridgedValueTy).value == 1030)
  }

  if true {
    var dOV: Dictionary<NSObject, TestBridgedValueTy>
      = _dictionaryBridgeToObjectiveC(d)

    assert(dOV.count == 3)
    var v = dOV[TestObjCKeyTy(10)]
    assert(v!.value == 1010)

    v = dOV[TestObjCKeyTy(20)]
    assert(v!.value == 1020)

    v = dOV[TestObjCKeyTy(30)]
    assert(v!.value == 1030)
  }

  if true {
    var dVO: Dictionary<TestBridgedKeyTy, AnyObject>
      = _dictionaryBridgeToObjectiveC(d)

    assert(dVO.count == 3)
    var v: AnyObject? = dVO[TestBridgedKeyTy(10)]
    assert((v! as TestBridgedValueTy).value == 1010)

    v = dVO[TestBridgedKeyTy(20)]
    assert((v! as TestBridgedValueTy).value == 1020)

    v = dVO[TestBridgedKeyTy(30)]
    assert((v! as TestBridgedValueTy).value == 1030)
  }
}

DictionaryTestCase.test("DictionaryUpcastBridged") {
  var d = Dictionary<TestBridgedKeyTy, TestBridgedValueTy>(minimumCapacity: 32)
  d[TestBridgedKeyTy(10)] = TestBridgedValueTy(1010)
  d[TestBridgedKeyTy(20)] = TestBridgedValueTy(1020)
  d[TestBridgedKeyTy(30)] = TestBridgedValueTy(1030)

  if true {
    var dOO: Dictionary<NSObject, AnyObject> = d

    assert(dOO.count == 3)
    var v: AnyObject? = dOO[TestObjCKeyTy(10)]
    assert((v! as TestBridgedValueTy).value == 1010)

    v = dOO[TestObjCKeyTy(20)]
    assert((v! as TestBridgedValueTy).value == 1020)

    v = dOO[TestObjCKeyTy(30)]
    assert((v! as TestBridgedValueTy).value == 1030)
  }

  if true {
    var dOV: Dictionary<NSObject, TestBridgedValueTy> = d

    assert(dOV.count == 3)
    var v = dOV[TestObjCKeyTy(10)]
    assert(v!.value == 1010)

    v = dOV[TestObjCKeyTy(20)]
    assert(v!.value == 1020)

    v = dOV[TestObjCKeyTy(30)]
    assert(v!.value == 1030)
  }

  if true {
    var dVO: Dictionary<TestBridgedKeyTy, AnyObject> = d

    assert(dVO.count == 3)
    var v: AnyObject? = dVO[TestBridgedKeyTy(10)]
    assert((v! as TestBridgedValueTy).value == 1010)

    v = dVO[TestBridgedKeyTy(20)]
    assert((v! as TestBridgedValueTy).value == 1020)

    v = dVO[TestBridgedKeyTy(30)]
    assert((v! as TestBridgedValueTy).value == 1030)
  }
}

//===---
// Dictionary downcasts
//===---

DictionaryTestCase.test("DictionaryDowncastEntryPoint") {
  var d = Dictionary<NSObject, AnyObject>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  // Successful downcast.
  let dCC: Dictionary<TestObjCKeyTy, TestObjCValueTy> = _dictionaryDownCast(d)
  assert(dCC.count == 3)
  var v = dCC[TestObjCKeyTy(10)]
  assert(v!.value == 1010)

  v = dCC[TestObjCKeyTy(20)]
  assert(v!.value == 1020)

  v = dCC[TestObjCKeyTy(30)]
  assert(v!.value == 1030)
}

DictionaryTestCase.test("DictionaryDowncast") {
  var d = Dictionary<NSObject, AnyObject>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  // Successful downcast.
  let dCC = d as Dictionary<TestObjCKeyTy, TestObjCValueTy>
  assert(dCC.count == 3)
  var v = dCC[TestObjCKeyTy(10)]
  assert(v!.value == 1010)

  v = dCC[TestObjCKeyTy(20)]
  assert(v!.value == 1020)

  v = dCC[TestObjCKeyTy(30)]
  assert(v!.value == 1030)
}

DictionaryTestCase.test("DictionaryDowncastConditionalEntryPoint") {
  var d = Dictionary<NSObject, AnyObject>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  // Successful downcast.
  if let dCC: Dictionary<TestObjCKeyTy, TestObjCValueTy>
       = _dictionaryDownCastConditional(d) {
    assert(dCC.count == 3)
    var v = dCC[TestObjCKeyTy(10)]
    assert(v!.value == 1010)

    v = dCC[TestObjCKeyTy(20)]
    assert(v!.value == 1020)

    v = dCC[TestObjCKeyTy(30)]
    assert(v!.value == 1030)
  } else {
    assert(false)
  }

  // Unsuccessful downcast
  d["hello"] = 17
  if let dCC: Dictionary<TestObjCKeyTy, TestObjCValueTy>
       = _dictionaryDownCastConditional(d) {
    assert(false)
  }
}

DictionaryTestCase.test("DictionaryDowncastConditional") {
  var d = Dictionary<NSObject, AnyObject>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  // Successful downcast.
  if let dCC = d as? Dictionary<TestObjCKeyTy, TestObjCValueTy> {
    assert(dCC.count == 3)
    var v = dCC[TestObjCKeyTy(10)]
    assert(v!.value == 1010)

    v = dCC[TestObjCKeyTy(20)]
    assert(v!.value == 1020)

    v = dCC[TestObjCKeyTy(30)]
    assert(v!.value == 1030)
  } else {
    assert(false)
  }

  // Unsuccessful downcast
  d["hello"] = 17
  if let dCC = d as? Dictionary<TestObjCKeyTy, TestObjCValueTy> {
    assert(false)
  }
}

DictionaryTestCase.test("DictionaryBridgeFromObjectiveCEntryPoint") {
  var d = Dictionary<NSObject, AnyObject>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  // Successful downcast.
  let dCV: Dictionary<TestObjCKeyTy, TestBridgedValueTy>
    = _dictionaryBridgeFromObjectiveC(d)
  if true {
    assert(dCV.count == 3)
    var v = dCV[TestObjCKeyTy(10)]
    assert(v!.value == 1010)

    v = dCV[TestObjCKeyTy(20)]
    assert(v!.value == 1020)

    v = dCV[TestObjCKeyTy(30)]
    assert(v!.value == 1030)
  }

  // Successful downcast.
  let dVC: Dictionary<TestBridgedKeyTy, TestObjCValueTy>
    = _dictionaryBridgeFromObjectiveC(d)
  if true {
    assert(dVC.count == 3)
    var v = dVC[TestBridgedKeyTy(10)]
    assert(v!.value == 1010)

    v = dVC[TestBridgedKeyTy(20)]
    assert(v!.value == 1020)

    v = dVC[TestBridgedKeyTy(30)]
    assert(v!.value == 1030)
  }

  // Successful downcast.
  let dVV: Dictionary<TestBridgedKeyTy, TestBridgedValueTy>
        = _dictionaryBridgeFromObjectiveC(d)
  if true {
    assert(dVV.count == 3)
    var v = dVV[TestBridgedKeyTy(10)]
    assert(v!.value == 1010)

    v = dVV[TestBridgedKeyTy(20)]
    assert(v!.value == 1020)

    v = dVV[TestBridgedKeyTy(30)]
    assert(v!.value == 1030)
  }
}

DictionaryTestCase.test("DictionaryBridgeFromObjectiveC") {
  var d = Dictionary<NSObject, AnyObject>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  // Successful downcast.
  let dCV = d as Dictionary<TestObjCKeyTy, TestBridgedValueTy>
  if true {
    assert(dCV.count == 3)
    var v = dCV[TestObjCKeyTy(10)]
    assert(v!.value == 1010)

    v = dCV[TestObjCKeyTy(20)]
    assert(v!.value == 1020)

    v = dCV[TestObjCKeyTy(30)]
    assert(v!.value == 1030)
  }

  // Successful downcast.
  let dVC = d as Dictionary<TestBridgedKeyTy, TestObjCValueTy>
  if true {
    assert(dVC.count == 3)
    var v = dVC[TestBridgedKeyTy(10)]
    assert(v!.value == 1010)

    v = dVC[TestBridgedKeyTy(20)]
    assert(v!.value == 1020)

    v = dVC[TestBridgedKeyTy(30)]
    assert(v!.value == 1030)
  }

  // Successful downcast.
  let dVV = d as Dictionary<TestBridgedKeyTy, TestBridgedValueTy>
  if true {
    assert(dVV.count == 3)
    var v = dVV[TestBridgedKeyTy(10)]
    assert(v!.value == 1010)

    v = dVV[TestBridgedKeyTy(20)]
    assert(v!.value == 1020)

    v = dVV[TestBridgedKeyTy(30)]
    assert(v!.value == 1030)
  }
}

DictionaryTestCase.test("DictionaryBridgeFromObjectiveCConditionalEntryPoint") {
  var d = Dictionary<NSObject, AnyObject>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  // Successful downcast.
  if let dCV: Dictionary<TestObjCKeyTy, TestBridgedValueTy>
       = _dictionaryBridgeFromObjectiveCConditional(d) {
    assert(dCV.count == 3)
    var v = dCV[TestObjCKeyTy(10)]
    assert(v!.value == 1010)

    v = dCV[TestObjCKeyTy(20)]
    assert(v!.value == 1020)

    v = dCV[TestObjCKeyTy(30)]
    assert(v!.value == 1030)
  } else {
    assert(false)
  }

  // Successful downcast.
  if let dVC: Dictionary<TestBridgedKeyTy, TestObjCValueTy>
       = _dictionaryBridgeFromObjectiveCConditional(d) {
    assert(dVC.count == 3)
    var v = dVC[TestBridgedKeyTy(10)]
    assert(v!.value == 1010)

    v = dVC[TestBridgedKeyTy(20)]
    assert(v!.value == 1020)

    v = dVC[TestBridgedKeyTy(30)]
    assert(v!.value == 1030)
  } else {
    assert(false)
  }

  // Successful downcast.
  if let dVV: Dictionary<TestBridgedKeyTy, TestBridgedValueTy>
       = _dictionaryBridgeFromObjectiveCConditional(d) {
    assert(dVV.count == 3)
    var v = dVV[TestBridgedKeyTy(10)]
    assert(v!.value == 1010)

    v = dVV[TestBridgedKeyTy(20)]
    assert(v!.value == 1020)

    v = dVV[TestBridgedKeyTy(30)]
    assert(v!.value == 1030)
  } else {
    assert(false)
  }

  // Unsuccessful downcasts
  d["hello"] = 17
  if let dCV: Dictionary<TestObjCKeyTy, TestBridgedValueTy>
       = _dictionaryBridgeFromObjectiveCConditional(d) {
    assert(false)
  }
  if let dVC: Dictionary<TestBridgedKeyTy, TestObjCValueTy>
       = _dictionaryBridgeFromObjectiveCConditional(d) {
    assert(false)
  }
  if let dVV: Dictionary<TestBridgedKeyTy, TestBridgedValueTy>
       = _dictionaryBridgeFromObjectiveCConditional(d) {
    assert(false)
  }
}

DictionaryTestCase.test("DictionaryBridgeFromObjectiveCConditional") {
  var d = Dictionary<NSObject, AnyObject>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  // Successful downcast.
  if let dCV = d as? Dictionary<TestObjCKeyTy, TestBridgedValueTy>  {
    assert(dCV.count == 3)
    var v = dCV[TestObjCKeyTy(10)]
    assert(v!.value == 1010)

    v = dCV[TestObjCKeyTy(20)]
    assert(v!.value == 1020)

    v = dCV[TestObjCKeyTy(30)]
    assert(v!.value == 1030)
  } else {
    assert(false)
  }

  // Successful downcast.
  if let dVC = d as? Dictionary<TestBridgedKeyTy, TestObjCValueTy> {
    assert(dVC.count == 3)
    var v = dVC[TestBridgedKeyTy(10)]
    assert(v!.value == 1010)

    v = dVC[TestBridgedKeyTy(20)]
    assert(v!.value == 1020)

    v = dVC[TestBridgedKeyTy(30)]
    assert(v!.value == 1030)
  } else {
    assert(false)
  }

  // Successful downcast.
  if let dVV = d as? Dictionary<TestBridgedKeyTy, TestBridgedValueTy> {
    assert(dVV.count == 3)
    var v = dVV[TestBridgedKeyTy(10)]
    assert(v!.value == 1010)

    v = dVV[TestBridgedKeyTy(20)]
    assert(v!.value == 1020)

    v = dVV[TestBridgedKeyTy(30)]
    assert(v!.value == 1030)
  } else {
    assert(false)
  }

  // Unsuccessful downcasts
  d["hello"] = 17
  if let dCV = d as? Dictionary<TestObjCKeyTy, TestBridgedValueTy> {
    assert(false)
  }
  if let dVC = d as? Dictionary<TestBridgedKeyTy, TestObjCValueTy> {
    assert(false)
  }
  if let dVV = d as? Dictionary<TestBridgedKeyTy, TestBridgedValueTy> {
    assert(false)
  }
}

//===---
// Tests for APIs implemented strictly based on public interface.  We only need
// to test them once, not for every storage type.
//===---

func getDerivedAPIsDictionary() -> Dictionary<Int, Int> {
  var d = Dictionary<Int, Int>(minimumCapacity: 10)
  d[10] = 1010
  d[20] = 1020
  d[30] = 1030
  return d
}

var DictionaryDerivedAPIs = TestCase("DictionaryDerivedAPIs")

DictionaryDerivedAPIs.test("isEmpty") {
  if true {
    var empty = Dictionary<Int, Int>()
    expectTrue(empty.isEmpty)
  }
  if true {
    var d = getDerivedAPIsDictionary()
    expectFalse(d.isEmpty)
  }
}

DictionaryDerivedAPIs.test("keys") {
  if true {
    var empty = Dictionary<Int, Int>()
    var keys = Array(empty.keys)
    expectTrue(equalsUnordered(keys, []))
  }
  if true {
    var d = getDerivedAPIsDictionary()
    var keys = Array(d.keys)
    expectTrue(equalsUnordered(keys, [ 10, 20, 30 ]))
  }
}

DictionaryDerivedAPIs.test("values") {
  if true {
    var empty = Dictionary<Int, Int>()
    var values = Array(empty.values)
    expectTrue(equalsUnordered(values, []))
  }
  if true {
    var d = getDerivedAPIsDictionary()

    var values = Array(d.values)
    expectTrue(equalsUnordered(values, [ 1010, 1020, 1030 ]))

    d[11] = 1010
    values = Array(d.values)
    expectTrue(equalsUnordered(values, [ 1010, 1010, 1020, 1030 ]))
  }
}

var ObjCThunks = TestCase("ObjCThunks")

class ObjCThunksHelper : NSObject {
  dynamic func acceptArrayBridgedVerbatim(array: [TestObjCValueTy]) {
    expectEqual(10, array[0].value)
    expectEqual(20, array[1].value)
    expectEqual(30, array[2].value)
  }

  dynamic func acceptArrayBridgedNonverbatim(array: [TestBridgedValueTy]) {
    // Can not check elements because doing so would bridge them.
    expectEqual(3, array.count)
  }

  dynamic func returnArrayBridgedVerbatim() -> [TestObjCValueTy] {
    return [ TestObjCValueTy(10), TestObjCValueTy(20),
        TestObjCValueTy(30) ]
  }

  dynamic func returnArrayBridgedNonverbatim() -> [TestBridgedValueTy] {
    return [ TestBridgedValueTy(10), TestBridgedValueTy(20),
        TestBridgedValueTy(30) ]
  }

  dynamic func acceptDictionaryBridgedVerbatim(
      d: [TestObjCKeyTy : TestObjCValueTy]) {
    expectEqual(3, d.count)
    expectEqual(1010, d[TestObjCKeyTy(10)]!.value)
    expectEqual(1020, d[TestObjCKeyTy(20)]!.value)
    expectEqual(1030, d[TestObjCKeyTy(30)]!.value)
  }

  dynamic func acceptDictionaryBridgedNonverbatim(
      d: [TestBridgedKeyTy : TestBridgedValueTy]) {
    expectEqual(3, d.count)
    // Can not check elements because doing so would bridge them.
  }

  dynamic func returnDictionaryBridgedVerbatim() ->
      [TestObjCKeyTy : TestObjCValueTy] {
    return [
        TestObjCKeyTy(10): TestObjCValueTy(1010),
        TestObjCKeyTy(20): TestObjCValueTy(1020),
        TestObjCKeyTy(30): TestObjCValueTy(1030),
    ]
  }

  dynamic func returnDictionaryBridgedNonverbatim() ->
      [TestBridgedKeyTy : TestBridgedValueTy] {
    return [
        TestBridgedKeyTy(10): TestBridgedValueTy(1010),
        TestBridgedKeyTy(20): TestBridgedValueTy(1020),
        TestBridgedKeyTy(30): TestBridgedValueTy(1030),
    ]
  }
}

ObjCThunks.test("Array/Accept") {
  var helper = ObjCThunksHelper()

  if true {
    helper.acceptArrayBridgedVerbatim(
        [ TestObjCValueTy(10), TestObjCValueTy(20), TestObjCValueTy(30) ])
  }
  if true {
    TestBridgedValueTy.bridgeOperations = 0
    helper.acceptArrayBridgedNonverbatim(
        [ TestBridgedValueTy(10), TestBridgedValueTy(20),
          TestBridgedValueTy(30) ])
    expectEqual(0, TestBridgedValueTy.bridgeOperations)
  }
}

ObjCThunks.test("Array/Return") {
  var helper = ObjCThunksHelper()

  if true {
    let a = helper.returnArrayBridgedVerbatim()
    expectEqual(10, a[0].value)
    expectEqual(20, a[1].value)
    expectEqual(30, a[2].value)
  }
  if true {
    TestBridgedValueTy.bridgeOperations = 0
    let a = helper.returnArrayBridgedNonverbatim()
    expectEqual(0, TestBridgedValueTy.bridgeOperations)

    TestBridgedValueTy.bridgeOperations = 0
    expectEqual(10, a[0].value)
    expectEqual(20, a[1].value)
    expectEqual(30, a[2].value)
    expectEqual(0, TestBridgedValueTy.bridgeOperations)
  }
}

ObjCThunks.test("Dictionary/Accept") {
  var helper = ObjCThunksHelper()

  if true {
    helper.acceptDictionaryBridgedVerbatim(
        [ TestObjCKeyTy(10): TestObjCValueTy(1010),
          TestObjCKeyTy(20): TestObjCValueTy(1020),
          TestObjCKeyTy(30): TestObjCValueTy(1030) ])
  }
  if true {
    TestBridgedKeyTy.bridgeOperations = 0
    TestBridgedValueTy.bridgeOperations = 0
    helper.acceptDictionaryBridgedNonverbatim(
        [ TestBridgedKeyTy(10): TestBridgedValueTy(1010),
          TestBridgedKeyTy(20): TestBridgedValueTy(1020),
          TestBridgedKeyTy(30): TestBridgedValueTy(1030) ])
    expectEqual(0, TestBridgedKeyTy.bridgeOperations)
    expectEqual(0, TestBridgedValueTy.bridgeOperations)
  }
}

ObjCThunks.test("Dictionary/Return") {
  var helper = ObjCThunksHelper()

  if true {
    let d = helper.returnDictionaryBridgedVerbatim()
    expectEqual(3, d.count)
    expectEqual(1010, d[TestObjCKeyTy(10)]!.value)
    expectEqual(1020, d[TestObjCKeyTy(20)]!.value)
    expectEqual(1030, d[TestObjCKeyTy(30)]!.value)
  }
  if true {
    TestBridgedKeyTy.bridgeOperations = 0
    TestBridgedValueTy.bridgeOperations = 0
    let d = helper.returnDictionaryBridgedNonverbatim()
    expectEqual(0, TestBridgedKeyTy.bridgeOperations)
    expectEqual(0, TestBridgedValueTy.bridgeOperations)

    TestBridgedKeyTy.bridgeOperations = 0
    TestBridgedValueTy.bridgeOperations = 0
    expectEqual(3, d.count)
    expectEqual(1010, d[TestBridgedKeyTy(10)]!.value)
    expectEqual(1020, d[TestBridgedKeyTy(20)]!.value)
    expectEqual(1030, d[TestBridgedKeyTy(30)]!.value)
    expectEqual(0, TestBridgedKeyTy.bridgeOperations)
    expectEqual(0, TestBridgedValueTy.bridgeOperations)
  }
}

//===---
// Misc tests.
//===---

DictionaryTestCase.test("misc") {
  if true {
    // Dictionary literal
    var dict = [ "Hello": 1, "World": 2 ]

    // Insertion
    dict["Swift"] = 3

    // Access
    expectOptionalEqual(1, dict["Hello"])
    expectOptionalEqual(2, dict["World"])
    expectOptionalEqual(3, dict["Swift"])
    expectEmpty(dict["Universe"])

    // Overwriting existing value
    dict["Hello"] = 0
    expectOptionalEqual(0, dict["Hello"])
    expectOptionalEqual(2, dict["World"])
    expectOptionalEqual(3, dict["Swift"])
    expectEmpty(dict["Universe"])
  }

  if true {
    // Dictionaries with other types
    var d = [ 1.2: 1, 2.6: 2 ]
    d[3.3] = 3
    expectOptionalEqual(1, d[1.2])
    expectOptionalEqual(2, d[2.6])
    expectOptionalEqual(3, d[3.3])
  }

  if true {
    var d = Dictionary<String, Int>(minimumCapacity: 13)
    d["one"] = 1
    d["two"] = 2
    d["three"] = 3
    d["four"] = 4
    d["five"] = 5
    expectOptionalEqual(1, d["one"])
    expectOptionalEqual(2, d["two"])
    expectOptionalEqual(3, d["three"])
    expectOptionalEqual(4, d["four"])
    expectOptionalEqual(5, d["five"])

    // Iterate over (key, value) tuples as a silly copy
    var d3 = Dictionary<String,Int>(minimumCapacity: 13)

    for (k, v) in d {
      d3[k] = v
    }
    expectOptionalEqual(1, d3["one"])
    expectOptionalEqual(2, d3["two"])
    expectOptionalEqual(3, d3["three"])
    expectOptionalEqual(4, d3["four"])
    expectOptionalEqual(5, d3["five"])

    expectEqual(3, d.values[find(d.keys, "three")!])
    expectEqual(4, d.values[find(d.keys, "four")!])

    expectEqual(3, d3.values[find(d.keys, "three")!])
    expectEqual(4, d3.values[find(d.keys, "four")!])
  }
}

DictionaryTestCase.test("noLeaks") {
  expectEqual(0, keyCount) { "key leak" }
  expectEqual(0, valueCount) { "value leak" }

  // FIXME: We should not be leaking
  // <rdar://problem/17944094> Dictionary.swift leaks again
  expectEqual(9, TestObjCKeyTy.objectCount) { "key leak" }
  expectEqual(6, TestObjCValueTy.objectCount) { "value leak" }
}

runAllTests()


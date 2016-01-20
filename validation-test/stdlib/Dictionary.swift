// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %S/../../utils/gyb %s -o %t/main.swift
// RUN: %S/../../utils/line-directive %t/main.swift -- %target-build-swift %S/Inputs/DictionaryKeyValueTypes.swift %t/main.swift -o %t/Dictionary -Xfrontend -disable-access-control
//
// RUN: %S/../../utils/line-directive %t/main.swift -- %target-run %t/Dictionary
// REQUIRES: executable_test

#if os(OSX)
import Darwin
#elseif os(Linux) || os(FreeBSD)
import Glibc
#endif
import StdlibUnittest

// Check that the generic parameters are called 'Key' and 'Value'.
protocol TestProtocol1 {}

extension Dictionary where Key : TestProtocol1, Value : TestProtocol1 {
  var _keyValueAreTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension DictionaryIndex where Key : TestProtocol1, Value : TestProtocol1 {
  var _keyValueAreTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension DictionaryGenerator
  where Key : TestProtocol1, Value : TestProtocol1 {

  var _keyValueAreTestProtocol1: Bool {
    fatalError("not implemented")
  }
}


var DictionaryTestSuite = TestSuite("Dictionary")

DictionaryTestSuite.test("sizeof") {
  var dict = [ 1: "meow", 2: "meow" ]
#if arch(i386) || arch(arm)
  expectEqual(4, sizeofValue(dict))
#else
  expectEqual(8, sizeofValue(dict))
#endif
}

DictionaryTestSuite.test("valueDestruction") {
  var d1 = Dictionary<Int, TestValueTy>()
  for i in 100...110 {
    d1[i] = TestValueTy(i)
  }

  var d2 = Dictionary<TestKeyTy, TestValueTy>()
  for i in 100...110 {
    d2[TestKeyTy(i)] = TestValueTy(i)
  }
}

DictionaryTestSuite.test("COW.Smoke") {
  var d1 = Dictionary<TestKeyTy, TestValueTy>(minimumCapacity: 10)
  var identity1 = unsafeBitCast(d1, Int.self)

  d1[TestKeyTy(10)] = TestValueTy(1010)
  d1[TestKeyTy(20)] = TestValueTy(1020)
  d1[TestKeyTy(30)] = TestValueTy(1030)

  var d2 = d1
  _fixLifetime(d2)
  assert(identity1 == unsafeBitCast(d2, Int.self))

  d2[TestKeyTy(40)] = TestValueTy(2040)
  assert(identity1 != unsafeBitCast(d2, Int.self))

  d1[TestKeyTy(50)] = TestValueTy(1050)
  assert(identity1 == unsafeBitCast(d1, Int.self))

  // Keep variables alive.
  _fixLifetime(d1)
  _fixLifetime(d2)
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


DictionaryTestSuite.test("COW.Fast.IndexesDontAffectUniquenessCheck") {
  var d = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d, Int.self)

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex != endIndex)
  assert(startIndex < endIndex)
  assert(startIndex <= endIndex)
  assert(!(startIndex >= endIndex))
  assert(!(startIndex > endIndex))

  assert(identity1 == unsafeBitCast(d, Int.self))

  d[40] = 2040
  assert(identity1 == unsafeBitCast(d, Int.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

DictionaryTestSuite.test("COW.Slow.IndexesDontAffectUniquenessCheck") {
  var d = getCOWSlowDictionary()
  var identity1 = unsafeBitCast(d, Int.self)

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex != endIndex)
  assert(startIndex < endIndex)
  assert(startIndex <= endIndex)
  assert(!(startIndex >= endIndex))
  assert(!(startIndex > endIndex))
  assert(identity1 == unsafeBitCast(d, Int.self))

  d[TestKeyTy(40)] = TestValueTy(2040)
  assert(identity1 == unsafeBitCast(d, Int.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}


DictionaryTestSuite.test("COW.Fast.SubscriptWithIndexDoesNotReallocate") {
  var d = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d, Int.self)

  var startIndex = d.startIndex
  let empty = startIndex == d.endIndex
  assert((d.startIndex < d.endIndex) == !empty)
  assert(d.startIndex <= d.endIndex)
  assert((d.startIndex >= d.endIndex) == empty)
  assert(!(d.startIndex > d.endIndex))
  assert(identity1 == unsafeBitCast(d, Int.self))

  assert(d[startIndex].1 != 0)
  assert(identity1 == unsafeBitCast(d, Int.self))
}

DictionaryTestSuite.test("COW.Slow.SubscriptWithIndexDoesNotReallocate") {
  var d = getCOWSlowDictionary()
  var identity1 = unsafeBitCast(d, Int.self)

  var startIndex = d.startIndex
  let empty = startIndex == d.endIndex
  assert((d.startIndex < d.endIndex) == !empty)
  assert(d.startIndex <= d.endIndex)
  assert((d.startIndex >= d.endIndex) == empty)
  assert(!(d.startIndex > d.endIndex))
  assert(identity1 == unsafeBitCast(d, Int.self))

  assert(d[startIndex].1.value != 0)
  assert(identity1 == unsafeBitCast(d, Int.self))
}


DictionaryTestSuite.test("COW.Fast.SubscriptWithKeyDoesNotReallocate") {
  var d = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d, Int.self)

  assert(d[10]! == 1010)
  assert(identity1 == unsafeBitCast(d, Int.self))

  // Insert a new key-value pair.
  d[40] = 2040
  assert(identity1 == unsafeBitCast(d, Int.self))
  assert(d.count == 4)
  assert(d[10]! == 1010)
  assert(d[20]! == 1020)
  assert(d[30]! == 1030)
  assert(d[40]! == 2040)

  // Overwrite a value in existing binding.
  d[10] = 2010
  assert(identity1 == unsafeBitCast(d, Int.self))
  assert(d.count == 4)
  assert(d[10]! == 2010)
  assert(d[20]! == 1020)
  assert(d[30]! == 1030)
  assert(d[40]! == 2040)

  // Delete an existing key.
  d[10] = nil
  assert(identity1 == unsafeBitCast(d, Int.self))
  assert(d.count == 3)
  assert(d[20]! == 1020)
  assert(d[30]! == 1030)
  assert(d[40]! == 2040)

  // Try to delete a key that does not exist.
  d[42] = nil
  assert(identity1 == unsafeBitCast(d, Int.self))
  assert(d.count == 3)
  assert(d[20]! == 1020)
  assert(d[30]! == 1030)
  assert(d[40]! == 2040)

  do {
    var d2: [MinimalHashableValue : OpaqueValue<Int>] = [:]
    MinimalHashableValue.timesEqualEqualWasCalled = 0
    MinimalHashableValue.timesHashValueWasCalled = 0
    expectEmpty(d2[MinimalHashableValue(42)])

    // If the dictionary is empty, we shouldn't be computing the hash value of
    // the provided key.
    expectEqual(0, MinimalHashableValue.timesEqualEqualWasCalled)
    expectEqual(0, MinimalHashableValue.timesHashValueWasCalled)
  }
}

DictionaryTestSuite.test("COW.Slow.SubscriptWithKeyDoesNotReallocate") {
  var d = getCOWSlowDictionary()
  var identity1 = unsafeBitCast(d, Int.self)

  assert(d[TestKeyTy(10)]!.value == 1010)
  assert(identity1 == unsafeBitCast(d, Int.self))

  // Insert a new key-value pair.
  d[TestKeyTy(40)] = TestValueTy(2040)
  assert(identity1 == unsafeBitCast(d, Int.self))
  assert(d.count == 4)
  assert(d[TestKeyTy(10)]!.value == 1010)
  assert(d[TestKeyTy(20)]!.value == 1020)
  assert(d[TestKeyTy(30)]!.value == 1030)
  assert(d[TestKeyTy(40)]!.value == 2040)

  // Overwrite a value in existing binding.
  d[TestKeyTy(10)] = TestValueTy(2010)
  assert(identity1 == unsafeBitCast(d, Int.self))
  assert(d.count == 4)
  assert(d[TestKeyTy(10)]!.value == 2010)
  assert(d[TestKeyTy(20)]!.value == 1020)
  assert(d[TestKeyTy(30)]!.value == 1030)
  assert(d[TestKeyTy(40)]!.value == 2040)

  // Delete an existing key.
  d[TestKeyTy(10)] = nil
  assert(identity1 == unsafeBitCast(d, Int.self))
  assert(d.count == 3)
  assert(d[TestKeyTy(20)]!.value == 1020)
  assert(d[TestKeyTy(30)]!.value == 1030)
  assert(d[TestKeyTy(40)]!.value == 2040)

  // Try to delete a key that does not exist.
  d[TestKeyTy(42)] = nil
  assert(identity1 == unsafeBitCast(d, Int.self))
  assert(d.count == 3)
  assert(d[TestKeyTy(20)]!.value == 1020)
  assert(d[TestKeyTy(30)]!.value == 1030)
  assert(d[TestKeyTy(40)]!.value == 2040)

  do {
    var d2: [MinimalHashableClass : OpaqueValue<Int>] = [:]
    MinimalHashableClass.timesEqualEqualWasCalled = 0
    MinimalHashableClass.timesHashValueWasCalled = 0

    expectEmpty(d2[MinimalHashableClass(42)])

    // If the dictionary is empty, we shouldn't be computing the hash value of
    // the provided key.
    expectEqual(0, MinimalHashableClass.timesEqualEqualWasCalled)
    expectEqual(0, MinimalHashableClass.timesHashValueWasCalled)
  }
}


DictionaryTestSuite.test("COW.Fast.UpdateValueForKeyDoesNotReallocate") {
  do {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    // Insert a new key-value pair.
    assert(d1.updateValue(2040, forKey: 40) == .None)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(d1[40]! == 2040)

    // Overwrite a value in existing binding.
    assert(d1.updateValue(2010, forKey: 10)! == 1010)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(d1[10]! == 2010)
  }

  do {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    var d2 = d1
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 == unsafeBitCast(d2, Int.self))

    // Insert a new key-value pair.
    d2.updateValue(2040, forKey: 40)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 != unsafeBitCast(d2, Int.self))

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
    _fixLifetime(d1)
    _fixLifetime(d2)
  }

  do {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    var d2 = d1
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 == unsafeBitCast(d2, Int.self))

    // Overwrite a value in existing binding.
    d2.updateValue(2010, forKey: 10)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 != unsafeBitCast(d2, Int.self))

    assert(d1.count == 3)
    assert(d1[10]! == 1010)
    assert(d1[20]! == 1020)
    assert(d1[30]! == 1030)

    assert(d2.count == 3)
    assert(d2[10]! == 2010)
    assert(d2[20]! == 1020)
    assert(d2[30]! == 1030)

    // Keep variables alive.
    _fixLifetime(d1)
    _fixLifetime(d2)
  }
}

DictionaryTestSuite.test("COW.Slow.AddDoesNotReallocate") {
  do {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    // Insert a new key-value pair.
    assert(d1.updateValue(TestValueTy(2040), forKey: TestKeyTy(40)) == nil)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(d1.count == 4)
    assert(d1[TestKeyTy(40)]!.value == 2040)

    // Overwrite a value in existing binding.
    assert(d1.updateValue(TestValueTy(2010), forKey: TestKeyTy(10))!.value == 1010)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(d1.count == 4)
    assert(d1[TestKeyTy(10)]!.value == 2010)
  }

  do {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    var d2 = d1
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 == unsafeBitCast(d2, Int.self))

    // Insert a new key-value pair.
    d2.updateValue(TestValueTy(2040), forKey: TestKeyTy(40))
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 != unsafeBitCast(d2, Int.self))

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
    _fixLifetime(d1)
    _fixLifetime(d2)
  }

  do {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    var d2 = d1
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 == unsafeBitCast(d2, Int.self))

    // Overwrite a value in existing binding.
    d2.updateValue(TestValueTy(2010), forKey: TestKeyTy(10))
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 != unsafeBitCast(d2, Int.self))

    assert(d1.count == 3)
    assert(d1[TestKeyTy(10)]!.value == 1010)
    assert(d1[TestKeyTy(20)]!.value == 1020)
    assert(d1[TestKeyTy(30)]!.value == 1030)

    assert(d2.count == 3)
    assert(d2[TestKeyTy(10)]!.value == 2010)
    assert(d2[TestKeyTy(20)]!.value == 1020)
    assert(d2[TestKeyTy(30)]!.value == 1030)

    // Keep variables alive.
    _fixLifetime(d1)
    _fixLifetime(d2)
  }
}


DictionaryTestSuite.test("COW.Fast.IndexForKeyDoesNotReallocate") {
  var d = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d, Int.self)

  // Find an existing key.
  do {
    var foundIndex1 = d.indexForKey(10)!
    assert(identity1 == unsafeBitCast(d, Int.self))

    var foundIndex2 = d.indexForKey(10)!
    assert(foundIndex1 == foundIndex2)

    assert(d[foundIndex1].0 == 10)
    assert(d[foundIndex1].1 == 1010)
    assert(identity1 == unsafeBitCast(d, Int.self))
  }

  // Try to find a key that is not present.
  do {
    var foundIndex1 = d.indexForKey(1111)
    assert(foundIndex1 == nil)
    assert(identity1 == unsafeBitCast(d, Int.self))
  }

  do {
    var d2: [MinimalHashableValue : OpaqueValue<Int>] = [:]
    MinimalHashableValue.timesEqualEqualWasCalled = 0
    MinimalHashableValue.timesHashValueWasCalled = 0
    expectEmpty(d2.indexForKey(MinimalHashableValue(42)))

    // If the dictionary is empty, we shouldn't be computing the hash value of
    // the provided key.
    expectEqual(0, MinimalHashableValue.timesEqualEqualWasCalled)
    expectEqual(0, MinimalHashableValue.timesHashValueWasCalled)
  }
}

DictionaryTestSuite.test("COW.Slow.IndexForKeyDoesNotReallocate") {
  var d = getCOWSlowDictionary()
  var identity1 = unsafeBitCast(d, Int.self)

  // Find an existing key.
  do {
    var foundIndex1 = d.indexForKey(TestKeyTy(10))!
    assert(identity1 == unsafeBitCast(d, Int.self))

    var foundIndex2 = d.indexForKey(TestKeyTy(10))!
    assert(foundIndex1 == foundIndex2)

    assert(d[foundIndex1].0 == TestKeyTy(10))
    assert(d[foundIndex1].1.value == 1010)
    assert(identity1 == unsafeBitCast(d, Int.self))
  }

  // Try to find a key that is not present.
  do {
    var foundIndex1 = d.indexForKey(TestKeyTy(1111))
    assert(foundIndex1 == nil)
    assert(identity1 == unsafeBitCast(d, Int.self))
  }

  do {
    var d2: [MinimalHashableClass : OpaqueValue<Int>] = [:]
    MinimalHashableClass.timesEqualEqualWasCalled = 0
    MinimalHashableClass.timesHashValueWasCalled = 0
    expectEmpty(d2.indexForKey(MinimalHashableClass(42)))

    // If the dictionary is empty, we shouldn't be computing the hash value of
    // the provided key.
    expectEqual(0, MinimalHashableClass.timesEqualEqualWasCalled)
    expectEqual(0, MinimalHashableClass.timesHashValueWasCalled)
  }
}


DictionaryTestSuite.test("COW.Fast.RemoveAtIndexDoesNotReallocate") {
  do {
    var d = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d, Int.self)

    let foundIndex1 = d.indexForKey(10)!
    assert(identity1 == unsafeBitCast(d, Int.self))

    assert(d[foundIndex1].0 == 10)
    assert(d[foundIndex1].1 == 1010)

    let removed = d.removeAtIndex(foundIndex1)
    assert(removed.0 == 10)
    assert(removed.1 == 1010)

    assert(identity1 == unsafeBitCast(d, Int.self))
    assert(d.indexForKey(10) == nil)
  }

  do {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    var d2 = d1
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 == unsafeBitCast(d2, Int.self))

    var foundIndex1 = d2.indexForKey(10)!
    assert(d2[foundIndex1].0 == 10)
    assert(d2[foundIndex1].1 == 1010)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 == unsafeBitCast(d2, Int.self))

    let removed = d2.removeAtIndex(foundIndex1)
    assert(removed.0 == 10)
    assert(removed.1 == 1010)

    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 != unsafeBitCast(d2, Int.self))
    assert(d2.indexForKey(10) == nil)
  }
}

DictionaryTestSuite.test("COW.Slow.RemoveAtIndexDoesNotReallocate") {
  do {
    var d = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d, Int.self)

    var foundIndex1 = d.indexForKey(TestKeyTy(10))!
    assert(identity1 == unsafeBitCast(d, Int.self))

    assert(d[foundIndex1].0 == TestKeyTy(10))
    assert(d[foundIndex1].1.value == 1010)

    let removed = d.removeAtIndex(foundIndex1)
    assert(removed.0 == TestKeyTy(10))
    assert(removed.1.value == 1010)

    assert(identity1 == unsafeBitCast(d, Int.self))
    assert(d.indexForKey(TestKeyTy(10)) == nil)
  }

  do {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    var d2 = d1
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 == unsafeBitCast(d2, Int.self))

    var foundIndex1 = d2.indexForKey(TestKeyTy(10))!
    assert(d2[foundIndex1].0 == TestKeyTy(10))
    assert(d2[foundIndex1].1.value == 1010)

    let removed = d2.removeAtIndex(foundIndex1)
    assert(removed.0 == TestKeyTy(10))
    assert(removed.1.value == 1010)

    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 != unsafeBitCast(d2, Int.self))
    assert(d2.indexForKey(TestKeyTy(10)) == nil)
  }
}


DictionaryTestSuite.test("COW.Fast.RemoveValueForKeyDoesNotReallocate") {
  do {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    var deleted = d1.removeValueForKey(0)
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d1, Int.self))

    deleted = d1.removeValueForKey(10)
    assert(deleted! == 1010)
    assert(identity1 == unsafeBitCast(d1, Int.self))

    // Keep variables alive.
    _fixLifetime(d1)
  }

  do {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    var d2 = d1
    var deleted = d2.removeValueForKey(0)
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 == unsafeBitCast(d2, Int.self))

    deleted = d2.removeValueForKey(10)
    assert(deleted! == 1010)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 != unsafeBitCast(d2, Int.self))

    // Keep variables alive.
    _fixLifetime(d1)
    _fixLifetime(d2)
  }
}

DictionaryTestSuite.test("COW.Slow.RemoveValueForKeyDoesNotReallocate") {
  do {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    var deleted = d1.removeValueForKey(TestKeyTy(0))
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d1, Int.self))

    deleted = d1.removeValueForKey(TestKeyTy(10))
    assert(deleted!.value == 1010)
    assert(identity1 == unsafeBitCast(d1, Int.self))

    // Keep variables alive.
    _fixLifetime(d1)
  }

  do {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    var d2 = d1
    var deleted = d2.removeValueForKey(TestKeyTy(0))
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 == unsafeBitCast(d2, Int.self))

    deleted = d2.removeValueForKey(TestKeyTy(10))
    assert(deleted!.value == 1010)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 != unsafeBitCast(d2, Int.self))

    // Keep variables alive.
    _fixLifetime(d1)
    _fixLifetime(d2)
  }
}


DictionaryTestSuite.test("COW.Fast.RemoveAllDoesNotReallocate") {
  do {
    var d = getCOWFastDictionary()
    let originalCapacity = d._variantStorage.native.capacity
    assert(d.count == 3)
    assert(d[10]! == 1010)

    d.removeAll()
    // We cannot assert that identity changed, since the new buffer of smaller
    // size can be allocated at the same address as the old one.
    var identity1 = unsafeBitCast(d, Int.self)
    assert(d._variantStorage.native.capacity < originalCapacity)
    assert(d.count == 0)
    assert(d[10] == nil)

    d.removeAll()
    assert(identity1 == unsafeBitCast(d, Int.self))
    assert(d.count == 0)
    assert(d[10] == nil)
  }

  do {
    var d = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d, Int.self)
    let originalCapacity = d._variantStorage.native.capacity
    assert(d.count == 3)
    assert(d[10]! == 1010)

    d.removeAll(keepCapacity: true)
    assert(identity1 == unsafeBitCast(d, Int.self))
    assert(d._variantStorage.native.capacity == originalCapacity)
    assert(d.count == 0)
    assert(d[10] == nil)

    d.removeAll(keepCapacity: true)
    assert(identity1 == unsafeBitCast(d, Int.self))
    assert(d._variantStorage.native.capacity == originalCapacity)
    assert(d.count == 0)
    assert(d[10] == nil)
  }

  do {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)
    assert(d1.count == 3)
    assert(d1[10]! == 1010)

    var d2 = d1
    d2.removeAll()
    var identity2 = unsafeBitCast(d2, Int.self)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[10]! == 1010)
    assert(d2.count == 0)
    assert(d2[10] == nil)

    // Keep variables alive.
    _fixLifetime(d1)
    _fixLifetime(d2)
  }

  do {
    var d1 = getCOWFastDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)
    let originalCapacity = d1._variantStorage.native.capacity
    assert(d1.count == 3)
    assert(d1[10] == 1010)

    var d2 = d1
    d2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(d2, Int.self)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[10]! == 1010)
    assert(d2._variantStorage.native.capacity == originalCapacity)
    assert(d2.count == 0)
    assert(d2[10] == nil)

    // Keep variables alive.
    _fixLifetime(d1)
    _fixLifetime(d2)
  }
}

DictionaryTestSuite.test("COW.Slow.RemoveAllDoesNotReallocate") {
  do {
    var d = getCOWSlowDictionary()
    let originalCapacity = d._variantStorage.native.capacity
    assert(d.count == 3)
    assert(d[TestKeyTy(10)]!.value == 1010)

    d.removeAll()
    // We cannot assert that identity changed, since the new buffer of smaller
    // size can be allocated at the same address as the old one.
    var identity1 = unsafeBitCast(d, Int.self)
    assert(d._variantStorage.native.capacity < originalCapacity)
    assert(d.count == 0)
    assert(d[TestKeyTy(10)] == nil)

    d.removeAll()
    assert(identity1 == unsafeBitCast(d, Int.self))
    assert(d.count == 0)
    assert(d[TestKeyTy(10)] == nil)
  }

  do {
    var d = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d, Int.self)
    let originalCapacity = d._variantStorage.native.capacity
    assert(d.count == 3)
    assert(d[TestKeyTy(10)]!.value == 1010)

    d.removeAll(keepCapacity: true)
    assert(identity1 == unsafeBitCast(d, Int.self))
    assert(d._variantStorage.native.capacity == originalCapacity)
    assert(d.count == 0)
    assert(d[TestKeyTy(10)] == nil)

    d.removeAll(keepCapacity: true)
    assert(identity1 == unsafeBitCast(d, Int.self))
    assert(d._variantStorage.native.capacity == originalCapacity)
    assert(d.count == 0)
    assert(d[TestKeyTy(10)] == nil)
  }

  do {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)
    assert(d1.count == 3)
    assert(d1[TestKeyTy(10)]!.value == 1010)

    var d2 = d1
    d2.removeAll()
    var identity2 = unsafeBitCast(d2, Int.self)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[TestKeyTy(10)]!.value == 1010)
    assert(d2.count == 0)
    assert(d2[TestKeyTy(10)] == nil)

    // Keep variables alive.
    _fixLifetime(d1)
    _fixLifetime(d2)
  }

  do {
    var d1 = getCOWSlowDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)
    let originalCapacity = d1._variantStorage.native.capacity
    assert(d1.count == 3)
    assert(d1[TestKeyTy(10)]!.value == 1010)

    var d2 = d1
    d2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(d2, Int.self)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[TestKeyTy(10)]!.value == 1010)
    assert(d2._variantStorage.native.capacity == originalCapacity)
    assert(d2.count == 0)
    assert(d2[TestKeyTy(10)] == nil)

    // Keep variables alive.
    _fixLifetime(d1)
    _fixLifetime(d2)
  }
}


DictionaryTestSuite.test("COW.Fast.CountDoesNotReallocate") {
  var d = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d, Int.self)

  assert(d.count == 3)
  assert(identity1 == unsafeBitCast(d, Int.self))
}

DictionaryTestSuite.test("COW.Slow.CountDoesNotReallocate") {
  var d = getCOWSlowDictionary()
  var identity1 = unsafeBitCast(d, Int.self)

  assert(d.count == 3)
  assert(identity1 == unsafeBitCast(d, Int.self))
}


DictionaryTestSuite.test("COW.Fast.GenerateDoesNotReallocate") {
  var d = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d, Int.self)

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value) = gen.next() {
    pairs += [(key, value)]
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  assert(identity1 == unsafeBitCast(d, Int.self))
}

DictionaryTestSuite.test("COW.Slow.GenerateDoesNotReallocate") {
  var d = getCOWSlowDictionary()
  var identity1 = unsafeBitCast(d, Int.self)

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
  assert(identity1 == unsafeBitCast(d, Int.self))
}


DictionaryTestSuite.test("COW.Fast.EqualityTestDoesNotReallocate") {
  var d1 = getCOWFastDictionary()
  var identity1 = unsafeBitCast(d1, Int.self)

  var d2 = getCOWFastDictionary()
  var identity2 = unsafeBitCast(d2, Int.self)

  assert(d1 == d2)
  assert(identity1 == unsafeBitCast(d1, Int.self))
  assert(identity2 == unsafeBitCast(d2, Int.self))

  d2[40] = 2040
  assert(d1 != d2)
  assert(identity1 == unsafeBitCast(d1, Int.self))
  assert(identity2 == unsafeBitCast(d2, Int.self))
}

DictionaryTestSuite.test("COW.Slow.EqualityTestDoesNotReallocate") {
  var d1 = getCOWSlowEquatableDictionary()
  var identity1 = unsafeBitCast(d1, Int.self)

  var d2 = getCOWSlowEquatableDictionary()
  var identity2 = unsafeBitCast(d2, Int.self)

  assert(d1 == d2)
  assert(identity1 == unsafeBitCast(d1, Int.self))
  assert(identity2 == unsafeBitCast(d2, Int.self))

  d2[TestKeyTy(40)] = TestEquatableValueTy(2040)
  assert(d1 != d2)
  assert(identity1 == unsafeBitCast(d1, Int.self))
  assert(identity2 == unsafeBitCast(d2, Int.self))
}


//===---
// Native dictionary tests.
//===---

func helperDeleteThree(k1: TestKeyTy, _ k2: TestKeyTy, _ k3: TestKeyTy) {
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

DictionaryTestSuite.test("deleteChainCollision") {
  var k1 = TestKeyTy(value: 10, hashValue: 0)
  var k2 = TestKeyTy(value: 20, hashValue: 0)
  var k3 = TestKeyTy(value: 30, hashValue: 0)

  helperDeleteThree(k1, k2, k3)
}

DictionaryTestSuite.test("deleteChainNoCollision") {
  var k1 = TestKeyTy(value: 10, hashValue: 0)
  var k2 = TestKeyTy(value: 20, hashValue: 1)
  var k3 = TestKeyTy(value: 30, hashValue: 2)

  helperDeleteThree(k1, k2, k3)
}

DictionaryTestSuite.test("deleteChainCollision2") {
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

DictionaryTestSuite.test("deleteChainCollisionRandomized") {
  let timeNow = CUnsignedInt(time(nil))
  print("time is \(timeNow)")
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
  print("chose parameters: collisionChains=\(collisionChains) chainLength=\(chainOverlap)")

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

DictionaryTestSuite.test("init(dictionaryLiteral:)") {
  do {
    var empty = Dictionary<Int, Int>()
    assert(empty.count == 0)
    assert(empty[1111] == nil)
  }
  do {
    var d = Dictionary(dictionaryLiteral: (10, 1010))
    assert(d.count == 1)
    assert(d[10]! == 1010)
    assert(d[1111] == nil)
  }
  do {
    var d = Dictionary(dictionaryLiteral: 
        (10, 1010), (20, 1020))
    assert(d.count == 2)
    assert(d[10]! == 1010)
    assert(d[20]! == 1020)
    assert(d[1111] == nil)
  }
  do {
    var d = Dictionary(dictionaryLiteral: 
        (10, 1010), (20, 1020), (30, 1030))
    assert(d.count == 3)
    assert(d[10]! == 1010)
    assert(d[20]! == 1020)
    assert(d[30]! == 1030)
    assert(d[1111] == nil)
  }
  do {
    var d = Dictionary(dictionaryLiteral: 
        (10, 1010), (20, 1020), (30, 1030), (40, 1040))
    assert(d.count == 4)
    assert(d[10]! == 1010)
    assert(d[20]! == 1020)
    assert(d[30]! == 1030)
    assert(d[40]! == 1040)
    assert(d[1111] == nil)
  }
  do {
    var d: Dictionary<Int, Int> = [ 10: 1010, 20: 1020, 30: 1030 ]
    assert(d.count == 3)
    assert(d[10]! == 1010)
    assert(d[20]! == 1020)
    assert(d[30]! == 1030)
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

var DictionaryDerivedAPIs = TestSuite("DictionaryDerivedAPIs")

DictionaryDerivedAPIs.test("isEmpty") {
  do {
    var empty = Dictionary<Int, Int>()
    expectTrue(empty.isEmpty)
  }
  do {
    var d = getDerivedAPIsDictionary()
    expectFalse(d.isEmpty)
  }
}

func callGenericIsEmpty<C : CollectionType>(collection: C) -> Bool {
  return collection.isEmpty
}

DictionaryDerivedAPIs.test("keys") {
  do {
    var empty = Dictionary<Int, Int>()
    var keys = Array(empty.keys)
    expectTrue(equalsUnordered(keys, []))
  }
  do {
    var d = getDerivedAPIsDictionary()
    var keys = Array(d.keys)
    expectTrue(equalsUnordered(keys, [ 10, 20, 30 ]))
  }
}

DictionaryDerivedAPIs.test("values") {
  do {
    var empty = Dictionary<Int, Int>()
    var values = Array(empty.values)
    expectTrue(equalsUnordered(values, []))
  }
  do {
    var d = getDerivedAPIsDictionary()

    var values = Array(d.values)
    expectTrue(equalsUnordered(values, [ 1010, 1020, 1030 ]))

    d[11] = 1010
    values = Array(d.values)
    expectTrue(equalsUnordered(values, [ 1010, 1010, 1020, 1030 ]))
  }
}

//===---
// Check that generators traverse a snapshot of the collection.
//===---

DictionaryTestSuite.test("mutationDoesNotAffectGenerator/subscript/store") {
  var dict = getDerivedAPIsDictionary()
  var g = dict.generate()
  dict[10] = 1011

  expectEqualsUnordered(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    Array(GeneratorSequence(g)))
}

DictionaryTestSuite.test("mutationDoesNotAffectGenerator/removeValueForKey,1") {
  var dict = getDerivedAPIsDictionary()
  var g = dict.generate()
  expectOptionalEqual(1010, dict.removeValueForKey(10))

  expectEqualsUnordered(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    Array(GeneratorSequence(g)))
}

DictionaryTestSuite.test("mutationDoesNotAffectGenerator/removeValueForKey,all") {
  var dict = getDerivedAPIsDictionary()
  var g = dict.generate()
  expectOptionalEqual(1010, dict.removeValueForKey(10))
  expectOptionalEqual(1020, dict.removeValueForKey(20))
  expectOptionalEqual(1030, dict.removeValueForKey(30))

  expectEqualsUnordered(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    Array(GeneratorSequence(g)))
}

DictionaryTestSuite.test(
  "mutationDoesNotAffectGenerator/removeAll,keepCapacity=false") {
  var dict = getDerivedAPIsDictionary()
  var g = dict.generate()
  dict.removeAll(keepCapacity: false)

  expectEqualsUnordered(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    Array(GeneratorSequence(g)))
}

DictionaryTestSuite.test(
  "mutationDoesNotAffectGenerator/removeAll,keepCapacity=true") {
  var dict = getDerivedAPIsDictionary()
  var g = dict.generate()
  dict.removeAll(keepCapacity: true)

  expectEqualsUnordered(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    Array(GeneratorSequence(g)))
}

//===---
// Misc tests.
//===---

DictionaryTestSuite.test("misc") {
  do {
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

  do {
    // Dictionaries with other types
    var d = [ 1.2: 1, 2.6: 2 ]
    d[3.3] = 3
    expectOptionalEqual(1, d[1.2])
    expectOptionalEqual(2, d[2.6])
    expectOptionalEqual(3, d[3.3])
  }

  do {
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

    expectEqual(3, d.values[d.keys.indexOf("three")!])
    expectEqual(4, d.values[d.keys.indexOf("four")!])

    expectEqual(3, d3.values[d.keys.indexOf("three")!])
    expectEqual(4, d3.values[d.keys.indexOf("four")!])
  }
}

DictionaryTestSuite.test("popFirst") {
  // Empty
  do {
    var d = [Int: Int]()
    let popped = d.popFirst()
    expectEmpty(popped)
  }

  do {
    var popped = [(Int, Int)]()
    var d: [Int: Int] = [
      1010: 1010,
      2020: 2020,
      3030: 3030,
    ]
    let expected = Array(d)
    while let element = d.popFirst() {
      popped.append(element)
    }
    expectEqualSequence(expected, Array(popped)) {
      (lhs: (Int, Int), rhs: (Int, Int)) -> Bool in
      lhs.0 == rhs.0 && lhs.1 == rhs.1
    }
    expectTrue(d.isEmpty)
  }
}

DictionaryTestSuite.test("removeAtIndex") {
  // Test removing from the startIndex, the middle, and the end of a dictionary.
  for i in 1...3 {
    var d: [Int: Int] = [
      10: 1010,
      20: 2020,
      30: 3030,
    ]
    let removed = d.removeAtIndex(d.indexForKey(i*10)!)
    expectEqual(i*10, removed.0)
    expectEqual(i*1010, removed.1)
    expectEqual(2, d.count)
    expectEmpty(d.indexForKey(i))
    let origKeys: [Int] = [10, 20, 30]
    expectEqual(origKeys.filter { $0 != (i*10) }, d.keys.sort())
  }
}

DictionaryTestSuite.setUp {
  resetLeaksOfDictionaryKeysValues()
}

DictionaryTestSuite.tearDown {
  expectNoLeaksOfDictionaryKeysValues()
}

runAllTests()


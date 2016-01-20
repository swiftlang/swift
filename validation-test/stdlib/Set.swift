// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %S/../../utils/gyb %s -o %t/main.swift
// RUN: %S/../../utils/line-directive %t/main.swift -- %target-build-swift %S/Inputs/DictionaryKeyValueTypes.swift %t/main.swift -o %t/Set -Xfrontend -disable-access-control
//
// RUN: %S/../../utils/line-directive %t/main.swift -- %target-run %t/Set
// REQUIRES: executable_test

#if os(OSX)
import Darwin
#elseif os(Linux) || os(FreeBSD)
import Glibc
#endif
import StdlibUnittest

// For experimental Set operators
import SwiftExperimental

// Check that the generic parameter is called 'Element'.
protocol TestProtocol1 {}

extension Set where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension SetIndex where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension SetGenerator where Element : TestProtocol1 {
  var _elementIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

let hugeNumberArray = (0..<500).map {
  (i: Int) -> Int in
  return random()
}

var SetTestSuite = TestSuite("Set")

SetTestSuite.setUp {
  resetLeaksOfDictionaryKeysValues()
}

SetTestSuite.tearDown {
  expectNoLeaksOfDictionaryKeysValues()
}

func getCOWFastSet(members: [Int] = [1010, 2020, 3030]) -> Set<Int> {
  var s = Set<Int>(minimumCapacity: 10)
  for member in members {
    s.insert(member)
  }
  expectTrue(isNativeSet(s))
  return s
}

func getCOWSlowSet(members: [Int] = [1010, 2020, 3030]) -> Set<TestKeyTy> {
  var s = Set<TestKeyTy>(minimumCapacity: 10)
  for member in members {
    s.insert(TestKeyTy(member))
  }
  expectTrue(isNativeSet(s))
  return s
}

func helperDeleteThree(k1: TestKeyTy, _ k2: TestKeyTy, _ k3: TestKeyTy) {
  var s1 = Set<TestKeyTy>(minimumCapacity: 10)

  s1.insert(k1)
  s1.insert(k2)
  s1.insert(k3)

  expectTrue(s1.contains(k1))
  expectTrue(s1.contains(k2))
  expectTrue(s1.contains(k3))

  s1.remove(k1)
  expectTrue(s1.contains(k2))
  expectTrue(s1.contains(k3))

  s1.remove(k2)
  expectTrue(s1.contains(k3))

  s1.remove(k3)
  expectEqual(0, s1.count)
}

func uniformRandom(max: Int) -> Int {
#if os(OSX)
  return Int(arc4random_uniform(UInt32(max)))
#elseif os(Linux) || os(FreeBSD)
  // FIXME: this is not uniform.
  return random() % max
#endif
}

func pickRandom<T>(a: [T]) -> T {
  return a[uniformRandom(a.count)]
}

func isNativeSet<T : Hashable>(s: Set<T>) -> Bool {
  switch s._variantStorage {
  case .Native:
    return true
  case .Cocoa:
    return false
  }
}

func equalsUnordered(lhs: Set<Int>, _ rhs: Set<Int>) -> Bool {
  return lhs.sort().elementsEqual(rhs.sort()) {
    $0 == $1
  }
}

SetTestSuite.test("sizeof") {
  var s = Set(["Hello", "world"])
#if arch(i386) || arch(arm)
  expectEqual(4, sizeofValue(s))
#else
  expectEqual(8, sizeofValue(s))
#endif
}

SetTestSuite.test("COW.Smoke") {
  var s1 = Set<TestKeyTy>(minimumCapacity: 10)
  for i in [1010, 2020, 3030]{ s1.insert(TestKeyTy(i)) }
  var identity1 = unsafeBitCast(s1, Int.self)

  var s2 = s1
  _fixLifetime(s2)
  expectEqual(identity1, unsafeBitCast(s2, Int.self))

  s2.insert(TestKeyTy(4040))
  expectNotEqual(identity1, unsafeBitCast(s2, Int.self))

  s2.insert(TestKeyTy(5050))
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  // Keep variables alive.
  _fixLifetime(s1)
  _fixLifetime(s2)
}

SetTestSuite.test("COW.Fast.IndexesDontAffectUniquenessCheck") {
  var s = getCOWFastSet()
  var identity1 = unsafeBitCast(s, Int.self)

  var startIndex = s.startIndex
  var endIndex = s.endIndex
  expectNotEqual(startIndex, endIndex)
  expectTrue(startIndex < endIndex)
  expectTrue(startIndex <= endIndex)
  expectFalse(startIndex >= endIndex)
  expectFalse(startIndex > endIndex)

  expectEqual(identity1, unsafeBitCast(s, Int.self))

  s.insert(4040)
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

SetTestSuite.test("COW.Slow.IndexesDontAffectUniquenessCheck") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Int.self)

  var startIndex = s.startIndex
  var endIndex = s.endIndex
  expectNotEqual(startIndex, endIndex)
  expectTrue(startIndex < endIndex)
  expectTrue(startIndex <= endIndex)
  expectFalse(startIndex >= endIndex)
  expectFalse(startIndex > endIndex)

  expectEqual(identity1, unsafeBitCast(s, Int.self))
  s.insert(TestKeyTy(4040))
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

SetTestSuite.test("COW.Fast.SubscriptWithIndexDoesNotReallocate") {
  var s = getCOWFastSet()
  var identity1 = unsafeBitCast(s, Int.self)

  var startIndex = s.startIndex
  let empty = startIndex == s.endIndex
  expectNotEqual(empty, (s.startIndex < s.endIndex))
  expectTrue(s.startIndex <= s.endIndex)
  expectEqual(empty, (s.startIndex >= s.endIndex))
  expectFalse(s.startIndex > s.endIndex)
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  expectNotEqual(0, s[startIndex])
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("COW.Slow.SubscriptWithIndexDoesNotReallocate") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Int.self)

  var startIndex = s.startIndex
  let empty = startIndex == s.endIndex
  expectNotEqual(empty, (s.startIndex < s.endIndex))
  expectTrue(s.startIndex <= s.endIndex)
  expectEqual(empty, (s.startIndex >= s.endIndex))
  expectFalse(s.startIndex > s.endIndex)
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  expectNotEqual(TestKeyTy(0), s[startIndex])
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("COW.Fast.ContainsDoesNotReallocate") {
  var s = getCOWFastSet()
  var identity1 = unsafeBitCast(s, Int.self)

  expectTrue(s.contains(1010))
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  do {
    var s2: Set<MinimalHashableValue> = []
    MinimalHashableValue.timesEqualEqualWasCalled = 0
    MinimalHashableValue.timesHashValueWasCalled = 0
    expectFalse(s2.contains(MinimalHashableValue(42)))

    // If the set is empty, we shouldn't be computing the hash value of the
    // provided key.
    expectEqual(0, MinimalHashableValue.timesEqualEqualWasCalled)
    expectEqual(0, MinimalHashableValue.timesHashValueWasCalled)
  }
}

SetTestSuite.test("COW.Slow.ContainsDoesNotReallocate") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Int.self)

  expectTrue(s.contains(TestKeyTy(1010)))
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  // Insert a new key-value pair.
  s.insert(TestKeyTy(4040))
  expectEqual(identity1, unsafeBitCast(s, Int.self))
  expectEqual(4, s.count)
  expectTrue(s.contains(TestKeyTy(1010)))
  expectTrue(s.contains(TestKeyTy(2020)))
  expectTrue(s.contains(TestKeyTy(3030)))
  expectTrue(s.contains(TestKeyTy(4040)))

  // Delete an existing key.
  s.remove(TestKeyTy(1010))
  expectEqual(identity1, unsafeBitCast(s, Int.self))
  expectEqual(3, s.count)
  expectTrue(s.contains(TestKeyTy(2020)))
  expectTrue(s.contains(TestKeyTy(3030)))
  expectTrue(s.contains(TestKeyTy(4040)))

  // Try to delete a key that does not exist.
  s.remove(TestKeyTy(777))
  expectEqual(identity1, unsafeBitCast(s, Int.self))
  expectEqual(3, s.count)
  expectTrue(s.contains(TestKeyTy(2020)))
  expectTrue(s.contains(TestKeyTy(3030)))
  expectTrue(s.contains(TestKeyTy(4040)))

  do {
    var s2: Set<MinimalHashableClass> = []
    MinimalHashableClass.timesEqualEqualWasCalled = 0
    MinimalHashableClass.timesHashValueWasCalled = 0
    expectFalse(s2.contains(MinimalHashableClass(42)))

    // If the set is empty, we shouldn't be computing the hash value of the
    // provided key.
    expectEqual(0, MinimalHashableClass.timesEqualEqualWasCalled)
    expectEqual(0, MinimalHashableClass.timesHashValueWasCalled)
  }
}

SetTestSuite.test("COW.Fast.InsertDoesNotReallocate") {
  var s1 = getCOWFastSet()

  let identity1 = unsafeBitCast(s1, Int.self)
  let count1 = s1.count

  // Inserting a redundant element should not create new storage
  s1.insert(2020)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectEqual(count1, s1.count)

  s1.insert(4040)
  s1.insert(5050)
  s1.insert(6060)
  expectEqual(count1 + 3, s1.count)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
}

SetTestSuite.test("COW.Slow.InsertDoesNotReallocate") {
  do {
    var s1 = getCOWSlowSet()

    let identity1 = unsafeBitCast(s1, Int.self)
    let count1 = s1.count

    // Inserting a redundant element should not create new storage
    s1.insert(TestKeyTy(2020))
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectEqual(count1, s1.count)

    s1.insert(TestKeyTy(4040))
    s1.insert(TestKeyTy(5050))
    s1.insert(TestKeyTy(6060))
    expectEqual(count1 + 3, s1.count)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
  }

  do {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Int.self)

    var s2 = s1
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectEqual(identity1, unsafeBitCast(s2, Int.self))

    s2.insert(TestKeyTy(2040))
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity1, unsafeBitCast(s2, Int.self))

    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestKeyTy(1010)))
    expectTrue(s1.contains(TestKeyTy(2020)))
    expectTrue(s1.contains(TestKeyTy(3030)))
    expectFalse(s1.contains(TestKeyTy(2040)))

    expectEqual(4, s2.count)
    expectTrue(s2.contains(TestKeyTy(1010)))
    expectTrue(s2.contains(TestKeyTy(2020)))
    expectTrue(s2.contains(TestKeyTy(3030)))
    expectTrue(s2.contains(TestKeyTy(2040)))

    // Keep variables alive.
    _fixLifetime(s1)
    _fixLifetime(s2)
  }

  do {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Int.self)

    var s2 = s1
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectEqual(identity1, unsafeBitCast(s2, Int.self))

    // Insert a redundant element.
    s2.insert(TestKeyTy(2020))
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity1, unsafeBitCast(s2, Int.self))

    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestKeyTy(1010)))
    expectTrue(s1.contains(TestKeyTy(2020)))
    expectTrue(s1.contains(TestKeyTy(3030)))

    expectEqual(3, s2.count)
    expectTrue(s2.contains(TestKeyTy(1010)))
    expectTrue(s2.contains(TestKeyTy(2020)))
    expectTrue(s2.contains(TestKeyTy(3030)))

    // Keep variables alive.
    _fixLifetime(s1)
    _fixLifetime(s2)
  }
}

SetTestSuite.test("COW.Fast.IndexForMemberDoesNotReallocate") {
  var s = getCOWFastSet()
  var identity1 = unsafeBitCast(s, Int.self)

  // Find an existing key.
  do {
    var foundIndex1 = s.indexOf(1010)!
    expectEqual(identity1, unsafeBitCast(s, Int.self))

    var foundIndex2 = s.indexOf(1010)!
    expectEqual(foundIndex1, foundIndex2)

    expectEqual(1010, s[foundIndex1])
    expectEqual(identity1, unsafeBitCast(s, Int.self))
  }

  // Try to find a key that is not present.
  do {
    var foundIndex1 = s.indexOf(1111)
    expectEmpty(foundIndex1)
    expectEqual(identity1, unsafeBitCast(s, Int.self))
  }

  do {
    var s2: Set<MinimalHashableValue> = []
    MinimalHashableValue.timesEqualEqualWasCalled = 0
    MinimalHashableValue.timesHashValueWasCalled = 0
    expectEmpty(s2.indexOf(MinimalHashableValue(42)))

    // If the set is empty, we shouldn't be computing the hash value of the
    // provided key.
    expectEqual(0, MinimalHashableValue.timesEqualEqualWasCalled)
    expectEqual(0, MinimalHashableValue.timesHashValueWasCalled)
  }
}

SetTestSuite.test("COW.Slow.IndexForMemberDoesNotReallocate") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Int.self)

  // Find an existing key.
  do {
    var foundIndex1 = s.indexOf(TestKeyTy(1010))!
    expectEqual(identity1, unsafeBitCast(s, Int.self))

    var foundIndex2 = s.indexOf(TestKeyTy(1010))!
    expectEqual(foundIndex1, foundIndex2)

    expectEqual(TestKeyTy(1010), s[foundIndex1])
    expectEqual(identity1, unsafeBitCast(s, Int.self))
  }

  // Try to find a key that is not present.
  do {
    var foundIndex1 = s.indexOf(TestKeyTy(1111))
    expectEmpty(foundIndex1)
    expectEqual(identity1, unsafeBitCast(s, Int.self))
  }

  do {
    var s2: Set<MinimalHashableClass> = []
    MinimalHashableClass.timesEqualEqualWasCalled = 0
    MinimalHashableClass.timesHashValueWasCalled = 0
    expectEmpty(s2.indexOf(MinimalHashableClass(42)))

    // If the set is empty, we shouldn't be computing the hash value of the
    // provided key.
    expectEqual(0, MinimalHashableClass.timesEqualEqualWasCalled)
    expectEqual(0, MinimalHashableClass.timesHashValueWasCalled)
  }
}

SetTestSuite.test("COW.Fast.RemoveAtIndexDoesNotReallocate") {
  do {
    var s = getCOWFastSet()
    var identity1 = unsafeBitCast(s, Int.self)

    let foundIndex1 = s.indexOf(1010)!
    expectEqual(identity1, unsafeBitCast(s, Int.self))

    expectEqual(1010, s[foundIndex1])

    let removed = s.removeAtIndex(foundIndex1)
    expectEqual(1010, removed)

    expectEqual(identity1, unsafeBitCast(s, Int.self))
    expectEmpty(s.indexOf(1010))
  }

  do {
    var s1 = getCOWFastSet()
    var identity1 = unsafeBitCast(s1, Int.self)

    var s2 = s1
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectEqual(identity1, unsafeBitCast(s2, Int.self))

    var foundIndex1 = s2.indexOf(1010)!
    expectEqual(1010, s2[foundIndex1])
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectEqual(identity1, unsafeBitCast(s2, Int.self))

    let removed = s2.removeAtIndex(foundIndex1)
    expectEqual(1010, removed)

    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity1, unsafeBitCast(s2, Int.self))
    expectEmpty(s2.indexOf(1010))
  }
}

SetTestSuite.test("COW.Slow.RemoveAtIndexDoesNotReallocate") {
  do {
    var s = getCOWSlowSet()
    var identity1 = unsafeBitCast(s, Int.self)

    let foundIndex1 = s.indexOf(TestKeyTy(1010))!
    expectEqual(identity1, unsafeBitCast(s, Int.self))

    expectEqual(TestKeyTy(1010), s[foundIndex1])

    let removed = s.removeAtIndex(foundIndex1)
    expectEqual(TestKeyTy(1010), removed)

    expectEqual(identity1, unsafeBitCast(s, Int.self))
    expectEmpty(s.indexOf(TestKeyTy(1010)))
  }

  do {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Int.self)

    var s2 = s1
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectEqual(identity1, unsafeBitCast(s2, Int.self))

    var foundIndex1 = s2.indexOf(TestKeyTy(1010))!
    expectEqual(TestKeyTy(1010), s2[foundIndex1])
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectEqual(identity1, unsafeBitCast(s2, Int.self))

    let removed = s2.removeAtIndex(foundIndex1)
    expectEqual(TestKeyTy(1010), removed)

    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity1, unsafeBitCast(s2, Int.self))
    expectEmpty(s2.indexOf(TestKeyTy(1010)))
  }
}

SetTestSuite.test("COW.Fast.RemoveDoesNotReallocate") {
  do {
    var s1 = getCOWFastSet()
    var identity1 = unsafeBitCast(s1, Int.self)

    var deleted = s1.remove(0)
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))

    deleted = s1.remove(1010)
    expectOptionalEqual(1010, deleted)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))

    // Keep variables alive.
    _fixLifetime(s1)
  }

  do {
    var s1 = getCOWFastSet()
    var identity1 = unsafeBitCast(s1, Int.self)

    var s2 = s1
    var deleted = s2.remove(0)
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectEqual(identity1, unsafeBitCast(s2, Int.self))

    deleted = s2.remove(1010)
    expectOptionalEqual(1010, deleted)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity1, unsafeBitCast(s2, Int.self))

    // Keep variables alive.
    _fixLifetime(s1)
    _fixLifetime(s2)
  }
}

SetTestSuite.test("COW.Slow.RemoveDoesNotReallocate") {
  do {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Int.self)

    var deleted = s1.remove(TestKeyTy(0))
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))

    deleted = s1.remove(TestKeyTy(1010))
    expectOptionalEqual(TestKeyTy(1010), deleted)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))

    // Keep variables alive.
    _fixLifetime(s1)
  }

  do {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Int.self)

    var s2 = s1
    var deleted = s2.remove(TestKeyTy(0))
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectEqual(identity1, unsafeBitCast(s2, Int.self))

    deleted = s2.remove(TestKeyTy(1010))
    expectOptionalEqual(TestKeyTy(1010), deleted)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity1, unsafeBitCast(s2, Int.self))

    // Keep variables alive.
    _fixLifetime(s1)
    _fixLifetime(s2)
  }
}

SetTestSuite.test("COW.Fast.UnionInPlaceSmallSetDoesNotReallocate") {
  var s1 = getCOWFastSet()
  let s2 = Set([4040, 5050, 6060])
  let s3 = Set([1010, 2020, 3030, 4040, 5050, 6060])

  let identity1 = unsafeBitCast(s1, Int.self)

  // Adding the empty set should obviously not allocate
  s1.unionInPlace(Set<Int>())
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  // adding a small set shouldn't cause a reallocation
  s1.unionInPlace(s2)
  expectEqual(s1, s3)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
}

SetTestSuite.test("COW.Fast.RemoveAllDoesNotReallocate") {
  do {
    var s = getCOWFastSet()
    let originalCapacity = s._variantStorage.native.capacity
    expectEqual(3, s.count)
    expectTrue(s.contains(1010))

    s.removeAll()
    // We cannot expectTrue that identity changed, since the new buffer of
    // smaller size can be allocated at the same address as the old one.
    var identity1 = unsafeBitCast(s, Int.self)
    expectTrue(s._variantStorage.native.capacity < originalCapacity)
    expectEqual(0, s.count)
    expectFalse(s.contains(1010))

    s.removeAll()
    expectEqual(identity1, unsafeBitCast(s, Int.self))
    expectEqual(0, s.count)
    expectFalse(s.contains(1010))
  }

  do {
    var s = getCOWFastSet()
    var identity1 = unsafeBitCast(s, Int.self)
    let originalCapacity = s._variantStorage.native.capacity
    expectEqual(3, s.count)
    expectTrue(s.contains(1010))

    s.removeAll(keepCapacity: true)
    expectEqual(identity1, unsafeBitCast(s, Int.self))
    expectEqual(originalCapacity, s._variantStorage.native.capacity)
    expectEqual(0, s.count)
    expectFalse(s.contains(1010))

    s.removeAll(keepCapacity: true)
    expectEqual(identity1, unsafeBitCast(s, Int.self))
    expectEqual(originalCapacity, s._variantStorage.native.capacity)
    expectEqual(0, s.count)
    expectFalse(s.contains(1010))
  }

  do {
    var s1 = getCOWFastSet()
    var identity1 = unsafeBitCast(s1, Int.self)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(1010))

    var s2 = s1
    s2.removeAll()
    var identity2 = unsafeBitCast(s2, Int.self)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity1, identity2)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(1010))
    expectEqual(0, s2.count)
    expectFalse(s2.contains(1010))

    // Keep variables alive.
    _fixLifetime(s1)
    _fixLifetime(s2)
  }

  do {
    var s1 = getCOWFastSet()
    var identity1 = unsafeBitCast(s1, Int.self)
    let originalCapacity = s1._variantStorage.native.capacity
    expectEqual(3, s1.count)
    expectTrue(s1.contains(1010))

    var s2 = s1
    s2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(s2, Int.self)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity1, identity2)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(1010))
    expectEqual(originalCapacity, s2._variantStorage.native.capacity)
    expectEqual(0, s2.count)
    expectFalse(s2.contains(1010))

    // Keep variables alive.
    _fixLifetime(s1)
    _fixLifetime(s2)
  }
}

SetTestSuite.test("COW.Slow.RemoveAllDoesNotReallocate") {
  do {
    var s = getCOWSlowSet()
    let originalCapacity = s._variantStorage.native.capacity
    expectEqual(3, s.count)
    expectTrue(s.contains(TestKeyTy(1010)))

    s.removeAll()
    // We cannot expectTrue that identity changed, since the new buffer of
    // smaller size can be allocated at the same address as the old one.
    var identity1 = unsafeBitCast(s, Int.self)
    expectTrue(s._variantStorage.native.capacity < originalCapacity)
    expectEqual(0, s.count)
    expectFalse(s.contains(TestKeyTy(1010)))

    s.removeAll()
    expectEqual(identity1, unsafeBitCast(s, Int.self))
    expectEqual(0, s.count)
    expectFalse(s.contains(TestKeyTy(1010)))
  }

  do {
    var s = getCOWSlowSet()
    var identity1 = unsafeBitCast(s, Int.self)
    let originalCapacity = s._variantStorage.native.capacity
    expectEqual(3, s.count)
    expectTrue(s.contains(TestKeyTy(1010)))

    s.removeAll(keepCapacity: true)
    expectEqual(identity1, unsafeBitCast(s, Int.self))
    expectEqual(originalCapacity, s._variantStorage.native.capacity)
    expectEqual(0, s.count)
    expectFalse(s.contains(TestKeyTy(1010)))

    s.removeAll(keepCapacity: true)
    expectEqual(identity1, unsafeBitCast(s, Int.self))
    expectEqual(originalCapacity, s._variantStorage.native.capacity)
    expectEqual(0, s.count)
    expectFalse(s.contains(TestKeyTy(1010)))
  }

  do {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Int.self)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestKeyTy(1010)))

    var s2 = s1
    s2.removeAll()
    var identity2 = unsafeBitCast(s2, Int.self)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity1, identity2)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestKeyTy(1010)))
    expectEqual(0, s2.count)
    expectFalse(s2.contains(TestKeyTy(1010)))

    // Keep variables alive.
    _fixLifetime(s1)
    _fixLifetime(s2)
  }

  do {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Int.self)
    let originalCapacity = s1._variantStorage.native.capacity
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestKeyTy(1010)))

    var s2 = s1
    s2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(s2, Int.self)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity1, identity2)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestKeyTy(1010)))
    expectEqual(originalCapacity, s2._variantStorage.native.capacity)
    expectEqual(0, s2.count)
    expectFalse(s2.contains(TestKeyTy(1010)))

    // Keep variables alive.
    _fixLifetime(s1)
    _fixLifetime(s2)
  }
}

SetTestSuite.test("COW.Fast.FirstDoesNotReallocate") {
  var s = getCOWFastSet()
  var identity1 = unsafeBitCast(s, Int.self)

  expectNotEmpty(s.first)
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("COW.Fast.CountDoesNotReallocate") {
  var s = getCOWFastSet()
  var identity1 = unsafeBitCast(s, Int.self)

  expectEqual(3, s.count)
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("COW.Slow.FirstDoesNotReallocate") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Int.self)

  expectNotEmpty(s.first)
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("COW.Slow.CountDoesNotReallocate") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Int.self)

  expectEqual(3, s.count)
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("COW.Fast.GenerateDoesNotReallocate") {
  var s = getCOWFastSet()
  var identity1 = unsafeBitCast(s, Int.self)

  var gen = s.generate()
  var items: [Int] = []
  while let value = gen.next() {
    items += [value]
  }
  expectTrue(equalsUnordered(items, [ 1010, 2020, 3030 ]))
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("COW.Slow.GenerateDoesNotReallocate") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Int.self)

  var gen = s.generate()
  var items: [Int] = []
  while let value = gen.next() {
    items.append(value.value)
  }
  expectTrue(equalsUnordered(items, [ 1010, 2020, 3030 ]))
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("COW.Fast.EqualityTestDoesNotReallocate") {
  var s1 = getCOWFastSet()
  var identity1 = unsafeBitCast(s1, Int.self)

  var s2 = getCOWFastSet()
  var identity2 = unsafeBitCast(s2, Int.self)

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectEqual(identity2, unsafeBitCast(s2, Int.self))
}

SetTestSuite.test("COW.Slow.EqualityTestDoesNotReallocate") {
  var s1 = getCOWFastSet()
  var identity1 = unsafeBitCast(s1, Int.self)

  var s2 = getCOWFastSet()
  var identity2 = unsafeBitCast(s2, Int.self)

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectEqual(identity2, unsafeBitCast(s2, Int.self))
}

//===---
// Native set tests.
//===---

SetTestSuite.test("deleteChainCollision") {
  var k1 = TestKeyTy(value: 1010, hashValue: 0)
  var k2 = TestKeyTy(value: 2020, hashValue: 0)
  var k3 = TestKeyTy(value: 3030, hashValue: 0)

  helperDeleteThree(k1, k2, k3)
}

SetTestSuite.test("deleteChainNoCollision") {
  var k1 = TestKeyTy(value: 1010, hashValue: 0)
  var k2 = TestKeyTy(value: 2020, hashValue: 1)
  var k3 = TestKeyTy(value: 3030, hashValue: 2)

  helperDeleteThree(k1, k2, k3)
}

SetTestSuite.test("deleteChainCollision2") {
  var k1_0 = TestKeyTy(value: 1010, hashValue: 0)
  var k2_0 = TestKeyTy(value: 2020, hashValue: 0)
  var k3_2 = TestKeyTy(value: 3030, hashValue: 2)
  var k4_0 = TestKeyTy(value: 4040, hashValue: 0)
  var k5_2 = TestKeyTy(value: 5050, hashValue: 2)
  var k6_0 = TestKeyTy(value: 6060, hashValue: 0)

  var s = Set<TestKeyTy>(minimumCapacity: 10)

  s.insert(k1_0) // in bucket 0
  s.insert(k2_0) // in bucket 1
  s.insert(k3_2) // in bucket 2
  s.insert(k4_0) // in bucket 3
  s.insert(k5_2) // in bucket 4
  s.insert(k6_0) // in bucket 5

  s.remove(k3_2)

  expectTrue(s.contains(k1_0))
  expectTrue(s.contains(k2_0))
  expectFalse(s.contains(k3_2))
  expectTrue(s.contains(k4_0))
  expectTrue(s.contains(k5_2))
  expectTrue(s.contains(k6_0))
}

SetTestSuite.test("deleteChainCollisionRandomized") {
  let timeNow = CUnsignedInt(time(nil))
  print("time is \(timeNow)")
  srandom(timeNow)

  func check(s: Set<TestKeyTy>) {
    var keys = Array(s)
    for i in 0..<keys.count {
      for j in 0..<i {
        expectNotEqual(keys[i], keys[j])
      }
    }

    for k in keys {
      expectTrue(s.contains(k))
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

  var s = Set<TestKeyTy>(minimumCapacity: 30)
  for i in 1..<300 {
    let key = getKey(uniformRandom(collisionChains * chainLength))
    if uniformRandom(chainLength * 2) == 0 {
      s.remove(key)
    } else {
      s.insert(TestKeyTy(key.value))
    }
    check(s)
  }
}

// Public API

SetTestSuite.test("init(SequenceType:)") {
  let s1 = Set([1010, 2020, 3030])
  var s2 = Set<Int>()
  s2.insert(1010)
  s2.insert(2020)
  s2.insert(3030)
  expectEqual(s1, s2)

  // Test the uniquing capabilities of a set
  let s3 = Set([
    1010, 1010, 1010, 1010, 1010, 1010,
    1010, 1010, 1010, 1010, 1010, 1010,
    2020, 2020, 2020, 3030, 3030, 3030
  ])
  expectEqual(s1, s3)
}

SetTestSuite.test("init(arrayLiteral:)") {
  let s1: Set<Int> = [1010, 2020, 3030, 1010, 2020, 3030]
  let s2 = Set([1010, 2020, 3030])
  var s3 = Set<Int>()
  s3.insert(1010)
  s3.insert(2020)
  s3.insert(3030)
  expectEqual(s1, s2)
  expectEqual(s2, s3)
}

SetTestSuite.test("isSubsetOf.Set.Set") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  expectTrue(Set<Int>().isSubsetOf(s1))
  expectFalse(s1.isSubsetOf(Set<Int>()))
  expectTrue(s1.isSubsetOf(s1))
  expectTrue(s2.isSubsetOf(s1))
}

SetTestSuite.test("⊆.Set.Set") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  expectTrue(Set<Int>().isSubsetOf(s1))
  expectFalse(s1 ⊆ Set<Int>())
  expectTrue(s1 ⊆ s1)
  expectTrue(s2 ⊆ s1)
}

SetTestSuite.test("⊈.Set.Set") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  expectFalse(Set<Int>() ⊈ s1)
  expectTrue(s1 ⊈ Set<Int>())
  expectFalse(s1 ⊈ s1)
  expectFalse(s2 ⊈ s1)
}

SetTestSuite.test("isSubsetOf.Set.Sequence") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = AnySequence([1010, 2020, 3030])
  expectTrue(Set<Int>().isSubsetOf(s1))
  expectFalse(s1.isSubsetOf(Set<Int>()))
  expectTrue(s1.isSubsetOf(s1))
}

SetTestSuite.test("⊆.Set.Sequence") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = AnySequence([1010, 2020, 3030])
  expectTrue(Set<Int>().isSubsetOf(s1))
  expectFalse(s1 ⊆ Set<Int>())
  expectTrue(s1 ⊆ s1)
}

SetTestSuite.test("⊈.Set.Sequence") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = AnySequence([1010, 2020, 3030])
  expectFalse(Set<Int>() ⊈ s1)
  expectTrue(s1 ⊈ Set<Int>())
  expectFalse(s1 ⊈ s1)
}

SetTestSuite.test("isStrictSubsetOf.Set.Set") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  expectTrue(Set<Int>().isStrictSubsetOf(s1))
  expectFalse(s1.isStrictSubsetOf(Set<Int>()))
  expectFalse(s1.isStrictSubsetOf(s1))
}

SetTestSuite.test("⊂.Set.Set") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  expectTrue(Set<Int>() ⊂ s1)
  expectFalse(s1 ⊂ Set<Int>())
  expectFalse(s1 ⊂ s1)
}

SetTestSuite.test("⊄.Set.Set") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  expectFalse(Set<Int>() ⊄ s1)
  expectTrue(s1 ⊄ Set<Int>())
  expectTrue(s1 ⊄ s1)
}

SetTestSuite.test("isStrictSubsetOf.Set.Sequence") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = AnySequence([1010, 2020, 3030])
  expectTrue(Set<Int>().isStrictSubsetOf(s1))
  expectFalse(s1.isStrictSubsetOf(Set<Int>()))
  expectFalse(s1.isStrictSubsetOf(s1))
}

SetTestSuite.test("⊂.Set.Sequence") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = AnySequence([1010, 2020, 3030])
  expectTrue(Set<Int>() ⊂ s1)
  expectFalse(s1 ⊂ Set<Int>())
  expectFalse(s1 ⊂ s1)
}

SetTestSuite.test("⊄.Set.Sequence") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = AnySequence([1010, 2020, 3030])
  expectFalse(Set<Int>() ⊄ s1)
  expectTrue(s1 ⊄ Set<Int>())
  expectTrue(s1 ⊄ s1)
}

SetTestSuite.test("isSupersetOf.Set.Set") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  expectTrue(s1.isSupersetOf(Set<Int>()))
  expectFalse(Set<Int>().isSupersetOf(s1))
  expectTrue(s1.isSupersetOf(s1))
  expectTrue(s1.isSupersetOf(s2))
  expectFalse(Set<Int>().isSupersetOf(s1))
}

SetTestSuite.test("⊇.Set.Set") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  expectTrue(s1 ⊇ Set<Int>())
  expectFalse(Set<Int>() ⊇ s1)
  expectTrue(s1 ⊇ s1)
  expectTrue(s1 ⊇ s2)
  expectFalse(Set<Int>() ⊇ s1)
}

SetTestSuite.test("⊉.Set.Set") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  expectFalse(s1 ⊉ Set<Int>())
  expectTrue(Set<Int>() ⊉ s1)
  expectFalse(s1 ⊉ s1)
  expectFalse(s1 ⊉ s2)
  expectTrue(Set<Int>() ⊉ s1)
}

SetTestSuite.test("isSupersetOf.Set.Sequence") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = AnySequence([1010, 2020, 3030])
  expectTrue(s1.isSupersetOf(Set<Int>()))
  expectFalse(Set<Int>().isSupersetOf(s1))
  expectTrue(s1.isSupersetOf(s1))
  expectTrue(s1.isSupersetOf(s2))
  expectFalse(Set<Int>().isSupersetOf(s1))
}

SetTestSuite.test("⊇.Set.Sequence") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = AnySequence([1010, 2020, 3030])
  expectTrue(s1 ⊇ Set<Int>())
  expectFalse(Set<Int>() ⊇ s1)
  expectTrue(s1 ⊇ s1)
  expectTrue(s1 ⊇ s2)
  expectFalse(Set<Int>() ⊇ s1)
}

SetTestSuite.test("⊉.Set.Sequence") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = AnySequence([1010, 2020, 3030])
  expectFalse(s1 ⊉ Set<Int>())
  expectTrue(Set<Int>() ⊉ s1)
  expectFalse(s1 ⊉ s1)
  expectFalse(s1 ⊉ s2)
  expectTrue(Set<Int>() ⊉ s1)
}

SetTestSuite.test("strictSuperset.Set.Set") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  expectTrue(s1.isStrictSupersetOf(Set<Int>()))
  expectFalse(Set<Int>().isStrictSupersetOf(s1))
  expectFalse(s1.isStrictSupersetOf(s1))
  expectTrue(s1.isStrictSupersetOf(s2))
}

SetTestSuite.test("⊃.Set.Set") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  expectTrue(s1 ⊃ Set<Int>())
  expectFalse(Set<Int>() ⊃ s1)
  expectFalse(s1 ⊃ s1)
  expectTrue(s1 ⊃ s2)
}

SetTestSuite.test("⊅.Set.Set") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  expectFalse(s1 ⊅ Set<Int>())
  expectTrue(Set<Int>() ⊅ s1)
  expectTrue(s1 ⊅ s1)
  expectFalse(s1 ⊅ s2)
}

SetTestSuite.test("strictSuperset.Set.Sequence") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = AnySequence([1010, 2020, 3030])
  expectTrue(s1.isStrictSupersetOf(Set<Int>()))
  expectFalse(Set<Int>().isStrictSupersetOf(s1))
  expectFalse(s1.isStrictSupersetOf(s1))
  expectTrue(s1.isStrictSupersetOf(s2))
}

SetTestSuite.test("⊃.Set.Sequence") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = AnySequence([1010, 2020, 3030])
  expectTrue(s1 ⊃ Set<Int>())
  expectFalse(Set<Int>() ⊃ s1)
  expectFalse(s1 ⊃ s1)
  expectTrue(s1 ⊃ s2)
}

SetTestSuite.test("⊅.Set.Sequence") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = AnySequence([1010, 2020, 3030])
  expectFalse(s1 ⊅ Set<Int>())
  expectTrue(Set<Int>() ⊅ s1)
  expectTrue(s1 ⊅ s1)
  expectFalse(s1 ⊅ s2)
}

SetTestSuite.test("Equatable.Native.Native") {
  let s1 = getCOWFastSet()
  let s2 = getCOWFastSet([1010, 2020, 3030, 4040, 5050, 6060])

  checkEquatable(true, s1, s1)
  checkEquatable(false, s1, Set<Int>())
  checkEquatable(true, Set<Int>(), Set<Int>())
  checkEquatable(false, s1, s2)
}

SetTestSuite.test("isDisjointWith.Set.Set") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  let s3 = Set([7070, 8080, 9090])
  expectTrue(s1.isDisjointWith(s3))
  expectFalse(s1.isDisjointWith(s2))
  expectTrue(Set<Int>().isDisjointWith(s1))
  expectTrue(Set<Int>().isDisjointWith(Set<Int>()))
}

SetTestSuite.test("isDisjointWith.Set.Sequence") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = AnySequence([1010, 2020, 3030])
  let s3 = AnySequence([7070, 8080, 9090])
  expectTrue(s1.isDisjointWith(s3))
  expectFalse(s1.isDisjointWith(s2))
  expectTrue(Set<Int>().isDisjointWith(s1))
  expectTrue(Set<Int>().isDisjointWith(Set<Int>()))
}

SetTestSuite.test("insert") {
  // These are anagrams - they should amount to the same sets.
  var s1 = Set([1010, 2020, 3030])

  let identity1 = unsafeBitCast(s1, Int.self)

  s1.insert(4040)
  expectEqual(4, s1.count)
  expectTrue(s1.contains(1010))
  expectTrue(s1.contains(2020))
  expectTrue(s1.contains(3030))
  expectTrue(s1.contains(4040))
}

SetTestSuite.test("union") {
  let s1 = Set([1010, 2020, 3030])
  let s2 = Set([4040, 5050, 6060])
  let s3 = Set([1010, 2020, 3030, 4040, 5050, 6060])

  let identity1 = unsafeBitCast(s1, Int.self)

  let s4 = s1.union(s2)
  expectEqual(s4, s3)

  // s1 should be unchanged
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectEqual(Set([1010, 2020, 3030]), s1)

  // s4 should be a fresh set
  expectNotEqual(identity1, unsafeBitCast(s4, Int.self))
  expectEqual(s4, s3)

  let s5 = s1.union(s1)
  expectEqual(s5, s1)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(s1, s1.union(Set<Int>()))
  expectEqual(s1, Set<Int>().union(s1))
}

SetTestSuite.test("∪") {
  let s1 = Set([1010, 2020, 3030])
  let s2 = Set([4040, 5050, 6060])
  let s3 = Set([1010, 2020, 3030, 4040, 5050, 6060])

  let identity1 = unsafeBitCast(s1, Int.self)

  let s4 = s1 ∪ s2
  expectEqual(s4, s3)

  // s1 should be unchanged
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectEqual(Set([1010, 2020, 3030]), s1)

  // s4 should be a fresh set
  expectNotEqual(identity1, unsafeBitCast(s4, Int.self))
  expectEqual(s4, s3)

  let s5 = s1 ∪ s1
  expectEqual(s5, s1)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(s1, s1 ∪ Set<Int>())
  expectEqual(s1, Set<Int>() ∪ s1)
}

SetTestSuite.test("unionInPlace") {
  // These are anagrams - they should amount to the same sets.
  var s1 = Set("the morse code".characters)
  let s2 = Set("here come dots".characters)
  let s3 = Set("and then dashes".characters)

  let identity1 = unsafeBitCast(s1, Int.self)

  s1.unionInPlace("".characters)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(s1, s2)
  s1.unionInPlace(s2)
  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  s1.unionInPlace(s3)
  expectNotEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
}

SetTestSuite.test("∪=") {
  // These are anagrams - they should amount to the same sets.
  var s1 = Set("the morse code".characters)
  let s2 = Set("here come dots".characters)
  let s3 = Set("and then dashes".characters)

  let identity1 = unsafeBitCast(s1, Int.self)

  s1 ∪= "".characters
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(s1, s2)
  s1 ∪= s2
  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  s1 ∪= s3
  expectNotEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
}

SetTestSuite.test("subtract") {
  let s1 = Set([1010, 2020, 3030])
  let s2 = Set([4040, 5050, 6060])
  let s3 = Set([1010, 2020, 3030, 4040, 5050, 6060])

  let identity1 = unsafeBitCast(s1, Int.self)

  // Subtracting a disjoint set should not create a
  // unique reference
  let s4 = s1.subtract(s2)
  expectEqual(s1, s4)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectEqual(identity1, unsafeBitCast(s4, Int.self))

  // Subtracting a superset will leave the set empty
  let s5 = s1.subtract(s3)
  expectTrue(s5.isEmpty)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectNotEqual(identity1, unsafeBitCast(s5, Int.self))

  // Subtracting the empty set does nothing
  expectEqual(s1, s1.subtract(Set<Int>()))
  expectEqual(Set<Int>(), Set<Int>().subtract(s1))
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
}

SetTestSuite.test("∖") {
  let s1 = Set([1010, 2020, 3030])
  let s2 = Set([4040, 5050, 6060])
  let s3 = Set([1010, 2020, 3030, 4040, 5050, 6060])

  let identity1 = unsafeBitCast(s1, Int.self)

  // Subtracting a disjoint set should not create a
  // unique reference
  let s4 = s1 ∖ s2
  expectEqual(s1, s4)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectEqual(identity1, unsafeBitCast(s4, Int.self))

  // Subtracting a superset will leave the set empty
  let s5 = s1 ∖ s3
  expectTrue(s5.isEmpty)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectNotEqual(identity1, unsafeBitCast(s5, Int.self))

  // Subtracting the empty set does nothing
  expectEqual(s1, s1 ∖ Set<Int>())
  expectEqual(Set<Int>(), Set<Int>() ∖ s1)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
}

SetTestSuite.test("subtractInPlace") {
  var s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  let s3 = Set([4040, 5050, 6060])

  let identity1 = unsafeBitCast(s1, Int.self)

  s1.subtractInPlace(Set<Int>())
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  s1.subtractInPlace(s3)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
}

SetTestSuite.test("∖=") {
  var s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010, 2020, 3030])
  let s3 = Set([4040, 5050, 6060])

  let identity1 = unsafeBitCast(s1, Int.self)

  s1 ∖= Set<Int>()
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  s1 ∖= s3
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
}

SetTestSuite.test("intersect") {
  let s1 = Set([1010, 2020, 3030])
  let s2 = Set([4040, 5050, 6060])
  var s3 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  var s4 = Set([1010, 2020, 3030])

  let identity1 = unsafeBitCast(s1, Int.self)
  expectEqual(Set([1010, 2020, 3030]),
    Set([1010, 2020, 3030]).intersect(Set([1010, 2020, 3030])) as Set<Int>)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(s1, s1.intersect(s3))
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(Set<Int>(), Set<Int>().intersect(Set<Int>()))
  expectEqual(Set<Int>(), s1.intersect(Set<Int>()))
  expectEqual(Set<Int>(), Set<Int>().intersect(s1))
}

SetTestSuite.test("∩") {
  let s1 = Set([1010, 2020, 3030])
  let s2 = Set([4040, 5050, 6060])
  var s3 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  var s4 = Set([1010, 2020, 3030])

  let identity1 = unsafeBitCast(s1, Int.self)
  expectEqual(Set([1010, 2020, 3030]),
    Set([1010, 2020, 3030]) ∩ Set([1010, 2020, 3030]) as Set<Int>)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(s1, s1 ∩ s3)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(Set<Int>(), Set<Int>() ∩ Set<Int>())
  expectEqual(Set<Int>(), s1 ∩ Set<Int>())
  expectEqual(Set<Int>(), Set<Int>() ∩ s1)
}

SetTestSuite.test("intersectInPlace") {
  var s1 = Set([1010, 2020, 3030])
  let s2 = Set([4040, 5050, 6060])
  var s3 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  var s4 = Set([1010, 2020, 3030])

  let identity1 = unsafeBitCast(s1, Int.self)
  s1.intersectInPlace(s4)
  expectEqual(s1, s4)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  s4.intersectInPlace(s2)
  expectEqual(Set<Int>(), s4)

  let identity2 = unsafeBitCast(s3, Int.self)
  s3.intersectInPlace(s2)
  expectEqual(s3, s2)
  expectTrue(s1.isDisjointWith(s3))
  expectNotEqual(identity1, unsafeBitCast(s3, Int.self))

  var s5 = Set<Int>()
  s5.intersectInPlace(s5)
  expectEqual(s5, Set<Int>())
  s5.intersectInPlace(s1)
  expectEqual(s5, Set<Int>())
}

SetTestSuite.test("∩=") {
  var s1 = Set([1010, 2020, 3030])
  let s2 = Set([4040, 5050, 6060])
  var s3 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  var s4 = Set([1010, 2020, 3030])

  let identity1 = unsafeBitCast(s1, Int.self)
  s1 ∩= s4
  expectEqual(s1, s4)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  s4 ∩= s2
  expectEqual(Set<Int>(), s4)

  let identity2 = unsafeBitCast(s3, Int.self)
  s3 ∩= s2
  expectEqual(s3, s2)
  expectTrue(s1.isDisjointWith(s3))
  expectNotEqual(identity1, unsafeBitCast(s3, Int.self))

  var s5 = Set<Int>()
  s5 ∩= s5
  expectEqual(s5, Set<Int>())
  s5 ∩= s1
  expectEqual(s5, Set<Int>())
}

SetTestSuite.test("exclusiveOr") {

  // Overlap with 4040, 5050, 6060
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([4040, 5050, 6060, 7070, 8080, 9090])
  let result = Set([1010, 2020, 3030, 7070, 8080, 9090])
  let universe = Set([1010, 2020, 3030, 4040, 5050, 6060,
                       7070, 8080, 9090])

  let identity1 = unsafeBitCast(s1, Int.self)

  let s3 = s1.exclusiveOr(s2)

  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(s3, result)

  expectEqual(s1.exclusiveOr(s2),
    s1.union(s2).intersect(universe.subtract(s1.intersect(s2))))

  expectEqual(s1.exclusiveOr(s2),
    s1.intersect(universe.subtract(s2)).union(universe.subtract(s1).intersect(s2)))

  expectTrue(s1.exclusiveOr(s1).isEmpty)
}

SetTestSuite.test("⨁") {

  // Overlap with 4040, 5050, 6060
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([4040, 5050, 6060, 7070, 8080, 9090])
  let result = Set([1010, 2020, 3030, 7070, 8080, 9090])
  let universe = Set([1010, 2020, 3030, 4040, 5050, 6060,
                       7070, 8080, 9090])

  let identity1 = unsafeBitCast(s1, Int.self)

  let s3 = s1 ⨁ s2

  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(s3, result)

  expectEqual(s1 ⨁ s2,
    (s1 ∪ s2) ∩ (universe ∖ (s1 ∩ s2)))

  expectEqual(s1 ⨁ s2,
    s1 ∩ (universe ∖ s2) ∪ (universe ∖ s1) ∩ s2)

  expectTrue((s1 ⨁ s1).isEmpty)
}

SetTestSuite.test("exclusiveOrInPlace") {
  // Overlap with 4040, 5050, 6060
  var s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010])
  let result = Set([2020, 3030, 4040, 5050, 6060])

  // s1 ⨁ s2 == result
  let identity1 = unsafeBitCast(s1, Int.self)
  s1.exclusiveOrInPlace(s2)

  // Removing just one element shouldn't cause an identity change
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(s1, result)

  // A ⨁ A == {}
  s1.exclusiveOrInPlace(s1)
  expectTrue(s1.isEmpty)

  // Removing all elements should cause an identity change
  expectNotEqual(identity1, unsafeBitCast(s1, Int.self))
}

SetTestSuite.test("⨁=") {
  // Overlap with 4040, 5050, 6060
  var s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = Set([1010])
  let result = Set([2020, 3030, 4040, 5050, 6060])

  let identity1 = unsafeBitCast(s1, Int.self)
  s1 ⨁= s2

  // Removing just one element shouldn't cause an identity change
  expectEqual(identity1, unsafeBitCast(s1, Int.self))

  expectEqual(s1, result)

  s1 ⨁= s1
  expectTrue(s1.isEmpty)

  // Removing all elements should cause an identity change
  expectNotEqual(identity1, unsafeBitCast(s1, Int.self))
}

SetTestSuite.test("removeFirst") {
  var s1 = Set([1010, 2020, 3030])
  let s2 = s1
  let empty = Set<Int>()

  let a1 = s1.removeFirst()

  expectFalse(s1.contains(a1))
  expectTrue(s2.contains(a1))
  expectNotEqual(unsafeBitCast(s1, Int.self), unsafeBitCast(s2, Int.self))
  expectTrue(s1.isSubsetOf(s2))
  expectEmpty(empty.first)
}

SetTestSuite.test("remove(member)") {
  let s1 = Set([1010, 2020, 3030])
  var s2 = Set<Int>(minimumCapacity: 10)
  for i in [1010, 2020, 3030] {
    s2.insert(i)
  }

  let identity1 = unsafeBitCast(s2, Int.self)
  s2.remove(4040)
  expectEqual(s2, s1)
  expectEqual(identity1, unsafeBitCast(s2, Int.self))

  s2.remove(3030)
  expectEqual(identity1, unsafeBitCast(s2, Int.self))

  s2.remove(2020)
  expectEqual(identity1, unsafeBitCast(s2, Int.self))

  s2.remove(1010)
  expectEqual(identity1, unsafeBitCast(s2, Int.self))
  expectEqual(Set<Int>(), s2)
  expectTrue(s2.isEmpty)
}

SetTestSuite.test("contains") {
  let s1 = Set([1010, 2020, 3030])
  expectTrue(s1.contains(1010))
  expectFalse(s1.contains(999))
}

SetTestSuite.test("∈") {
  let s1 = Set([1010, 2020, 3030])
  expectTrue(1010  ∈ s1)
  expectFalse(999 ∈ s1)
}

SetTestSuite.test("∉") {
  let s1 = Set([1010, 2020, 3030])
  expectFalse(1010 ∉ s1)
  expectTrue(999 ∉ s1)
}

SetTestSuite.test("memberAtIndex") {
  let s1 = Set([1010, 2020, 3030])

  let foundIndex = s1.indexOf(1010)!
  expectEqual(1010, s1[foundIndex])
}

SetTestSuite.test("first") {
  let s1 = Set([1010, 2020, 3030])
  let emptySet = Set<Int>()

  expectTrue(s1.contains(s1.first!))
  expectEmpty(emptySet.first)
}

SetTestSuite.test("isEmpty") {
  let s1 = Set([1010, 2020, 3030])
  expectFalse(s1.isEmpty)

  let emptySet = Set<Int>()
  expectTrue(emptySet.isEmpty)
}

SetTestSuite.test("count") {
  let s1 = Set([1010, 2020, 3030])
  var s2 = Set([4040, 5050, 6060])
  expectEqual(0, Set<Int>().count)
  expectEqual(3, s1.count)
}

SetTestSuite.test("contains") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  expectTrue(s1.contains(1010))
  expectFalse(s1.contains(999))
  expectFalse(Set<Int>().contains(1010))
}

SetTestSuite.test("_customContainsEquatableElement") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  expectTrue(s1._customContainsEquatableElement(1010)!)
  expectFalse(s1._customContainsEquatableElement(999)!)
  expectFalse(Set<Int>()._customContainsEquatableElement(1010)!)
}

SetTestSuite.test("indexOf") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let foundIndex1 = s1.indexOf(1010)!
  expectEqual(1010, s1[foundIndex1])

  expectEmpty(s1.indexOf(999))
}

SetTestSuite.test("popFirst") {
  // Empty
  do {
    var s = Set<Int>()
    let popped = s.popFirst()
    expectEmpty(popped)
    expectTrue(s.isEmpty)
  }

  do {
    var popped = [Int]()
    var s = Set([1010, 2020, 3030])
    let expected = Array(s)
    while let element = s.popFirst() {
      popped.append(element)
    }
    expectEqualSequence(expected, Array(popped))
    expectTrue(s.isEmpty)
  }
}

SetTestSuite.test("removeAtIndex") {
  // Test removing from the startIndex, the middle, and the end of a set.
  for i in 1...3 {
    var s = Set<Int>([1010, 2020, 3030])
    let removed = s.removeAtIndex(s.indexOf(i*1010)!)
    expectEqual(i*1010, removed)
    expectEqual(2, s.count)
    expectEmpty(s.indexOf(i*1010))
    let origKeys: [Int] = [1010, 2020, 3030]
    expectEqual(origKeys.filter { $0 != (i*1010) }, [Int](s).sort())
  }
}

SetTestSuite.test("_customIndexOfEquatableElement") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let foundIndex1 = s1._customIndexOfEquatableElement(1010)!!
  expectEqual(1010, s1[foundIndex1])

  expectEmpty(s1._customIndexOfEquatableElement(999)!)
}

SetTestSuite.test("commutative") {
  let s1 = Set([1010, 2020, 3030])
  let s2 = Set([2020, 3030])
  expectTrue(equalsUnordered(s1.intersect(s2), s2.intersect(s1)))
  expectTrue(equalsUnordered(s1.union(s2), s2.union(s1)))
}

SetTestSuite.test("associative") {
  let s1 = Set([1010, 2020, 3030])
  let s2 = Set([2020, 3030])
  let s3 = Set([1010, 2020, 3030])
  let s4 = Set([2020, 3030])
  let s5 = Set([7070, 8080, 9090])

  expectTrue(equalsUnordered(s1.intersect(s2).intersect(s3),
    s1.intersect(s2.intersect(s3))))
  expectTrue(equalsUnordered(s3.union(s4).union(s5), s3.union(s4.union(s5))))
}

SetTestSuite.test("distributive") {
  let s1 = Set([1010])
  let s2 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s3 = Set([2020, 3030])
  expectTrue(equalsUnordered(s1.union(s2.intersect(s3)),
    s1.union(s2).intersect(s1.union(s3))))

  let s4 = Set([2020, 3030])
  expectTrue(equalsUnordered(s4.intersect(s1.union(s3)),
    s4.intersect(s1).union(s4.intersect(s3))))
}

SetTestSuite.test("idempotent") {
  let s1 = Set([1010, 2020, 3030, 4040, 5050, 6060])
  expectTrue(equalsUnordered(s1, s1.intersect(s1)))
  expectTrue(equalsUnordered(s1, s1.union(s1)))
}

SetTestSuite.test("absorption") {
  let s1 = Set([1010, 2020, 3030])
  let s2 = Set([4040, 5050, 6060])
  let s3 = Set([2020, 3030])
  expectTrue(equalsUnordered(s1, s1.union(s1.intersect(s2))))
  expectTrue(equalsUnordered(s1, s1.intersect(s1.union(s3))))
}

SetTestSuite.test("misc") {
  // Set with other types
  do {
    var s = Set([1.1, 2.2, 3.3])
    s.insert(4.4)
    expectTrue(s.contains(1.1))
    expectTrue(s.contains(2.2))
    expectTrue(s.contains(3.3))
  }

  do {
    var s = Set(["Hello", "world"])
    expectTrue(s.contains("Hello"))
    expectTrue(s.contains("world"))
  }
}

SetTestSuite.test("Hashable") {
  let s1 = Set([1010])
  let s2 = Set([2020])
  checkHashable(s1 == s2, s1, s2)

  // Explicit types help the type checker quite a bit.
  let ss1 = Set([Set([1010] as [Int]), Set([2020] as [Int]), Set([3030] as [Int])])
  let ss11 = Set([Set([2020] as [Int]), Set([3030] as [Int]), Set([2020] as [Int])])
  let ss2 = Set([Set([9090] as [Int])])
  checkHashable(ss1 == ss11, ss1, ss11)
  checkHashable(ss1 == ss2, ss1, ss2)
}

SetTestSuite.test("Operator.Precedence") {
  let s1 = Set([1010, 2020, 3030])
  let s2 = Set([3030, 4040, 5050])
  let s3 = Set([6060, 7070, 8080])
  let s4 = Set([8080, 9090, 100100])

  // intersection higher precedence than union
  expectEqual(s1 ∪ (s2 ∩ s3) ∪ s4, s1 ∪ s2 ∩ s3 ∪ s4 as Set<Int>)

  // intersection higher precedence than complement
  expectEqual(s1 ∖ (s2 ∩ s3) ∖ s4, s1 ∖ s2 ∩ s3 ∖ s4 as Set<Int>)

  // intersection higher precedence than exclusive-or
  expectEqual(s1 ⨁ (s2 ∩ s3) ⨁ s4, s1 ⨁ s2 ∩ s3 ⨁ s4 as Set<Int>)

  // union/complement/exclusive-or same precedence
  expectEqual((((s1 ∪ s3) ∖ s2) ⨁ s4), s1 ∪ s3 ∖ s2 ⨁ s4 as Set<Int>)

  // ∪= should happen last.
  var s5 = Set([1010, 2020, 3030])
  s5 ∪= Set([4040, 5050, 6060]) ∪ [7070]
  expectEqual(Set([1010, 2020, 3030, 4040, 5050, 6060, 7070]), s5)

  // ∩= should happen last.
  var s6 = Set([1010, 2020, 3030])
  s6 ∩= Set([1010, 2020, 3030]) ∩ [3030]
  expectEqual(Set([3030]), s6)

  // ⨁= should happen last.
  var s7 = Set([1010, 2020, 3030])
  s7 ⨁= Set([1010, 2020, 3030]) ⨁ [1010, 3030]
  expectEqual(Set([1010, 3030]), s7)

  // ∖= should happen last.
  var s8 = Set([1010, 2020, 3030])
  s8 ∖= Set([2020, 3030]) ∖ [3030]
  expectEqual(Set([1010, 3030]), s8)
}

//===---
// Check that generators traverse a snapshot of the collection.
//===---

SetTestSuite.test("mutationDoesNotAffectGenerator/remove,1") {
  var set = Set([ 1010, 1020, 1030 ])
  var g = set.generate()
  expectOptionalEqual(1010, set.remove(1010))

  expectEqualsUnordered([ 1010, 1020, 1030 ], Array(GeneratorSequence(g)))
}

SetTestSuite.test("mutationDoesNotAffectGenerator/remove,all") {
  var set = Set([ 1010, 1020, 1030 ])
  var g = set.generate()
  expectOptionalEqual(1010, set.remove(1010))
  expectOptionalEqual(1020, set.remove(1020))
  expectOptionalEqual(1030, set.remove(1030))

  expectEqualsUnordered([ 1010, 1020, 1030 ], Array(GeneratorSequence(g)))
}

SetTestSuite.test("mutationDoesNotAffectGenerator/removeAll,keepCapacity=false") {
  var set = Set([ 1010, 1020, 1030 ])
  var g = set.generate()
  set.removeAll(keepCapacity: false)

  expectEqualsUnordered([ 1010, 1020, 1030 ], Array(GeneratorSequence(g)))
}

SetTestSuite.test("mutationDoesNotAffectGenerator/removeAll,keepCapacity=true") {
  var set = Set([ 1010, 1020, 1030 ])
  var g = set.generate()
  set.removeAll(keepCapacity: true)

  expectEqualsUnordered([ 1010, 1020, 1030 ], Array(GeneratorSequence(g)))
}

runAllTests()

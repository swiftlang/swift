// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// FIXME: -fobjc-abi-version=2 is a band-aid fix for for rdar://16946936
//
// RUN: cp %s %t/main.swift
// RUN: xcrun -sdk %target-sdk-name clang++ -fobjc-arc -fobjc-abi-version=2 -arch %target-cpu %S/Inputs/SlurpFastEnumeration/SlurpFastEnumeration.m -c -o %t/SlurpFastEnumeration.o
// RUN: %target-build-swift %S/Inputs/DictionaryKeyValueTypes.swift %t/main.swift -I %S/Inputs/SlurpFastEnumeration/ -Xlinker %t/SlurpFastEnumeration.o -o %t/Set -Xfrontend -disable-access-control
//
// RUN: %target-run %t/Set

import StdlibUnittest
import Foundation

let hugeNumberArray = map(Array(0..<500), {
  (i: Int) -> Int in
      return random() })

var SetTestSuite = TestSuite("Set")

SetTestSuite.setUp {
  resetLeaksOfDictionaryKeysValues()
}

SetTestSuite.tearDown {
  expectNoLeaksOfDictionaryKeysValues()
}

func getCOWFastSet(_ members: [Int] = [1010, 2020, 3030]) -> _Set<Int> {
  var s = _Set<Int>(minimumCapacity: 10)
  for member in members {
    s.insert(member)
  }
  expectTrue(isNativeSet(s))
  return s
}

func getCOWSlowSet(_ members: [Int] = [1010, 2020, 3030]) -> _Set<TestKeyTy> {
  var s = _Set<TestKeyTy>(minimumCapacity: 10)
  for member in members {
    s.insert(TestKeyTy(member))
  }
  expectTrue(isNativeSet(s))
  return s
}

func helperDeleteThree(k1: TestKeyTy, k2: TestKeyTy, k3: TestKeyTy) {
  var s1 = _Set<TestKeyTy>(minimumCapacity: 10)

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
  // FIXME: this is not uniform.
  return random() % max
}

func pickRandom<T>(a: [T]) -> T {
  return a[uniformRandom(a.count)]
}

func isNativeSet<T: Hashable>(s: _Set<T>) -> Bool {
  switch s._variantStorage {
  case .Native:
    return true
  case .Cocoa:
    return false
  }
}

func isNativeNSSet(s: NSSet) -> Bool {
  var className: NSString = NSStringFromClass(s.dynamicType)
  return className.rangeOfString("NativeSetStorage").length > 0
}

func isCocoaNSSet(s: NSSet) -> Bool {
  var className: NSString = NSStringFromClass(s.dynamicType)
  return className.rangeOfString("NSSet").length > 0 ||
    className.rangeOfString("NSCFSet").length > 0
}

func getBridgedEmptyNSSet() -> NSSet {
  var s = _Set<TestObjCKeyTy>()

  let bridged = unsafeBitCast(_convertSetToNSSet(s), NSSet.self)
  expectTrue(isNativeNSSet(bridged))

  return bridged
}


func isCocoaSet<T: Hashable>(s: _Set<T>) -> Bool {
  return !isNativeSet(s)
}

func equalsUnordered(lhs: _Set<Int>, rhs: _Set<Int>) -> Bool {
  return equal(sorted(lhs), sorted(rhs)) {
    $0 == $1
  }
}

func equalsUnorderedPairs(lhs: [(Int, Int)], rhs: [(Int, Int)]) -> Bool {
  let compareRight = { (a: (Int, Int), b: (Int, Int)) -> Bool in
    return a.1 < b.1
  }
  let compareLeft = { (a: (Int, Int), b: (Int, Int)) -> Bool in
    return a.0 < b.0
  }
  return equal(
    sorted(sorted(lhs, compareRight), compareLeft),
    sorted(sorted(rhs, compareRight), compareLeft)) {
      $0.0 == $1.0 && $0.1 == $1.1
  }
}

/// Get an NSSet of TestObjCKeyTy values
func getAsNSSet(_ members: [Int] = [1010, 2020, 3030]) -> NSSet {
  let nsArray = NSMutableArray()
  for member in members {
    nsArray.addObject(TestObjCKeyTy(member))
  }
  return NSMutableSet(array: nsArray)
}

/// Get an NSMutableSet of TestObjCKeyTy values
func getAsNSMutableSet(_ members: [Int] = [1010, 2020, 3030]) -> NSMutableSet {
  let nsArray = NSMutableArray()
  for member in members {
    nsArray.addObject(TestObjCKeyTy(member))
  }
  return NSMutableSet(array: nsArray)
}

/// Get a _Set<NSObject> (Set<TestObjCKeyTy>) backed by Cocoa storage
func getBridgedVerbatimSet(_ members: [Int] = [1010, 2020, 3030])
  -> _Set<NSObject> {
  var nss = getAsNSSet(members)
  let result: _Set<NSObject> = _convertNSSetToSet(nss)
  expectTrue(isCocoaSet(result))
  return result
}

/// Get a _Set<NSObject> (Set<TestObjCKeyTy>) backed by native storage
func getNativeBridgedVerbatimSet(_ members: [Int] = [1010, 2020, 3030]) ->
  _Set<NSObject> {
  let result: _Set<NSObject> = _Set(members.map({ TestObjCKeyTy($0) }))
  expectTrue(isNativeSet(result))
  return result
}

/// Get a _Set<NSObject> (Set<TestObjCKeyTy>) backed by Cocoa storage
func getHugeBridgedVerbatimSet() -> _Set<NSObject> {
  var nss = getAsNSSet(hugeNumberArray)
  let result: _Set<NSObject> = _convertNSSetToSet(nss)
  expectTrue(isCocoaSet(result))
  return result
}

/// Get a _Set<TestBridgedKeyTy> backed by native storage
func getBridgedNonverbatimSet(_ members: [Int] = [1010, 2020, 3030]) ->
  _Set<TestBridgedKeyTy> {
  var nss = getAsNSSet(members)
  let identity1 = unsafeBitCast(nss, Word.self)
  let result: _Set<TestBridgedKeyTy> =
    Swift._forceBridgeFromObjectiveC(nss, _Set.self)
  expectTrue(isNativeSet(result))
  return result
}

/// Get a larger _Set<TestBridgedKeyTy> backed by native storage
func getHugeBridgedNonverbatimSet() -> _Set<TestBridgedKeyTy> {
  var nss = getAsNSSet(hugeNumberArray)
  let identity1 = unsafeBitCast(nss, Word.self)
  let result: _Set<TestBridgedKeyTy> =
    Swift._forceBridgeFromObjectiveC(nss, _Set.self)
  expectTrue(isNativeSet(result))
  return result
}

func getBridgedVerbatimSetAndNSMutableSet() -> (_Set<NSObject>, NSMutableSet) {
  var nss = getAsNSMutableSet()
  return (_convertNSSetToSet(nss), nss)
}

func getBridgedNonverbatimSetAndNSMutableSet()
    -> (_Set<TestBridgedKeyTy>, NSMutableSet) {
  var nss = getAsNSMutableSet()
  return (Swift._forceBridgeFromObjectiveC(nss, _Set.self), nss)
}

func getBridgedNSSetOfRefTypesBridgedVerbatim() -> NSSet {
  expectTrue(_isBridgedVerbatimToObjectiveC(TestObjCKeyTy.self))

  var s = _Set<TestObjCKeyTy>(minimumCapacity: 32)
  s.insert(TestObjCKeyTy(1010))
  s.insert(TestObjCKeyTy(2020))
  s.insert(TestObjCKeyTy(3030))

  let bridged =
    unsafeBitCast(_convertSetToNSSet(s), NSSet.self)

  expectTrue(isNativeNSSet(bridged))

  return bridged
}

func getBridgedNSSet_ValueTypesCustomBridged(
  numElements: Int = 3
) -> NSSet {
  expectTrue(!_isBridgedVerbatimToObjectiveC(TestBridgedKeyTy.self))

  var s = _Set<TestBridgedKeyTy>()
  for i in 1..<(numElements + 1) {
    s.insert(TestBridgedKeyTy(i * 1000 + i * 10))
  }

  let bridged = _convertSetToNSSet(s)
  expectTrue(isNativeNSSet(bridged))

  return bridged
}

func getRoundtripBridgedNSSet() -> NSSet {
  let items = NSMutableArray()
  items.addObject(TestObjCKeyTy(1010))
  items.addObject(TestObjCKeyTy(2020))
  items.addObject(TestObjCKeyTy(3030))

  var nss = NSSet(array: items)

  var s: _Set<NSObject> = _convertNSSetToSet(nss)

  let bridgedBack = _convertSetToNSSet(s)
  expectTrue(isCocoaNSSet(bridgedBack))
  // FIXME: this should be true.
  //expectTrue(unsafeBitCast(nsd, Word.self) == unsafeBitCast(bridgedBack, Word.self))

  return bridgedBack
}

func getBridgedNSSet_MemberTypesCustomBridged() -> NSSet {
  expectFalse(_isBridgedVerbatimToObjectiveC(TestBridgedKeyTy.self))

  var s = _Set<TestBridgedKeyTy>()
  s.insert(TestBridgedKeyTy(1010))
  s.insert(TestBridgedKeyTy(2020))
  s.insert(TestBridgedKeyTy(3030))

  let bridged = _convertSetToNSSet(s)
  expectTrue(isNativeNSSet(bridged))

  return bridged
}

SetTestSuite.test("sizeof") {
  var s = _Set(["Hello", "world"])
#if arch(i386) || arch(arm)
  expectEqual(4, sizeofValue(s))
#else
  expectEqual(8, sizeofValue(s))
#endif
}

SetTestSuite.test("COW.Smoke") {
  var s1 = _Set<TestKeyTy>(minimumCapacity: 10)
  for i in [1010, 2020, 3030]{ s1.insert(TestKeyTy(i)) }
  var identity1 = unsafeBitCast(s1, Word.self)

  var s2 = s1
  _fixLifetime(s2)
  expectEqual(identity1, unsafeBitCast(s2, Word.self))

  s2.insert(TestKeyTy(4040))
  expectNotEqual(identity1, unsafeBitCast(s2, Word.self))

  s2.insert(TestKeyTy(5050))
  expectEqual(identity1, unsafeBitCast(s1, Word.self))

  // Keep variables alive.
  _fixLifetime(s1)
  _fixLifetime(s2)
}

SetTestSuite.test("COW.Fast.IndexesDontAffectUniquenessCheck") {
  var s = getCOWFastSet()
  var identity1 = unsafeBitCast(s, Word.self)

  var startIndex = s.startIndex
  var endIndex = s.endIndex
  expectNotEqual(startIndex, endIndex)
  expectTrue(startIndex < endIndex)
  expectTrue(startIndex <= endIndex)
  expectFalse(startIndex >= endIndex)
  expectFalse(startIndex > endIndex)

  expectEqual(identity1, unsafeBitCast(s, Word.self))

  s.insert(4040)
  expectEqual(identity1, unsafeBitCast(s, Word.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

SetTestSuite.test("COW.Slow.IndexesDontAffectUniquenessCheck") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Word.self)

  var startIndex = s.startIndex
  var endIndex = s.endIndex
  expectNotEqual(startIndex, endIndex)
  expectTrue(startIndex < endIndex)
  expectTrue(startIndex <= endIndex)
  expectFalse(startIndex >= endIndex)
  expectFalse(startIndex > endIndex)

  expectEqual(identity1, unsafeBitCast(s, Word.self))
  s.insert(TestKeyTy(4040))
  expectEqual(identity1, unsafeBitCast(s, Word.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

SetTestSuite.test("COW.Fast.SubscriptWithIndexDoesNotReallocate") {
  var s = getCOWFastSet()
  var identity1 = unsafeBitCast(s, Word.self)

  var startIndex = s.startIndex
  let empty = startIndex == s.endIndex
  expectNotEqual(empty, (s.startIndex < s.endIndex))
  expectTrue(s.startIndex <= s.endIndex)
  expectEqual(empty, (s.startIndex >= s.endIndex))
  expectFalse(s.startIndex > s.endIndex)
  expectEqual(identity1, unsafeBitCast(s, Word.self))

  expectNotEqual(0, s[startIndex])
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("COW.Slow.SubscriptWithIndexDoesNotReallocate") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Word.self)

  var startIndex = s.startIndex
  let empty = startIndex == s.endIndex
  expectNotEqual(empty, (s.startIndex < s.endIndex))
  expectTrue(s.startIndex <= s.endIndex)
  expectEqual(empty, (s.startIndex >= s.endIndex))
  expectFalse(s.startIndex > s.endIndex)
  expectEqual(identity1, unsafeBitCast(s, Word.self))

  expectNotEqual(TestKeyTy(0), s[startIndex])
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("COW.Fast.ContainsDoesNotReallocate") {
  var s = getCOWFastSet()
  var identity1 = unsafeBitCast(s, Word.self)

  expectTrue(s.contains(1010))
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("COW.Slow.ContainsDoesNotReallocate") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Word.self)

  expectTrue(s.contains(TestKeyTy(1010)))
  expectEqual(identity1, unsafeBitCast(s, Word.self))

  // Insert a new key-value pair.
  s.insert(TestKeyTy(4040))
  expectEqual(identity1, unsafeBitCast(s, Word.self))
  expectEqual(4, s.count)
  expectTrue(s.contains(TestKeyTy(1010)))
  expectTrue(s.contains(TestKeyTy(2020)))
  expectTrue(s.contains(TestKeyTy(3030)))
  expectTrue(s.contains(TestKeyTy(4040)))

  // Delete an existing key.
  s.remove(TestKeyTy(1010))
  expectEqual(identity1, unsafeBitCast(s, Word.self))
  expectEqual(3, s.count)
  expectTrue(s.contains(TestKeyTy(2020)))
  expectTrue(s.contains(TestKeyTy(3030)))
  expectTrue(s.contains(TestKeyTy(4040)))

  // Try to delete a key that does not exist.
  s.remove(TestKeyTy(777))
  expectEqual(identity1, unsafeBitCast(s, Word.self))
  expectEqual(3, s.count)
  expectTrue(s.contains(TestKeyTy(2020)))
  expectTrue(s.contains(TestKeyTy(3030)))
  expectTrue(s.contains(TestKeyTy(4040)))
}

SetTestSuite.test("COW.Fast.InsertDoesNotReallocate") {
  var s1 = getCOWFastSet()

  let identity1 = unsafeBitCast(s1, Word.self)
  let count1 = s1.count

  // Inserting a redundant element should not create new storage
  s1.insert(2020)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
  expectEqual(count1, s1.count)

  s1.insert(4040)
  s1.insert(5050)
  s1.insert(6060)
  expectEqual(count1 + 3, s1.count)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
}

SetTestSuite.test("COW.Slow.InsertDoesNotReallocate") {
  if true {
    var s1 = getCOWSlowSet()

    let identity1 = unsafeBitCast(s1, Word.self)
    let count1 = s1.count

    // Inserting a redundant element should not create new storage
    s1.insert(TestKeyTy(2020))
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectEqual(count1, s1.count)

    s1.insert(TestKeyTy(4040))
    s1.insert(TestKeyTy(5050))
    s1.insert(TestKeyTy(6060))
    expectEqual(count1 + 3, s1.count)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
  }

  if true {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Word.self)

    var s2 = s1
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectEqual(identity1, unsafeBitCast(s2, Word.self))

    s2.insert(TestKeyTy(2040))
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectNotEqual(identity1, unsafeBitCast(s2, Word.self))

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

  if true {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Word.self)

    var s2 = s1
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectEqual(identity1, unsafeBitCast(s2, Word.self))

    // Insert a redundant element.
    s2.insert(TestKeyTy(2020))
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectNotEqual(identity1, unsafeBitCast(s2, Word.self))

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
  var identity1 = unsafeBitCast(s, Word.self)

  // Find an existing key.
  if true {
    var foundIndex1 = s.indexOf(1010)!
    expectEqual(identity1, unsafeBitCast(s, Word.self))

    var foundIndex2 = s.indexOf(1010)!
    expectEqual(foundIndex1, foundIndex2)

    expectEqual(1010, s[foundIndex1])
    expectEqual(identity1, unsafeBitCast(s, Word.self))
  }

  // Try to find a key that is not present.
  if true {
    var foundIndex1 = s.indexOf(1111)
    expectEmpty(foundIndex1)
    expectEqual(identity1, unsafeBitCast(s, Word.self))
  }
}

SetTestSuite.test("COW.Slow.IndexForMemberDoesNotReallocate") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Word.self)

  // Find an existing key.
  if true {
    var foundIndex1 = s.indexOf(TestKeyTy(1010))!
    expectEqual(identity1, unsafeBitCast(s, Word.self))

    var foundIndex2 = s.indexOf(TestKeyTy(1010))!
    expectEqual(foundIndex1, foundIndex2)

    expectEqual(TestKeyTy(1010), s[foundIndex1])
    expectEqual(identity1, unsafeBitCast(s, Word.self))
  }

  // Try to find a key that is not present.
  if true {
    var foundIndex1 = s.indexOf(TestKeyTy(1111))
    expectEmpty(foundIndex1)
    expectEqual(identity1, unsafeBitCast(s, Word.self))
  }
}

SetTestSuite.test("COW.Fast.RemoveAtIndexDoesNotReallocate") {
  if true {
    var s = getCOWFastSet()
    var identity1 = unsafeBitCast(s, Word.self)

    let foundIndex1 = s.indexOf(1010)!
    expectEqual(identity1, unsafeBitCast(s, Word.self))

    expectEqual(1010, s[foundIndex1])

    s.removeAtIndex(foundIndex1)
    expectEqual(identity1, unsafeBitCast(s, Word.self))
    expectEmpty(s.indexOf(1010))
  }

  if true {
    var s1 = getCOWFastSet()
    var identity1 = unsafeBitCast(s1, Word.self)

    var s2 = s1
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectEqual(identity1, unsafeBitCast(s2, Word.self))

    var foundIndex1 = s2.indexOf(1010)!
    expectEqual(1010, s2[foundIndex1])
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectEqual(identity1, unsafeBitCast(s2, Word.self))

    s2.removeAtIndex(foundIndex1)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectNotEqual(identity1, unsafeBitCast(s2, Word.self))
    expectEmpty(s2.indexOf(1010))
  }
}

SetTestSuite.test("COW.Slow.RemoveAtIndexDoesNotReallocate") {
  if true {
    var s = getCOWSlowSet()
    var identity1 = unsafeBitCast(s, Word.self)

    let foundIndex1 = s.indexOf(TestKeyTy(1010))!
    expectEqual(identity1, unsafeBitCast(s, Word.self))

    expectEqual(TestKeyTy(1010), s[foundIndex1])

    s.removeAtIndex(foundIndex1)
    expectEqual(identity1, unsafeBitCast(s, Word.self))
    expectEmpty(s.indexOf(TestKeyTy(1010)))
  }

  if true {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Word.self)

    var s2 = s1
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectEqual(identity1, unsafeBitCast(s2, Word.self))

    var foundIndex1 = s2.indexOf(TestKeyTy(1010))!
    expectEqual(TestKeyTy(1010), s2[foundIndex1])
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectEqual(identity1, unsafeBitCast(s2, Word.self))

    s2.removeAtIndex(foundIndex1)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectNotEqual(identity1, unsafeBitCast(s2, Word.self))
    expectEmpty(s2.indexOf(TestKeyTy(1010)))
  }
}

SetTestSuite.test("COW.Fast.RemoveDoesNotReallocate") {
  if true {
    var s1 = getCOWFastSet()
    var identity1 = unsafeBitCast(s1, Word.self)

    var deleted = s1.remove(0)
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))

    deleted = s1.remove(1010)
    expectOptionalEqual(1010, deleted)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))

    // Keep variables alive.
    _fixLifetime(s1)
  }

  if true {
    var s1 = getCOWFastSet()
    var identity1 = unsafeBitCast(s1, Word.self)

    var s2 = s1
    var deleted = s2.remove(0)
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectEqual(identity1, unsafeBitCast(s2, Word.self))

    deleted = s2.remove(1010)
    expectOptionalEqual(1010, deleted)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectNotEqual(identity1, unsafeBitCast(s2, Word.self))

    // Keep variables alive.
    _fixLifetime(s1)
    _fixLifetime(s2)
  }
}

SetTestSuite.test("COW.Slow.RemoveDoesNotReallocate") {
  if true {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Word.self)

    var deleted = s1.remove(TestKeyTy(0))
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))

    deleted = s1.remove(TestKeyTy(1010))
    expectOptionalEqual(TestKeyTy(1010), deleted)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))

    // Keep variables alive.
    _fixLifetime(s1)
  }

  if true {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Word.self)

    var s2 = s1
    var deleted = s2.remove(TestKeyTy(0))
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectEqual(identity1, unsafeBitCast(s2, Word.self))

    deleted = s2.remove(TestKeyTy(1010))
    expectOptionalEqual(TestKeyTy(1010), deleted)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectNotEqual(identity1, unsafeBitCast(s2, Word.self))

    // Keep variables alive.
    _fixLifetime(s1)
    _fixLifetime(s2)
  }
}

SetTestSuite.test("COW.Fast.UnionInPlaceSmallSetDoesNotReallocate") {
  var s1 = getCOWFastSet()
  let s2 = _Set([4040, 5050, 6060])
  let s3 = _Set([1010, 2020, 3030, 4040, 5050, 6060])

  let identity1 = unsafeBitCast(s1, Word.self)

  // Adding the empty set should obviously not allocate
  s1.unionInPlace(_Set<Int>())
  expectEqual(identity1, unsafeBitCast(s1, Word.self))

  // adding a small set shouldn't cause a reallocation
  s1.unionInPlace(s2)
  expectEqual(s1, s3)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
}

SetTestSuite.test("COW.Fast.RemoveAllDoesNotReallocate") {
  if true {
    var s = getCOWFastSet()
    let originalCapacity = s._variantStorage.native.capacity
    expectEqual(3, s.count)
    expectTrue(s.contains(1010))

    s.removeAll()
    // We can not expectTrue that identity changed, since the new buffer of
    // smaller size can be allocated at the same address as the old one.
    var identity1 = unsafeBitCast(s, Word.self)
    expectTrue(s._variantStorage.native.capacity < originalCapacity)
    expectEqual(0, s.count)
    expectFalse(s.contains(1010))

    s.removeAll()
    expectEqual(identity1, unsafeBitCast(s, Word.self))
    expectEqual(0, s.count)
    expectFalse(s.contains(1010))
  }

  if true {
    var s = getCOWFastSet()
    var identity1 = unsafeBitCast(s, Word.self)
    let originalCapacity = s._variantStorage.native.capacity
    expectEqual(3, s.count)
    expectTrue(s.contains(1010))

    s.removeAll(keepCapacity: true)
    expectEqual(identity1, unsafeBitCast(s, Word.self))
    expectEqual(originalCapacity, s._variantStorage.native.capacity)
    expectEqual(0, s.count)
    expectFalse(s.contains(1010))

    s.removeAll(keepCapacity: true)
    expectEqual(identity1, unsafeBitCast(s, Word.self))
    expectEqual(originalCapacity, s._variantStorage.native.capacity)
    expectEqual(0, s.count)
    expectFalse(s.contains(1010))
  }

  if true {
    var s1 = getCOWFastSet()
    var identity1 = unsafeBitCast(s1, Word.self)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(1010))

    var s2 = s1
    s2.removeAll()
    var identity2 = unsafeBitCast(s2, Word.self)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectNotEqual(identity1, identity2)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(1010))
    expectEqual(0, s2.count)
    expectFalse(s2.contains(1010))

    // Keep variables alive.
    _fixLifetime(s1)
    _fixLifetime(s2)
  }

  if true {
    var s1 = getCOWFastSet()
    var identity1 = unsafeBitCast(s1, Word.self)
    let originalCapacity = s1._variantStorage.native.capacity
    expectEqual(3, s1.count)
    expectTrue(s1.contains(1010))

    var s2 = s1
    s2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(s2, Word.self)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
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
  if true {
    var s = getCOWSlowSet()
    let originalCapacity = s._variantStorage.native.capacity
    expectEqual(3, s.count)
    expectTrue(s.contains(TestKeyTy(1010)))

    s.removeAll()
    // We can not expectTrue that identity changed, since the new buffer of
    // smaller size can be allocated at the same address as the old one.
    var identity1 = unsafeBitCast(s, Word.self)
    expectTrue(s._variantStorage.native.capacity < originalCapacity)
    expectEqual(0, s.count)
    expectFalse(s.contains(TestKeyTy(1010)))

    s.removeAll()
    expectEqual(identity1, unsafeBitCast(s, Word.self))
    expectEqual(0, s.count)
    expectFalse(s.contains(TestKeyTy(1010)))
  }

  if true {
    var s = getCOWSlowSet()
    var identity1 = unsafeBitCast(s, Word.self)
    let originalCapacity = s._variantStorage.native.capacity
    expectEqual(3, s.count)
    expectTrue(s.contains(TestKeyTy(1010)))

    s.removeAll(keepCapacity: true)
    expectEqual(identity1, unsafeBitCast(s, Word.self))
    expectEqual(originalCapacity, s._variantStorage.native.capacity)
    expectEqual(0, s.count)
    expectFalse(s.contains(TestKeyTy(1010)))

    s.removeAll(keepCapacity: true)
    expectEqual(identity1, unsafeBitCast(s, Word.self))
    expectEqual(originalCapacity, s._variantStorage.native.capacity)
    expectEqual(0, s.count)
    expectFalse(s.contains(TestKeyTy(1010)))
  }

  if true {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Word.self)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestKeyTy(1010)))

    var s2 = s1
    s2.removeAll()
    var identity2 = unsafeBitCast(s2, Word.self)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectNotEqual(identity1, identity2)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestKeyTy(1010)))
    expectEqual(0, s2.count)
    expectFalse(s2.contains(TestKeyTy(1010)))

    // Keep variables alive.
    _fixLifetime(s1)
    _fixLifetime(s2)
  }

  if true {
    var s1 = getCOWSlowSet()
    var identity1 = unsafeBitCast(s1, Word.self)
    let originalCapacity = s1._variantStorage.native.capacity
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestKeyTy(1010)))

    var s2 = s1
    s2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(s2, Word.self)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
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
  var identity1 = unsafeBitCast(s, Word.self)

  expectNotEmpty(s.first)
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("COW.Fast.CountDoesNotReallocate") {
  var s = getCOWFastSet()
  var identity1 = unsafeBitCast(s, Word.self)

  expectEqual(3, s.count)
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("COW.Slow.FirstDoesNotReallocate") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Word.self)

  expectNotEmpty(s.first)
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("COW.Slow.CountDoesNotReallocate") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Word.self)

  expectEqual(3, s.count)
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("COW.Fast.GenerateDoesNotReallocate") {
  var s = getCOWFastSet()
  var identity1 = unsafeBitCast(s, Word.self)

  var gen = s.generate()
  var items = Array<Int>()
  while let value = gen.next() {
    items += [value]
  }
  expectTrue(equalsUnordered(items, [ 1010, 2020, 3030 ]))
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("COW.Slow.GenerateDoesNotReallocate") {
  var s = getCOWSlowSet()
  var identity1 = unsafeBitCast(s, Word.self)

  var gen = s.generate()
  var items = Array<Int>()
  while let value = gen.next() {
    items.append(value.value)
  }
  expectTrue(equalsUnordered(items, [ 1010, 2020, 3030 ]))
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("COW.Fast.EqualityTestDoesNotReallocate") {
  var s1 = getCOWFastSet()
  var identity1 = unsafeBitCast(s1, Word.self)

  var s2 = getCOWFastSet()
  var identity2 = unsafeBitCast(s2, Word.self)

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
  expectEqual(identity2, unsafeBitCast(s2, Word.self))
}

SetTestSuite.test("COW.Slow.EqualityTestDoesNotReallocate") {
  var s1 = getCOWFastSet()
  var identity1 = unsafeBitCast(s1, Word.self)

  var s2 = getCOWFastSet()
  var identity2 = unsafeBitCast(s2, Word.self)

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
  expectEqual(identity2, unsafeBitCast(s2, Word.self))
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

  var s = _Set<TestKeyTy>(minimumCapacity: 10)

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
  println("time is \(timeNow)")
  srandom(timeNow)

  func check(s: _Set<TestKeyTy>) {
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

  var s = _Set<TestKeyTy>(minimumCapacity: 30)
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

SetTestSuite.test("BridgedFromObjC.Verbatim.SetIsCopied") {
  var (s, nss) = getBridgedVerbatimSetAndNSMutableSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isCocoaSet(s))

  expectTrue(s.contains(TestObjCKeyTy(1010)))
  expectNotEmpty(nss.member(TestObjCKeyTy(1010)))

  nss.removeObject(TestObjCKeyTy(1010))
  expectEmpty(nss.member(TestObjCKeyTy(1010)))

  expectTrue(s.contains(TestObjCKeyTy(1010)))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.SetIsCopied") {
  var (s, nss) = getBridgedNonverbatimSetAndNSMutableSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isNativeSet(s))

  expectTrue(s.contains(TestBridgedKeyTy(1010)))
  expectNotEmpty(nss.member(TestBridgedKeyTy(1010)))

  nss.removeObject(TestBridgedKeyTy(1010))
  expectEmpty(nss.member(TestBridgedKeyTy(1010)))

  expectTrue(s.contains(TestBridgedKeyTy(1010)))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.IndexForMember") {
  var s = getBridgedVerbatimSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isCocoaSet(s))

  // Find an existing key.
  var member = s[s.indexOf(TestObjCKeyTy(1010))!]
  expectEqual(TestObjCKeyTy(1010), member)

  member = s[s.indexOf(TestObjCKeyTy(2020))!]
  expectEqual(TestObjCKeyTy(2020), member)

  member = s[s.indexOf(TestObjCKeyTy(3030))!]
  expectEqual(TestObjCKeyTy(3030), member)

  // Try to find a key that does not exist.
  expectEmpty(s.indexOf(TestObjCKeyTy(4040)))
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.IndexForMember") {
  var s = getBridgedNonverbatimSet()
  var identity1 = unsafeBitCast(s, Word.self)

  if true {
    var member = s[s.indexOf(TestBridgedKeyTy(1010))!]
    expectEqual(TestBridgedKeyTy(1010), member)

    member = s[s.indexOf(TestBridgedKeyTy(2020))!]
    expectEqual(TestBridgedKeyTy(2020), member)

    member = s[s.indexOf(TestBridgedKeyTy(3030))!]
    expectEqual(TestBridgedKeyTy(3030), member)
  }

  expectEmpty(s.indexOf(TestBridgedKeyTy(4040)))
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Insert") {
  if true {
    var s = getBridgedVerbatimSet()
    var identity1 = unsafeBitCast(s, Word.self)
    expectTrue(isCocoaSet(s))

    expectFalse(s.contains(TestObjCKeyTy(2040)))
    s.insert(TestObjCKeyTy(2040))

    var identity2 = unsafeBitCast(s, Word.self)
    expectNotEqual(identity1, identity2)
    expectTrue(isNativeSet(s))
    expectEqual(4, s.count)

    expectTrue(s.contains(TestObjCKeyTy(1010)))
    expectTrue(s.contains(TestObjCKeyTy(2020)))
    expectTrue(s.contains(TestObjCKeyTy(3030)))
    expectTrue(s.contains(TestObjCKeyTy(2040)))
  }
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.Insert") {
  if true {
    var s = getBridgedNonverbatimSet()
    var identity1 = unsafeBitCast(s, Word.self)
    expectTrue(isNativeSet(s))

    expectFalse(s.contains(TestBridgedKeyTy(2040)))
    s.insert(TestObjCKeyTy(2040))

    var identity2 = unsafeBitCast(s, Word.self)
    expectNotEqual(identity1, identity2)
    expectTrue(isNativeSet(s))
    expectEqual(4, s.count)

    expectTrue(s.contains(TestBridgedKeyTy(1010)))
    expectTrue(s.contains(TestBridgedKeyTy(2020)))
    expectTrue(s.contains(TestBridgedKeyTy(3030)))
    expectTrue(s.contains(TestBridgedKeyTy(2040)))
  }
}

SetTestSuite.test("BridgedFromObjC.Verbatim.SubscriptWithIndex") {
  var s = getBridgedVerbatimSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isCocoaSet(s))

  var startIndex = s.startIndex
  var endIndex = s.endIndex
  expectNotEqual(startIndex, endIndex)
  expectTrue(startIndex < endIndex)
  expectTrue(startIndex <= endIndex)
  expectFalse(startIndex >= endIndex)
  expectFalse(startIndex > endIndex)
  expectEqual(identity1, unsafeBitCast(s, Word.self))

  var members = [Int]()
  for var i = startIndex; i != endIndex; ++i {
    var foundMember: AnyObject = s[i]
    let member = foundMember as TestObjCKeyTy
    members.append(member.value)
  }
  expectTrue(equalsUnordered(members, [1010, 2020, 3030]))
  expectEqual(identity1, unsafeBitCast(s, Word.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.SubscriptWithIndex") {
  var s = getBridgedNonverbatimSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isNativeSet(s))

  var startIndex = s.startIndex
  var endIndex = s.endIndex
  expectNotEqual(startIndex, endIndex)
  expectTrue(startIndex < endIndex)
  expectTrue(startIndex <= endIndex)
  expectFalse(startIndex >= endIndex)
  expectFalse(startIndex > endIndex)
  expectEqual(identity1, unsafeBitCast(s, Word.self))

  var members = [Int]()
  for var i = startIndex; i != endIndex; ++i {
    var foundMember: AnyObject = s[i]
    let member = foundMember as TestObjCKeyTy
    members.append(member.value)
  }
  expectTrue(equalsUnordered(members, [1010, 2020, 3030]))
  expectEqual(identity1, unsafeBitCast(s, Word.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

SetTestSuite.test("BridgedFromObjC.Verbatim.SubscriptWithIndex_Empty") {
  var s = getBridgedVerbatimSet([])
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isCocoaSet(s))

  var startIndex = s.startIndex
  var endIndex = s.endIndex
  expectEqual(startIndex, endIndex)
  expectFalse(startIndex < endIndex)
  expectTrue(startIndex <= endIndex)
  expectTrue(startIndex >= endIndex)
  expectFalse(startIndex > endIndex)
  expectEqual(identity1, unsafeBitCast(s, Word.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.SubscriptWithIndex_Empty") {
  var s = getBridgedNonverbatimSet([])
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isNativeSet(s))

  var startIndex = s.startIndex
  var endIndex = s.endIndex
  expectEqual(startIndex, endIndex)
  expectFalse(startIndex < endIndex)
  expectTrue(startIndex <= endIndex)
  expectTrue(startIndex >= endIndex)
  expectFalse(startIndex > endIndex)
  expectEqual(identity1, unsafeBitCast(s, Word.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Contains") {
  var s = getBridgedVerbatimSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isCocoaSet(s))

  expectTrue(s.contains(TestObjCKeyTy(1010)))
  expectTrue(s.contains(TestObjCKeyTy(2020)))
  expectTrue(s.contains(TestObjCKeyTy(3030)))

  expectEqual(identity1, unsafeBitCast(s, Word.self))

  // Inserting an item should now create storage unique from the bridged set.
  s.insert(TestObjCKeyTy(4040))
  var identity2 = unsafeBitCast(s, Word.self)
  expectNotEqual(identity1, identity2)
  expectTrue(isNativeSet(s))
  expectEqual(4, s.count)

  // Verify items are still present.
  expectTrue(s.contains(TestObjCKeyTy(1010)))
  expectTrue(s.contains(TestObjCKeyTy(2020)))
  expectTrue(s.contains(TestObjCKeyTy(3030)))
  expectTrue(s.contains(TestObjCKeyTy(4040)))

  // Insert a redundant item to the set.
  // A copy should *not* occur here.
  s.insert(TestObjCKeyTy(1010))
  expectEqual(identity2, unsafeBitCast(s, Word.self))
  expectTrue(isNativeSet(s))
  expectEqual(4, s.count)

  // Again, verify items are still present.
  expectTrue(s.contains(TestObjCKeyTy(1010)))
  expectTrue(s.contains(TestObjCKeyTy(2020)))
  expectTrue(s.contains(TestObjCKeyTy(3030)))
  expectTrue(s.contains(TestObjCKeyTy(4040)))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.Contains") {
  var s = getBridgedNonverbatimSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isNativeSet(s))

  expectTrue(s.contains(TestBridgedKeyTy(1010)))
  expectTrue(s.contains(TestBridgedKeyTy(2020)))
  expectTrue(s.contains(TestBridgedKeyTy(3030)))

  expectEqual(identity1, unsafeBitCast(s, Word.self))

  // Inserting an item should now create storage unique from the bridged set.
  s.insert(TestBridgedKeyTy(4040))
  var identity2 = unsafeBitCast(s, Word.self)
  expectNotEqual(identity1, identity2)
  expectTrue(isNativeSet(s))
  expectEqual(4, s.count)

  // Verify items are still present.
  expectTrue(s.contains(TestBridgedKeyTy(1010)))
  expectTrue(s.contains(TestBridgedKeyTy(2020)))
  expectTrue(s.contains(TestBridgedKeyTy(3030)))
  expectTrue(s.contains(TestBridgedKeyTy(4040)))

  // Insert a redundant item to the set.
  // A copy should *not* occur here.
  s.insert(TestBridgedKeyTy(1010))
  expectEqual(identity2, unsafeBitCast(s, Word.self))
  expectTrue(isNativeSet(s))
  expectEqual(4, s.count)

  // Again, verify items are still present.
  expectTrue(s.contains(TestBridgedKeyTy(1010)))
  expectTrue(s.contains(TestBridgedKeyTy(2020)))
  expectTrue(s.contains(TestBridgedKeyTy(3030)))
  expectTrue(s.contains(TestBridgedKeyTy(4040)))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.SubscriptWithMember") {
  var s = getBridgedNonverbatimSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isNativeSet(s))

  expectTrue(s.contains(TestBridgedKeyTy(1010)))
  expectTrue(s.contains(TestBridgedKeyTy(2020)))
  expectTrue(s.contains(TestBridgedKeyTy(3030)))

  expectEqual(identity1, unsafeBitCast(s, Word.self))

  // Insert a new member.
  // This should trigger a copy.
  s.insert(TestBridgedKeyTy(4040))
  var identity2 = unsafeBitCast(s, Word.self)

  expectTrue(isNativeSet(s))
  expectEqual(4, s.count)

  // Verify all items in place.
  expectTrue(s.contains(TestBridgedKeyTy(1010)))
  expectTrue(s.contains(TestBridgedKeyTy(2020)))
  expectTrue(s.contains(TestBridgedKeyTy(3030)))
  expectTrue(s.contains(TestBridgedKeyTy(4040)))

  // Insert a redundant member.
  // This should *not* trigger a copy.
  s.insert(TestBridgedKeyTy(1010))
  expectEqual(identity2, unsafeBitCast(s, Word.self))
  expectTrue(isNativeSet(s))
  expectEqual(4, s.count)

  // Again, verify all items in place.
  expectTrue(s.contains(TestBridgedKeyTy(1010)))
  expectTrue(s.contains(TestBridgedKeyTy(2020)))
  expectTrue(s.contains(TestBridgedKeyTy(3030)))
  expectTrue(s.contains(TestBridgedKeyTy(4040)))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.RemoveAtIndex") {
  var s = getBridgedVerbatimSet()
  let identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isCocoaSet(s))

  let foundIndex1 = s.indexOf(TestObjCKeyTy(1010))!
  expectEqual(TestObjCKeyTy(1010), s[foundIndex1])
  expectEqual(identity1, unsafeBitCast(s, Word.self))

  s.removeAtIndex(foundIndex1)
  expectNotEqual(identity1, unsafeBitCast(s, Word.self))
  expectTrue(isNativeSet(s))
  expectEqual(2, s.count)
  expectEmpty(s.indexOf(TestObjCKeyTy(1010)))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.RemoveAtIndex") {
  var s = getBridgedNonverbatimSet()
  let identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isNativeSet(s))

  let foundIndex1 = s.indexOf(TestBridgedKeyTy(1010))!
  expectEqual(1010, s[foundIndex1].value)
  expectEqual(identity1, unsafeBitCast(s, Word.self))

  s.removeAtIndex(foundIndex1)
  expectEqual(identity1, unsafeBitCast(s, Word.self))
  expectTrue(isNativeSet(s))
  expectEqual(2, s.count)
  expectEmpty(s.indexOf(TestBridgedKeyTy(1010)))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Remove") {
  if true {
    var s = getBridgedVerbatimSet()
    var identity1 = unsafeBitCast(s, Word.self)
    expectTrue(isCocoaSet(s))

    var deleted: AnyObject? = s.remove(TestObjCKeyTy(0))
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s, Word.self))
    expectTrue(isCocoaSet(s))

    deleted = s.remove(TestObjCKeyTy(1010))
    expectEqual(1010, deleted!.value)
    var identity2 = unsafeBitCast(s, Word.self)
    expectNotEqual(identity1, identity2)
    expectTrue(isNativeSet(s))
    expectEqual(2, s.count)

    expectFalse(s.contains(TestObjCKeyTy(1010)))
    expectTrue(s.contains(TestObjCKeyTy(2020)))
    expectTrue(s.contains(TestObjCKeyTy(3030)))
    expectEqual(identity2, unsafeBitCast(s, Word.self))
  }

  if true {
    var s1 = getBridgedVerbatimSet()
    var identity1 = unsafeBitCast(s1, Word.self)

    var s2 = s1
    expectTrue(isCocoaSet(s1))
    expectTrue(isCocoaSet(s2))

    var deleted: AnyObject? = s2.remove(TestObjCKeyTy(0))
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectEqual(identity1, unsafeBitCast(s2, Word.self))
    expectTrue(isCocoaSet(s1))
    expectTrue(isCocoaSet(s2))

    deleted = s2.remove(TestObjCKeyTy(1010))
    expectEqual(1010, deleted!.value)
    var identity2 = unsafeBitCast(s2, Word.self)
    expectNotEqual(identity1, identity2)
    expectTrue(isCocoaSet(s1))
    expectTrue(isNativeSet(s2))
    expectEqual(2, s2.count)

    expectTrue(s1.contains(TestObjCKeyTy(1010)))
    expectTrue(s1.contains(TestObjCKeyTy(2020)))
    expectTrue(s1.contains(TestObjCKeyTy(3030)))
    expectEqual(identity1, unsafeBitCast(s1, Word.self))

    expectFalse(s2.contains(TestObjCKeyTy(1010)))
    expectTrue(s2.contains(TestObjCKeyTy(2020)))
    expectTrue(s2.contains(TestObjCKeyTy(3030)))

    expectEqual(identity2, unsafeBitCast(s2, Word.self))
  }
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.Remove") {
  if true {
    var s = getBridgedNonverbatimSet()
    var identity1 = unsafeBitCast(s, Word.self)
    expectTrue(isNativeSet(s))

    // Trying to remove something not in the set should
    // leave it completely unchanged.
    var deleted = s.remove(TestBridgedKeyTy(0))
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s, Word.self))
    expectTrue(isNativeSet(s))

    // Now remove an item that is in the set. This should
    // not create a new set altogether, however.
    deleted = s.remove(TestBridgedKeyTy(1010))
    expectEqual(1010, deleted!.value)
    var identity2 = unsafeBitCast(s, Word.self)
    expectEqual(identity1, identity2)
    expectTrue(isNativeSet(s))
    expectEqual(2, s.count)

    // Double-check - the removed member should not be found.
    expectFalse(s.contains(TestBridgedKeyTy(1010)))

    // ... but others not removed should be found.
    expectTrue(s.contains(TestBridgedKeyTy(2020)))
    expectTrue(s.contains(TestBridgedKeyTy(3030)))

    // Triple-check - we should still be working with the same object.
    expectEqual(identity2, unsafeBitCast(s, Word.self))
  }

  if true {
    var s1 = getBridgedNonverbatimSet()
    let identity1 = unsafeBitCast(s1, Word.self)

    var s2 = s1
    expectTrue(isNativeSet(s1))
    expectTrue(isNativeSet(s2))

    var deleted = s2.remove(TestBridgedKeyTy(0))
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectEqual(identity1, unsafeBitCast(s2, Word.self))
    expectTrue(isNativeSet(s1))
    expectTrue(isNativeSet(s2))

    deleted = s2.remove(TestBridgedKeyTy(1010))
    expectEqual(1010, deleted!.value)
    let identity2 = unsafeBitCast(s2, Word.self)
    expectNotEqual(identity1, identity2)
    expectTrue(isNativeSet(s1))
    expectTrue(isNativeSet(s2))
    expectEqual(2, s2.count)

    expectTrue(s1.contains(TestBridgedKeyTy(1010)))
    expectTrue(s1.contains(TestBridgedKeyTy(2020)))
    expectTrue(s1.contains(TestBridgedKeyTy(3030)))
    expectEqual(identity1, unsafeBitCast(s1, Word.self))

    expectFalse(s2.contains(TestBridgedKeyTy(1010)))
    expectTrue(s2.contains(TestBridgedKeyTy(2020)))
    expectTrue(s2.contains(TestBridgedKeyTy(3030)))
    expectEqual(identity2, unsafeBitCast(s2, Word.self))
  }
}

SetTestSuite.test("BridgedFromObjC.Verbatim.RemoveAll") {
  if true {
    var s = getBridgedVerbatimSet([])
    var identity1 = unsafeBitCast(s, Word.self)
    expectTrue(isCocoaSet(s))
    expectEqual(0, s.count)

    s.removeAll()
    expectEqual(identity1, unsafeBitCast(s, Word.self))
    expectEqual(0, s.count)
  }

  if true {
    var s = getBridgedVerbatimSet()
    var identity1 = unsafeBitCast(s, Word.self)
    expectTrue(isCocoaSet(s))
    expectEqual(3, s.count)
    expectTrue(s.contains(TestBridgedKeyTy(1010)))

    s.removeAll()
    expectEqual(0, s.count)
    expectFalse(s.contains(TestBridgedKeyTy(1010)))
  }

  if true {
    var s1 = getBridgedVerbatimSet()
    var identity1 = unsafeBitCast(s1, Word.self)
    expectTrue(isCocoaSet(s1))
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))

    var s2 = s1
    s2.removeAll()
    var identity2 = unsafeBitCast(s2, Word.self)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectNotEqual(identity2, identity1)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))
    expectEqual(0, s2.count)
    expectFalse(s2.contains(TestBridgedKeyTy(1010)))
  }

  if true {
    var s1 = getBridgedVerbatimSet()
    var identity1 = unsafeBitCast(s1, Word.self)
    expectTrue(isCocoaSet(s1))
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))

    var s2 = s1
    s2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(s2, Word.self)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectNotEqual(identity2, identity1)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))
    expectEqual(0 , s2.count)
    expectFalse(s2.contains(TestBridgedKeyTy(1010)))
  }
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.RemoveAll") {
  if true {
    var s = getBridgedNonverbatimSet([])
    var identity1 = unsafeBitCast(s, Word.self)
    expectTrue(isNativeSet(s))
    expectEqual(0, s.count)

    s.removeAll()
    expectEqual(identity1, unsafeBitCast(s, Word.self))
    expectEqual(0, s.count)
  }

  if true {
    var s = getBridgedNonverbatimSet()
    var identity1 = unsafeBitCast(s, Word.self)
    expectTrue(isNativeSet(s))
    expectEqual(3, s.count)
    expectTrue(s.contains(TestBridgedKeyTy(1010)))

    s.removeAll()
    expectEqual(0, s.count)
    expectFalse(s.contains(TestBridgedKeyTy(1010)))
  }

  if true {
    var s1 = getBridgedNonverbatimSet()
    var identity1 = unsafeBitCast(s1, Word.self)
    expectTrue(isNativeSet(s1))
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))

    var s2 = s1
    s2.removeAll()
    var identity2 = unsafeBitCast(s2, Word.self)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectNotEqual(identity2, identity1)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))
    expectEqual(0, s2.count)
    expectFalse(s2.contains(TestBridgedKeyTy(1010)))
  }

  if true {
    var s1 = getBridgedNonverbatimSet()
    var identity1 = unsafeBitCast(s1, Word.self)
    expectTrue(isNativeSet(s1))
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))

    var s2 = s1
    s2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(s2, Word.self)
    expectEqual(identity1, unsafeBitCast(s1, Word.self))
    expectNotEqual(identity2, identity1)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestObjCKeyTy(1010)))
    expectEqual(0, s2.count)
    expectFalse(s2.contains(TestObjCKeyTy(1010)))
  }
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Count") {
  var s = getBridgedVerbatimSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isCocoaSet(s))

  expectEqual(3, s.count)
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.Count") {
  var s = getBridgedNonverbatimSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isNativeSet(s))

  expectEqual(3, s.count)
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Generate") {
  var s = getBridgedVerbatimSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isCocoaSet(s))

  var gen = s.generate()
  var members = Array<Int>()
  while let (member: AnyObject) = gen.next() {
    members.append((member as TestObjCKeyTy).value)
  }
  expectTrue(equalsUnordered(members, [1010, 2020, 3030]))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.Generate") {
  var s = getBridgedNonverbatimSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isNativeSet(s))

  var gen = s.generate()
  var members = Array<Int>()
  while let member = gen.next() {
    members.append(member.value)
  }
  expectTrue(equalsUnordered(members, [1010, 2020, 3030]))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Generate_Empty") {
  var s = getBridgedVerbatimSet([])
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isCocoaSet(s))

  var gen = s.generate()
  expectEmpty(gen.next())
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.Generate_Empty") {
  var s = getBridgedNonverbatimSet([])
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isNativeSet(s))

  var gen = s.generate()
  expectEmpty(gen.next())
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Generate_Huge") {
  var s = getHugeBridgedVerbatimSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isCocoaSet(s))

  var gen = s.generate()
  var members = [Int]()
  while let member: AnyObject = gen.next() {
    members.append((member as TestObjCKeyTy).value)
  }
  expectTrue(equalsUnordered(members, hugeNumberArray))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.Generate_Huge") {
  var s = getHugeBridgedNonverbatimSet()
  var identity1 = unsafeBitCast(s, Word.self)
  expectTrue(isNativeSet(s))

  var gen = s.generate()
  var members = [Int]()
  while let member: AnyObject = gen.next() {
    members.append((member as TestBridgedKeyTy).value)
  }
  expectTrue(equalsUnordered(members, hugeNumberArray))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEqual(identity1, unsafeBitCast(s, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.EqualityTest_Empty") {
  var s1 = getBridgedVerbatimSet([])
  var identity1 = unsafeBitCast(s1, Word.self)
  expectTrue(isCocoaSet(s1))

  var s2 = getBridgedVerbatimSet([])
  var identity2 = unsafeBitCast(s2, Word.self)
  expectTrue(isCocoaSet(s2))
  expectNotEqual(identity1, identity2)

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
  expectEqual(identity2, unsafeBitCast(s2, Word.self))

  s2.insert(TestObjCKeyTy(4040))
  expectTrue(isNativeSet(s2))
  expectNotEqual(identity2, unsafeBitCast(s2, Word.self))
  identity2 = unsafeBitCast(s2, Word.self)

  expectNotEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
  expectEqual(identity2, unsafeBitCast(s2, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.EqualityTest_Empty") {
  var s1 = getBridgedNonverbatimSet([])
  var identity1 = unsafeBitCast(s1, Word.self)
  expectTrue(isNativeSet(s1))

  var s2 = getBridgedNonverbatimSet([])
  var identity2 = unsafeBitCast(s2, Word.self)
  expectTrue(isNativeSet(s2))
  expectNotEqual(identity1, identity2)

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
  expectEqual(identity2, unsafeBitCast(s2, Word.self))

  s2.insert(TestObjCKeyTy(4040))
  expectTrue(isNativeSet(s2))
  expectEqual(identity2, unsafeBitCast(s2, Word.self))

  expectNotEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
  expectEqual(identity2, unsafeBitCast(s2, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.EqualityTest_Small") {
  var s1 = getBridgedVerbatimSet()
  let identity1 = unsafeBitCast(s1, Word.self)
  expectTrue(isCocoaSet(s1))

  var s2 = getBridgedVerbatimSet()
  let identity2 = unsafeBitCast(s2, Word.self)
  expectTrue(isCocoaSet(s2))
  expectNotEqual(identity1, identity2)

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
  expectEqual(identity2, unsafeBitCast(s2, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.EqualityTest_Small") {
  var s1 = getBridgedNonverbatimSet()
  let identity1 = unsafeBitCast(s1, Word.self)
  expectTrue(isNativeSet(s1))

  var s2 = getBridgedNonverbatimSet()
  let identity2 = unsafeBitCast(s2, Word.self)
  expectTrue(isNativeSet(s2))
  expectNotEqual(identity1, identity2)

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
  expectEqual(identity2, unsafeBitCast(s2, Word.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.ArrayOfSets") {
  var nsa = NSMutableArray()
  for i in 0..<3 {
    nsa.addObject(
        getAsNSSet([ 1 + i,  2 + i, 3 + i ]))
  }

  var a = nsa as [AnyObject] as [_Set<NSObject>]
  for i in 0..<3 {
    var s = a[i]
    var gen = s.generate()
    var items = Array<Int>()
    while let value: AnyObject = gen.next() {
      let v = (value as TestObjCKeyTy).value
      items.append(v)
    }
    var expectedItems = [ 1 + i, 2 + i, 3 + i ]
    expectTrue(equalsUnordered(items, expectedItems))
  }
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.ArrayOfSets") {
  var nsa = NSMutableArray()
  for i in 0..<3 {
    nsa.addObject(
        getAsNSSet([ 1 + i, 2 + i, 3 + i ]))
  }

  var a = nsa as [AnyObject] as [_Set<TestBridgedKeyTy>]
  for i in 0..<3 {
    var d = a[i]
    var gen = d.generate()
    var items = Array<Int>()
    while let value = gen.next() {
      items.append(value.value)
    }
    var expectedItems = [ 1 + i, 2 + i, 3 + i ]
    expectTrue(equalsUnordered(items, expectedItems))
  }
}


//===---
// Dictionary -> NSDictionary bridging tests.
//
// Value is bridged verbatim.
//===---

SetTestSuite.test("BridgedToObjC.Verbatim.Count") {
  let s = getBridgedNSSetOfRefTypesBridgedVerbatim()

  expectEqual(3, s.count)
}

SetTestSuite.test("BridgedToObjC.Verbatim.Contains") {
  let nss = getBridgedNSSetOfRefTypesBridgedVerbatim()

  expectNotEmpty(nss.member(TestObjCKeyTy(1010)))
  expectNotEmpty(nss.member(TestObjCKeyTy(2020)))
  expectNotEmpty(nss.member(TestObjCKeyTy(3030)))
  expectEmpty(nss.member(TestObjCKeyTy(4040)))

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

SetTestSuite.test("BridgingRoundtrip") {
  let s = getRoundtripBridgedNSSet()
  let enumerator = s.objectEnumerator()

  var items = Array<Int>()
  while let value: AnyObject = enumerator.nextObject() {
    let v = (value as TestObjCKeyTy).value
    items.append(v)
  }
  expectTrue(equalsUnordered([ 1010, 2020, 3030 ], items))
}

SetTestSuite.test("BridgedToObjC.Verbatim.ObjectEnumerator.FastEnumeration.UseFromSwift") {
  let s = getBridgedNSSetOfRefTypesBridgedVerbatim()

  checkSetFastEnumerationFromSwift(
    [ 1010, 2020, 3030 ],
    s, { s.objectEnumerator() },
    { ($0 as TestObjCKeyTy).value })

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

SetTestSuite.test("BridgedToObjC.Verbatim.ObjectEnumerator.FastEnumeration.UseFromObjC") {
  let s = getBridgedNSSetOfRefTypesBridgedVerbatim()

  checkSetFastEnumerationFromObjC(
    [ 1010, 2020, 3030 ],
    s, { s.objectEnumerator() },
    { ($0 as TestObjCKeyTy).value })

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

SetTestSuite.test("BridgedToObjC.Verbatim.ObjectEnumerator.FastEnumeration_Empty") {
  let s = getBridgedEmptyNSSet()

  checkSetFastEnumerationFromSwift(
    [], s, { s.objectEnumerator() },
    { ($0 as TestObjCKeyTy).value })

  checkSetFastEnumerationFromObjC(
    [], s, { s.objectEnumerator() },
    { ($0 as TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Custom.ObjectEnumerator.FastEnumeration.UseFromObjC") {
  let s = getBridgedNSSet_MemberTypesCustomBridged()

  checkSetFastEnumerationFromObjC(
    [ 1010, 2020, 3030 ],
    s, { s.objectEnumerator() },
    { ($0 as TestObjCKeyTy).value })

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

SetTestSuite.test("BridgedToObjC.Custom.ObjectEnumerator.FastEnumeration.UseFromSwift") {
  let s = getBridgedNSSet_MemberTypesCustomBridged()

  checkSetFastEnumerationFromSwift(
    [ 1010, 2020, 3030 ],
    s, { s.objectEnumerator() },
    { ($0 as TestObjCKeyTy).value })

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

SetTestSuite.test("BridgedToObjC.Verbatim.FastEnumeration.UseFromSwift") {
  let s = getBridgedNSSetOfRefTypesBridgedVerbatim()

  checkSetFastEnumerationFromSwift(
    [ 1010, 2020, 3030 ],
    s, { s },
    { ($0 as TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Verbatim.FastEnumeration.UseFromObjC") {
  let s = getBridgedNSSetOfRefTypesBridgedVerbatim()

  checkSetFastEnumerationFromObjC(
    [ 1010, 2020, 3030 ],
    s, { s },
    { ($0 as TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Verbatim.FastEnumeration_Empty") {
  let s = getBridgedEmptyNSSet()

  checkSetFastEnumerationFromSwift(
    [], s, { s },
    { ($0 as TestObjCKeyTy).value })

  checkSetFastEnumerationFromObjC(
    [], s, { s },
    { ($0 as TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Custom.FastEnumeration.UseFromSwift") {
  let s = getBridgedNSSet_ValueTypesCustomBridged()

  checkSetFastEnumerationFromSwift(
    [ 1010, 2020, 3030 ],
    s, { s },
    { ($0 as TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Custom.FastEnumeration.UseFromObjC") {
  let s = getBridgedNSSet_ValueTypesCustomBridged()

  checkSetFastEnumerationFromObjC(
    [ 1010, 2020, 3030 ],
    s, { s },
    { ($0 as TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Custom.FastEnumeration_Empty") {
  let s = getBridgedNSSet_ValueTypesCustomBridged(
    numElements: 0)

  checkSetFastEnumerationFromSwift(
    [], s, { s },
    { ($0 as TestObjCKeyTy).value })

  checkSetFastEnumerationFromObjC(
    [], s, { s },
    { ($0 as TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Count") {
  let s = getBridgedNSSetOfRefTypesBridgedVerbatim()
  expectEqual(3, s.count)
}

SetTestSuite.test("BridgedToObjC.ObjectEnumerator.NextObject") {
  let s = getBridgedNSSetOfRefTypesBridgedVerbatim()
  let enumerator = s.objectEnumerator()

  var members = [Int]()
  while let nextObject: AnyObject = enumerator.nextObject() {
    members.append((nextObject as TestObjCKeyTy).value)
  }
  expectTrue(equalsUnordered([1010, 2020, 3030], members))

  expectEmpty(enumerator.nextObject())
  expectEmpty(enumerator.nextObject())
  expectEmpty(enumerator.nextObject())

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

SetTestSuite.test("BridgedToObjC.ObjectEnumerator.NextObject.Empty") {
  let s = getBridgedEmptyNSSet()
  let enumerator = s.objectEnumerator()

  expectEmpty(enumerator.nextObject())
  expectEmpty(enumerator.nextObject())
  expectEmpty(enumerator.nextObject())
}

//
// Set -> NSSet Bridging
//

SetTestSuite.test("BridgedToObjC.MemberTypesCustomBridged") {
  let s = getBridgedNSSet_MemberTypesCustomBridged()
  let enumerator = s.objectEnumerator()

  var members = [Int]()
  while let nextObject: AnyObject = enumerator.nextObject() {
    members.append((nextObject as TestObjCKeyTy).value)
  }
  expectTrue(equalsUnordered([ 1010, 2020, 3030 ], members))

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

//
// NSSet -> Set -> NSSet Round trip bridging
//

SetTestSuite.test("BridgingRoundTrip") {
  let s = getRoundtripBridgedNSSet()
  let enumerator = s.objectEnumerator()

  var members = [Int]()
  while let nextObject: AnyObject = enumerator.nextObject() {
    members.append((nextObject as TestObjCKeyTy).value)
  }
  expectTrue(equalsUnordered([1010, 2020, 3030] ,members))
}

//
// NSSet -> Set implicit conversion
//

SetTestSuite.test("NSSetToSetConversion") {
  let nsArray = NSMutableArray()
  for i in [1010, 2020, 3030] {
    nsArray.addObject(TestObjCKeyTy(i))
  }

  let nss = NSSet(array: nsArray)

  let s: _Set = nss

  var members = [Int]()
  for member: AnyObject in s {
    members.append((member as TestObjCKeyTy).value)
  }
  expectTrue(equalsUnordered(members, [1010, 2020, 3030]))
}

SetTestSuite.test("SetToNSSetConversion") {
  var s = _Set<TestObjCKeyTy>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
    s.insert(TestObjCKeyTy(i))
  }
  let nss: NSSet = s

  expectTrue(equalsUnordered(map(Array(s), { $0.value }), [1010, 2020, 3030]))
}

//
// Set Casts
//

// FIXME: <rdar://problem/18853078> Implement _Set<T> up and downcasting

//SetTestSuite.test("SetUpcastEntryPoint") {
//  var s = _Set<TestObjCKeyTy>(minimumCapacity: 32)
//  for i in [1010, 2020, 3030] {
//      s.insert(TestObjCKeyTy(i))
//  }
//
//  var sAsAnyObject: _Set<NSObject> = _setUpCast(s)
//
//  expectEqual(3, sAsAnyObject.count)
//  expectTrue(sAsAnyObject.contains(TestObjCKeyTy(1010)))
//  expectTrue(sAsAnyObject.contains(TestObjCKeyTy(2020)))
//  expectTrue(sAsAnyObject.contains(TestObjCKeyTy(3030)))
//}
//
//SetTestSuite.test("SetUpcast") {
//  var s = _Set<TestObjCKeyTy>(minimumCapacity: 32)
//  for i in [1010, 2020, 3030] {
//      s.insert(TestObjCKeyTy(i))
//  }
//
//  var sAsAnyObject: _Set<NSObject> = s
//
//  expectEqual(3, sAsAnyObject.count)
//  expectTrue(sAsAnyObject.contains(TestObjCKeyTy(1010)))
//  expectTrue(sAsAnyObject.contains(TestObjCKeyTy(2020)))
//  expectTrue(sAsAnyObject.contains(TestObjCKeyTy(3030)))
//}
//
//SetTestSuite.test("SetUpcastBridgedEntryPoint") {
//  var s = _Set<TestBridgedKeyTy>(minimumCapacity: 32)
//  for i in [1010, 2020, 3030] {
//      s.insert(TestBridgedKeyTy(i))
//  }
//
//  if true {
//    var s: _Set<NSObject> = _setBridgeToObjectiveC(s)
//
//    expectTrue(s.contains(TestBridgedKeyTy(1010)))
//    expectTrue(s.contains(TestBridgedKeyTy(2020)))
//    expectTrue(s.contains(TestBridgedKeyTy(3030)))
//  }
//
//  if true {
//    var s: _Set<TestBridgedKeyTy> = _setBridgeToObjectiveC(s)
//
//    expectEqual(3, s.count)
//    expectTrue(s.contains(TestBridgedKeyTy(1010)))
//    expectTrue(s.contains(TestBridgedKeyTy(2020)))
//    expectTrue(s.contains(TestBridgedKeyTy(3030)))
//  }
//}
//
//SetTestSuite.test("SetUpcastBridged") {
//  var s = _Set<TestBridgedKeyTy>(minimumCapacity: 32)
//  for i in [1010, 2020, 3030] {
//      s.insert(TestBridgedKeyTy(i))
//  }
//
//  if true {
//    var s: _Set<NSObject> = s
//
//    expectEqual(3, s.count)
//    expectTrue(s.contains(TestBridgedKeyTy(1010)))
//    expectTrue(s.contains(TestBridgedKeyTy(2020)))
//    expectTrue(s.contains(TestBridgedKeyTy(3030)))
//  }
//
//  if true {
//    var s: _Set<TestBridgedKeyTy> = s
//
//    expectEqual(3, s.count)
//    expectTrue(s.contains(TestBridgedKeyTy(1010)))
//    expectTrue(s.contains(TestBridgedKeyTy(2020)))
//    expectTrue(s.contains(TestBridgedKeyTy(3030)))
//  }
//}
//
////
//// Set downcasts
////
//
//SetTestSuite.test("SetDowncastEntryPoint") {
//  var s = _Set<NSObject>(minimumCapacity: 32)
//  for i in [1010, 2020, 3030] {
//      s.insert(TestObjCKeyTy(i))
//  }
//
//  // Successful downcast.
//  let dCC: _Set<TestObjCKeyTy> = _setDownCast(s)
//  expectEqual(3, dCC.count)
//  expectTrue(dCC.contains(TestObjCKeyTy(1010)))
//  expectTrue(dCC.contains(TestObjCKeyTy(2020)))
//  expectTrue(dCC.contains(TestObjCKeyTy(3030)))
//}
//
//SetTestSuite.test("SetDowncast") {
//  var s = _Set<NSObject>(minimumCapacity: 32)
//  for i in [1010, 2020, 3030] {
//      s.insert(TestObjCKeyTy(i))
//  }
//
//  // Successful downcast.
//  let dCC = s as _Set<TestObjCKeyTy>
//  expectEqual(3, dCC.count)
//  expectTrue(dCC.contains(TestObjCKeyTy(1010)))
//  expectTrue(dCC.contains(TestObjCKeyTy(2020)))
//  expectTrue(dCC.contains(TestObjCKeyTy(3030)))
//}
//
//SetTestSuite.test("SetDowncastConditionalEntryPoint") {
//  var s = _Set<NSObject>(minimumCapacity: 32)
//  for i in [1010, 2020, 3030] {
//      s.insert(TestObjCKeyTy(i))
//  }
//
//  // Successful downcast.
//  if let dCC: _Set<TestObjCKeyTy> = _setDownCastConditional(s) {
//    expectEqual(3, dCC.count)
//    expectTrue(dCC.contains(TestObjCKeyTy(1010)))
//    expectTrue(dCC.contains(TestObjCKeyTy(2020)))
//    expectTrue(dCC.contains(TestObjCKeyTy(3030)))
//  } else {
//    expectTrue(false)
//  }
//
//  // Unsuccessful downcast
//  s.insert("Hello, world")
//  if let dCC: _Set<TestObjCKeyTy>
//       = _setDownCastConditional(s) {
//    expectTrue(false)
//  }
//}
//
//SetTestSuite.test("SetDowncastConditional") {
//  var s = _Set<NSObject>(minimumCapacity: 32)
//  for i in [1010, 2020, 3030] {
//      s.insert(TestObjCKeyTy(i))
//  }
//
//  // Successful downcast.
//  if let dCC = s as? _Set<TestObjCKeyTy> {
//    expectEqual(3, dCC.count)
//    expectTrue(dCC.contains(TestObjCKeyTy(1010)))
//    expectTrue(dCC.contains(TestObjCKeyTy(2020)))
//    expectTrue(dCC.contains(TestObjCKeyTy(3030)))
//  } else {
//    expectTrue(false)
//  }
//
//  // Unsuccessful downcast
//  s.insert("Hello, world, I'm your wild girl. I'm your ch-ch-ch-ch-ch-ch cherry bomb")
//  if let dCC = s as? _Set<TestObjCKeyTy> {
//    expectTrue(false)
//  }
//}
//
//SetTestSuite.test("SetBridgeFromObjectiveCEntryPoint") {
//  var s = _Set<NSObject>(minimumCapacity: 32)
//  for i in [1010, 2020, 3030] {
//      s.insert(TestObjCKeyTy(i))
//  }
//
//  // Successful downcast.
//  let sCV: _Set<TestObjCKeyTy> = _setBridgeFromObjectiveC(s)
//  if true {
//    expectEqual(3, dCV.count)
//    expectTrue(dCV.contains(TestObjCKeyTy(1010)))
//    expectTrue(dCV.contains(TestObjCKeyTy(2020)))
//    expectTrue(dCV.contains(TestObjCKeyTy(3030)))
//  }
//}
//
//SetTestSuite.test("SetBridgeFromObjectiveC") {
//  var s = _Set<NSObject>(minimumCapacity: 32)
//  for i in [1010, 2020, 3030] {
//      s.insert(TestObjCKeyTy(i))
//  }
//
//  // Successful downcast.
//  let sCV = s as _Set<TestObjCKeyTy>
//  if true {
//    expectEqual(3, dCV.count)
//    expectTrue(dCV.contains(TestObjCKeyTy(1010)))
//    expectTrue(dCV.contains(TestObjCKeyTy(2020)))
//    expectTrue(dCV.contains(TestObjCKeyTy(3030)))
//  }
//
//  // Successful downcast.
//  let sVC = s as _Set<TestBridgedKeyTy>
//  if true {
//    expectEqual(3, dVC.count)
//    expectTrue(dVC.contains(TestBridgedKeyTy(1010)))
//    expectTrue(dVC.contains(TestBridgedKeyTy(2020)))
//    expectTrue(dVC.contains(TestBridgedKeyTy(3030)))
//  }
//}
//
//SetTestSuite.test("SetBridgeFromObjectiveCConditionalEntryPoint") {
//  var s = _Set<NSObject>(minimumCapacity: 32)
//  for i in [1010, 2020, 3030] {
//      s.insert(TestObjCKeyTy(i))
//  }
//
//  // Successful downcast.
//  if let sCV: _Set<TestObjCKeyTy> = _setBridgeFromObjectiveCConditional(s) {
//    expectEqual(3, dCV.count)
//    expectTrue(dCV.contains(TestObjCKeyTy(1010)))
//    expectTrue(dCV.contains(TestObjCKeyTy(2020)))
//    expectTrue(dCV.contains(TestObjCKeyTy(3030)))
//  } else {
//    expectTrue(false)
//  }
//
//  // Successful downcast.
//  if let sVC: _Set<TestBridgedKeyTy> = _setBridgeFromObjectiveCConditional(s) {
//    expectEqual(3, dVC.count)
//    expectTrue(dVC.contains(TestBridgedKeyTy(1010)))
//    expectTrue(dVC.contains(TestBridgedKeyTy(2020)))
//    expectTrue(dVC.contains(TestBridgedKeyTy(3030)))
//  } else {
//    expectTrue(false)
//  }
//
//  // Unsuccessful downcasts
//  s.insert("Hello, world, I'm your wild girl. I'm your ch-ch-ch-ch-ch-ch cherry bomb")
//  if let sCV: _Set<TestObjCKeyTy> = _setBridgeFromObjectiveCConditional(s) {
//    expectTrue(false)
//  }
//  if let sVC: _Set<TestBridgedKeyTy>
//       = _setBridgeFromObjectiveCConditional(s) {
//    expectTrue(false)
//  }
//  if let sVV: _Set<TestBridgedKeyTy>
//       = _setBridgeFromObjectiveCConditional(s) {
//    expectTrue(false)
//  }
//}
//
//SetTestSuite.test("SetBridgeFromObjectiveCConditional") {
//  var s = _Set<NSObject>(minimumCapacity: 32)
//  for i in [1010, 2020, 3030] {
//      s.insert(TestObjCKeyTy(i))
//  }
//
//  // Successful downcast.
//  if let dCm = s as? _Set<TestObjCKeyTy>  {
//    expectEqual(3, dCV.count)
//    expectTrue(dCV.contains(TestObjCKeyTy(1010)))
//    expectTrue(dCV.contains(TestObjCKeyTy(2020)))
//    expectTrue(dCV.contains(TestObjCKeyTy(3030)))
//  } else {
//    expectTrue(false)
//  }
//
//  // Successful downcast.
//  if let sVC = s as? _Set<TestBridgedKeyTy> {
//    expectEqual(3, dVC.count)
//    expectTrue(dVC.contains(TestBridgedKeyTy(1010)))
//    expectTrue(dVC.contains(TestBridgedKeyTy(2020)))
//    expectTrue(dVC.contains(TestBridgedKeyTy(3030)))
//  } else {
//    expectTrue(false)
//  }
//
//  // Unsuccessful downcasts
//  s.insert("Hello, world, I'm your wild girl. I'm your ch-ch-ch-ch-ch-ch cherry bomb")
//  if let dCm = s as? _Set<TestObjCKeyTy> {
//    expectTrue(false)
//  }
//  if let sVC = s as? _Set<TestBridgedKeyTy> {
//    expectTrue(false)
//  }
//  if let sVm = s as? _Set<TestBridgedKeyTy> {
//    expectTrue(false)
//  }
//}

// Public API

SetTestSuite.test("init(SequenceType:)") {
    let s1 = _Set([1010, 2020, 3030])
    var s2 = _Set<Int>()
    s2.insert(1010)
    s2.insert(2020)
    s2.insert(3030)
    expectEqual(s1, s2)

    // Test the uniquing capabilities of a set
    let s3 = _Set([
      1010, 1010, 1010, 1010, 1010, 1010,
      1010, 1010, 1010, 1010, 1010, 1010,
      2020, 2020, 2020, 3030, 3030, 3030
    ])
    expectEqual(s1, s3)
}

SetTestSuite.test("isSubsetOf.Set.Set") {
  let s1 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = _Set([1010, 2020, 3030])
  expectTrue(_Set<Int>().isSubsetOf(s1))
  expectFalse(s1.isSubsetOf(_Set<Int>()))
  expectTrue(s1.isSubsetOf(s1))
  expectTrue(s2.isSubsetOf(s1))
}

SetTestSuite.test("isSubsetOf.Set.Sequence") {
  let s1 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = SequenceOf([1010, 2020, 3030])
  expectTrue(_Set<Int>().isSubsetOf(s1))
  expectFalse(s1.isSubsetOf(_Set<Int>()))
  expectTrue(s1.isSubsetOf(s1))
}

SetTestSuite.test("isStrictSubsetOf.Set.Set") {
  let s1 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = _Set([1010, 2020, 3030])
  expectTrue(_Set<Int>().isStrictSubsetOf(s1))
  expectFalse(s1.isStrictSubsetOf(_Set<Int>()))
  expectFalse(s1.isStrictSubsetOf(s1))
}

SetTestSuite.test("isStrictSubsetOf.Set.Sequence") {
  let s1 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = SequenceOf([1010, 2020, 3030])
  expectTrue(_Set<Int>().isStrictSubsetOf(s1))
  expectFalse(s1.isStrictSubsetOf(_Set<Int>()))
  expectFalse(s1.isStrictSubsetOf(s1))
}

SetTestSuite.test("isSupersetOf.Set.Set") {
  let s1 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = _Set([1010, 2020, 3030])
  expectTrue(s1.isSupersetOf(_Set<Int>()))
  expectFalse(_Set<Int>().isSupersetOf(s1))
  expectTrue(s1.isSupersetOf(s1))
  expectTrue(s1.isSupersetOf(s2))
  expectFalse(_Set<Int>().isSupersetOf(s1))
}

SetTestSuite.test("isSupersetOf.Set.Sequence") {
  let s1 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = SequenceOf([1010, 2020, 3030])
  expectTrue(s1.isSupersetOf(_Set<Int>()))
  expectFalse(_Set<Int>().isSupersetOf(s1))
  expectTrue(s1.isSupersetOf(s1))
  expectTrue(s1.isSupersetOf(s2))
  expectFalse(_Set<Int>().isSupersetOf(s1))
}

SetTestSuite.test("strictSuperset.Set.Set") {
  let s1 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = _Set([1010, 2020, 3030])
  expectTrue(s1.isStrictSupersetOf(_Set<Int>()))
  expectFalse(_Set<Int>().isStrictSupersetOf(s1))
  expectFalse(s1.isStrictSupersetOf(s1))
  expectTrue(s1.isStrictSupersetOf(s2))
}

SetTestSuite.test("strictSuperset.Set.Sequence") {
  let s1 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = SequenceOf([1010, 2020, 3030])
  expectTrue(s1.isStrictSupersetOf(_Set<Int>()))
  expectFalse(_Set<Int>().isStrictSupersetOf(s1))
  expectFalse(s1.isStrictSupersetOf(s1))
  expectTrue(s1.isStrictSupersetOf(s2))
}

SetTestSuite.test("Equatable.Native.Native") {
  let s1 = getCOWFastSet()
  let s2 = getCOWFastSet([1010, 2020, 3030, 4040, 5050, 6060])

  checkEquatable(true, s1, s1, nil)
  checkEquatable(false, s1, _Set<Int>(), nil)
  checkEquatable(true, _Set<Int>(), _Set<Int>(), nil)
  checkEquatable(false, s1, s2, nil)
}

SetTestSuite.test("Equatable.Native.BridgedVerbatim") {
  let s1 = getNativeBridgedVerbatimSet()
  let bvs1 = getBridgedVerbatimSet()
  let bvs2 = getBridgedVerbatimSet([1010, 2020, 3030, 4040, 5050, 6060])
  let bvsEmpty = getBridgedVerbatimSet([])

  checkEquatable(true, s1, bvs1)
  checkEquatable(false, s1, bvs2)
  checkEquatable(false, s1, bvsEmpty)
}

SetTestSuite.test("Equatable.BridgedVerbatim.BridgedVerbatim") {
  let bvs1 = getBridgedVerbatimSet()
  let bvs2 = getBridgedVerbatimSet([1010, 2020, 3030, 4040, 5050, 6060])
  let bvsEmpty = getBridgedVerbatimSet([])

  checkEquatable(true, bvs1, bvs1)
  checkEquatable(false, bvs1, bvs2)
  checkEquatable(false, bvs1, bvsEmpty)
}

SetTestSuite.test("Equatable.BridgedNonverbatim.BridgedNonverbatim") {
  let bnvs1 = getBridgedNonverbatimSet()
  let bnvs2 = getBridgedNonverbatimSet([1010, 2020, 3030, 4040, 5050, 6060])
  let bnvsEmpty = getBridgedNonverbatimSet([])

  checkEquatable(true, bnvs1, bnvs1, nil)
  checkEquatable(false, bnvs1, bnvs2, nil)
  checkEquatable(false, bnvs1, bnvsEmpty, nil)
  checkEquatable(false, bnvs2, bnvsEmpty, nil)
}

SetTestSuite.test("isDisjointWith.Set.Set") {
  let s1 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = _Set([1010, 2020, 3030])
  let s3 = _Set([7070, 8080, 9090])
  expectTrue(s1.isDisjointWith(s3))
  expectFalse(s1.isDisjointWith(s2))
  expectTrue(_Set<Int>().isDisjointWith(s1))
  expectTrue(_Set<Int>().isDisjointWith(_Set<Int>()))
}

SetTestSuite.test("isDisjointWith.Set.Sequence") {
  let s1 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = SequenceOf([1010, 2020, 3030])
  let s3 = SequenceOf([7070, 8080, 9090])
  expectTrue(s1.isDisjointWith(s3))
  expectFalse(s1.isDisjointWith(s2))
  expectTrue(_Set<Int>().isDisjointWith(s1))
  expectTrue(_Set<Int>().isDisjointWith(_Set<Int>()))
}

SetTestSuite.test("insert") {
  // These are anagrams - they should amount to the same sets.
  var s1 = _Set([1010, 2020, 3030])

  let identity1 = unsafeBitCast(s1, Word.self)

  s1.insert(4040)
  expectEqual(4, s1.count)
  expectTrue(s1.contains(1010))
  expectTrue(s1.contains(2020))
  expectTrue(s1.contains(3030))
  expectTrue(s1.contains(4040))
}

SetTestSuite.test("unionInPlace") {
  // These are anagrams - they should amount to the same sets.
  var s1 = _Set("the morse code")
  let s2 = _Set("here come dots")
  let s3 = _Set("and then dashes")

  let identity1 = unsafeBitCast(s1, Word.self)

  s1.unionInPlace("")
  expectEqual(identity1, unsafeBitCast(s1, Word.self))

  expectEqual(s1, s2)
  s1.unionInPlace(s2)
  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))

  s1.unionInPlace(s3)
  expectNotEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
}

SetTestSuite.test("union") {
  let s1 = _Set([1010, 2020, 3030])
  let s2 = _Set([4040, 5050, 6060])
  let s3 = _Set([1010, 2020, 3030, 4040, 5050, 6060])

  let identity1 = unsafeBitCast(s1, Word.self)

  let s4 = s1.union(s2)
  expectEqual(s4, s3)

  // s1 should be unchanged
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
  expectEqual(_Set([1010, 2020, 3030]), s1)

  // s4 should be a fresh set
  expectNotEqual(identity1, unsafeBitCast(s4, Word.self))
  expectEqual(s4, s3)

  let s5 = s1.union(s1)
  expectEqual(s5, s1)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))

  expectEqual(s1, s1.union(_Set<Int>()))
  expectEqual(s1, _Set<Int>().union(s1))
}

SetTestSuite.test("subtractInPlace") {
  var s1 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s2 = _Set([1010, 2020, 3030])
  let s3 = _Set([4040, 5050, 6060])

  let identity1 = unsafeBitCast(s1, Word.self)

  s1.subtractInPlace(_Set<Int>())
  expectEqual(identity1, unsafeBitCast(s1, Word.self))

  s1.subtractInPlace(s3)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
}

SetTestSuite.test("subtract") {
  let s1 = _Set([1010, 2020, 3030])
  let s2 = _Set([4040, 5050, 6060])
  let s3 = _Set([1010, 2020, 3030, 4040, 5050, 6060])

  let identity1 = unsafeBitCast(s1, Word.self)

  // Subtracting a disjoint set should not create a
  // unique reference
  let s4 = s1.subtract(s2)
  expectEqual(s1, s4)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
  expectEqual(identity1, unsafeBitCast(s4, Word.self))

  // Subtracting a superset will leave the set empty
  let s5 = s1.subtract(s3)
  expectTrue(s5.isEmpty)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
  expectNotEqual(identity1, unsafeBitCast(s5, Word.self))

  // Subtracting the empty set does nothing
  expectEqual(s1, s1.subtract(_Set<Int>()))
  expectEqual(_Set<Int>(), _Set<Int>().subtract(s1))
  expectEqual(identity1, unsafeBitCast(s1, Word.self))
}

SetTestSuite.test("intersectInPlace") {
  var s1 = _Set([1010, 2020, 3030])
  let s2 = _Set([4040, 5050, 6060])
  var s3 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  var s4 = _Set([1010, 2020, 3030])

  let identity1 = unsafeBitCast(s1, Word.self)
  s1.intersectInPlace(s4)
  expectEqual(s1, s4)
  expectEqual(identity1, unsafeBitCast(s1, Word.self))

  s4.intersectInPlace(s2)
  expectEqual(_Set<Int>(), s4)

  let identity2 = unsafeBitCast(s3, Word.self)
  s3.intersectInPlace(s2)
  expectEqual(s3, s2)
  expectTrue(s1.isDisjointWith(s3))
  expectNotEqual(identity1, unsafeBitCast(s3, Word.self))

  var s5 = _Set<Int>()
  s5.intersectInPlace(s5)
  expectEqual(s5, _Set<Int>())
  s5.intersectInPlace(s1)
  expectEqual(s5, _Set<Int>())
}

SetTestSuite.test("intersect") {
  let s1 = _Set([1010, 2020, 3030])
  let s2 = _Set([4040, 5050, 6060])
  var s3 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  var s4 = _Set([1010, 2020, 3030])

  let identity1 = unsafeBitCast(s1, Word.self)
  expectEqual(_Set([1010, 2020, 3030]),
    _Set([1010, 2020, 3030]).intersect(_Set([1010, 2020, 3030])))
  expectEqual(identity1, unsafeBitCast(s1, Word.self))

  expectEqual(s1, s1.intersect(s3))
  expectEqual(identity1, unsafeBitCast(s1, Word.self))

  expectEqual(_Set<Int>(), _Set<Int>().intersect(_Set<Int>()))
  expectEqual(_Set<Int>(), s1.intersect(_Set<Int>()))
  expectEqual(_Set<Int>(), _Set<Int>().intersect(s1))
}

SetTestSuite.test("removeMember") {
  let s1 = _Set([1010, 2020, 3030])
  var s2 = _Set<Int>(minimumCapacity: 10)
  for i in [1010, 2020, 3030] {
    s2.insert(i)
  }

  let identity1 = unsafeBitCast(s2, Word.self)
  s2.remove(4040)
  expectEqual(s2, s1)
  expectEqual(identity1, unsafeBitCast(s2, Word.self))

  s2.remove(3030)
  expectEqual(identity1, unsafeBitCast(s2, Word.self))

  s2.remove(2020)
  expectEqual(identity1, unsafeBitCast(s2, Word.self))

  s2.remove(1010)
  expectEqual(identity1, unsafeBitCast(s2, Word.self))
  expectEqual(_Set<Int>(), s2)
  expectTrue(s2.isEmpty)
}

SetTestSuite.test("contains") {
  let s1 = _Set([1010, 2020, 3030])
  expectTrue(s1.contains(1010))
  expectFalse(s1.contains(999))
}

SetTestSuite.test("memberAtIndex") {
  let s1 = _Set([1010, 2020, 3030])

  let foundIndex = s1.indexOf(1010)!
  expectEqual(1010, s1[foundIndex])
}

SetTestSuite.test("first") {
  let s1 = _Set([1010, 2020, 3030])
  let emptySet = _Set<Int>()

  let a1 = s1.first
  expectTrue(s1.contains(a1!))

  let none = emptySet.first
  expectEmpty(none)
}

SetTestSuite.test("count") {
  let s1 = _Set([1010, 2020, 3030])
  var s2 = _Set([4040, 5050, 6060])
  expectEqual(0, _Set<Int>().count)
  expectEqual(3, s1.count)
}

SetTestSuite.test("contains") {
  let s1 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  expectTrue(s1.contains(1010))
  expectFalse(s1.contains(999))
  expectFalse(_Set<Int>().contains(1010))
}

SetTestSuite.test("commutative") {
  let s1 = _Set([1010, 2020, 3030])
  let s2 = _Set([2020, 3030])
  expectTrue(equalsUnordered(s1.intersect(s2), s2.intersect(s1)))
  expectTrue(equalsUnordered(s1.union(s2), s2.union(s1)))
}

SetTestSuite.test("associative") {
  let s1 = _Set([1010, 2020, 3030])
  let s2 = _Set([2020, 3030])
  let s3 = _Set([1010, 2020, 3030])
  let s4 = _Set([2020, 3030])
  let s5 = _Set([7070, 8080, 9090])

  expectTrue(equalsUnordered(s1.intersect(s2).intersect(s3),
    s1.intersect(s2.intersect(s3))))
  expectTrue(equalsUnordered(s3.union(s4).union(s5), s3.union(s4.union(s5))))
}

SetTestSuite.test("distributive") {
  let s1 = _Set([1010])
  let s2 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  let s3 = _Set([2020, 3030])
  expectTrue(equalsUnordered(s1.union(s2.intersect(s3)),
    s1.union(s2).intersect(s1.union(s3))))

  let s4 = _Set([2020, 3030])
  expectTrue(equalsUnordered(s4.intersect(s1.union(s3)),
    s4.intersect(s1).union(s4.intersect(s3))))
}

SetTestSuite.test("idempotent") {
  let s1 = _Set([1010, 2020, 3030, 4040, 5050, 6060])
  expectTrue(equalsUnordered(s1, s1.intersect(s1)))
  expectTrue(equalsUnordered(s1, s1.union(s1)))
}

SetTestSuite.test("absorption") {
  let s1 = _Set([1010, 2020, 3030])
  let s2 = _Set([4040, 5050, 6060])
  let s3 = _Set([2020, 3030])
  expectTrue(equalsUnordered(s1, s1.union(s1.intersect(s2))))
  expectTrue(equalsUnordered(s1, s1.intersect(s1.union(s3))))
}

SetTestSuite.test("misc") {
  // Set with other types
  if true {
    var s = _Set([1.1, 2.2, 3.3])
    s.insert(4.4)
    expectTrue(s.contains(1.1))
    expectTrue(s.contains(2.2))
    expectTrue(s.contains(3.3))
  }

  if true {
    var s = _Set(["Hello", "world"])
    expectTrue(s.contains("Hello"))
    expectTrue(s.contains("world"))
  }
}

SetTestSuite.test("Hashable") {
  let s1 = _Set([1010])
  let s2 = _Set([2020])
  checkHashable(s1 == s2, s1, s2)

  let ss1 = _Set([_Set([1010]), _Set([2020]), _Set([3030])])
  let ss11 = _Set([_Set([2020]), _Set([3030]), _Set([2020])])
  let ss2 = _Set([_Set([9090])])
  checkHashable(ss1 == ss11, ss1, ss11)
  checkHashable(ss1 == ss2, ss1, ss2)
}

runAllTests()

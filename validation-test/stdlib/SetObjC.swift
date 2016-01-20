// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %S/../../utils/gyb %s -o %t/main.swift
// RUN: %target-clang -fobjc-arc %S/Inputs/SlurpFastEnumeration/SlurpFastEnumeration.m -c -o %t/SlurpFastEnumeration.o
// RUN: %S/../../utils/line-directive %t/main.swift -- %target-build-swift %S/Inputs/DictionaryKeyValueTypes.swift %S/Inputs/DictionaryKeyValueTypesObjC.swift %t/main.swift -I %S/Inputs/SlurpFastEnumeration/ -Xlinker %t/SlurpFastEnumeration.o -o %t/Set -Xfrontend -disable-access-control
//
// RUN: %S/../../utils/line-directive %t/main.swift -- %target-run %t/Set
// REQUIRES: executable_test
// REQUIRES: objc_interop

#if os(OSX)
import Darwin
#elseif os(Linux) || os(FreeBSD)
import Glibc
#endif
import StdlibUnittest
import Foundation

// For experimental Set operators
import SwiftExperimental

let hugeNumberArray = (0..<500).map {
  (i: Int) -> Int in
  return random()
}

var SetTestSuite = TestSuite("Set")

SetTestSuite.setUp {
  resetLeaksOfDictionaryKeysValues()
  resetLeaksOfObjCDictionaryKeysValues()
}

SetTestSuite.tearDown {
  expectNoLeaksOfDictionaryKeysValues()
  expectNoLeaksOfObjCDictionaryKeysValues()
}

func isNativeSet<T : Hashable>(s: Set<T>) -> Bool {
  switch s._variantStorage {
  case .Native:
    return true
  case .Cocoa:
    return false
  }
}

func isNativeNSSet(s: NSSet) -> Bool {
  let className: NSString = NSStringFromClass(s.dynamicType)
  return className.rangeOfString("NativeSetStorage").length > 0
}

func isCocoaNSSet(s: NSSet) -> Bool {
  let className: NSString = NSStringFromClass(s.dynamicType)
  return className.rangeOfString("NSSet").length > 0 ||
    className.rangeOfString("NSCFSet").length > 0
}

func getBridgedEmptyNSSet() -> NSSet {
  let s = Set<TestObjCKeyTy>()

  let bridged = unsafeBitCast(_convertSetToNSSet(s), NSSet.self)
  expectTrue(isNativeNSSet(bridged))

  return bridged
}


func isCocoaSet<T : Hashable>(s: Set<T>) -> Bool {
  return !isNativeSet(s)
}

func equalsUnordered(lhs: Set<Int>, _ rhs: Set<Int>) -> Bool {
  return lhs.sort().elementsEqual(rhs.sort()) {
    $0 == $1
  }
}

/// Get an NSSet of TestObjCKeyTy values
func getAsNSSet(members: [Int] = [1010, 2020, 3030]) -> NSSet {
  let nsArray = NSMutableArray()
  for member in members {
    nsArray.addObject(TestObjCKeyTy(member))
  }
  return NSMutableSet(array: nsArray as [AnyObject])
}

/// Get an NSMutableSet of TestObjCKeyTy values
func getAsNSMutableSet(members: [Int] = [1010, 2020, 3030]) -> NSMutableSet {
  let nsArray = NSMutableArray()
  for member in members {
    nsArray.addObject(TestObjCKeyTy(member))
  }
  return NSMutableSet(array: nsArray as [AnyObject])
}

/// Get a Set<NSObject> (Set<TestObjCKeyTy>) backed by Cocoa storage
func getBridgedVerbatimSet(members: [Int] = [1010, 2020, 3030])
  -> Set<NSObject> {
  let nss = getAsNSSet(members)
  let result: Set<NSObject> = _convertNSSetToSet(nss)
  expectTrue(isCocoaSet(result))
  return result
}

/// Get a Set<NSObject> (Set<TestObjCKeyTy>) backed by native storage
func getNativeBridgedVerbatimSet(members: [Int] = [1010, 2020, 3030]) ->
  Set<NSObject> {
  let result: Set<NSObject> = Set(members.map({ TestObjCKeyTy($0) }))
  expectTrue(isNativeSet(result))
  return result
}

/// Get a Set<NSObject> (Set<TestObjCKeyTy>) backed by Cocoa storage
func getHugeBridgedVerbatimSet() -> Set<NSObject> {
  let nss = getAsNSSet(hugeNumberArray)
  let result: Set<NSObject> = _convertNSSetToSet(nss)
  expectTrue(isCocoaSet(result))
  return result
}

/// Get a Set<TestBridgedKeyTy> backed by native storage
func getBridgedNonverbatimSet(members: [Int] = [1010, 2020, 3030]) ->
  Set<TestBridgedKeyTy> {
  let nss = getAsNSSet(members)
  let _ = unsafeBitCast(nss, Int.self)
  let result: Set<TestBridgedKeyTy> =
    Swift._forceBridgeFromObjectiveC(nss, Set.self)
  expectTrue(isNativeSet(result))
  return result
}

/// Get a larger Set<TestBridgedKeyTy> backed by native storage
func getHugeBridgedNonverbatimSet() -> Set<TestBridgedKeyTy> {
  let nss = getAsNSSet(hugeNumberArray)
  let _ = unsafeBitCast(nss, Int.self)
  let result: Set<TestBridgedKeyTy> =
    Swift._forceBridgeFromObjectiveC(nss, Set.self)
  expectTrue(isNativeSet(result))
  return result
}

func getBridgedVerbatimSetAndNSMutableSet() -> (Set<NSObject>, NSMutableSet) {
  let nss = getAsNSMutableSet()
  return (_convertNSSetToSet(nss), nss)
}

func getBridgedNonverbatimSetAndNSMutableSet()
    -> (Set<TestBridgedKeyTy>, NSMutableSet) {
  let nss = getAsNSMutableSet()
  return (Swift._forceBridgeFromObjectiveC(nss, Set.self), nss)
}

func getBridgedNSSetOfRefTypesBridgedVerbatim() -> NSSet {
  expectTrue(_isBridgedVerbatimToObjectiveC(TestObjCKeyTy.self))

  var s = Set<TestObjCKeyTy>(minimumCapacity: 32)
  s.insert(TestObjCKeyTy(1010))
  s.insert(TestObjCKeyTy(2020))
  s.insert(TestObjCKeyTy(3030))

  let bridged =
    unsafeBitCast(_convertSetToNSSet(s), NSSet.self)

  expectTrue(isNativeNSSet(bridged))

  return bridged
}

func getBridgedNSSet_ValueTypesCustomBridged(
  numElements numElements: Int = 3
) -> NSSet {
  expectTrue(!_isBridgedVerbatimToObjectiveC(TestBridgedKeyTy.self))

  var s = Set<TestBridgedKeyTy>()
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

  let nss = NSSet(array: items as [AnyObject])

  let s: Set<NSObject> = _convertNSSetToSet(nss)

  let bridgedBack = _convertSetToNSSet(s)
  expectTrue(isCocoaNSSet(bridgedBack))
  // FIXME: this should be true.
  //expectTrue(unsafeBitCast(nsd, Int.self) == unsafeBitCast(bridgedBack, Int.self))

  return bridgedBack
}

func getBridgedNSSet_MemberTypesCustomBridged() -> NSSet {
  expectFalse(_isBridgedVerbatimToObjectiveC(TestBridgedKeyTy.self))

  var s = Set<TestBridgedKeyTy>()
  s.insert(TestBridgedKeyTy(1010))
  s.insert(TestBridgedKeyTy(2020))
  s.insert(TestBridgedKeyTy(3030))

  let bridged = _convertSetToNSSet(s)
  expectTrue(isNativeNSSet(bridged))

  return bridged
}

@objc
class CustomImmutableNSSet : NSSet {
  init(_privateInit: ()) {
    super.init()
  }

  override init() {
    expectUnreachable()
    super.init()
  }

  override init(objects: UnsafePointer<AnyObject?>, count: Int) {
    expectUnreachable()
    super.init(objects: objects, count: count)
  }

  required init(coder aDecoder: NSCoder) {
    fatalError("init(coder:) not implemented by CustomImmutableNSSet")
  }

  @objc override func copyWithZone(zone: NSZone) -> AnyObject {
    ++CustomImmutableNSSet.timesCopyWithZoneWasCalled
    return self
  }

  override func member(object: AnyObject) -> AnyObject? {
    ++CustomImmutableNSSet.timesMemberWasCalled
    return getAsNSSet([ 1010, 1020, 1030 ]).member(object)
  }

  override func objectEnumerator() -> NSEnumerator {
    ++CustomImmutableNSSet.timesObjectEnumeratorWasCalled
    return getAsNSSet([ 1010, 1020, 1030 ]).objectEnumerator()
  }

  override var count: Int {
    ++CustomImmutableNSSet.timesCountWasCalled
    return 3
  }

  static var timesCopyWithZoneWasCalled = 0
  static var timesMemberWasCalled = 0
  static var timesObjectEnumeratorWasCalled = 0
  static var timesCountWasCalled = 0
}


SetTestSuite.test("BridgedFromObjC.Verbatim.SetIsCopied") {
  var (s, nss) = getBridgedVerbatimSetAndNSMutableSet()
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isCocoaSet(s))

  expectTrue(s.contains(TestObjCKeyTy(1010)))
  expectNotEmpty(nss.member(TestObjCKeyTy(1010)))

  nss.removeObject(TestObjCKeyTy(1010))
  expectEmpty(nss.member(TestObjCKeyTy(1010)))

  expectTrue(s.contains(TestObjCKeyTy(1010)))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.SetIsCopied") {
  var (s, nss) = getBridgedNonverbatimSetAndNSMutableSet()
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isNativeSet(s))

  expectTrue(s.contains(TestBridgedKeyTy(1010)))
  expectNotEmpty(nss.member(TestBridgedKeyTy(1010)))

  nss.removeObject(TestBridgedKeyTy(1010))
  expectEmpty(nss.member(TestBridgedKeyTy(1010)))

  expectTrue(s.contains(TestBridgedKeyTy(1010)))
}


SetTestSuite.test("BridgedFromObjC.Verbatim.NSSetIsRetained") {
  var nss: NSSet = NSSet(set: getAsNSSet([ 1010, 1020, 1030 ]))

  var s: Set<NSObject> = _convertNSSetToSet(nss)

  var bridgedBack: NSSet = _convertSetToNSSet(s)

  expectEqual(
    unsafeBitCast(nss, Int.self),
    unsafeBitCast(bridgedBack, Int.self))

  _fixLifetime(nss)
  _fixLifetime(s)
  _fixLifetime(bridgedBack)
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.NSSetIsCopied") {
  var nss: NSSet = NSSet(set: getAsNSSet([ 1010, 1020, 1030 ]))

  var s: Set<TestBridgedKeyTy> = _convertNSSetToSet(nss)

  var bridgedBack: NSSet = _convertSetToNSSet(s)

  expectNotEqual(
    unsafeBitCast(nss, Int.self),
    unsafeBitCast(bridgedBack, Int.self))

  _fixLifetime(nss)
  _fixLifetime(s)
  _fixLifetime(bridgedBack)
}


SetTestSuite.test("BridgedFromObjC.Verbatim.ImmutableSetIsRetained") {
  var nss: NSSet = CustomImmutableNSSet(_privateInit: ())

  CustomImmutableNSSet.timesCopyWithZoneWasCalled = 0
  CustomImmutableNSSet.timesMemberWasCalled = 0
  CustomImmutableNSSet.timesObjectEnumeratorWasCalled = 0
  CustomImmutableNSSet.timesCountWasCalled = 0
  var s: Set<NSObject> = _convertNSSetToSet(nss)
  expectEqual(1, CustomImmutableNSSet.timesCopyWithZoneWasCalled)
  expectEqual(0, CustomImmutableNSSet.timesMemberWasCalled)
  expectEqual(0, CustomImmutableNSSet.timesObjectEnumeratorWasCalled)
  expectEqual(0, CustomImmutableNSSet.timesCountWasCalled)

  var bridgedBack: NSSet = _convertSetToNSSet(s)
  expectEqual(
    unsafeBitCast(nss, Int.self),
    unsafeBitCast(bridgedBack, Int.self))

  _fixLifetime(nss)
  _fixLifetime(s)
  _fixLifetime(bridgedBack)
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.ImmutableSetIsCopied") {
  var nss: NSSet = CustomImmutableNSSet(_privateInit: ())

  CustomImmutableNSSet.timesCopyWithZoneWasCalled = 0
  CustomImmutableNSSet.timesMemberWasCalled = 0
  CustomImmutableNSSet.timesObjectEnumeratorWasCalled = 0
  CustomImmutableNSSet.timesCountWasCalled = 0
  var s: Set<TestBridgedKeyTy> = _convertNSSetToSet(nss)
  expectEqual(0, CustomImmutableNSSet.timesCopyWithZoneWasCalled)
  expectEqual(0, CustomImmutableNSSet.timesMemberWasCalled)
  expectEqual(1, CustomImmutableNSSet.timesObjectEnumeratorWasCalled)
  expectNotEqual(0, CustomImmutableNSSet.timesCountWasCalled)

  var bridgedBack: NSSet = _convertSetToNSSet(s)
  expectNotEqual(
    unsafeBitCast(nss, Int.self),
    unsafeBitCast(bridgedBack, Int.self))

  _fixLifetime(nss)
  _fixLifetime(s)
  _fixLifetime(bridgedBack)
}


SetTestSuite.test("BridgedFromObjC.Verbatim.IndexForMember") {
  var s = getBridgedVerbatimSet()
  var identity1 = unsafeBitCast(s, Int.self)
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
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.IndexForMember") {
  var s = getBridgedNonverbatimSet()
  var identity1 = unsafeBitCast(s, Int.self)

  do {
    var member = s[s.indexOf(TestBridgedKeyTy(1010))!]
    expectEqual(TestBridgedKeyTy(1010), member)

    member = s[s.indexOf(TestBridgedKeyTy(2020))!]
    expectEqual(TestBridgedKeyTy(2020), member)

    member = s[s.indexOf(TestBridgedKeyTy(3030))!]
    expectEqual(TestBridgedKeyTy(3030), member)
  }

  expectEmpty(s.indexOf(TestBridgedKeyTy(4040)))
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Insert") {
  do {
    var s = getBridgedVerbatimSet()
    var identity1 = unsafeBitCast(s, Int.self)
    expectTrue(isCocoaSet(s))

    expectFalse(s.contains(TestObjCKeyTy(2040)))
    s.insert(TestObjCKeyTy(2040))

    var identity2 = unsafeBitCast(s, Int.self)
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
  do {
    var s = getBridgedNonverbatimSet()
    var identity1 = unsafeBitCast(s, Int.self)
    expectTrue(isNativeSet(s))

    expectFalse(s.contains(TestBridgedKeyTy(2040)))
    s.insert(TestObjCKeyTy(2040) as TestBridgedKeyTy)

    var identity2 = unsafeBitCast(s, Int.self)
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
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isCocoaSet(s))

  var startIndex = s.startIndex
  var endIndex = s.endIndex
  expectNotEqual(startIndex, endIndex)
  expectTrue(startIndex < endIndex)
  expectTrue(startIndex <= endIndex)
  expectFalse(startIndex >= endIndex)
  expectFalse(startIndex > endIndex)
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  var members = [Int]()
  for var i = startIndex; i != endIndex; ++i {
    var foundMember: AnyObject = s[i]
    let member = foundMember as! TestObjCKeyTy
    members.append(member.value)
  }
  expectTrue(equalsUnordered(members, [1010, 2020, 3030]))
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.SubscriptWithIndex") {
  var s = getBridgedNonverbatimSet()
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isNativeSet(s))

  var startIndex = s.startIndex
  var endIndex = s.endIndex
  expectNotEqual(startIndex, endIndex)
  expectTrue(startIndex < endIndex)
  expectTrue(startIndex <= endIndex)
  expectFalse(startIndex >= endIndex)
  expectFalse(startIndex > endIndex)
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  var members = [Int]()
  for var i = startIndex; i != endIndex; ++i {
    var foundMember: AnyObject = s[i]
    let member = foundMember as! TestObjCKeyTy
    members.append(member.value)
  }
  expectTrue(equalsUnordered(members, [1010, 2020, 3030]))
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

SetTestSuite.test("BridgedFromObjC.Verbatim.SubscriptWithIndex_Empty") {
  var s = getBridgedVerbatimSet([])
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isCocoaSet(s))

  var startIndex = s.startIndex
  var endIndex = s.endIndex
  expectEqual(startIndex, endIndex)
  expectFalse(startIndex < endIndex)
  expectTrue(startIndex <= endIndex)
  expectTrue(startIndex >= endIndex)
  expectFalse(startIndex > endIndex)
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.SubscriptWithIndex_Empty") {
  var s = getBridgedNonverbatimSet([])
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isNativeSet(s))

  var startIndex = s.startIndex
  var endIndex = s.endIndex
  expectEqual(startIndex, endIndex)
  expectFalse(startIndex < endIndex)
  expectTrue(startIndex <= endIndex)
  expectTrue(startIndex >= endIndex)
  expectFalse(startIndex > endIndex)
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Contains") {
  var s = getBridgedVerbatimSet()
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isCocoaSet(s))

  expectTrue(s.contains(TestObjCKeyTy(1010)))
  expectTrue(s.contains(TestObjCKeyTy(2020)))
  expectTrue(s.contains(TestObjCKeyTy(3030)))

  expectEqual(identity1, unsafeBitCast(s, Int.self))

  // Inserting an item should now create storage unique from the bridged set.
  s.insert(TestObjCKeyTy(4040))
  var identity2 = unsafeBitCast(s, Int.self)
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
  expectEqual(identity2, unsafeBitCast(s, Int.self))
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
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isNativeSet(s))

  expectTrue(s.contains(TestBridgedKeyTy(1010)))
  expectTrue(s.contains(TestBridgedKeyTy(2020)))
  expectTrue(s.contains(TestBridgedKeyTy(3030)))

  expectEqual(identity1, unsafeBitCast(s, Int.self))

  // Inserting an item should now create storage unique from the bridged set.
  s.insert(TestBridgedKeyTy(4040))
  var identity2 = unsafeBitCast(s, Int.self)
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
  expectEqual(identity2, unsafeBitCast(s, Int.self))
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
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isNativeSet(s))

  expectTrue(s.contains(TestBridgedKeyTy(1010)))
  expectTrue(s.contains(TestBridgedKeyTy(2020)))
  expectTrue(s.contains(TestBridgedKeyTy(3030)))

  expectEqual(identity1, unsafeBitCast(s, Int.self))

  // Insert a new member.
  // This should trigger a copy.
  s.insert(TestBridgedKeyTy(4040))
  var identity2 = unsafeBitCast(s, Int.self)

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
  expectEqual(identity2, unsafeBitCast(s, Int.self))
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
  let identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isCocoaSet(s))

  let foundIndex1 = s.indexOf(TestObjCKeyTy(1010))!
  expectEqual(TestObjCKeyTy(1010), s[foundIndex1])
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  let removedElement = s.removeAtIndex(foundIndex1)
  expectNotEqual(identity1, unsafeBitCast(s, Int.self))
  expectTrue(isNativeSet(s))
  expectEqual(2, s.count)
  expectEqual(TestObjCKeyTy(1010), removedElement)
  expectEmpty(s.indexOf(TestObjCKeyTy(1010)))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.RemoveAtIndex") {
  var s = getBridgedNonverbatimSet()
  let identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isNativeSet(s))

  let foundIndex1 = s.indexOf(TestBridgedKeyTy(1010))!
  expectEqual(1010, s[foundIndex1].value)
  expectEqual(identity1, unsafeBitCast(s, Int.self))

  let removedElement = s.removeAtIndex(foundIndex1)
  expectEqual(identity1, unsafeBitCast(s, Int.self))
  expectTrue(isNativeSet(s))
  expectEqual(1010, removedElement.value)
  expectEqual(2, s.count)
  expectEmpty(s.indexOf(TestBridgedKeyTy(1010)))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Remove") {
  do {
    var s = getBridgedVerbatimSet()
    var identity1 = unsafeBitCast(s, Int.self)
    expectTrue(isCocoaSet(s))

    var deleted: AnyObject? = s.remove(TestObjCKeyTy(0))
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s, Int.self))
    expectTrue(isCocoaSet(s))

    deleted = s.remove(TestObjCKeyTy(1010))
    expectEqual(1010, deleted!.value)
    var identity2 = unsafeBitCast(s, Int.self)
    expectNotEqual(identity1, identity2)
    expectTrue(isNativeSet(s))
    expectEqual(2, s.count)

    expectFalse(s.contains(TestObjCKeyTy(1010)))
    expectTrue(s.contains(TestObjCKeyTy(2020)))
    expectTrue(s.contains(TestObjCKeyTy(3030)))
    expectEqual(identity2, unsafeBitCast(s, Int.self))
  }

  do {
    var s1 = getBridgedVerbatimSet()
    var identity1 = unsafeBitCast(s1, Int.self)

    var s2 = s1
    expectTrue(isCocoaSet(s1))
    expectTrue(isCocoaSet(s2))

    var deleted: AnyObject? = s2.remove(TestObjCKeyTy(0))
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectEqual(identity1, unsafeBitCast(s2, Int.self))
    expectTrue(isCocoaSet(s1))
    expectTrue(isCocoaSet(s2))

    deleted = s2.remove(TestObjCKeyTy(1010))
    expectEqual(1010, deleted!.value)
    var identity2 = unsafeBitCast(s2, Int.self)
    expectNotEqual(identity1, identity2)
    expectTrue(isCocoaSet(s1))
    expectTrue(isNativeSet(s2))
    expectEqual(2, s2.count)

    expectTrue(s1.contains(TestObjCKeyTy(1010)))
    expectTrue(s1.contains(TestObjCKeyTy(2020)))
    expectTrue(s1.contains(TestObjCKeyTy(3030)))
    expectEqual(identity1, unsafeBitCast(s1, Int.self))

    expectFalse(s2.contains(TestObjCKeyTy(1010)))
    expectTrue(s2.contains(TestObjCKeyTy(2020)))
    expectTrue(s2.contains(TestObjCKeyTy(3030)))

    expectEqual(identity2, unsafeBitCast(s2, Int.self))
  }
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.Remove") {
  do {
    var s = getBridgedNonverbatimSet()
    var identity1 = unsafeBitCast(s, Int.self)
    expectTrue(isNativeSet(s))

    // Trying to remove something not in the set should
    // leave it completely unchanged.
    var deleted = s.remove(TestBridgedKeyTy(0))
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s, Int.self))
    expectTrue(isNativeSet(s))

    // Now remove an item that is in the set. This should
    // not create a new set altogether, however.
    deleted = s.remove(TestBridgedKeyTy(1010))
    expectEqual(1010, deleted!.value)
    var identity2 = unsafeBitCast(s, Int.self)
    expectEqual(identity1, identity2)
    expectTrue(isNativeSet(s))
    expectEqual(2, s.count)

    // Double-check - the removed member should not be found.
    expectFalse(s.contains(TestBridgedKeyTy(1010)))

    // ... but others not removed should be found.
    expectTrue(s.contains(TestBridgedKeyTy(2020)))
    expectTrue(s.contains(TestBridgedKeyTy(3030)))

    // Triple-check - we should still be working with the same object.
    expectEqual(identity2, unsafeBitCast(s, Int.self))
  }

  do {
    var s1 = getBridgedNonverbatimSet()
    let identity1 = unsafeBitCast(s1, Int.self)

    var s2 = s1
    expectTrue(isNativeSet(s1))
    expectTrue(isNativeSet(s2))

    var deleted = s2.remove(TestBridgedKeyTy(0))
    expectEmpty(deleted)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectEqual(identity1, unsafeBitCast(s2, Int.self))
    expectTrue(isNativeSet(s1))
    expectTrue(isNativeSet(s2))

    deleted = s2.remove(TestBridgedKeyTy(1010))
    expectEqual(1010, deleted!.value)
    let identity2 = unsafeBitCast(s2, Int.self)
    expectNotEqual(identity1, identity2)
    expectTrue(isNativeSet(s1))
    expectTrue(isNativeSet(s2))
    expectEqual(2, s2.count)

    expectTrue(s1.contains(TestBridgedKeyTy(1010)))
    expectTrue(s1.contains(TestBridgedKeyTy(2020)))
    expectTrue(s1.contains(TestBridgedKeyTy(3030)))
    expectEqual(identity1, unsafeBitCast(s1, Int.self))

    expectFalse(s2.contains(TestBridgedKeyTy(1010)))
    expectTrue(s2.contains(TestBridgedKeyTy(2020)))
    expectTrue(s2.contains(TestBridgedKeyTy(3030)))
    expectEqual(identity2, unsafeBitCast(s2, Int.self))
  }
}

SetTestSuite.test("BridgedFromObjC.Verbatim.RemoveAll") {
  do {
    var s = getBridgedVerbatimSet([])
    var identity1 = unsafeBitCast(s, Int.self)
    expectTrue(isCocoaSet(s))
    expectEqual(0, s.count)

    s.removeAll()
    expectEqual(identity1, unsafeBitCast(s, Int.self))
    expectEqual(0, s.count)
  }

  do {
    var s = getBridgedVerbatimSet()
    var identity1 = unsafeBitCast(s, Int.self)
    expectTrue(isCocoaSet(s))
    expectEqual(3, s.count)
    expectTrue(s.contains(TestBridgedKeyTy(1010)))

    s.removeAll()
    expectEqual(0, s.count)
    expectFalse(s.contains(TestBridgedKeyTy(1010)))
  }

  do {
    var s1 = getBridgedVerbatimSet()
    var identity1 = unsafeBitCast(s1, Int.self)
    expectTrue(isCocoaSet(s1))
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))

    var s2 = s1
    s2.removeAll()
    var identity2 = unsafeBitCast(s2, Int.self)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity2, identity1)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))
    expectEqual(0, s2.count)
    expectFalse(s2.contains(TestBridgedKeyTy(1010)))
  }

  do {
    var s1 = getBridgedVerbatimSet()
    var identity1 = unsafeBitCast(s1, Int.self)
    expectTrue(isCocoaSet(s1))
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))

    var s2 = s1
    s2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(s2, Int.self)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity2, identity1)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))
    expectEqual(0 , s2.count)
    expectFalse(s2.contains(TestBridgedKeyTy(1010)))
  }
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.RemoveAll") {
  do {
    var s = getBridgedNonverbatimSet([])
    var identity1 = unsafeBitCast(s, Int.self)
    expectTrue(isNativeSet(s))
    expectEqual(0, s.count)

    s.removeAll()
    expectEqual(identity1, unsafeBitCast(s, Int.self))
    expectEqual(0, s.count)
  }

  do {
    var s = getBridgedNonverbatimSet()
    var identity1 = unsafeBitCast(s, Int.self)
    expectTrue(isNativeSet(s))
    expectEqual(3, s.count)
    expectTrue(s.contains(TestBridgedKeyTy(1010)))

    s.removeAll()
    expectEqual(0, s.count)
    expectFalse(s.contains(TestBridgedKeyTy(1010)))
  }

  do {
    var s1 = getBridgedNonverbatimSet()
    var identity1 = unsafeBitCast(s1, Int.self)
    expectTrue(isNativeSet(s1))
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))

    var s2 = s1
    s2.removeAll()
    var identity2 = unsafeBitCast(s2, Int.self)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity2, identity1)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))
    expectEqual(0, s2.count)
    expectFalse(s2.contains(TestBridgedKeyTy(1010)))
  }

  do {
    var s1 = getBridgedNonverbatimSet()
    var identity1 = unsafeBitCast(s1, Int.self)
    expectTrue(isNativeSet(s1))
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestBridgedKeyTy(1010)))

    var s2 = s1
    s2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(s2, Int.self)
    expectEqual(identity1, unsafeBitCast(s1, Int.self))
    expectNotEqual(identity2, identity1)
    expectEqual(3, s1.count)
    expectTrue(s1.contains(TestObjCKeyTy(1010) as TestBridgedKeyTy))
    expectEqual(0, s2.count)
    expectFalse(s2.contains(TestObjCKeyTy(1010) as TestBridgedKeyTy))
  }
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Count") {
  var s = getBridgedVerbatimSet()
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isCocoaSet(s))

  expectEqual(3, s.count)
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.Count") {
  var s = getBridgedNonverbatimSet()
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isNativeSet(s))

  expectEqual(3, s.count)
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Generate") {
  var s = getBridgedVerbatimSet()
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isCocoaSet(s))

  var gen = s.generate()
  var members: [Int] = []
  while let member = gen.next() {
    members.append((member as! TestObjCKeyTy).value)
  }
  expectTrue(equalsUnordered(members, [1010, 2020, 3030]))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.Generate") {
  var s = getBridgedNonverbatimSet()
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isNativeSet(s))

  var gen = s.generate()
  var members: [Int] = []
  while let member = gen.next() {
    members.append(member.value)
  }
  expectTrue(equalsUnordered(members, [1010, 2020, 3030]))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Generate_Empty") {
  var s = getBridgedVerbatimSet([])
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isCocoaSet(s))

  var gen = s.generate()
  expectEmpty(gen.next())
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.Generate_Empty") {
  var s = getBridgedNonverbatimSet([])
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isNativeSet(s))

  var gen = s.generate()
  expectEmpty(gen.next())
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.Generate_Huge") {
  var s = getHugeBridgedVerbatimSet()
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isCocoaSet(s))

  var gen = s.generate()
  var members = [Int]()
  while let member = gen.next() {
    members.append((member as! TestObjCKeyTy).value)
  }
  expectTrue(equalsUnordered(members, hugeNumberArray))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.Generate_Huge") {
  var s = getHugeBridgedNonverbatimSet()
  var identity1 = unsafeBitCast(s, Int.self)
  expectTrue(isNativeSet(s))

  var gen = s.generate()
  var members = [Int]()
  while let member = gen.next() as AnyObject? {
    members.append((member as! TestBridgedKeyTy).value)
  }
  expectTrue(equalsUnordered(members, hugeNumberArray))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEmpty(gen.next())
  expectEqual(identity1, unsafeBitCast(s, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.EqualityTest_Empty") {
  var s1 = getBridgedVerbatimSet([])
  var identity1 = unsafeBitCast(s1, Int.self)
  expectTrue(isCocoaSet(s1))

  var s2 = getBridgedVerbatimSet([])
  var identity2 = unsafeBitCast(s2, Int.self)
  expectTrue(isCocoaSet(s2))
  expectEqual(identity1, identity2)

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectEqual(identity2, unsafeBitCast(s2, Int.self))

  s2.insert(TestObjCKeyTy(4040))
  expectTrue(isNativeSet(s2))
  expectNotEqual(identity2, unsafeBitCast(s2, Int.self))
  identity2 = unsafeBitCast(s2, Int.self)

  expectNotEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectEqual(identity2, unsafeBitCast(s2, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.EqualityTest_Empty") {
  var s1 = getBridgedNonverbatimSet([])
  var identity1 = unsafeBitCast(s1, Int.self)
  expectTrue(isNativeSet(s1))

  var s2 = getBridgedNonverbatimSet([])
  var identity2 = unsafeBitCast(s2, Int.self)
  expectTrue(isNativeSet(s2))
  expectNotEqual(identity1, identity2)

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectEqual(identity2, unsafeBitCast(s2, Int.self))

  s2.insert(TestObjCKeyTy(4040) as TestBridgedKeyTy)
  expectTrue(isNativeSet(s2))
  expectEqual(identity2, unsafeBitCast(s2, Int.self))

  expectNotEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectEqual(identity2, unsafeBitCast(s2, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.EqualityTest_Small") {
  var s1 = getBridgedVerbatimSet()
  let identity1 = unsafeBitCast(s1, Int.self)
  expectTrue(isCocoaSet(s1))

  var s2 = getBridgedVerbatimSet()
  let identity2 = unsafeBitCast(s2, Int.self)
  expectTrue(isCocoaSet(s2))
  expectNotEqual(identity1, identity2)

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectEqual(identity2, unsafeBitCast(s2, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Nonverbatim.EqualityTest_Small") {
  var s1 = getBridgedNonverbatimSet()
  let identity1 = unsafeBitCast(s1, Int.self)
  expectTrue(isNativeSet(s1))

  var s2 = getBridgedNonverbatimSet()
  let identity2 = unsafeBitCast(s2, Int.self)
  expectTrue(isNativeSet(s2))
  expectNotEqual(identity1, identity2)

  expectEqual(s1, s2)
  expectEqual(identity1, unsafeBitCast(s1, Int.self))
  expectEqual(identity2, unsafeBitCast(s2, Int.self))
}

SetTestSuite.test("BridgedFromObjC.Verbatim.ArrayOfSets") {
  var nsa = NSMutableArray()
  for i in 0..<3 {
    nsa.addObject(
        getAsNSSet([ 1 + i,  2 + i, 3 + i ]))
  }

  var a = nsa as [AnyObject] as! [Set<NSObject>]
  for i in 0..<3 {
    var s = a[i]
    var gen = s.generate()
    var items: [Int] = []
    while let value = gen.next() {
      let v = (value as! TestObjCKeyTy).value
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

  var a = nsa as [AnyObject] as! [Set<TestBridgedKeyTy>]
  for i in 0..<3 {
    var d = a[i]
    var gen = d.generate()
    var items: [Int] = []
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

  var items: [Int] = []
  while let value = enumerator.nextObject() {
    let v = (value as! TestObjCKeyTy).value
    items.append(v)
  }
  expectTrue(equalsUnordered([ 1010, 2020, 3030 ], items))
}

SetTestSuite.test("BridgedToObjC.Verbatim.ObjectEnumerator.FastEnumeration.UseFromSwift") {
  let s = getBridgedNSSetOfRefTypesBridgedVerbatim()

  checkSetFastEnumerationFromSwift(
    [ 1010, 2020, 3030 ],
    s, { s.objectEnumerator() },
    { ($0 as! TestObjCKeyTy).value })

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

SetTestSuite.test("BridgedToObjC.Verbatim.ObjectEnumerator.FastEnumeration.UseFromObjC") {
  let s = getBridgedNSSetOfRefTypesBridgedVerbatim()

  checkSetFastEnumerationFromObjC(
    [ 1010, 2020, 3030 ],
    s, { s.objectEnumerator() },
    { ($0 as! TestObjCKeyTy).value })

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

SetTestSuite.test("BridgedToObjC.Verbatim.ObjectEnumerator.FastEnumeration_Empty") {
  let s = getBridgedEmptyNSSet()

  checkSetFastEnumerationFromSwift(
    [], s, { s.objectEnumerator() },
    { ($0 as! TestObjCKeyTy).value })

  checkSetFastEnumerationFromObjC(
    [], s, { s.objectEnumerator() },
    { ($0 as! TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Custom.ObjectEnumerator.FastEnumeration.UseFromObjC") {
  let s = getBridgedNSSet_MemberTypesCustomBridged()

  checkSetFastEnumerationFromObjC(
    [ 1010, 2020, 3030 ],
    s, { s.objectEnumerator() },
    { ($0 as! TestObjCKeyTy).value })

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

SetTestSuite.test("BridgedToObjC.Custom.ObjectEnumerator.FastEnumeration.UseFromSwift") {
  let s = getBridgedNSSet_MemberTypesCustomBridged()

  checkSetFastEnumerationFromSwift(
    [ 1010, 2020, 3030 ],
    s, { s.objectEnumerator() },
    { ($0 as! TestObjCKeyTy).value })

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

SetTestSuite.test("BridgedToObjC.Verbatim.FastEnumeration.UseFromSwift") {
  let s = getBridgedNSSetOfRefTypesBridgedVerbatim()

  checkSetFastEnumerationFromSwift(
    [ 1010, 2020, 3030 ],
    s, { s },
    { ($0 as! TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Verbatim.FastEnumeration.UseFromObjC") {
  let s = getBridgedNSSetOfRefTypesBridgedVerbatim()

  checkSetFastEnumerationFromObjC(
    [ 1010, 2020, 3030 ],
    s, { s },
    { ($0 as! TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Verbatim.FastEnumeration_Empty") {
  let s = getBridgedEmptyNSSet()

  checkSetFastEnumerationFromSwift(
    [], s, { s },
    { ($0 as! TestObjCKeyTy).value })

  checkSetFastEnumerationFromObjC(
    [], s, { s },
    { ($0 as! TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Custom.FastEnumeration.UseFromSwift") {
  let s = getBridgedNSSet_ValueTypesCustomBridged()

  checkSetFastEnumerationFromSwift(
    [ 1010, 2020, 3030 ],
    s, { s },
    { ($0 as! TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Custom.FastEnumeration.UseFromObjC") {
  let s = getBridgedNSSet_ValueTypesCustomBridged()

  checkSetFastEnumerationFromObjC(
    [ 1010, 2020, 3030 ],
    s, { s },
    { ($0 as! TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Custom.FastEnumeration_Empty") {
  let s = getBridgedNSSet_ValueTypesCustomBridged(
    numElements: 0)

  checkSetFastEnumerationFromSwift(
    [], s, { s },
    { ($0 as! TestObjCKeyTy).value })

  checkSetFastEnumerationFromObjC(
    [], s, { s },
    { ($0 as! TestObjCKeyTy).value })
}

SetTestSuite.test("BridgedToObjC.Count") {
  let s = getBridgedNSSetOfRefTypesBridgedVerbatim()
  expectEqual(3, s.count)
}

SetTestSuite.test("BridgedToObjC.ObjectEnumerator.NextObject") {
  let s = getBridgedNSSetOfRefTypesBridgedVerbatim()
  let enumerator = s.objectEnumerator()

  var members = [Int]()
  while let nextObject = enumerator.nextObject() {
    members.append((nextObject as! TestObjCKeyTy).value)
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
  while let nextObject = enumerator.nextObject() {
    members.append((nextObject as! TestObjCKeyTy).value)
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
  while let nextObject = enumerator.nextObject() {
    members.append((nextObject as! TestObjCKeyTy).value)
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

  let nss = NSSet(array: nsArray as [AnyObject])

  let s = nss as Set

  var members = [Int]()
  for member: AnyObject in s {
    members.append((member as! TestObjCKeyTy).value)
  }
  expectTrue(equalsUnordered(members, [1010, 2020, 3030]))
}

SetTestSuite.test("SetToNSSetConversion") {
  var s = Set<TestObjCKeyTy>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
    s.insert(TestObjCKeyTy(i))
  }
  let nss: NSSet = s

  expectTrue(equalsUnordered(Array(s).map { $0.value }, [1010, 2020, 3030]))
}

//
// Set Casts
//

SetTestSuite.test("SetUpcastEntryPoint") {
  var s = Set<TestObjCKeyTy>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
      s.insert(TestObjCKeyTy(i))
  }

  var sAsAnyObject: Set<NSObject> = _setUpCast(s)

  expectEqual(3, sAsAnyObject.count)
  expectTrue(sAsAnyObject.contains(TestObjCKeyTy(1010)))
  expectTrue(sAsAnyObject.contains(TestObjCKeyTy(2020)))
  expectTrue(sAsAnyObject.contains(TestObjCKeyTy(3030)))
}

SetTestSuite.test("SetUpcast") {
  var s = Set<TestObjCKeyTy>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
      s.insert(TestObjCKeyTy(i))
  }

  var sAsAnyObject: Set<NSObject> = s

  expectEqual(3, sAsAnyObject.count)
  expectTrue(sAsAnyObject.contains(TestObjCKeyTy(1010)))
  expectTrue(sAsAnyObject.contains(TestObjCKeyTy(2020)))
  expectTrue(sAsAnyObject.contains(TestObjCKeyTy(3030)))
}

SetTestSuite.test("SetUpcastBridgedEntryPoint") {
  var s = Set<TestBridgedKeyTy>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
      s.insert(TestBridgedKeyTy(i))
  }

  do {
    var s: Set<NSObject> = _setBridgeToObjectiveC(s)

    expectTrue(s.contains(TestBridgedKeyTy(1010)))
    expectTrue(s.contains(TestBridgedKeyTy(2020)))
    expectTrue(s.contains(TestBridgedKeyTy(3030)))
  }

  do {
    var s: Set<TestObjCKeyTy> = _setBridgeToObjectiveC(s)

    expectEqual(3, s.count)
    expectTrue(s.contains(TestBridgedKeyTy(1010)))
    expectTrue(s.contains(TestBridgedKeyTy(2020)))
    expectTrue(s.contains(TestBridgedKeyTy(3030)))
  }
}

SetTestSuite.test("SetUpcastBridged") {
  var s = Set<TestBridgedKeyTy>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
      s.insert(TestBridgedKeyTy(i))
  }

  do {
    var s: Set<NSObject> = s

    expectEqual(3, s.count)
    expectTrue(s.contains(TestBridgedKeyTy(1010)))
    expectTrue(s.contains(TestBridgedKeyTy(2020)))
    expectTrue(s.contains(TestBridgedKeyTy(3030)))
  }

  do {
    var s: Set<TestObjCKeyTy> = s

    expectEqual(3, s.count)
    expectTrue(s.contains(TestBridgedKeyTy(1010)))
    expectTrue(s.contains(TestBridgedKeyTy(2020)))
    expectTrue(s.contains(TestBridgedKeyTy(3030)))
  }
}

//
// Set downcasts
//

SetTestSuite.test("SetDowncastEntryPoint") {
  var s = Set<NSObject>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
      s.insert(TestObjCKeyTy(i))
  }

  // Successful downcast.
  let sCC: Set<TestObjCKeyTy> = _setDownCast(s)
  expectEqual(3, sCC.count)
  expectTrue(sCC.contains(TestObjCKeyTy(1010)))
  expectTrue(sCC.contains(TestObjCKeyTy(2020)))
  expectTrue(sCC.contains(TestObjCKeyTy(3030)))

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

SetTestSuite.test("SetDowncast") {
  var s = Set<NSObject>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
      s.insert(TestObjCKeyTy(i))
  }

  // Successful downcast.
  let sCC = s as! Set<TestObjCKeyTy>
  expectEqual(3, sCC.count)
  expectTrue(sCC.contains(TestObjCKeyTy(1010)))
  expectTrue(sCC.contains(TestObjCKeyTy(2020)))
  expectTrue(sCC.contains(TestObjCKeyTy(3030)))

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

SetTestSuite.test("SetDowncastConditionalEntryPoint") {
  var s = Set<NSObject>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
      s.insert(TestObjCKeyTy(i))
  }

  // Successful downcast.
  if let sCC  = _setDownCastConditional(s) as Set<TestObjCKeyTy>? {
    expectEqual(3, sCC.count)
    expectTrue(sCC.contains(TestObjCKeyTy(1010)))
    expectTrue(sCC.contains(TestObjCKeyTy(2020)))
    expectTrue(sCC.contains(TestObjCKeyTy(3030)))
  } else {
    expectTrue(false)
  }

  // Unsuccessful downcast
  s.insert("Hello, world")
  if let sCC = _setDownCastConditional(s) as Set<TestObjCKeyTy>? {
    expectTrue(false)
  }
}

SetTestSuite.test("SetDowncastConditional") {
  var s = Set<NSObject>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
      s.insert(TestObjCKeyTy(i))
  }

  // Successful downcast.
  if let sCC = s as? Set<TestObjCKeyTy> {
    expectEqual(3, sCC.count)
    expectTrue(sCC.contains(TestObjCKeyTy(1010)))
    expectTrue(sCC.contains(TestObjCKeyTy(2020)))
    expectTrue(sCC.contains(TestObjCKeyTy(3030)))
  } else {
    expectTrue(false)
  }

  // Unsuccessful downcast
  s.insert("Hello, world, I'm your wild girl. I'm your ch-ch-ch-ch-ch-ch cherry bomb")
  if let sCC = s as? Set<TestObjCKeyTy> {
    expectTrue(false)
  }
}

SetTestSuite.test("SetBridgeFromObjectiveCEntryPoint") {
  var s = Set<NSObject>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
      s.insert(TestObjCKeyTy(i))
  }

  // Successful downcast.
  let sCV: Set<TestBridgedKeyTy> = _setBridgeFromObjectiveC(s)
  do {
    expectEqual(3, sCV.count)
    expectTrue(sCV.contains(TestBridgedKeyTy(1010)))
    expectTrue(sCV.contains(TestBridgedKeyTy(2020)))
    expectTrue(sCV.contains(TestBridgedKeyTy(3030)))
  }
}

SetTestSuite.test("SetBridgeFromObjectiveC") {
  var s = Set<NSObject>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
      s.insert(TestObjCKeyTy(i))
  }

  // Successful downcast.
  let sCV = s as! Set<TestObjCKeyTy>
  do {
    expectEqual(3, sCV.count)
    expectTrue(sCV.contains(TestObjCKeyTy(1010)))
    expectTrue(sCV.contains(TestObjCKeyTy(2020)))
    expectTrue(sCV.contains(TestObjCKeyTy(3030)))
  }

  // Successful downcast.
  let sVC = s as! Set<TestBridgedKeyTy>
  do {
    expectEqual(3, sVC.count)
    expectTrue(sVC.contains(TestBridgedKeyTy(1010)))
    expectTrue(sVC.contains(TestBridgedKeyTy(2020)))
    expectTrue(sVC.contains(TestBridgedKeyTy(3030)))
  }

  expectAutoreleasedKeysAndValues(unopt: (3, 0))
}

SetTestSuite.test("SetBridgeFromObjectiveCConditionalEntryPoint") {
  var s = Set<NSObject>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
      s.insert(TestObjCKeyTy(i))
  }

  // Successful downcast.
  if let sVC = _setBridgeFromObjectiveCConditional(s) as Set<TestBridgedKeyTy>? {
    expectEqual(3, sVC.count)
    expectTrue(sVC.contains(TestBridgedKeyTy(1010)))
    expectTrue(sVC.contains(TestBridgedKeyTy(2020)))
    expectTrue(sVC.contains(TestBridgedKeyTy(3030)))
  } else {
    expectTrue(false)
  }

  // Unsuccessful downcasts
  s.insert("Hello, world, I'm your wild girl. I'm your ch-ch-ch-ch-ch-ch cherry bomb")
  if let sVC = _setBridgeFromObjectiveCConditional(s) as Set<TestBridgedKeyTy>? {
    expectTrue(false)
  }
}

SetTestSuite.test("SetBridgeFromObjectiveCConditional") {
  var s = Set<NSObject>(minimumCapacity: 32)
  for i in [1010, 2020, 3030] {
      s.insert(TestObjCKeyTy(i))
  }

  // Successful downcast.
  if let sCV = s as? Set<TestObjCKeyTy>  {
    expectEqual(3, sCV.count)
    expectTrue(sCV.contains(TestObjCKeyTy(1010)))
    expectTrue(sCV.contains(TestObjCKeyTy(2020)))
    expectTrue(sCV.contains(TestObjCKeyTy(3030)))
  } else {
    expectTrue(false)
  }

  // Successful downcast.
  if let sVC = s as? Set<TestBridgedKeyTy> {
    expectEqual(3, sVC.count)
    expectTrue(sVC.contains(TestBridgedKeyTy(1010)))
    expectTrue(sVC.contains(TestBridgedKeyTy(2020)))
    expectTrue(sVC.contains(TestBridgedKeyTy(3030)))
  } else {
    expectTrue(false)
  }

  // Unsuccessful downcasts
  s.insert("Hello, world, I'm your wild girl. I'm your ch-ch-ch-ch-ch-ch cherry bomb")
  if let sCm = s as? Set<TestObjCKeyTy> {
    expectTrue(false)
  }
  if let sVC = s as? Set<TestBridgedKeyTy> {
    expectTrue(false)
  }
  if let sVm = s as? Set<TestBridgedKeyTy> {
    expectTrue(false)
  }
}

// Public API


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

  checkEquatable(true, bnvs1, bnvs1)
  checkEquatable(false, bnvs1, bnvs2)
  checkEquatable(false, bnvs1, bnvsEmpty)
  checkEquatable(false, bnvs2, bnvsEmpty)
}

@objc
class MockSetWithCustomCount : NSSet {
  init(count: Int) {
    self._count = count
    super.init()
  }

  override init() {
    expectUnreachable()
    super.init()
  }

  override init( objects: UnsafePointer<AnyObject?>, count: Int) {
    expectUnreachable()
    super.init(objects: objects, count: count)
  }

  required init(coder aDecoder: NSCoder) {
    fatalError("init(coder:) not implemented by MockSetWithCustomCount")
  }

  @objc override func copyWithZone(zone: NSZone) -> AnyObject {
    // Ensure that copying this set produces an object of the same
    // dynamic type.
    return self
  }

  override func member(object: AnyObject) -> AnyObject? {
    expectUnreachable()
    return object
  }

  override func objectEnumerator() -> NSEnumerator {
    expectUnreachable()
    return getAsNSSet([ 1010, 1020, 1030 ]).objectEnumerator()
  }

  override var count: Int {
    ++MockSetWithCustomCount.timesCountWasCalled
    return _count
  }

  var _count: Int = 0

  static var timesCountWasCalled = 0
}

func getMockSetWithCustomCount(count count: Int)
  -> Set<NSObject> {

  return MockSetWithCustomCount(count: count) as Set
}

func callGenericIsEmpty<C : CollectionType>(collection: C) -> Bool {
  return collection.isEmpty
}

SetTestSuite.test("isEmpty/ImplementationIsCustomized") {
  do {
    var d = getMockSetWithCustomCount(count: 0)
    MockSetWithCustomCount.timesCountWasCalled = 0
    expectTrue(d.isEmpty)
    expectEqual(1, MockSetWithCustomCount.timesCountWasCalled)
  }
  do {
    var d = getMockSetWithCustomCount(count: 0)
    MockSetWithCustomCount.timesCountWasCalled = 0
    expectTrue(callGenericIsEmpty(d))
    expectEqual(1, MockSetWithCustomCount.timesCountWasCalled)
  }

  do {
    var d = getMockSetWithCustomCount(count: 4)
    MockSetWithCustomCount.timesCountWasCalled = 0
    expectFalse(d.isEmpty)
    expectEqual(1, MockSetWithCustomCount.timesCountWasCalled)
  }
  do {
    var d = getMockSetWithCustomCount(count: 4)
    MockSetWithCustomCount.timesCountWasCalled = 0
    expectFalse(callGenericIsEmpty(d))
    expectEqual(1, MockSetWithCustomCount.timesCountWasCalled)
  }
}

runAllTests()

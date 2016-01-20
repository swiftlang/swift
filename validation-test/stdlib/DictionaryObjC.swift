// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %S/../../utils/gyb %s -o %t/main.swift
// RUN: %target-clang -fobjc-arc %S/Inputs/SlurpFastEnumeration/SlurpFastEnumeration.m -c -o %t/SlurpFastEnumeration.o
// RUN: %S/../../utils/line-directive %t/main.swift -- %target-build-swift %S/Inputs/DictionaryKeyValueTypes.swift %S/Inputs/DictionaryKeyValueTypesObjC.swift %t/main.swift -I %S/Inputs/SlurpFastEnumeration/ -Xlinker %t/SlurpFastEnumeration.o -o %t/Dictionary -Xfrontend -disable-access-control
//
// RUN: %S/../../utils/line-directive %t/main.swift -- %target-run %t/Dictionary
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Darwin
import StdlibUnittest
import Foundation
import StdlibUnittestFoundationExtras


var DictionaryTestSuite = TestSuite("Dictionary")

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

//===---
// NSDictionary -> Dictionary bridging tests.
//===---

func getAsNSDictionary(d: Dictionary<Int, Int>) -> NSDictionary {
  let keys = Array(d.keys.map { TestObjCKeyTy($0) })
  let values = Array(d.values.map { TestObjCValueTy($0) })

  // Return an `NSMutableDictionary` to make sure that it has a unique
  // pointer identity.
  return NSMutableDictionary(objects: values, forKeys: keys)
}

func getAsEquatableNSDictionary(d: Dictionary<Int, Int>) -> NSDictionary {
  let keys = Array(d.keys.map { TestObjCKeyTy($0) })
  let values = Array(d.values.map { TestObjCEquatableValueTy($0) })

  // Return an `NSMutableDictionary` to make sure that it has a unique
  // pointer identity.
  return NSMutableDictionary(objects: values, forKeys: keys)
}

func getAsNSMutableDictionary(d: Dictionary<Int, Int>) -> NSMutableDictionary {
  let keys = Array(d.keys.map { TestObjCKeyTy($0) })
  let values = Array(d.values.map { TestObjCValueTy($0) })

  return NSMutableDictionary(objects: values, forKeys: keys)
}

func getBridgedVerbatimDictionary() -> Dictionary<NSObject, AnyObject> {
  let nsd = getAsNSDictionary([ 10: 1010, 20: 1020, 30: 1030 ])
  return _convertNSDictionaryToDictionary(nsd)
}

func getBridgedVerbatimDictionary(d: Dictionary<Int, Int>) -> Dictionary<NSObject, AnyObject> {
  let nsd = getAsNSDictionary(d)
  return _convertNSDictionaryToDictionary(nsd)
}

func getBridgedVerbatimDictionaryAndNSMutableDictionary()
    -> (Dictionary<NSObject, AnyObject>, NSMutableDictionary) {
  let nsd = getAsNSMutableDictionary([ 10: 1010, 20: 1020, 30: 1030 ])
  return (_convertNSDictionaryToDictionary(nsd), nsd)
}

func getBridgedNonverbatimDictionary() -> Dictionary<TestBridgedKeyTy, TestBridgedValueTy> {
  let nsd = getAsNSDictionary([ 10: 1010, 20: 1020, 30: 1030 ])
  return Swift._forceBridgeFromObjectiveC(nsd, Dictionary.self)
}

func getBridgedNonverbatimDictionary(d: Dictionary<Int, Int>) -> Dictionary<TestBridgedKeyTy, TestBridgedValueTy> {
  let nsd = getAsNSDictionary(d)
  return Swift._forceBridgeFromObjectiveC(nsd, Dictionary.self)
}

func getBridgedNonverbatimDictionaryAndNSMutableDictionary()
    -> (Dictionary<TestBridgedKeyTy, TestBridgedValueTy>, NSMutableDictionary) {
  let nsd = getAsNSMutableDictionary([ 10: 1010, 20: 1020, 30: 1030 ])
  return (Swift._forceBridgeFromObjectiveC(nsd, Dictionary.self), nsd)
}

func getBridgedVerbatimEquatableDictionary(d: Dictionary<Int, Int>) -> Dictionary<NSObject, TestObjCEquatableValueTy> {
  let nsd = getAsEquatableNSDictionary(d)
  return _convertNSDictionaryToDictionary(nsd)
}

func getBridgedNonverbatimEquatableDictionary(d: Dictionary<Int, Int>) -> Dictionary<TestBridgedKeyTy, TestBridgedEquatableValueTy> {
  let nsd = getAsEquatableNSDictionary(d)
  return Swift._forceBridgeFromObjectiveC(nsd, Dictionary.self)
}

func getHugeBridgedVerbatimDictionaryHelper() -> NSDictionary {
  let keys = (1...32).map { TestObjCKeyTy($0) }
  let values = (1...32).map { TestObjCValueTy(1000 + $0) }

  return NSMutableDictionary(objects: values, forKeys: keys)
}

func getHugeBridgedVerbatimDictionary() -> Dictionary<NSObject, AnyObject> {
  let nsd = getHugeBridgedVerbatimDictionaryHelper()
  return _convertNSDictionaryToDictionary(nsd)
}

func getHugeBridgedNonverbatimDictionary() -> Dictionary<TestBridgedKeyTy, TestBridgedValueTy> {
  let nsd = getHugeBridgedVerbatimDictionaryHelper()
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

  override init(
    objects: UnsafePointer<AnyObject?>,
    forKeys keys: UnsafePointer<NSCopying?>,
    count: Int) {
    super.init(objects: objects, forKeys: keys, count: count)
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

  override func objectForKey(aKey: AnyObject) -> AnyObject? {
    return value
  }

  override var count: Int {
    return 4
  }
}

func getParallelArrayBridgedVerbatimDictionary() -> Dictionary<NSObject, AnyObject> {
  let nsd: NSDictionary = ParallelArrayDictionary()
  return _convertNSDictionaryToDictionary(nsd)
}

func getParallelArrayBridgedNonverbatimDictionary() -> Dictionary<TestBridgedKeyTy, TestBridgedValueTy> {
  let nsd: NSDictionary = ParallelArrayDictionary()
  return Swift._forceBridgeFromObjectiveC(nsd, Dictionary.self)
}

@objc
class CustomImmutableNSDictionary : NSDictionary {
  init(_privateInit: ()) {
    super.init()
  }

  override init() {
    expectUnreachable()
    super.init()
  }

  override init(
    objects: UnsafePointer<AnyObject?>,
    forKeys keys: UnsafePointer<NSCopying?>,
    count: Int) {
    expectUnreachable()
    super.init(objects: objects, forKeys: keys, count: count)
  }

  required init(coder aDecoder: NSCoder) {
    fatalError("init(coder:) not implemented by CustomImmutableNSDictionary")
  }

  @objc override func copyWithZone(zone: NSZone) -> AnyObject {
    ++CustomImmutableNSDictionary.timesCopyWithZoneWasCalled
    return self
  }

  override func objectForKey(aKey: AnyObject) -> AnyObject? {
    ++CustomImmutableNSDictionary.timesObjectForKeyWasCalled
    return getAsNSDictionary([ 10: 1010, 20: 1020, 30: 1030 ]).objectForKey(aKey)
  }

  override func keyEnumerator() -> NSEnumerator {
    ++CustomImmutableNSDictionary.timesKeyEnumeratorWasCalled
    return getAsNSDictionary([ 10: 1010, 20: 1020, 30: 1030 ]).keyEnumerator()
  }

  override var count: Int {
    ++CustomImmutableNSDictionary.timesCountWasCalled
    return 3
  }

  static var timesCopyWithZoneWasCalled = 0
  static var timesObjectForKeyWasCalled = 0
  static var timesKeyEnumeratorWasCalled = 0
  static var timesCountWasCalled = 0
}

DictionaryTestSuite.test("BridgedFromObjC.Verbatim.DictionaryIsCopied") {
  var (d, nsd) = getBridgedVerbatimDictionaryAndNSMutableDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isCocoaDictionary(d))

  // Find an existing key.
  do {
    var kv = d[d.indexForKey(TestObjCKeyTy(10))!]
    assert(kv.0 == TestObjCKeyTy(10))
    assert(kv.1.value == 1010)
  }

  // Delete the key from the NSMutableDictionary.
  assert(nsd[TestObjCKeyTy(10)] != nil)
  nsd.removeObjectForKey(TestObjCKeyTy(10))
  assert(nsd[TestObjCKeyTy(10)] == nil)

  // Find an existing key, again.
  do {
    var kv = d[d.indexForKey(TestObjCKeyTy(10))!]
    assert(kv.0 == TestObjCKeyTy(10))
    assert(kv.1.value == 1010)
  }
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.DictionaryIsCopied") {
  var (d, nsd) = getBridgedNonverbatimDictionaryAndNSMutableDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isNativeDictionary(d))

  // Find an existing key.
  do {
    var kv = d[d.indexForKey(TestBridgedKeyTy(10))!]
    assert(kv.0 == TestBridgedKeyTy(10))
    assert(kv.1.value == 1010)
  }

  // Delete the key from the NSMutableDictionary.
  assert(nsd[TestBridgedKeyTy(10)] != nil)
  nsd.removeObjectForKey(TestBridgedKeyTy(10))
  assert(nsd[TestBridgedKeyTy(10)] == nil)

  // Find an existing key, again.
  do {
    var kv = d[d.indexForKey(TestBridgedKeyTy(10))!]
    assert(kv.0 == TestBridgedKeyTy(10))
    assert(kv.1.value == 1010)
  }
}


DictionaryTestSuite.test("BridgedFromObjC.Verbatim.NSDictionaryIsRetained") {
  var nsd: NSDictionary = NSDictionary(dictionary:
    getAsNSDictionary([ 10: 1010, 20: 1020, 30: 1030 ]))

  var d: [NSObject : AnyObject] = _convertNSDictionaryToDictionary(nsd)

  var bridgedBack: NSDictionary = _convertDictionaryToNSDictionary(d)

  expectEqual(
    unsafeBitCast(nsd, Int.self),
    unsafeBitCast(bridgedBack, Int.self))

  _fixLifetime(nsd)
  _fixLifetime(d)
  _fixLifetime(bridgedBack)
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.NSDictionaryIsCopied") {
  var nsd: NSDictionary = NSDictionary(dictionary:
    getAsNSDictionary([ 10: 1010, 20: 1020, 30: 1030 ]))

  var d: [TestBridgedKeyTy : TestBridgedValueTy] =
    _convertNSDictionaryToDictionary(nsd)

  var bridgedBack: NSDictionary = _convertDictionaryToNSDictionary(d)

  expectNotEqual(
    unsafeBitCast(nsd, Int.self),
    unsafeBitCast(bridgedBack, Int.self))

  _fixLifetime(nsd)
  _fixLifetime(d)
  _fixLifetime(bridgedBack)
}


DictionaryTestSuite.test("BridgedFromObjC.Verbatim.ImmutableDictionaryIsRetained") {
  var nsd: NSDictionary = CustomImmutableNSDictionary(_privateInit: ())

  CustomImmutableNSDictionary.timesCopyWithZoneWasCalled = 0
  CustomImmutableNSDictionary.timesObjectForKeyWasCalled = 0
  CustomImmutableNSDictionary.timesKeyEnumeratorWasCalled = 0
  CustomImmutableNSDictionary.timesCountWasCalled = 0
  var d: [NSObject : AnyObject] = _convertNSDictionaryToDictionary(nsd)
  expectEqual(1, CustomImmutableNSDictionary.timesCopyWithZoneWasCalled)
  expectEqual(0, CustomImmutableNSDictionary.timesObjectForKeyWasCalled)
  expectEqual(0, CustomImmutableNSDictionary.timesKeyEnumeratorWasCalled)
  expectEqual(0, CustomImmutableNSDictionary.timesCountWasCalled)

  var bridgedBack: NSDictionary = _convertDictionaryToNSDictionary(d)
  expectEqual(
    unsafeBitCast(nsd, Int.self),
    unsafeBitCast(bridgedBack, Int.self))

  _fixLifetime(nsd)
  _fixLifetime(d)
  _fixLifetime(bridgedBack)
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.ImmutableDictionaryIsCopied") {
  var nsd: NSDictionary = CustomImmutableNSDictionary(_privateInit: ())

  CustomImmutableNSDictionary.timesCopyWithZoneWasCalled = 0
  CustomImmutableNSDictionary.timesObjectForKeyWasCalled = 0
  CustomImmutableNSDictionary.timesKeyEnumeratorWasCalled = 0
  CustomImmutableNSDictionary.timesCountWasCalled = 0
  TestBridgedValueTy.bridgeOperations = 0
  var d: [TestBridgedKeyTy : TestBridgedValueTy] =
    _convertNSDictionaryToDictionary(nsd)
  expectEqual(0, CustomImmutableNSDictionary.timesCopyWithZoneWasCalled)
  expectEqual(3, CustomImmutableNSDictionary.timesObjectForKeyWasCalled)
  expectEqual(1, CustomImmutableNSDictionary.timesKeyEnumeratorWasCalled)
  expectNotEqual(0, CustomImmutableNSDictionary.timesCountWasCalled)
  expectEqual(3, TestBridgedValueTy.bridgeOperations)

  var bridgedBack: NSDictionary = _convertDictionaryToNSDictionary(d)
  expectNotEqual(
    unsafeBitCast(nsd, Int.self),
    unsafeBitCast(bridgedBack, Int.self))

  _fixLifetime(nsd)
  _fixLifetime(d)
  _fixLifetime(bridgedBack)
}


DictionaryTestSuite.test("BridgedFromObjC.Verbatim.IndexForKey") {
  var d = getBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isCocoaDictionary(d))

  // Find an existing key.
  do {
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
  assert(identity1 == unsafeBitCast(d, Int.self))
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.IndexForKey") {
  var d = getBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isNativeDictionary(d))

  // Find an existing key.
  do {
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
  assert(identity1 == unsafeBitCast(d, Int.self))
}

DictionaryTestSuite.test("BridgedFromObjC.Verbatim.SubscriptWithIndex") {
  var d = getBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isCocoaDictionary(d))

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex != endIndex)
  assert(startIndex < endIndex)
  assert(startIndex <= endIndex)
  assert(!(startIndex >= endIndex))
  assert(!(startIndex > endIndex))
  assert(identity1 == unsafeBitCast(d, Int.self))

  var pairs = Array<(Int, Int)>()
  for var i = startIndex; i != endIndex; ++i {
    var (key, value) = d[i]
    let kv = ((key as! TestObjCKeyTy).value, (value as! TestObjCValueTy).value)
    pairs += [kv]
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  assert(identity1 == unsafeBitCast(d, Int.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.SubscriptWithIndex") {
  var d = getBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isNativeDictionary(d))

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex != endIndex)
  assert(startIndex < endIndex)
  assert(startIndex <= endIndex)
  assert(!(startIndex >= endIndex))
  assert(!(startIndex > endIndex))
  assert(identity1 == unsafeBitCast(d, Int.self))

  var pairs = Array<(Int, Int)>()
  for var i = startIndex; i != endIndex; ++i {
    var (key, value) = d[i]
    let kv = (key.value, value.value)
    pairs += [kv]
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  assert(identity1 == unsafeBitCast(d, Int.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

DictionaryTestSuite.test("BridgedFromObjC.Verbatim.SubscriptWithIndex_Empty") {
  var d = getBridgedVerbatimDictionary([:])
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isCocoaDictionary(d))

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex == endIndex)
  assert(!(startIndex < endIndex))
  assert(startIndex <= endIndex)
  assert(startIndex >= endIndex)
  assert(!(startIndex > endIndex))
  assert(identity1 == unsafeBitCast(d, Int.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.SubscriptWithIndex_Empty") {
  var d = getBridgedNonverbatimDictionary([:])
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isNativeDictionary(d))

  var startIndex = d.startIndex
  var endIndex = d.endIndex
  assert(startIndex == endIndex)
  assert(!(startIndex < endIndex))
  assert(startIndex <= endIndex)
  assert(startIndex >= endIndex)
  assert(!(startIndex > endIndex))
  assert(identity1 == unsafeBitCast(d, Int.self))

  // Keep indexes alive during the calls above.
  _fixLifetime(startIndex)
  _fixLifetime(endIndex)
}

DictionaryTestSuite.test("BridgedFromObjC.Verbatim.SubscriptWithKey") {
  var d = getBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isCocoaDictionary(d))

  // Read existing key-value pairs.
  var v = d[TestObjCKeyTy(10)] as! TestObjCValueTy
  assert(v.value == 1010)

  v = d[TestObjCKeyTy(20)] as! TestObjCValueTy
  assert(v.value == 1020)

  v = d[TestObjCKeyTy(30)] as! TestObjCValueTy
  assert(v.value == 1030)

  assert(identity1 == unsafeBitCast(d, Int.self))

  // Insert a new key-value pair.
  d[TestObjCKeyTy(40)] = TestObjCValueTy(2040)
  var identity2 = unsafeBitCast(d, Int.self)
  assert(identity1 != identity2)
  assert(isNativeDictionary(d))
  assert(d.count == 4)

  v = d[TestObjCKeyTy(10)] as! TestObjCValueTy
  assert(v.value == 1010)

  v = d[TestObjCKeyTy(20)] as! TestObjCValueTy
  assert(v.value == 1020)

  v = d[TestObjCKeyTy(30)] as! TestObjCValueTy
  assert(v.value == 1030)

  v = d[TestObjCKeyTy(40)] as! TestObjCValueTy
  assert(v.value == 2040)

  // Overwrite value in existing binding.
  d[TestObjCKeyTy(10)] = TestObjCValueTy(2010)
  assert(identity2 == unsafeBitCast(d, Int.self))
  assert(isNativeDictionary(d))
  assert(d.count == 4)

  v = d[TestObjCKeyTy(10)] as! TestObjCValueTy
  assert(v.value == 2010)

  v = d[TestObjCKeyTy(20)] as! TestObjCValueTy
  assert(v.value == 1020)

  v = d[TestObjCKeyTy(30)] as! TestObjCValueTy
  assert(v.value == 1030)

  v = d[TestObjCKeyTy(40)] as! TestObjCValueTy
  assert(v.value == 2040)
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.SubscriptWithKey") {
  var d = getBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isNativeDictionary(d))

  // Read existing key-value pairs.
  var v = d[TestBridgedKeyTy(10)]
  assert(v!.value == 1010)

  v = d[TestBridgedKeyTy(20)]
  assert(v!.value == 1020)

  v = d[TestBridgedKeyTy(30)]
  assert(v!.value == 1030)

  assert(identity1 == unsafeBitCast(d, Int.self))

  // Insert a new key-value pair.
  d[TestBridgedKeyTy(40)] = TestBridgedValueTy(2040)
  var identity2 = unsafeBitCast(d, Int.self)
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
  assert(identity2 == unsafeBitCast(d, Int.self))
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

DictionaryTestSuite.test("BridgedFromObjC.Verbatim.UpdateValueForKey") {
  // Insert a new key-value pair.
  do {
    var d = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d, Int.self)
    assert(isCocoaDictionary(d))

    var oldValue: AnyObject? =
        d.updateValue(TestObjCValueTy(2040), forKey: TestObjCKeyTy(40))
    assert(oldValue == nil)
    var identity2 = unsafeBitCast(d, Int.self)
    assert(identity1 != identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 4)

    assert(d[TestObjCKeyTy(10)]!.value == 1010)
    assert(d[TestObjCKeyTy(20)]!.value == 1020)
    assert(d[TestObjCKeyTy(30)]!.value == 1030)
    assert(d[TestObjCKeyTy(40)]!.value == 2040)
  }

  // Overwrite a value in existing binding.
  do {
    var d = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d, Int.self)
    assert(isCocoaDictionary(d))

    var oldValue: AnyObject? =
        d.updateValue(TestObjCValueTy(2010), forKey: TestObjCKeyTy(10))
    assert((oldValue as! TestObjCValueTy).value == 1010)

    var identity2 = unsafeBitCast(d, Int.self)
    assert(identity1 != identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 3)

    assert(d[TestObjCKeyTy(10)]!.value == 2010)
    assert(d[TestObjCKeyTy(20)]!.value == 1020)
    assert(d[TestObjCKeyTy(30)]!.value == 1030)
  }
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.UpdateValueForKey") {
  // Insert a new key-value pair.
  do {
    var d = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d, Int.self)
    assert(isNativeDictionary(d))

    var oldValue =
        d.updateValue(TestBridgedValueTy(2040), forKey: TestBridgedKeyTy(40))
    assert(oldValue == nil)
    var identity2 = unsafeBitCast(d, Int.self)
    assert(identity1 != identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 4)

    assert(d[TestBridgedKeyTy(10)]!.value == 1010)
    assert(d[TestBridgedKeyTy(20)]!.value == 1020)
    assert(d[TestBridgedKeyTy(30)]!.value == 1030)
    assert(d[TestBridgedKeyTy(40)]!.value == 2040)
  }

  // Overwrite a value in existing binding.
  do {
    var d = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d, Int.self)
    assert(isNativeDictionary(d))

    var oldValue =
        d.updateValue(TestBridgedValueTy(2010), forKey: TestBridgedKeyTy(10))!
    assert(oldValue.value == 1010)

    var identity2 = unsafeBitCast(d, Int.self)
    assert(identity1 == identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 3)

    assert(d[TestBridgedKeyTy(10)]!.value == 2010)
    assert(d[TestBridgedKeyTy(20)]!.value == 1020)
    assert(d[TestBridgedKeyTy(30)]!.value == 1030)
  }
}


DictionaryTestSuite.test("BridgedFromObjC.Verbatim.RemoveAtIndex") {
  var d = getBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isCocoaDictionary(d))

  let foundIndex1 = d.indexForKey(TestObjCKeyTy(10))!
  assert(d[foundIndex1].0 == TestObjCKeyTy(10))
  assert(d[foundIndex1].1.value == 1010)
  assert(identity1 == unsafeBitCast(d, Int.self))

  let removedElement = d.removeAtIndex(foundIndex1)
  assert(identity1 != unsafeBitCast(d, Int.self))
  assert(isNativeDictionary(d))
  assert(removedElement.0 == TestObjCKeyTy(10))
  assert(removedElement.1.value == 1010)
  assert(d.count == 2)
  assert(d.indexForKey(TestObjCKeyTy(10)) == nil)
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.RemoveAtIndex") {
  var d = getBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isNativeDictionary(d))

  let foundIndex1 = d.indexForKey(TestBridgedKeyTy(10))!
  assert(d[foundIndex1].0 == TestBridgedKeyTy(10))
  assert(d[foundIndex1].1.value == 1010)
  assert(identity1 == unsafeBitCast(d, Int.self))

  let removedElement = d.removeAtIndex(foundIndex1)
  assert(identity1 == unsafeBitCast(d, Int.self))
  assert(isNativeDictionary(d))
  assert(removedElement.0 == TestObjCKeyTy(10))
  assert(removedElement.1.value == 1010)
  assert(d.count == 2)
  assert(d.indexForKey(TestBridgedKeyTy(10)) == nil)
}


DictionaryTestSuite.test("BridgedFromObjC.Verbatim.RemoveValueForKey") {
  do {
    var d = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d, Int.self)
    assert(isCocoaDictionary(d))

    var deleted: AnyObject? = d.removeValueForKey(TestObjCKeyTy(0))
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d, Int.self))
    assert(isCocoaDictionary(d))

    deleted = d.removeValueForKey(TestObjCKeyTy(10))
    assert(deleted!.value == 1010)
    var identity2 = unsafeBitCast(d, Int.self)
    assert(identity1 != identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 2)

    assert(d[TestObjCKeyTy(10)] == nil)
    assert(d[TestObjCKeyTy(20)]!.value == 1020)
    assert(d[TestObjCKeyTy(30)]!.value == 1030)
    assert(identity2 == unsafeBitCast(d, Int.self))
  }

  do {
    var d1 = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    var d2 = d1
    assert(isCocoaDictionary(d1))
    assert(isCocoaDictionary(d2))

    var deleted: AnyObject? = d2.removeValueForKey(TestObjCKeyTy(0))
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 == unsafeBitCast(d2, Int.self))
    assert(isCocoaDictionary(d1))
    assert(isCocoaDictionary(d2))

    deleted = d2.removeValueForKey(TestObjCKeyTy(10))
    assert(deleted!.value == 1010)
    var identity2 = unsafeBitCast(d2, Int.self)
    assert(identity1 != identity2)
    assert(isCocoaDictionary(d1))
    assert(isNativeDictionary(d2))
    assert(d2.count == 2)

    assert(d1[TestObjCKeyTy(10)]!.value == 1010)
    assert(d1[TestObjCKeyTy(20)]!.value == 1020)
    assert(d1[TestObjCKeyTy(30)]!.value == 1030)
    assert(identity1 == unsafeBitCast(d1, Int.self))

    assert(d2[TestObjCKeyTy(10)] == nil)
    assert(d2[TestObjCKeyTy(20)]!.value == 1020)
    assert(d2[TestObjCKeyTy(30)]!.value == 1030)
    assert(identity2 == unsafeBitCast(d2, Int.self))
  }
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.RemoveValueForKey") {
  do {
    var d = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d, Int.self)
    assert(isNativeDictionary(d))

    var deleted = d.removeValueForKey(TestBridgedKeyTy(0))
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d, Int.self))
    assert(isNativeDictionary(d))

    deleted = d.removeValueForKey(TestBridgedKeyTy(10))
    assert(deleted!.value == 1010)
    var identity2 = unsafeBitCast(d, Int.self)
    assert(identity1 == identity2)
    assert(isNativeDictionary(d))
    assert(d.count == 2)

    assert(d[TestBridgedKeyTy(10)] == nil)
    assert(d[TestBridgedKeyTy(20)]!.value == 1020)
    assert(d[TestBridgedKeyTy(30)]!.value == 1030)
    assert(identity2 == unsafeBitCast(d, Int.self))
  }

  do {
    var d1 = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)

    var d2 = d1
    assert(isNativeDictionary(d1))
    assert(isNativeDictionary(d2))

    var deleted = d2.removeValueForKey(TestBridgedKeyTy(0))
    assert(deleted == nil)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity1 == unsafeBitCast(d2, Int.self))
    assert(isNativeDictionary(d1))
    assert(isNativeDictionary(d2))

    deleted = d2.removeValueForKey(TestBridgedKeyTy(10))
    assert(deleted!.value == 1010)
    var identity2 = unsafeBitCast(d2, Int.self)
    assert(identity1 != identity2)
    assert(isNativeDictionary(d1))
    assert(isNativeDictionary(d2))
    assert(d2.count == 2)

    assert(d1[TestBridgedKeyTy(10)]!.value == 1010)
    assert(d1[TestBridgedKeyTy(20)]!.value == 1020)
    assert(d1[TestBridgedKeyTy(30)]!.value == 1030)
    assert(identity1 == unsafeBitCast(d1, Int.self))

    assert(d2[TestBridgedKeyTy(10)] == nil)
    assert(d2[TestBridgedKeyTy(20)]!.value == 1020)
    assert(d2[TestBridgedKeyTy(30)]!.value == 1030)
    assert(identity2 == unsafeBitCast(d2, Int.self))
  }
}


DictionaryTestSuite.test("BridgedFromObjC.Verbatim.RemoveAll") {
  do {
    var d = getBridgedVerbatimDictionary([:])
    var identity1 = unsafeBitCast(d, Int.self)
    assert(isCocoaDictionary(d))
    assert(d.count == 0)

    d.removeAll()
    assert(identity1 == unsafeBitCast(d, Int.self))
    assert(d.count == 0)
  }

  do {
    var d = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d, Int.self)
    assert(isCocoaDictionary(d))
    let originalCapacity = d.count
    assert(d.count == 3)
    assert(d[TestObjCKeyTy(10)]!.value == 1010)

    d.removeAll()
    assert(identity1 != unsafeBitCast(d, Int.self))
    assert(d._variantStorage.native.capacity < originalCapacity)
    assert(d.count == 0)
    assert(d[TestObjCKeyTy(10)] == nil)
  }

  do {
    var d = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d, Int.self)
    assert(isCocoaDictionary(d))
    let originalCapacity = d.count
    assert(d.count == 3)
    assert(d[TestObjCKeyTy(10)]!.value == 1010)

    d.removeAll(keepCapacity: true)
    assert(identity1 != unsafeBitCast(d, Int.self))
    assert(d._variantStorage.native.capacity >= originalCapacity)
    assert(d.count == 0)
    assert(d[TestObjCKeyTy(10)] == nil)
  }

  do {
    var d1 = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)
    assert(isCocoaDictionary(d1))
    let originalCapacity = d1.count
    assert(d1.count == 3)
    assert(d1[TestObjCKeyTy(10)]!.value == 1010)

    var d2 = d1
    d2.removeAll()
    var identity2 = unsafeBitCast(d2, Int.self)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[TestObjCKeyTy(10)]!.value == 1010)
    assert(d2._variantStorage.native.capacity < originalCapacity)
    assert(d2.count == 0)
    assert(d2[TestObjCKeyTy(10)] == nil)
  }

  do {
    var d1 = getBridgedVerbatimDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)
    assert(isCocoaDictionary(d1))
    let originalCapacity = d1.count
    assert(d1.count == 3)
    assert(d1[TestObjCKeyTy(10)]!.value == 1010)

    var d2 = d1
    d2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(d2, Int.self)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[TestObjCKeyTy(10)]!.value == 1010)
    assert(d2._variantStorage.native.capacity >= originalCapacity)
    assert(d2.count == 0)
    assert(d2[TestObjCKeyTy(10)] == nil)
  }
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.RemoveAll") {
  do {
    var d = getBridgedNonverbatimDictionary([:])
    var identity1 = unsafeBitCast(d, Int.self)
    assert(isNativeDictionary(d))
    assert(d.count == 0)

    d.removeAll()
    assert(identity1 == unsafeBitCast(d, Int.self))
    assert(d.count == 0)
  }

  do {
    var d = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d, Int.self)
    assert(isNativeDictionary(d))
    let originalCapacity = d.count
    assert(d.count == 3)
    assert(d[TestBridgedKeyTy(10)]!.value == 1010)

    d.removeAll()
    assert(identity1 != unsafeBitCast(d, Int.self))
    assert(d._variantStorage.native.capacity < originalCapacity)
    assert(d.count == 0)
    assert(d[TestBridgedKeyTy(10)] == nil)
  }

  do {
    var d = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d, Int.self)
    assert(isNativeDictionary(d))
    let originalCapacity = d.count
    assert(d.count == 3)
    assert(d[TestBridgedKeyTy(10)]!.value == 1010)

    d.removeAll(keepCapacity: true)
    assert(identity1 == unsafeBitCast(d, Int.self))
    assert(d._variantStorage.native.capacity >= originalCapacity)
    assert(d.count == 0)
    assert(d[TestBridgedKeyTy(10)] == nil)
  }

  do {
    var d1 = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)
    assert(isNativeDictionary(d1))
    let originalCapacity = d1.count
    assert(d1.count == 3)
    assert(d1[TestBridgedKeyTy(10)]!.value == 1010)

    var d2 = d1
    d2.removeAll()
    var identity2 = unsafeBitCast(d2, Int.self)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[TestBridgedKeyTy(10)]!.value == 1010)
    assert(d2._variantStorage.native.capacity < originalCapacity)
    assert(d2.count == 0)
    assert(d2[TestBridgedKeyTy(10)] == nil)
  }

  do {
    var d1 = getBridgedNonverbatimDictionary()
    var identity1 = unsafeBitCast(d1, Int.self)
    assert(isNativeDictionary(d1))
    let originalCapacity = d1.count
    assert(d1.count == 3)
    assert(d1[TestBridgedKeyTy(10)]!.value == 1010)

    var d2 = d1
    d2.removeAll(keepCapacity: true)
    var identity2 = unsafeBitCast(d2, Int.self)
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity2 != identity1)
    assert(d1.count == 3)
    assert(d1[TestBridgedKeyTy(10)]!.value == 1010)
    assert(d2._variantStorage.native.capacity >= originalCapacity)
    assert(d2.count == 0)
    assert(d2[TestBridgedKeyTy(10)] == nil)
  }
}


DictionaryTestSuite.test("BridgedFromObjC.Verbatim.Count") {
  var d = getBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isCocoaDictionary(d))

  assert(d.count == 3)
  assert(identity1 == unsafeBitCast(d, Int.self))
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.Count") {
  var d = getBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isNativeDictionary(d))

  assert(d.count == 3)
  assert(identity1 == unsafeBitCast(d, Int.self))
}


DictionaryTestSuite.test("BridgedFromObjC.Verbatim.Generate") {
  var d = getBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isCocoaDictionary(d))

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value) = gen.next() {
    let kv = ((key as! TestObjCKeyTy).value, (value as! TestObjCValueTy).value)
    pairs.append(kv)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(identity1 == unsafeBitCast(d, Int.self))
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.Generate") {
  var d = getBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
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
  assert(identity1 == unsafeBitCast(d, Int.self))
}

DictionaryTestSuite.test("BridgedFromObjC.Verbatim.Generate_Empty") {
  var d = getBridgedVerbatimDictionary([:])
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isCocoaDictionary(d))

  var gen = d.generate()
  // Cannot write code below because of
  // <rdar://problem/16811736> Optional tuples are broken as optionals regarding == comparison
  // assert(gen.next() == .None)
  assert(gen.next() == nil)
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(identity1 == unsafeBitCast(d, Int.self))
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.Generate_Empty") {
  var d = getBridgedNonverbatimDictionary([:])
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isNativeDictionary(d))

  var gen = d.generate()
  // Cannot write code below because of
  // <rdar://problem/16811736> Optional tuples are broken as optionals regarding == comparison
  // assert(gen.next() == .None)
  assert(gen.next() == nil)
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(identity1 == unsafeBitCast(d, Int.self))
}


DictionaryTestSuite.test("BridgedFromObjC.Verbatim.Generate_Huge") {
  var d = getHugeBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isCocoaDictionary(d))

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value) = gen.next() {
    let kv = ((key as! TestObjCKeyTy).value, (value as! TestObjCValueTy).value)
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
  assert(identity1 == unsafeBitCast(d, Int.self))
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.Generate_Huge") {
  var d = getHugeBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
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
  assert(identity1 == unsafeBitCast(d, Int.self))
}


DictionaryTestSuite.test("BridgedFromObjC.Verbatim.Generate_ParallelArray") {
autoreleasepoolIfUnoptimizedReturnAutoreleased {
  // Add an autorelease pool because ParallelArrayDictionary autoreleases
  // values in objectForKey.

  var d = getParallelArrayBridgedVerbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
  assert(isCocoaDictionary(d))

  var gen = d.generate()
  var pairs = Array<(Int, Int)>()
  while let (key, value) = gen.next() {
    let kv = ((key as! TestObjCKeyTy).value, (value as! TestObjCValueTy).value)
    pairs.append(kv)
  }
  var expectedPairs = [ (10, 1111), (20, 1111), (30, 1111), (40, 1111) ]
  assert(equalsUnordered(pairs, expectedPairs))
  // The following is not required by the GeneratorType protocol, but
  // it is a nice QoI.
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(gen.next() == nil)
  assert(identity1 == unsafeBitCast(d, Int.self))
}
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.Generate_ParallelArray") {
autoreleasepoolIfUnoptimizedReturnAutoreleased {
  // Add an autorelease pool because ParallelArrayDictionary autoreleases
  // values in objectForKey.

  var d = getParallelArrayBridgedNonverbatimDictionary()
  var identity1 = unsafeBitCast(d, Int.self)
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
  assert(identity1 == unsafeBitCast(d, Int.self))
}
}


DictionaryTestSuite.test("BridgedFromObjC.Verbatim.EqualityTest_Empty") {
  var d1 = getBridgedVerbatimEquatableDictionary([:])
  var identity1 = unsafeBitCast(d1, Int.self)
  assert(isCocoaDictionary(d1))

  var d2 = getBridgedVerbatimEquatableDictionary([:])
  var identity2 = unsafeBitCast(d2, Int.self)
  assert(isCocoaDictionary(d2))

  // We can't check that `identity1 != identity2` because Foundation might be
  // returning the same singleton NSDictionary for empty dictionaries.

  assert(d1 == d2)
  assert(identity1 == unsafeBitCast(d1, Int.self))
  assert(identity2 == unsafeBitCast(d2, Int.self))

  d2[TestObjCKeyTy(10)] = TestObjCEquatableValueTy(2010)
  assert(isNativeDictionary(d2))
  assert(identity2 != unsafeBitCast(d2, Int.self))
  identity2 = unsafeBitCast(d2, Int.self)

  assert(d1 != d2)
  assert(identity1 == unsafeBitCast(d1, Int.self))
  assert(identity2 == unsafeBitCast(d2, Int.self))
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.EqualityTest_Empty") {
  var d1 = getBridgedNonverbatimEquatableDictionary([:])
  var identity1 = unsafeBitCast(d1, Int.self)
  assert(isNativeDictionary(d1))

  var d2 = getBridgedNonverbatimEquatableDictionary([:])
  var identity2 = unsafeBitCast(d2, Int.self)
  assert(isNativeDictionary(d2))
  assert(identity1 != identity2)

  assert(d1 == d2)
  assert(identity1 == unsafeBitCast(d1, Int.self))
  assert(identity2 == unsafeBitCast(d2, Int.self))

  d2[TestBridgedKeyTy(10)] = TestBridgedEquatableValueTy(2010)
  assert(isNativeDictionary(d2))
  assert(identity2 == unsafeBitCast(d2, Int.self))

  assert(d1 != d2)
  assert(identity1 == unsafeBitCast(d1, Int.self))
  assert(identity2 == unsafeBitCast(d2, Int.self))
}


DictionaryTestSuite.test("BridgedFromObjC.Verbatim.EqualityTest_Small") {
  func helper(nd1: Dictionary<Int, Int>, _ nd2: Dictionary<Int, Int>, _ expectedEq: Bool) {
    let d1 = getBridgedVerbatimEquatableDictionary(nd1)
    let identity1 = unsafeBitCast(d1, Int.self)
    assert(isCocoaDictionary(d1))

    var d2 = getBridgedVerbatimEquatableDictionary(nd2)
    var identity2 = unsafeBitCast(d2, Int.self)
    assert(isCocoaDictionary(d2))

    do {
      let eq1 = (d1 == d2)
      assert(eq1 == expectedEq)

      let eq2 = (d2 == d1)
      assert(eq2 == expectedEq)

      let neq1 = (d1 != d2)
      assert(neq1 != expectedEq)

      let neq2 = (d2 != d1)
      assert(neq2 != expectedEq)
    }
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity2 == unsafeBitCast(d2, Int.self))

    d2[TestObjCKeyTy(1111)] = TestObjCEquatableValueTy(1111)
    d2[TestObjCKeyTy(1111)] = nil
    assert(isNativeDictionary(d2))
    assert(identity2 != unsafeBitCast(d2, Int.self))
    identity2 = unsafeBitCast(d2, Int.self)

    do {
      let eq1 = (d1 == d2)
      assert(eq1 == expectedEq)

      let eq2 = (d2 == d1)
      assert(eq2 == expectedEq)

      let neq1 = (d1 != d2)
      assert(neq1 != expectedEq)

      let neq2 = (d2 != d1)
      assert(neq2 != expectedEq)
    }
    assert(identity1 == unsafeBitCast(d1, Int.self))
    assert(identity2 == unsafeBitCast(d2, Int.self))
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


DictionaryTestSuite.test("BridgedFromObjC.Verbatim.ArrayOfDictionaries") {
  var nsa = NSMutableArray()
  for i in 0..<3 {
    nsa.addObject(
        getAsNSDictionary([ 10: 1010 + i, 20: 1020 + i, 30: 1030 + i ]))
  }

  var a = nsa as [AnyObject] as! [Dictionary<NSObject, AnyObject>]
  for i in 0..<3 {
    var d = a[i]
    var gen = d.generate()
    var pairs = Array<(Int, Int)>()
    while let (key, value) = gen.next() {
      let kv = ((key as! TestObjCKeyTy).value, (value as! TestObjCValueTy).value)
      pairs.append(kv)
    }
    var expectedPairs = [ (10, 1010 + i), (20, 1020 + i), (30, 1030 + i) ]
    assert(equalsUnordered(pairs, expectedPairs))
  }
}

DictionaryTestSuite.test("BridgedFromObjC.Nonverbatim.ArrayOfDictionaries") {
  var nsa = NSMutableArray()
  for i in 0..<3 {
    nsa.addObject(
        getAsNSDictionary([ 10: 1010 + i, 20: 1020 + i, 30: 1030 + i ]))
  }

  var a = nsa as [AnyObject] as! [Dictionary<TestBridgedKeyTy, TestBridgedValueTy>]
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

DictionaryTestSuite.test("BridgedToObjC.Verbatim.Count") {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  assert(d.count == 3)
}

DictionaryTestSuite.test("BridgedToObjC.Verbatim.ObjectForKey") {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  var v: AnyObject? = d.objectForKey(TestObjCKeyTy(10))
  expectEqual(1010, (v as! TestObjCValueTy).value)
  let idValue10 = unsafeBitCast(v, UInt.self)

  v = d.objectForKey(TestObjCKeyTy(20))
  expectEqual(1020, (v as! TestObjCValueTy).value)
  let idValue20 = unsafeBitCast(v, UInt.self)

  v = d.objectForKey(TestObjCKeyTy(30))
  expectEqual(1030, (v as! TestObjCValueTy).value)
  let idValue30 = unsafeBitCast(v, UInt.self)

  expectEmpty(d.objectForKey(TestObjCKeyTy(40)))

  for i in 0..<3 {
    expectEqual(idValue10, unsafeBitCast(
      d.objectForKey(TestObjCKeyTy(10)), UInt.self))

    expectEqual(idValue20, unsafeBitCast(
      d.objectForKey(TestObjCKeyTy(20)), UInt.self))

    expectEqual(idValue30, unsafeBitCast(
      d.objectForKey(TestObjCKeyTy(30)), UInt.self))
  }

  expectAutoreleasedKeysAndValues(unopt: (0, 3))
}

DictionaryTestSuite.test("BridgedToObjC.Verbatim.KeyEnumerator.NextObject") {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  var capturedIdentityPairs = Array<(UInt, UInt)>()

  for i in 0..<3 {
    let enumerator = d.keyEnumerator()

    var dataPairs = Array<(Int, Int)>()
    var identityPairs = Array<(UInt, UInt)>()
    while let key = enumerator.nextObject() {
      let value: AnyObject = d.objectForKey(key)!

      let dataPair =
        ((key as! TestObjCKeyTy).value, (value as! TestObjCValueTy).value)
      dataPairs.append(dataPair)

      let identityPair =
        (unsafeBitCast(key, UInt.self), unsafeBitCast(value, UInt.self))
      identityPairs.append(identityPair)
    }
    expectTrue(
      equalsUnordered(dataPairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

    if capturedIdentityPairs.isEmpty {
      capturedIdentityPairs = identityPairs
    } else {
      expectTrue(equalsUnordered(capturedIdentityPairs, identityPairs))
    }

    assert(enumerator.nextObject() == nil)
    assert(enumerator.nextObject() == nil)
    assert(enumerator.nextObject() == nil)
  }

  expectAutoreleasedKeysAndValues(unopt: (3, 3))
}

DictionaryTestSuite.test("BridgedToObjC.Verbatim.KeyEnumerator.NextObject_Empty") {
  let d = getBridgedEmptyNSDictionary()
  let enumerator = d.keyEnumerator()

  assert(enumerator.nextObject() == nil)
  assert(enumerator.nextObject() == nil)
  assert(enumerator.nextObject() == nil)
}

DictionaryTestSuite.test("BridgedToObjC.Verbatim.KeyEnumerator.FastEnumeration.UseFromSwift") {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  checkDictionaryFastEnumerationFromSwift(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    d, { d.keyEnumerator() },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })

  expectAutoreleasedKeysAndValues(unopt: (3, 3))
}

DictionaryTestSuite.test("BridgedToObjC.Verbatim.KeyEnumerator.FastEnumeration.UseFromObjC") {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  checkDictionaryFastEnumerationFromObjC(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    d, { d.keyEnumerator() },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })

  expectAutoreleasedKeysAndValues(unopt: (3, 3))
}

DictionaryTestSuite.test("BridgedToObjC.Verbatim.KeyEnumerator.FastEnumeration_Empty") {
  let d = getBridgedEmptyNSDictionary()

  checkDictionaryFastEnumerationFromSwift(
    [], d, { d.keyEnumerator() },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })

  checkDictionaryFastEnumerationFromObjC(
    [], d, { d.keyEnumerator() },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })
}

DictionaryTestSuite.test("BridgedToObjC.Verbatim.FastEnumeration.UseFromSwift") {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  checkDictionaryFastEnumerationFromSwift(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    d, { d },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })

  expectAutoreleasedKeysAndValues(unopt: (0, 3))
}

DictionaryTestSuite.test("BridgedToObjC.Verbatim.FastEnumeration.UseFromObjC") {
  let d = getBridgedNSDictionaryOfRefTypesBridgedVerbatim()

  checkDictionaryFastEnumerationFromObjC(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    d, { d },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })

  expectAutoreleasedKeysAndValues(unopt: (0, 3))
}

DictionaryTestSuite.test("BridgedToObjC.Verbatim.FastEnumeration_Empty") {
  let d = getBridgedEmptyNSDictionary()

  checkDictionaryFastEnumerationFromSwift(
    [], d, { d },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })

  checkDictionaryFastEnumerationFromObjC(
    [], d, { d },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })
}

//===---
// Dictionary -> NSDictionary bridging tests.
//
// Key type and value type are bridged non-verbatim.
//===---

DictionaryTestSuite.test("BridgedToObjC.KeyValue_ValueTypesCustomBridged") {
  let d = getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)!
    let kv = ((key as! TestObjCKeyTy).value, (value as! TestObjCValueTy).value)
    pairs.append(kv)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  expectAutoreleasedKeysAndValues(unopt: (3, 3))
}

DictionaryTestSuite.test("BridgedToObjC.Custom.KeyEnumerator.FastEnumeration.UseFromSwift") {
  let d = getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged()

  checkDictionaryFastEnumerationFromSwift(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    d, { d.keyEnumerator() },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })

  expectAutoreleasedKeysAndValues(unopt: (3, 3))
}

DictionaryTestSuite.test("BridgedToObjC.Custom.KeyEnumerator.FastEnumeration.UseFromSwift.Partial") {
  let d = getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged(
    numElements: 9)

  checkDictionaryEnumeratorPartialFastEnumerationFromSwift(
    [ (10, 1010), (20, 1020), (30, 1030), (40, 1040), (50, 1050),
      (60, 1060), (70, 1070), (80, 1080), (90, 1090) ],
    d, maxFastEnumerationItems: 5,
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })

  expectAutoreleasedKeysAndValues(unopt: (9, 9))
}

DictionaryTestSuite.test("BridgedToObjC.Custom.KeyEnumerator.FastEnumeration.UseFromObjC") {
  let d = getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged()

  checkDictionaryFastEnumerationFromObjC(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    d, { d.keyEnumerator() },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })

  expectAutoreleasedKeysAndValues(unopt: (3, 3))
}

DictionaryTestSuite.test("BridgedToObjC.Custom.FastEnumeration.UseFromSwift") {
  let d = getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged()

  checkDictionaryFastEnumerationFromSwift(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    d, { d },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })

  expectAutoreleasedKeysAndValues(unopt: (0, 3))
}

DictionaryTestSuite.test("BridgedToObjC.Custom.FastEnumeration.UseFromObjC") {
  let d = getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged()

  checkDictionaryFastEnumerationFromObjC(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    d, { d },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })

  expectAutoreleasedKeysAndValues(unopt: (0, 3))
}

DictionaryTestSuite.test("BridgedToObjC.Custom.FastEnumeration_Empty") {
  let d = getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged(
    numElements: 0)

  checkDictionaryFastEnumerationFromSwift(
    [], d, { d },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })

  checkDictionaryFastEnumerationFromObjC(
    [], d, { d },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })
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

DictionaryTestSuite.test("BridgedToObjC.Key_ValueTypeCustomBridged") {
  let d = getBridgedNSDictionaryOfKey_ValueTypeCustomBridged()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)!
    let kv = ((key as! TestObjCKeyTy).value, (value as! TestObjCValueTy).value)
    pairs.append(kv)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  expectAutoreleasedKeysAndValues(unopt: (3, 3))
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

DictionaryTestSuite.test("BridgedToObjC.Value_ValueTypeCustomBridged") {
  let d = getBridgedNSDictionaryOfValue_ValueTypeCustomBridged()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)!
    let kv = ((key as! TestObjCKeyTy).value, (value as! TestObjCValueTy).value)
    pairs.append(kv)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))

  expectAutoreleasedKeysAndValues(unopt: (3, 3))
}


//===---
// NSDictionary -> Dictionary -> NSDictionary bridging tests.
//===---

func getRoundtripBridgedNSDictionary() -> NSDictionary {
  let keys = [ 10, 20, 30 ].map { TestObjCKeyTy($0) }
  let values = [ 1010, 1020, 1030 ].map { TestObjCValueTy($0) }

  let nsd = NSDictionary(objects: values, forKeys: keys)

  let d: Dictionary<NSObject, AnyObject> = _convertNSDictionaryToDictionary(nsd)

  let bridgedBack = _convertDictionaryToNSDictionary(d)
  assert(isCocoaNSDictionary(bridgedBack))
  // FIXME: this should be true.
  //assert(unsafeBitCast(nsd, Int.self) == unsafeBitCast(bridgedBack, Int.self))

  return bridgedBack
}

DictionaryTestSuite.test("BridgingRoundtrip") {
  let d = getRoundtripBridgedNSDictionary()
  let enumerator = d.keyEnumerator()

  var pairs = Array<(Int, Int)>()
  while let key = enumerator.nextObject() {
    let value: AnyObject = d.objectForKey(key)!
    let kv = ((key as! TestObjCKeyTy).value, (value as! TestObjCValueTy).value)
    pairs.append(kv)
  }
  expectEqualsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ])
}

//===---
// NSDictionary -> Dictionary implicit conversion.
//===---

DictionaryTestSuite.test("NSDictionaryToDictionaryConversion") {
  let keys = [ 10, 20, 30 ].map { TestObjCKeyTy($0) }
  let values = [ 1010, 1020, 1030 ].map { TestObjCValueTy($0) }

  let nsd = NSDictionary(objects: values, forKeys: keys)

  let d: Dictionary = nsd as Dictionary

  var pairs = Array<(Int, Int)>()
  for (key, value) in d {
    let kv = ((key as! TestObjCKeyTy).value, (value as! TestObjCValueTy).value)
    pairs.append(kv)
  }
  assert(equalsUnordered(pairs, [ (10, 1010), (20, 1020), (30, 1030) ]))
}

DictionaryTestSuite.test("DictionaryToNSDictionaryConversion") {
  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)
  let nsd: NSDictionary = d

  checkDictionaryFastEnumerationFromSwift(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    d, { d },
    { ($0 as! TestObjCKeyTy).value },
    { ($0 as! TestObjCValueTy).value })

  expectAutoreleasedKeysAndValues(unopt: (0, 3))
}

//===---
// Dictionary upcasts
//===---

DictionaryTestSuite.test("DictionaryUpcastEntryPoint") {
  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  var dAsAnyObject: Dictionary<NSObject, AnyObject> = _dictionaryUpCast(d)

  assert(dAsAnyObject.count == 3)
  var v: AnyObject? = dAsAnyObject[TestObjCKeyTy(10)]
  assert((v! as! TestObjCValueTy).value == 1010)

  v = dAsAnyObject[TestObjCKeyTy(20)]
  assert((v! as! TestObjCValueTy).value == 1020)

  v = dAsAnyObject[TestObjCKeyTy(30)]
  assert((v! as! TestObjCValueTy).value == 1030)
}

DictionaryTestSuite.test("DictionaryUpcast") {
  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  var dAsAnyObject: Dictionary<NSObject, AnyObject> = d

  assert(dAsAnyObject.count == 3)
  var v: AnyObject? = dAsAnyObject[TestObjCKeyTy(10)]
  assert((v! as! TestObjCValueTy).value == 1010)

  v = dAsAnyObject[TestObjCKeyTy(20)]
  assert((v! as! TestObjCValueTy).value == 1020)

  v = dAsAnyObject[TestObjCKeyTy(30)]
  assert((v! as! TestObjCValueTy).value == 1030)
}

DictionaryTestSuite.test("DictionaryUpcastBridgedEntryPoint") {
  var d = Dictionary<TestBridgedKeyTy, TestBridgedValueTy>(minimumCapacity: 32)
  d[TestBridgedKeyTy(10)] = TestBridgedValueTy(1010)
  d[TestBridgedKeyTy(20)] = TestBridgedValueTy(1020)
  d[TestBridgedKeyTy(30)] = TestBridgedValueTy(1030)

  do {
    var dOO: Dictionary<NSObject, AnyObject> = _dictionaryBridgeToObjectiveC(d)

    assert(dOO.count == 3)
    var v: AnyObject? = dOO[TestObjCKeyTy(10)]
    assert((v! as! TestBridgedValueTy).value == 1010)

    v = dOO[TestObjCKeyTy(20)]
    assert((v! as! TestBridgedValueTy).value == 1020)

    v = dOO[TestObjCKeyTy(30)]
    assert((v! as! TestBridgedValueTy).value == 1030)
  }

  do {
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

  do {
    var dVO: Dictionary<TestBridgedKeyTy, AnyObject>
      = _dictionaryBridgeToObjectiveC(d)

    assert(dVO.count == 3)
    var v: AnyObject? = dVO[TestBridgedKeyTy(10)]
    assert((v! as! TestBridgedValueTy).value == 1010)

    v = dVO[TestBridgedKeyTy(20)]
    assert((v! as! TestBridgedValueTy).value == 1020)

    v = dVO[TestBridgedKeyTy(30)]
    assert((v! as! TestBridgedValueTy).value == 1030)
  }
}

DictionaryTestSuite.test("DictionaryUpcastBridged") {
  var d = Dictionary<TestBridgedKeyTy, TestBridgedValueTy>(minimumCapacity: 32)
  d[TestBridgedKeyTy(10)] = TestBridgedValueTy(1010)
  d[TestBridgedKeyTy(20)] = TestBridgedValueTy(1020)
  d[TestBridgedKeyTy(30)] = TestBridgedValueTy(1030)

  do {
    var dOO: Dictionary<NSObject, AnyObject> = d

    assert(dOO.count == 3)
    var v: AnyObject? = dOO[TestObjCKeyTy(10)]
    assert((v! as! TestBridgedValueTy).value == 1010)

    v = dOO[TestObjCKeyTy(20)]
    assert((v! as! TestBridgedValueTy).value == 1020)

    v = dOO[TestObjCKeyTy(30)]
    assert((v! as! TestBridgedValueTy).value == 1030)
  }

  do {
    var dOV: Dictionary<NSObject, TestBridgedValueTy> = d

    assert(dOV.count == 3)
    var v = dOV[TestObjCKeyTy(10)]
    assert(v!.value == 1010)

    v = dOV[TestObjCKeyTy(20)]
    assert(v!.value == 1020)

    v = dOV[TestObjCKeyTy(30)]
    assert(v!.value == 1030)
  }

  do {
    var dVO: Dictionary<TestBridgedKeyTy, AnyObject> = d

    assert(dVO.count == 3)
    var v: AnyObject? = dVO[TestBridgedKeyTy(10)]
    assert((v! as! TestBridgedValueTy).value == 1010)

    v = dVO[TestBridgedKeyTy(20)]
    assert((v! as! TestBridgedValueTy).value == 1020)

    v = dVO[TestBridgedKeyTy(30)]
    assert((v! as! TestBridgedValueTy).value == 1030)
  }
}

//===---
// Dictionary downcasts
//===---

DictionaryTestSuite.test("DictionaryDowncastEntryPoint") {
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

  expectAutoreleasedKeysAndValues(unopt: (0, 3))
}

DictionaryTestSuite.test("DictionaryDowncast") {
  var d = Dictionary<NSObject, AnyObject>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  // Successful downcast.
  let dCC = d as! Dictionary<TestObjCKeyTy, TestObjCValueTy>
  assert(dCC.count == 3)
  var v = dCC[TestObjCKeyTy(10)]
  assert(v!.value == 1010)

  v = dCC[TestObjCKeyTy(20)]
  assert(v!.value == 1020)

  v = dCC[TestObjCKeyTy(30)]
  assert(v!.value == 1030)

  expectAutoreleasedKeysAndValues(unopt: (0, 3))
}

DictionaryTestSuite.test("DictionaryDowncastConditionalEntryPoint") {
  var d = Dictionary<NSObject, AnyObject>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  // Successful downcast.
  if let dCC
       = _dictionaryDownCastConditional(d) as Dictionary<TestObjCKeyTy, TestObjCValueTy>? {
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
  if let dCC
       = _dictionaryDownCastConditional(d) as Dictionary<TestObjCKeyTy, TestObjCValueTy>? {
    assert(false)
  }
}

DictionaryTestSuite.test("DictionaryDowncastConditional") {
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

DictionaryTestSuite.test("DictionaryBridgeFromObjectiveCEntryPoint") {
  var d = Dictionary<NSObject, AnyObject>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  // Successful downcast.
  let dCV: Dictionary<TestObjCKeyTy, TestBridgedValueTy>
    = _dictionaryBridgeFromObjectiveC(d)
  do {
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
  do {
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
  do {
    assert(dVV.count == 3)
    var v = dVV[TestBridgedKeyTy(10)]
    assert(v!.value == 1010)

    v = dVV[TestBridgedKeyTy(20)]
    assert(v!.value == 1020)

    v = dVV[TestBridgedKeyTy(30)]
    assert(v!.value == 1030)
  }
}

DictionaryTestSuite.test("DictionaryBridgeFromObjectiveC") {
  var d = Dictionary<NSObject, AnyObject>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  // Successful downcast.
  let dCV = d as! Dictionary<TestObjCKeyTy, TestBridgedValueTy>
  do {
    assert(dCV.count == 3)
    var v = dCV[TestObjCKeyTy(10)]
    assert(v!.value == 1010)

    v = dCV[TestObjCKeyTy(20)]
    assert(v!.value == 1020)

    v = dCV[TestObjCKeyTy(30)]
    assert(v!.value == 1030)
  }

  // Successful downcast.
  let dVC = d as! Dictionary<TestBridgedKeyTy, TestObjCValueTy>
  do {
    assert(dVC.count == 3)
    var v = dVC[TestBridgedKeyTy(10)]
    assert(v!.value == 1010)

    v = dVC[TestBridgedKeyTy(20)]
    assert(v!.value == 1020)

    v = dVC[TestBridgedKeyTy(30)]
    assert(v!.value == 1030)
  }

  // Successful downcast.
  let dVV = d as! Dictionary<TestBridgedKeyTy, TestBridgedValueTy>
  do {
    assert(dVV.count == 3)
    var v = dVV[TestBridgedKeyTy(10)]
    assert(v!.value == 1010)

    v = dVV[TestBridgedKeyTy(20)]
    assert(v!.value == 1020)

    v = dVV[TestBridgedKeyTy(30)]
    assert(v!.value == 1030)
  }
}

DictionaryTestSuite.test("DictionaryBridgeFromObjectiveCConditionalEntryPoint") {
  var d = Dictionary<NSObject, AnyObject>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  // Successful downcast.
  if let dCV
       = _dictionaryBridgeFromObjectiveCConditional(d) as
         Dictionary<TestObjCKeyTy, TestBridgedValueTy>? {
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
  if let dVC
       = _dictionaryBridgeFromObjectiveCConditional(d) as Dictionary<TestBridgedKeyTy, TestObjCValueTy>? {
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
  if let dVV
       = _dictionaryBridgeFromObjectiveCConditional(d) as Dictionary<TestBridgedKeyTy, TestBridgedValueTy>? {
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
  if let dCV
       = _dictionaryBridgeFromObjectiveCConditional(d) as Dictionary<TestObjCKeyTy, TestBridgedValueTy>?{
    assert(false)
  }
  if let dVC
       = _dictionaryBridgeFromObjectiveCConditional(d) as Dictionary<TestBridgedKeyTy, TestObjCValueTy>?{
    assert(false)
  }
  if let dVV
       = _dictionaryBridgeFromObjectiveCConditional(d) as Dictionary<TestBridgedKeyTy, TestBridgedValueTy>?{
    assert(false)
  }
}

DictionaryTestSuite.test("DictionaryBridgeFromObjectiveCConditional") {
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

var DictionaryDerivedAPIs = TestSuite("DictionaryDerivedAPIs")

@objc
class MockDictionaryWithCustomCount : NSDictionary {
  init(count: Int) {
    self._count = count
    super.init()
  }

  override init() {
    expectUnreachable()
    super.init()
  }

  override init(
    objects: UnsafePointer<AnyObject?>,
    forKeys keys: UnsafePointer<NSCopying?>,
    count: Int) {
    expectUnreachable()
    super.init(objects: objects, forKeys: keys, count: count)
  }

  required init(coder aDecoder: NSCoder) {
    fatalError("init(coder:) not implemented by MockDictionaryWithCustomCount")
  }

  @objc override func copyWithZone(zone: NSZone) -> AnyObject {
    // Ensure that copying this dictionary produces an object of the same
    // dynamic type.
    return self
  }

  override func objectForKey(aKey: AnyObject) -> AnyObject? {
    expectUnreachable()
    return NSObject()
  }

  override var count: Int {
    ++MockDictionaryWithCustomCount.timesCountWasCalled
    return _count
  }

  var _count: Int = 0

  static var timesCountWasCalled = 0
}

func getMockDictionaryWithCustomCount(count count: Int)
  -> Dictionary<NSObject, AnyObject> {

  return MockDictionaryWithCustomCount(count: count) as Dictionary
}

func callGenericIsEmpty<C : CollectionType>(collection: C) -> Bool {
  return collection.isEmpty
}

DictionaryDerivedAPIs.test("isEmpty/ImplementationIsCustomized") {
  do {
    var d = getMockDictionaryWithCustomCount(count: 0)
    MockDictionaryWithCustomCount.timesCountWasCalled = 0
    expectTrue(d.isEmpty)
    expectEqual(1, MockDictionaryWithCustomCount.timesCountWasCalled)
  }
  do {
    var d = getMockDictionaryWithCustomCount(count: 0)
    MockDictionaryWithCustomCount.timesCountWasCalled = 0
    expectTrue(callGenericIsEmpty(d))
    expectEqual(1, MockDictionaryWithCustomCount.timesCountWasCalled)
  }

  do {
    var d = getMockDictionaryWithCustomCount(count: 4)
    MockDictionaryWithCustomCount.timesCountWasCalled = 0
    expectFalse(d.isEmpty)
    expectEqual(1, MockDictionaryWithCustomCount.timesCountWasCalled)
  }
  do {
    var d = getMockDictionaryWithCustomCount(count: 4)
    MockDictionaryWithCustomCount.timesCountWasCalled = 0
    expectFalse(callGenericIsEmpty(d))
    expectEqual(1, MockDictionaryWithCustomCount.timesCountWasCalled)
  }
}

var ObjCThunks = TestSuite("ObjCThunks")

class ObjCThunksHelper : NSObject {
  dynamic func acceptArrayBridgedVerbatim(array: [TestObjCValueTy]) {
    expectEqual(10, array[0].value)
    expectEqual(20, array[1].value)
    expectEqual(30, array[2].value)
  }

  dynamic func acceptArrayBridgedNonverbatim(array: [TestBridgedValueTy]) {
    // Cannot check elements because doing so would bridge them.
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
    // Cannot check elements because doing so would bridge them.
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

  do {
    helper.acceptArrayBridgedVerbatim(
        [ TestObjCValueTy(10), TestObjCValueTy(20), TestObjCValueTy(30) ])
  }
  do {
    TestBridgedValueTy.bridgeOperations = 0
    helper.acceptArrayBridgedNonverbatim(
        [ TestBridgedValueTy(10), TestBridgedValueTy(20),
          TestBridgedValueTy(30) ])
    expectEqual(0, TestBridgedValueTy.bridgeOperations)
  }
}

ObjCThunks.test("Array/Return") {
  var helper = ObjCThunksHelper()

  do {
    let a = helper.returnArrayBridgedVerbatim()
    expectEqual(10, a[0].value)
    expectEqual(20, a[1].value)
    expectEqual(30, a[2].value)
  }
  do {
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

  do {
    helper.acceptDictionaryBridgedVerbatim(
        [ TestObjCKeyTy(10): TestObjCValueTy(1010),
          TestObjCKeyTy(20): TestObjCValueTy(1020),
          TestObjCKeyTy(30): TestObjCValueTy(1030) ])
  }
  do {
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

  do {
    let d = helper.returnDictionaryBridgedVerbatim()
    expectEqual(3, d.count)
    expectEqual(1010, d[TestObjCKeyTy(10)]!.value)
    expectEqual(1020, d[TestObjCKeyTy(20)]!.value)
    expectEqual(1030, d[TestObjCKeyTy(30)]!.value)
  }
  do {
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

DictionaryTestSuite.test("dropsBridgedCache") {
  // rdar://problem/18544533
  // Previously this code would segfault due to a double free in the Dictionary
  // implementation.
  // This test will only fail in address sanitizer.
  var dict = [0:10]
  do {
    var bridged: NSDictionary = dict
    expectEqual(10, bridged[0] as! Int)
  }

  dict[0] = 11
  do {
    var bridged: NSDictionary = dict
    expectEqual(11, bridged[0] as! Int)
  }
}

DictionaryTestSuite.test("getObjects:andKeys:") {
  let d = ([1: "one", 2: "two"] as Dictionary<Int, String>) as NSDictionary
  var keys = UnsafeMutableBufferPointer(
    start: UnsafeMutablePointer<NSNumber>.alloc(2), count: 2)
  var values = UnsafeMutableBufferPointer(
    start: UnsafeMutablePointer<NSString>.alloc(2), count: 2)
  var kp = AutoreleasingUnsafeMutablePointer<AnyObject?>(keys.baseAddress)
  var vp = AutoreleasingUnsafeMutablePointer<AnyObject?>(values.baseAddress)
  var null: AutoreleasingUnsafeMutablePointer<AnyObject?> = nil

  d.getObjects(null, andKeys: null) // don't segfault

  d.getObjects(null, andKeys: kp)
  expectEqual([2, 1] as [NSNumber], Array(keys))

  d.getObjects(vp, andKeys: null)
  expectEqual(["two", "one"] as [NSString], Array(values))

  d.getObjects(vp, andKeys: kp)
  expectEqual([2, 1] as [NSNumber], Array(keys))
  expectEqual(["two", "one"] as [NSString], Array(values))
}

DictionaryTestSuite.setUp {
  resetLeaksOfDictionaryKeysValues()
  resetLeaksOfObjCDictionaryKeysValues()
}

DictionaryTestSuite.tearDown {
  expectNoLeaksOfDictionaryKeysValues()
  expectNoLeaksOfObjCDictionaryKeysValues()
}

runAllTests()


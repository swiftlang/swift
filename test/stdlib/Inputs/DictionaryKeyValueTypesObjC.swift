import Swift
import SwiftPrivate
import Darwin
import StdlibUnittest
import Foundation

func convertDictionaryToNSDictionary<Key, Value>(
  _ d: [Key : Value]
) -> NSDictionary {
  return d._bridgeToObjectiveC()
}

public func convertNSDictionaryToDictionary<
  Key : Hashable, Value
>(_ d: NSDictionary?) -> [Key : Value] {
  if _slowPath(d == nil) { return [:] }
  var result: [Key : Value]?
  Dictionary._forceBridgeFromObjectiveC(d!, result: &result)
  return result!
}

func isNativeDictionary<KeyTy : Hashable, ValueTy>(
  _ d: Dictionary<KeyTy, ValueTy>) -> Bool {
  return d._variant.isNative
}

func isCocoaDictionary<KeyTy : Hashable, ValueTy>(
  _ d: Dictionary<KeyTy, ValueTy>) -> Bool {
  return !isNativeDictionary(d)
}

func isNativeNSDictionary(_ d: NSDictionary) -> Bool {
  let className: NSString = NSStringFromClass(type(of: d)) as NSString
  return [
    "_SwiftDeferredNSDictionary",
    "_EmptyDictionarySingleton",
    "_DictionaryStorage"].contains {
    className.range(of: $0).length > 0
  }
}

func isCocoaNSDictionary(_ d: NSDictionary) -> Bool {
  let className: NSString = NSStringFromClass(type(of: d)) as NSString
  return className.range(of: "NSDictionary").length > 0 ||
    className.range(of: "NSCFDictionary").length > 0
}

func isNativeNSArray(_ d: NSArray) -> Bool {
  let className: NSString = NSStringFromClass(type(of: d)) as NSString
  return ["__SwiftDeferredNSArray", "_ContiguousArray", "_EmptyArray"].contains {
    className.range(of: $0).length > 0
  }
}

var _objcKeyCount = _stdlib_AtomicInt(0)
var _objcKeySerial = _stdlib_AtomicInt(0)

class TestObjCKeyTy : NSObject, NSCopying {
  class var objectCount: Int {
    get {
      return _objcKeyCount.load()
    }
    set {
      _objcKeyCount.store(newValue)
    }
  }

  init(_ value: Int) {
    _objcKeyCount.fetchAndAdd(1)
    serial = _objcKeySerial.addAndFetch(1)
    self.value = value
    self._hashValue = value
    super.init()
  }

  convenience init(value: Int, hashValue: Int) {
    self.init(value)
    self._hashValue = hashValue
  }

  deinit {
    assert(serial > 0, "double destruction")
    _objcKeyCount.fetchAndAdd(-1)
    serial = -serial
  }

  @objc(copyWithZone:)
  func copy(with zone: NSZone?) -> Any {
    return TestObjCKeyTy(value)
  }

  override var description: String {
    assert(serial > 0, "dead TestObjCKeyTy")
    return value.description
  }

  override func isEqual(_ object: Any!) -> Bool {
    if let other = object {
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

// A type that satisfies the requirements of an NSDictionary key (or an NSSet
// member), but traps when any of its methods are called.
class TestObjCInvalidKeyTy {
  init() {
    _objcKeyCount.fetchAndAdd(1)
    serial = _objcKeySerial.addAndFetch(1)
  }

  deinit {
    assert(serial > 0, "double destruction")
    _objcKeyCount.fetchAndAdd(-1)
    serial = -serial
  }

  @objc
  var description: String {
    assert(serial > 0, "dead TestObjCInvalidKeyTy")
    fatalError()
  }

  @objc
  func isEqual(_ object: Any!) -> Bool {
    fatalError()
  }

  @objc
  var hash : Int {
    fatalError()
  }

  var serial: Int
}

var _objcValueCount = _stdlib_AtomicInt(0)
var _objcValueSerial = _stdlib_AtomicInt(0)

class TestObjCValueTy : NSObject {
  class var objectCount: Int {
    get {
      return _objcValueCount.load()
    }
    set {
      _objcValueCount.store(newValue)
    }
  }

  init(_ value: Int) {
    _objcValueCount.fetchAndAdd(1)
    serial = _objcValueSerial.addAndFetch(1)
    self.value = value
  }

  deinit {
    assert(serial > 0, "double destruction")
    _objcValueCount.fetchAndAdd(-1)
    serial = -serial
  }

  override var description: String {
    assert(serial > 0, "dead TestObjCValueTy")
    return value.description
  }

  var value: Int
  var serial: Int
}

var _objcEquatableValueCount = _stdlib_AtomicInt(0)
var _objcEquatableValueSerial = _stdlib_AtomicInt(0)

class TestObjCEquatableValueTy : NSObject {
  class var objectCount: Int {
    get {
      return _objcEquatableValueCount.load()
    }
    set {
      _objcEquatableValueCount.store(newValue)
    }
  }

  init(_ value: Int) {
    _objcEquatableValueCount.fetchAndAdd(1)
    serial = _objcEquatableValueSerial.addAndFetch(1)
    self.value = value
  }

  deinit {
    assert(serial > 0, "double destruction")
    _objcEquatableValueCount.fetchAndAdd(-1)
    serial = -serial
  }

  override func isEqual(_ object: Any!) -> Bool {
    if let other = object {
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

var _bridgedKeySerial = _stdlib_AtomicInt(0)
var _bridgedKeyBridgeOperations = _stdlib_AtomicInt(0)

struct TestBridgedKeyTy
  : Equatable, Hashable, CustomStringConvertible, _ObjectiveCBridgeable {
  var value: Int
  var _hashValue: Int
  var serial: Int

  static var bridgeOperations: Int {
    get {
      return _bridgedKeyBridgeOperations.load()
    }
    set {
      _bridgedKeyBridgeOperations.store(newValue)
    }
  }

  init(_ value: Int) {
    serial = _bridgedKeySerial.addAndFetch(1)
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

  func hash(into hasher: inout Hasher) {
    hasher.combine(_hashValue)
  }

  func _bridgeToObjectiveC() -> TestObjCKeyTy {
    _bridgedKeyBridgeOperations.fetchAndAdd(1)
    return TestObjCKeyTy(value)
  }

  static func _forceBridgeFromObjectiveC(
    _ x: TestObjCKeyTy,
    result: inout TestBridgedKeyTy?
  ) {
    _bridgedKeyBridgeOperations.fetchAndAdd(1)
    result = TestBridgedKeyTy(x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    _ x: TestObjCKeyTy,
    result: inout TestBridgedKeyTy?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  static func _unconditionallyBridgeFromObjectiveC(_ source: TestObjCKeyTy?)
      -> TestBridgedKeyTy {
    var result: TestBridgedKeyTy? = nil
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }
}

func == (lhs: TestBridgedKeyTy, rhs: TestBridgedKeyTy) -> Bool {
  return lhs.value == rhs.value
}

func == (lhs: TestBridgedKeyTy, rhs: TestKeyTy) -> Bool {
  return lhs.value == rhs.value
}

var _bridgedValueSerial = _stdlib_AtomicInt(0)
var _bridgedValueBridgeOperations = _stdlib_AtomicInt(0)

struct TestBridgedValueTy : CustomStringConvertible, _ObjectiveCBridgeable {
  static var bridgeOperations: Int {
    get {
      return _bridgedValueBridgeOperations.load()
    }
    set {
      _bridgedValueBridgeOperations.store(newValue)
    }
  }

  init(_ value: Int) {
    serial = _bridgedValueSerial.fetchAndAdd(1)
    self.value = value
  }

  var description: String {
    assert(serial > 0, "dead TestBridgedValueTy")
    return value.description
  }

  func _bridgeToObjectiveC() -> TestObjCValueTy {
    TestBridgedValueTy.bridgeOperations += 1
    return TestObjCValueTy(value)
  }

  static func _forceBridgeFromObjectiveC(
    _ x: TestObjCValueTy,
    result: inout TestBridgedValueTy?
  ) {
    TestBridgedValueTy.bridgeOperations += 1
    result = TestBridgedValueTy(x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    _ x: TestObjCValueTy,
    result: inout TestBridgedValueTy?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  static func _unconditionallyBridgeFromObjectiveC(_ source: TestObjCValueTy?)
      -> TestBridgedValueTy {
    var result: TestBridgedValueTy? = nil
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }

  var value: Int
  var serial: Int
}

var _bridgedEquatableValueSerial = _stdlib_AtomicInt(0)
var _bridgedEquatableValueBridgeOperations = _stdlib_AtomicInt(0)

struct TestBridgedEquatableValueTy
  : Equatable, CustomStringConvertible, _ObjectiveCBridgeable {

  static var bridgeOperations: Int {
    get {
      return _bridgedEquatableValueBridgeOperations.load()
    }
    set {
      _bridgedEquatableValueBridgeOperations.store(newValue)
    }
  }

  init(_ value: Int) {
    serial = _bridgedEquatableValueSerial.addAndFetch(1)
    self.value = value
  }

  var description: String {
    assert(serial > 0, "dead TestBridgedValueTy")
    return value.description
  }

  func _bridgeToObjectiveC() -> TestObjCEquatableValueTy {
    _bridgedEquatableValueBridgeOperations.fetchAndAdd(1)
    return TestObjCEquatableValueTy(value)
  }

  static func _forceBridgeFromObjectiveC(
    _ x: TestObjCEquatableValueTy,
    result: inout TestBridgedEquatableValueTy?
  ) {
    _bridgedEquatableValueBridgeOperations.fetchAndAdd(1)
    result = TestBridgedEquatableValueTy(x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    _ x: TestObjCEquatableValueTy,
    result: inout TestBridgedEquatableValueTy?
  ) -> Bool {
    self._forceBridgeFromObjectiveC(x, result: &result)
    return true
  }

  static func _unconditionallyBridgeFromObjectiveC(
    _ source: TestObjCEquatableValueTy?
  ) -> TestBridgedEquatableValueTy {
    var result: TestBridgedEquatableValueTy? = nil
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }

  var value: Int
  var serial: Int
}

func == (lhs: TestBridgedEquatableValueTy, rhs: TestBridgedEquatableValueTy) -> Bool {
  return lhs.value == rhs.value
}

/// Expect some number of autoreleased key and value objects.
///
/// - parameter opt: applies to platforms that have the return-autoreleased
///   optimization.
///
/// - parameter unopt: applies to platforms that don't.
///
/// FIXME: Some non-zero `opt` might be cases of missed return-autorelease.
func expectAutoreleasedKeysAndValues(
  opt: (Int, Int) = (0, 0), unopt: (Int, Int) = (0, 0)) {
  var expectedKeys = 0
  var expectedValues = 0
#if arch(i386)
  (expectedKeys, expectedValues) = unopt
#else
  (expectedKeys, expectedValues) = opt
#endif

  TestObjCKeyTy.objectCount -= expectedKeys
  TestObjCValueTy.objectCount -= expectedValues
}

/// Expect some number of autoreleased value objects.
///
/// - parameter opt: applies to platforms that have the return-autoreleased
///   optimization.
///
/// - parameter unopt: applies to platforms that don't.
///
/// FIXME: Some non-zero `opt` might be cases of missed return-autorelease.
func expectAutoreleasedValues(
  opt: Int = 0, unopt: Int = 0) {
  expectAutoreleasedKeysAndValues(opt: (0, opt), unopt: (0, unopt))
}

func resetLeaksOfObjCDictionaryKeysValues() {
  TestObjCKeyTy.objectCount = 0
  TestObjCValueTy.objectCount = 0
  TestObjCEquatableValueTy.objectCount = 0
}

func expectNoLeaksOfObjCDictionaryKeysValues() {
  expectEqual(0, TestObjCKeyTy.objectCount, "TestObjCKeyTy leak")
  expectEqual(0, TestObjCValueTy.objectCount, "TestObjCValueTy leak")
  expectEqual(
    0, TestObjCEquatableValueTy.objectCount, "TestObjCEquatableValueTy leak")
}

func getBridgedNSDictionaryOfRefTypesBridgedVerbatim() -> NSDictionary {
  assert(_isBridgedVerbatimToObjectiveC(TestObjCKeyTy.self))
  assert(_isBridgedVerbatimToObjectiveC(TestObjCValueTy.self))

  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  let bridged = convertDictionaryToNSDictionary(d)

  assert(isNativeNSDictionary(bridged))

  return bridged
}

func getBridgedEmptyNSDictionary() -> NSDictionary {
  let d = Dictionary<TestObjCKeyTy, TestObjCValueTy>()

  let bridged = convertDictionaryToNSDictionary(d)
  assert(isNativeNSDictionary(bridged))

  return bridged
}

func getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged(
  numElements: Int = 3
) -> NSDictionary {
  assert(!_isBridgedVerbatimToObjectiveC(TestBridgedKeyTy.self))
  assert(!_isBridgedVerbatimToObjectiveC(TestBridgedValueTy.self))

  var d = Dictionary<TestBridgedKeyTy, TestBridgedValueTy>()
  for i in 1..<(numElements + 1) {
    d[TestBridgedKeyTy(i * 10)] = TestBridgedValueTy(i * 10 + 1000)
  }

  let bridged = convertDictionaryToNSDictionary(d)
  assert(isNativeNSDictionary(bridged))

  return bridged
}

import SlurpFastEnumeration

 func slurpFastEnumerationFromSwift(
  _ a: NSArray, _ fe: NSFastEnumeration, _ sink: (AnyObject) -> Void,
  maxItems: Int? = nil
) {
  var state = NSFastEnumerationState()

  let bufferSize = 3
  let buffer =
    UnsafeMutableBufferPointer<AnyObject?>.allocate(capacity: bufferSize)
  defer { buffer.deallocate() }

  var itemsReturned = 0
  while true {
    let returnedCount = fe.countByEnumerating(
      with: &state,
      objects: AutoreleasingUnsafeMutablePointer(buffer.baseAddress!),
      count: buffer.count)
    expectNotEqual(0, state.state)
    expectNotNil(state.mutationsPtr)
    if returnedCount == 0 {
      break
    }
    for i in 0..<returnedCount {
      let value: AnyObject = state.itemsPtr![i]!
      sink(value)
      itemsReturned += 1
    }
    if maxItems != nil && itemsReturned >= maxItems! {
      return
    }
  }

  for _ in 0..<3 {
    let returnedCount = fe.countByEnumerating(
      with: &state,
      objects: AutoreleasingUnsafeMutablePointer(buffer.baseAddress!),
      count: buffer.count)
    expectNotEqual(0, state.state)
    expectNotNil(state.mutationsPtr)
    expectEqual(0, returnedCount)
  }
}

typealias AnyObjectTuple2 = (AnyObject, AnyObject)

 func slurpFastEnumerationFromSwift(
  _ d: NSDictionary, _ fe: NSFastEnumeration, _ sink: (AnyObjectTuple2) -> Void,
  maxItems: Int? = nil
) {
  var state = NSFastEnumerationState()

  let bufferSize = 3
  let buffer =
    UnsafeMutableBufferPointer<AnyObject?>.allocate(capacity: bufferSize)
  defer { buffer.deallocate() }

  var itemsReturned = 0
  while true {
    let returnedCount = fe.countByEnumerating(
      with: &state,
      objects: AutoreleasingUnsafeMutablePointer(buffer.baseAddress!),
      count: buffer.count)
    expectNotEqual(0, state.state)
    expectNotNil(state.mutationsPtr)
    if returnedCount == 0 {
      break
    }
    for i in 0..<returnedCount {
      let key: AnyObject = state.itemsPtr![i]!
      let value: AnyObject = d.object(forKey: key)! as AnyObject
      let kv = (key, value)
      sink(kv)
      itemsReturned += 1
    }
    if maxItems != nil && itemsReturned >= maxItems! {
      return
    }
  }

  for _ in 0..<3 {
    let returnedCount = fe.countByEnumerating(
      with: &state,
      objects: AutoreleasingUnsafeMutablePointer(buffer.baseAddress!),
      count: buffer.count)
    expectEqual(0, returnedCount)
  }
}

 func slurpFastEnumerationOfNSEnumeratorFromSwift(
  _ a: NSArray, _ enumerator: NSEnumerator, _ sink: (AnyObject) -> Void,
  maxFastEnumerationItems: Int
) {
  slurpFastEnumerationFromSwift(
    a, enumerator, sink, maxItems: maxFastEnumerationItems)
  while let value = enumerator.nextObject() {
    sink(value as AnyObject)
  }
}

 func slurpFastEnumerationOfNSEnumeratorFromSwift(
  _ d: NSDictionary, _ enumerator: NSEnumerator,
  _ sink: (AnyObjectTuple2) -> Void,
  maxFastEnumerationItems: Int
) {
  slurpFastEnumerationFromSwift(
    d, enumerator, sink, maxItems: maxFastEnumerationItems)
  while let key = enumerator.nextObject() {
    let value: AnyObject = d.object(forKey: key)! as AnyObject
    let kv = (key as AnyObject, value)
    sink(kv)
  }
}

 func slurpFastEnumerationFromObjC(
  _ a: NSArray, _ fe: NSFastEnumeration, _ sink: (AnyObject) -> Void
) {
  let objcValues = NSMutableArray()
  slurpFastEnumerationOfArrayFromObjCImpl(a, fe, objcValues)
  for value in objcValues {
    sink(value as AnyObject)
  }
}

 func _checkArrayFastEnumerationImpl(
  _ expected: [Int],
  _ a: NSArray,
  _ makeEnumerator: () -> NSFastEnumeration,
  _ useEnumerator: (NSArray, NSFastEnumeration, (AnyObject) -> ()) -> Void,
  _ convertValue: @escaping (AnyObject) -> Int
) {
  let expectedContentsWithoutIdentity =
  _makeExpectedArrayContents(expected)
  
  var expectedContents = [ExpectedArrayElement]()
  
  for i in 0..<3 {
    var actualContents = [ExpectedArrayElement]()
    let sink = {
      (value: AnyObject) in
      actualContents.append(ExpectedArrayElement(
        value: convertValue(value),
        valueIdentity: unsafeBitCast(value, to: UInt.self)))
    }

    useEnumerator(a, makeEnumerator(), sink)

    expectTrue(
      _equalsWithoutElementIdentity(
        expectedContentsWithoutIdentity, actualContents),
      "expected: \(expectedContentsWithoutIdentity)\n" +
      "actual: \(actualContents)\n")

    if i == 0 {
      expectedContents = actualContents
    }
    
    expectEqualSequence(expectedContents, actualContents)
  }
}

 func checkArrayFastEnumerationFromSwift(
  _ expected: [Int],
  _ a: NSArray, _ makeEnumerator: () -> NSFastEnumeration,
  _ convertValue: @escaping (AnyObject) -> Int
) {
  _checkArrayFastEnumerationImpl(
    expected, a, makeEnumerator,
    { (a, fe, sink) in
      slurpFastEnumerationFromSwift(a, fe, sink)
    },
    convertValue)
}

 func checkArrayFastEnumerationFromObjC(
  _ expected: [Int],
  _ a: NSArray, _ makeEnumerator: () -> NSFastEnumeration,
  _ convertValue: @escaping (AnyObject) -> Int
) {
  _checkArrayFastEnumerationImpl(
    expected, a, makeEnumerator,
    { (a, fe, sink) in
      slurpFastEnumerationFromObjC(a, fe, sink)
    },
    convertValue)
}

 func checkArrayEnumeratorPartialFastEnumerationFromSwift(
  _ expected: [Int],
  _ a: NSArray,
  maxFastEnumerationItems: Int,
  _ convertValue: @escaping (AnyObject) -> Int
) {
  _checkArrayFastEnumerationImpl(
    expected, a, { a.objectEnumerator() },
    { (a, fe, sink) in
      slurpFastEnumerationOfNSEnumeratorFromSwift(
        a, fe as! NSEnumerator, sink,
        maxFastEnumerationItems: maxFastEnumerationItems)
    },
    convertValue)
}

 func _checkSetFastEnumerationImpl(
  _ expected: [Int],
  _ s: NSSet,
  _ makeEnumerator: () -> NSFastEnumeration,
  _ useEnumerator: (NSSet, NSFastEnumeration, (AnyObject) -> ()) -> Void,
  _ convertMember: @escaping (AnyObject) -> Int
) {
  let expectedContentsWithoutIdentity =
    _makeExpectedSetContents(expected)
  var expectedContents = [ExpectedSetElement]()

  for i in 0..<3 {
    var actualContents = [ExpectedSetElement]()
    let sink = {
      (value: AnyObject) in
      actualContents.append(ExpectedSetElement(
        value: convertMember(value),
        valueIdentity: unsafeBitCast(value, to: UInt.self)))
    }

    useEnumerator(s, makeEnumerator(), sink)

    expectTrue(
      _equalsUnorderedWithoutElementIdentity(
        expectedContentsWithoutIdentity, actualContents),
      "expected: \(expectedContentsWithoutIdentity)\n" +
      "actual: \(actualContents)\n")

    if i == 0 {
      expectedContents = actualContents
    }
    expectTrue(equalsUnordered(expectedContents, actualContents))
  }
}

 func slurpFastEnumerationFromObjC(
  _ s: NSSet, _ fe: NSFastEnumeration, _ sink: (AnyObject) -> Void
) {
  let objcValues = NSMutableArray()
  slurpFastEnumerationOfArrayFromObjCImpl(s, fe, objcValues)
  for value in objcValues {
    sink(value as AnyObject)
  }
}

 func slurpFastEnumerationOfNSEnumeratorFromSwift(
  _ s: NSSet, _ enumerator: NSEnumerator, _ sink: (AnyObject) -> Void,
  maxFastEnumerationItems: Int
) {
  slurpFastEnumerationFromSwift(
    s, enumerator, sink, maxItems: maxFastEnumerationItems)
  while let value = enumerator.nextObject() {
    sink(value as AnyObject)
  }
}

 func slurpFastEnumerationFromSwift(
  _ s: NSSet, _ fe: NSFastEnumeration, _ sink: (AnyObject) -> Void,
  maxItems: Int? = nil
) {
  var state = NSFastEnumerationState()

  let bufferSize = 3
  let buffer =
    UnsafeMutableBufferPointer<AnyObject?>.allocate(capacity: bufferSize)
  defer { buffer.deallocate() }

  var itemsReturned = 0
  while true {
    let returnedCount = fe.countByEnumerating(
      with: &state,
      objects: AutoreleasingUnsafeMutablePointer(buffer.baseAddress!),
      count: buffer.count)
    expectNotEqual(0, state.state)
    expectNotNil(state.mutationsPtr)
    if returnedCount == 0 {
      break
    }
    for i in 0..<returnedCount {
      let value: AnyObject = state.itemsPtr![i]!
      sink(value)
      itemsReturned += 1
    }
    if maxItems != nil && itemsReturned >= maxItems! {
      return
    }
  }

  for _ in 0..<3 {
    let returnedCount = fe.countByEnumerating(
      with: &state,
      objects: AutoreleasingUnsafeMutablePointer(buffer.baseAddress!),
      count: buffer.count)
    expectNotEqual(0, state.state)
    expectNotNil(state.mutationsPtr)
    expectEqual(0, returnedCount)
  }
}

 func checkSetFastEnumerationFromSwift(
  _ expected: [Int],
  _ s: NSSet, _ makeEnumerator: () -> NSFastEnumeration,
  _ convertMember: @escaping (AnyObject) -> Int
) {
  _checkSetFastEnumerationImpl(
    expected, s, makeEnumerator,
    { (s, fe, sink) in
      slurpFastEnumerationFromSwift(s, fe, sink)
    },
    convertMember)
}

 func checkSetFastEnumerationFromObjC(
  _ expected: [Int],
  _ s: NSSet, _ makeEnumerator: () -> NSFastEnumeration,
  _ convertMember: @escaping (AnyObject) -> Int
) {
  _checkSetFastEnumerationImpl(
    expected, s, makeEnumerator,
    { (s, fe, sink) in
      slurpFastEnumerationFromObjC(s, fe, sink)
    },
    convertMember)
}

 func checkSetEnumeratorPartialFastEnumerationFromSwift(
  _ expected: [Int],
  _ s: NSSet,
  maxFastEnumerationItems: Int,
  _ convertMember: @escaping (AnyObject) -> Int
) {
  _checkSetFastEnumerationImpl(
    expected, s, { s.objectEnumerator() },
    { (s, fe, sink) in
      slurpFastEnumerationOfNSEnumeratorFromSwift(
        s, fe as! NSEnumerator, sink,
        maxFastEnumerationItems: maxFastEnumerationItems)
    },
    convertMember)
}

 func slurpFastEnumerationFromObjC(
  _ d: NSDictionary, _ fe: NSFastEnumeration, _ sink: (AnyObjectTuple2) -> Void
) {
  let objcPairs = NSMutableArray()
  slurpFastEnumerationOfDictionaryFromObjCImpl(d, fe, objcPairs)
  for i in 0..<objcPairs.count/2 {
    let key = objcPairs[i * 2] as AnyObject
    let value = objcPairs[i * 2 + 1] as AnyObject
    let kv = (key, value)
    sink(kv)
  }
}

 func _checkDictionaryFastEnumerationImpl(
  _ expected: [(Int, Int)],
  _ d: NSDictionary,
  _ makeEnumerator: () -> NSFastEnumeration,
  _ useEnumerator: (NSDictionary, NSFastEnumeration, (AnyObjectTuple2) -> ()) -> Void,
  _ convertKey: @escaping (AnyObject) -> Int,
  _ convertValue: @escaping (AnyObject) -> Int
) {
  let expectedContentsWithoutIdentity =
    _makeExpectedDictionaryContents(expected)
  var expectedContents = [ExpectedDictionaryElement]()

  for i in 0..<3 {
    var actualContents = [ExpectedDictionaryElement]()
    let sink: (AnyObjectTuple2) -> Void = {
      let (key, value) = $0
      actualContents.append(ExpectedDictionaryElement(
        key: convertKey(key),
        value: convertValue(value),
        keyIdentity: unsafeBitCast(key, to: UInt.self),
        valueIdentity: unsafeBitCast(value, to: UInt.self)))
    }

    useEnumerator(d, makeEnumerator(), sink)

    expectTrue(
      _equalsUnorderedWithoutElementIdentity(
        expectedContentsWithoutIdentity, actualContents),
      "expected: \(expectedContentsWithoutIdentity)\n" +
      "actual: \(actualContents)\n")

    if i == 0 {
      expectedContents = actualContents
    }
    expectTrue(equalsUnordered(expectedContents, actualContents))
  }
}

 func checkDictionaryFastEnumerationFromSwift(
  _ expected: [(Int, Int)],
  _ d: NSDictionary, _ makeEnumerator: () -> NSFastEnumeration,
  _ convertKey: @escaping (AnyObject) -> Int,
  _ convertValue: @escaping (AnyObject) -> Int
) {
  _checkDictionaryFastEnumerationImpl(
    expected, d, makeEnumerator,
    { (d, fe, sink) in
      slurpFastEnumerationFromSwift(d, fe, sink)
    },
    convertKey, convertValue)
}

 func checkDictionaryFastEnumerationFromObjC(
  _ expected: [(Int, Int)],
  _ d: NSDictionary, _ makeEnumerator: () -> NSFastEnumeration,
  _ convertKey: @escaping (AnyObject) -> Int,
  _ convertValue: @escaping (AnyObject) -> Int
) {
  _checkDictionaryFastEnumerationImpl(
    expected, d, makeEnumerator,
    { (d, fe, sink) in
      slurpFastEnumerationFromObjC(d, fe, sink)
    },
    convertKey, convertValue)
}

 func checkDictionaryEnumeratorPartialFastEnumerationFromSwift(
  _ expected: [(Int, Int)],
  _ d: NSDictionary,
  maxFastEnumerationItems: Int,
  _ convertKey: @escaping (AnyObject) -> Int,
  _ convertValue: @escaping (AnyObject) -> Int
) {
  _checkDictionaryFastEnumerationImpl(
    expected, d, { d.keyEnumerator() },
    { (d, fe, sink) in
      slurpFastEnumerationOfNSEnumeratorFromSwift(
        d, fe as! NSEnumerator, sink,
        maxFastEnumerationItems: maxFastEnumerationItems)
    },
    convertKey, convertValue)
}

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

  let bridged = convertArrayToNSArray(a)
  assert(isNativeNSArray(bridged))

  return bridged
}

func convertNSArrayToArray<T>(_ source: NSArray?) -> [T] {
  if _slowPath(source == nil) { return [] }
  var result: [T]?
  Array._forceBridgeFromObjectiveC(source!, result: &result)
  return result!
}

func convertArrayToNSArray<T>(_ array: [T]) -> NSArray {
  return array._bridgeToObjectiveC()
}

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

  let bridged = convertArrayToNSArray(a)
  assert(isNativeNSArray(bridged))

  return bridged
}

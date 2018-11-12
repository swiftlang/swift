import Swift
import StdlibUnittest
#if _runtime(_ObjC)
import Darwin
import Foundation
#endif


#if _runtime(_ObjC)

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
  _expectAutoreleasedKeysAndValues(opt: (0, opt), unopt: (0, unopt))
}

func isNativeNSArray(_ d: NSArray) -> Bool {
  let className: NSString = NSStringFromClass(type(of: d)) as NSString
  return ["__SwiftDeferredNSArray", "_ContiguousArray", "_EmptyArray"].contains {
    className.range(of: $0).length > 0
  }
}

func isCocoaNSArray(_ a: NSArray) -> Bool {
  let className: NSString = NSStringFromClass(type(of: a)) as NSString
  return className.range(of: "NSArray").length > 0 ||
    className.range(of: "NSCFArray").length > 0
}

func getBridgedNSArrayOfRefTypesBridgedVerbatim() -> NSArray {
  expectTrue(_isBridgedVerbatimToObjectiveC(TestObjCValueTy.self))

  var a = Array<TestObjCValueTy>()
  a.reserveCapacity(32)
  a.append(TestObjCValueTy(1010))
  a.append(TestObjCValueTy(1020))
  a.append(TestObjCValueTy(1030))

  let bridged = convertArrayToNSArray(a)

  assert(isNativeNSArray(bridged))

  return bridged
}

func getBridgedEmptyNSArray() -> NSArray {
  let a = Array<TestObjCValueTy>()

  let bridged = convertArrayToNSArray(a)
  assert(isNativeNSArray(bridged))

  return bridged
}

#endif

func getDerivedAPIsArray() -> Array<Int> {
  var a = Array<Int>()
  a.append(1010)
  a.append(1020)
  a.append(1030)
  return a
}

func _makeExpectedArrayContents(
  _ expected: [Int]
) -> [ExpectedArrayElement] {
  var result = [ExpectedArrayElement]()
  for value in expected {
    result.append(ExpectedArrayElement(value: value))
  }
  return result
}

func _equalsWithoutElementIdentity(
  _ lhs: [ExpectedArrayElement], _ rhs: [ExpectedArrayElement]
) -> Bool {
  func stripIdentity(
    _ list: [ExpectedArrayElement]
  ) -> [ExpectedArrayElement] {
    return list.map { ExpectedArrayElement(value: $0.value) }
  }

  return stripIdentity(lhs).elementsEqual(stripIdentity(rhs))
}

struct ExpectedArrayElement : Comparable, CustomStringConvertible {
  var value: Int
  var valueIdentity: UInt

  init(value: Int, valueIdentity: UInt = 0) {
    self.value = value
    self.valueIdentity = valueIdentity
  }

  var description: String {
    return "(\(value), \(valueIdentity))"
  }
}

func == (
  lhs: ExpectedArrayElement,
  rhs: ExpectedArrayElement
) -> Bool {
  return
    lhs.value == rhs.value &&
    lhs.valueIdentity == rhs.valueIdentity
}

func < (
  lhs: ExpectedArrayElement,
  rhs: ExpectedArrayElement
) -> Bool {
  let lhsElements = [ lhs.value, Int(bitPattern: lhs.valueIdentity) ]
  let rhsElements = [ rhs.value, Int(bitPattern: rhs.valueIdentity) ]
  return lhsElements.lexicographicallyPrecedes(rhsElements)
}

#if _runtime(_ObjC)
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
#endif

func getCOWFastArray() -> Array<Int> {
  var a = Array<Int>()
  a.reserveCapacity(10)
  a.append(1)
  a.append(2)
  a.append(3)
  return a
}

func getCOWSlowArray() -> Array<COWBox<Int>> {
  var a = Array<COWBox<Int>>()
  a.reserveCapacity(10)
  a.append(COWBox(1))
  a.append(COWBox(2))
  a.append(COWBox(3))
  return a
}

#if _runtime(_ObjC)
 func slurpFastEnumerationFromObjC(
  _ a: NSArray, _ fe: NSFastEnumeration, _ sink: (AnyObject) -> Void
) {
  let objcValues = NSMutableArray()
  slurpFastEnumerationOfArrayFromObjCImpl(a, fe, objcValues)
  for value in objcValues {
    sink(value as AnyObject)
  }
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

func getBridgedNSArrayOfObjValue_ValueTypesCustomBridged(
  numElements: Int = 3
) -> NSArray {
  expectFalse(_isBridgedVerbatimToObjectiveC(TestBridgedValueTy.self))

  var a = Array<TestBridgedValueTy>()
  for i in 1..<(numElements + 1) {
    a.append(TestBridgedValueTy(i * 10 + 1000))
  }

  let bridged = convertArrayToNSArray(a)
  expectTrue(isNativeNSArray(bridged))

  return bridged
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

func _expectAutoreleasedKeysAndValues(
  opt: (Int, Int) = (0, 0), unopt: (Int, Int) = (0, 0)) {
  var expectedKeys = 0
  var expectedValues = 0
#if arch(i386)
  (expectedKeys, expectedValues) = unopt
#else
  (expectedKeys, expectedValues) = opt
#endif

  TestObjCValueTy.objectCount -= expectedValues
}

func isCocoaArray<T>(_ a: Array<T>) -> Bool {
  return !isNativeArray(a)
}

func isNativeArray<T>(_ a: Array<T>) -> Bool {
  return a._buffer._storage.isNative
}

func convertArrayToNSArray<T>(_ a: [T]) -> NSArray {
  return a._bridgeToObjectiveC()
}

func convertNSArrayToArray<T>(_ a: NSArray?) -> [T] {
  if _slowPath(a == nil) { return [] }
  var result: [T]?
  Array._forceBridgeFromObjectiveC(a!, result: &result)
  return result!
}

func getAsNSArray(_ a: Array<Int>) -> NSArray {
  let values = Array(a.map { TestObjCValueTy($0) })
  
  // Return an `NSMutableArray` to make sure that it has a unique
  // pointer identity.
  let nsarray = NSMutableArray()
  nsarray.addObjects(from: values)
  return nsarray
}

func getAsEquatableNSArray(_ a: Array<Int>) -> NSArray {
  let values = Array(a.map { TestObjCEquatableValueTy($0) })

  // Return an `NSMutableArray` to make sure that it has a unique
  // pointer identity.
  let nsarray = NSMutableArray()
  nsarray.addObjects(from: values)
  return nsarray
}

func getAsNSMutableArray(_ a: Array<Int>) -> NSMutableArray {
  let values = Array(a.map { TestObjCValueTy($0) })
  let nsarray = NSMutableArray()
  nsarray.addObjects(from: values)
  return nsarray
}

func getBridgedVerbatimArrayAndNSMutableArray()
    -> (Array<AnyObject>, NSMutableArray) {
  let nsa = getAsNSMutableArray([1010, 1020, 1030])
  return (convertNSArrayToArray(nsa), nsa)
}

func getBridgedVerbatimArray() -> Array<AnyObject> {
  let nsa = getAsNSArray([1010, 1020, 1030])
  return convertNSArrayToArray(nsa)
}

func getBridgedVerbatimArray(_ a: Array<Int>) -> Array<AnyObject> {
  let nsa = getAsNSArray(a)
  return convertNSArrayToArray(nsa)
}

func getBridgedNonverbatimArray() -> Array<TestBridgedValueTy> {
  let nsa = getAsNSArray([1010, 1020, 1030 ])
  return Swift._forceBridgeFromObjectiveC(nsa, Array.self)
}

func getBridgedNonverbatimArray(_ a: Array<Int>) -> Array<TestBridgedValueTy> {
  let nsa = getAsNSArray(a)
  return Swift._forceBridgeFromObjectiveC(nsa, Array.self)
}

func getBridgedNonverbatimArrayAndNSMutableArray()
    -> (Array<TestBridgedValueTy>, NSMutableArray) {
  let nsa = getAsNSMutableArray([1010, 1020, 1030])
  return (Swift._forceBridgeFromObjectiveC(nsa, Array.self), nsa)
}

func getBridgedVerbatimEquatableArray(_ a: Array<Int>) 
    -> Array<TestObjCEquatableValueTy> {
  let nsa = getAsEquatableNSArray(a)
  return convertNSArrayToArray(nsa)
}

func getBridgedNonverbatimEquatableArray(_ a: Array<Int>) 
    -> Array<TestBridgedEquatableValueTy> {
  let nsa = getAsEquatableNSArray(a)
  return Swift._forceBridgeFromObjectiveC(nsa, Array.self)
}

func getHugeBridgedVerbatimArrayHelper() -> NSArray {
  let values = (1...32).map { TestObjCValueTy(1000 + $0) }

  let nsa = NSMutableArray()
  nsa.addObjects(from: values)
  return nsa
}

func getHugeBridgedVerbatimArray() -> Array<AnyObject> {
  let nsa = getHugeBridgedVerbatimArrayHelper()
  return convertNSArrayToArray(nsa)
}

func getHugeBridgedNonverbatimArray() 
    -> Array<TestBridgedValueTy> {
  let nsa = getHugeBridgedVerbatimArrayHelper()
  return Swift._forceBridgeFromObjectiveC(nsa, Array.self)
}

func getBridgedNSArrayOfObj_ValueTypeCustomBridged() -> NSArray {
  expectTrue(_isBridgedVerbatimToObjectiveC(TestObjCValueTy.self))

  var a = Array<TestObjCValueTy>()
  a.append(TestObjCValueTy(1010))
  a.append(TestObjCValueTy(1020))
  a.append(TestObjCValueTy(1030))

  let bridged = convertArrayToNSArray(a)
  expectTrue(isNativeNSArray(bridged))

  return bridged
}

func getBridgedNSArrayOfValue_ValueTypeCustomBridged() -> NSArray {
  expectFalse(_isBridgedVerbatimToObjectiveC(TestBridgedValueTy.self))

  var a = Array<TestBridgedValueTy>()
  a.append(TestBridgedValueTy(1010))
  a.append(TestBridgedValueTy(1020))
  a.append(TestBridgedValueTy(1030))

  let bridged = convertArrayToNSArray(a)
  expectTrue(isNativeNSArray(bridged))

  return bridged
}

func getRoundtripBridgedNSArray() -> NSArray {
  let values = [ 1010, 1020, 1030 ].map { TestObjCValueTy($0) }

  let nsa = NSArray(array: values)

  let a: Array<AnyObject> = convertNSArrayToArray(nsa)

  let bridgedBack = convertArrayToNSArray(a)
  expectTrue(isCocoaNSArray(bridgedBack))
  expectEqual(unsafeBitCast(nsa, to: Int.self), unsafeBitCast(bridgedBack, to: Int.self))

  return bridgedBack
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

@objc
class CustomImmutableNSArray : NSArray {
  init(_privateInit: ()) {
    super.init()
  }
    
  override init() {
    expectUnreachable()
    super.init()
  }
  
  override init(objects: UnsafePointer<AnyObject>!, count cnt: Int) {
    expectUnreachable()
    super.init(objects: objects, count: cnt)
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) not implemented by CustomImmutableNSArray")
  }

  @objc(copyWithZone:)
  override func copy(with zone: NSZone?) -> Any {
    CustomImmutableNSArray.timesCopyWithZoneWasCalled += 1
    return self
  }

  override func object(at index: Int) -> Any {
    CustomImmutableNSArray.timesObjectAtIndexWasCalled += 1
    return getAsNSArray([1010, 1020, 1030]).object(at: index)
  }
  
  override func objectEnumerator() -> NSEnumerator {
    CustomImmutableNSArray.timesObjectEnumeratorWasCalled += 1
    return getAsNSArray([1010, 1020, 1030]).objectEnumerator()
  }

  override var count: Int {
    CustomImmutableNSArray.timesCountWasCalled += 1
    return 3
  }

  static var timesCopyWithZoneWasCalled = 0
  static var timesObjectAtIndexWasCalled = 0
  static var timesObjectEnumeratorWasCalled = 0
  static var timesCountWasCalled = 0
}

#endif

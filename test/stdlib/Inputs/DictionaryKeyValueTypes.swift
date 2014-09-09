import Darwin
import StdlibUnittest
import Foundation

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

func isNativeNSDictionary(d: NSDictionary) -> Bool {
  var className: NSString = NSStringFromClass(d.dynamicType)
  return className.rangeOfString("_NativeDictionaryStorageOwner").length > 0
}

func isCocoaNSDictionary(d: NSDictionary) -> Bool {
  var className: NSString = NSStringFromClass(d.dynamicType)
  return className.rangeOfString("NSDictionary").length > 0 ||
    className.rangeOfString("NSCFDictionary").length > 0
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

func getBridgedNSDictionaryOfRefTypesBridgedVerbatim() -> NSDictionary {
  assert(_isBridgedVerbatimToObjectiveC(TestObjCKeyTy.self))
  assert(_isBridgedVerbatimToObjectiveC(TestObjCValueTy.self))

  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>(minimumCapacity: 32)
  d[TestObjCKeyTy(10)] = TestObjCValueTy(1010)
  d[TestObjCKeyTy(20)] = TestObjCValueTy(1020)
  d[TestObjCKeyTy(30)] = TestObjCValueTy(1030)

  let bridged =
    unsafeBitCast(_convertDictionaryToNSDictionary(d), NSDictionary.self)

  assert(isNativeNSDictionary(bridged))

  return bridged
}

func getBridgedEmptyNSDictionary() -> NSDictionary {
  var d = Dictionary<TestObjCKeyTy, TestObjCValueTy>()

  let bridged =
    unsafeBitCast(_convertDictionaryToNSDictionary(d), NSDictionary.self)
  assert(isNativeNSDictionary(bridged))

  return bridged
}

func getBridgedNSDictionaryOfKeyValue_ValueTypesCustomBridged(
  numElements: Int = 3
) -> NSDictionary {
  assert(!_isBridgedVerbatimToObjectiveC(TestBridgedKeyTy.self))
  assert(!_isBridgedVerbatimToObjectiveC(TestBridgedValueTy.self))

  var d = Dictionary<TestBridgedKeyTy, TestBridgedValueTy>()
  for i in 0..<numElements {
    d[TestBridgedKeyTy((i + 1) * 10)] = TestBridgedValueTy((i + 1) * 10 + 1000)
  }

  let bridged = _convertDictionaryToNSDictionary(d)
  assert(isNativeNSDictionary(bridged))

  return bridged
}


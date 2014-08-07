// RUN: %target-build-swift -parse-stdlib -Xfrontend -disable-access-control -module-name a %s -o %t.out
// RUN: %target-run %t.out

import Swift
import StdlibUnittest
import Foundation

var nsObjectCanaryCount = 0
@objc class NSObjectCanary : NSObject {
  override init() {
    ++nsObjectCanaryCount
  }
  deinit {
    --nsObjectCanaryCount
  }
}

struct NSObjectCanaryStruct {
  var ref = NSObjectCanary()
}

var swiftObjectCanaryCount = 0
class SwiftObjectCanary {
  init() {
    ++swiftObjectCanaryCount
  }
  deinit {
    --swiftObjectCanaryCount
  }
}

struct SwiftObjectCanaryStruct {
  var ref = SwiftObjectCanary()
}

@objc class ClassA {
  init(value: Int) {
    self.value = value
  }

  var value: Int
}

struct NotBridgedValueType {
  // Keep it pointer-sized.
  var canaryRef = SwiftObjectCanary()
}

struct BridgedValueType : _ObjectiveCBridgeable {
  init(value: Int) {
    self.value = value
  }

  static func _getObjectiveCType() -> Any.Type {
    return ClassA.self
  }

  func _bridgeToObjectiveC() -> ClassA {
    return ClassA(value: value)
  }

  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }

  static func _forceBridgeFromObjectiveC(
    x: ClassA,
    inout result: BridgedValueType?
  ) {
    assert(x.value % 2 == 0, "not bridged to Objective-C")
    result = BridgedValueType(value: x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    x: ClassA,
    inout result: BridgedValueType?
  ) -> Bool {
    if x.value % 2 == 0 {
      result = BridgedValueType(value: x.value)
      return true
    }

    result = nil
    return false
  }

  var value: Int
  var canaryRef = SwiftObjectCanary()
}

struct BridgedLargeValueType : _ObjectiveCBridgeable {
  init(value: Int) {
    value0 = value
    value1 = value
    value2 = value
    value3 = value
    value4 = value
    value5 = value
    value6 = value
    value7 = value
  }

  static func _getObjectiveCType() -> Any.Type {
    return ClassA.self
  }

  func _bridgeToObjectiveC() -> ClassA {
    assert(value == value0)
    return ClassA(value: value0)
  }

  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }

  static func _forceBridgeFromObjectiveC(
    x: ClassA,
    inout result: BridgedLargeValueType?
  ) {
    assert(x.value % 2 == 0, "not bridged to Objective-C")
    result = BridgedLargeValueType(value: x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    x: ClassA,
    inout result: BridgedLargeValueType?
  ) -> Bool {
    if x.value % 2 == 0 {
      result = BridgedLargeValueType(value: x.value)
      return true
    }

    result = nil
    return false
  }

  var value: Int {
    let x = value0
    assert(value0 == x && value1 == x && value2 == x && value3 == x &&
           value4 == x && value5 == x && value6 == x && value7 == x)
    return x
  }

  var (value0, value1, value2, value3): (Int, Int, Int, Int)
  var (value4, value5, value6, value7): (Int, Int, Int, Int)
  var canaryRef = SwiftObjectCanary()
}


struct ConditionallyBridgedValueType<T> : _ObjectiveCBridgeable {
  init(value: Int) {
    self.value = value
  }

  static func _getObjectiveCType() -> Any.Type {
    return ClassA.self
  }

  func _bridgeToObjectiveC() -> ClassA {
    return ClassA(value: value)
  }

  static func _forceBridgeFromObjectiveC(
    x: ClassA,
    inout result: ConditionallyBridgedValueType?
  ) {
    assert(x.value % 2 == 0, "not bridged from Objective-C")
    result = ConditionallyBridgedValueType(value: x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    x: ClassA,
    inout result: ConditionallyBridgedValueType?
  ) -> Bool {
    if x.value % 2 == 0 {
      result = ConditionallyBridgedValueType(value: x.value)
      return true
    }

    result = nil
    return false
  }

  static func _isBridgedToObjectiveC() -> Bool {
    return ((T.self as Any) as? String.Type) == nil
  }

  var value: Int
  var canaryRef = SwiftObjectCanary()
}

class BridgedVerbatimRefType {
  var value: Int = 42
  var canaryRef = SwiftObjectCanary()
}

func withSwiftObjectCanary<T>(
  createValue: () -> T,
  check: (T) -> (),
  file: String = __FILE__, line: UWord = __LINE__
) {
  let stackTrace = SourceLocStack(SourceLoc(file, line))

  swiftObjectCanaryCount = 0
  if true {
    var valueWithCanary = createValue()
    expectEqual(1, swiftObjectCanaryCount, stackTrace: stackTrace)
    check(valueWithCanary)
  }
  expectEqual(0, swiftObjectCanaryCount, stackTrace: stackTrace)
}

var Runtime = TestCase("Runtime")

Runtime.test("bridgeToObjectiveC") {
  expectEmpty(_bridgeToObjectiveC(NotBridgedValueType()))

  expectEqual(42, (_bridgeToObjectiveC(BridgedValueType(value: 42)) as ClassA).value)

  expectEqual(42, (_bridgeToObjectiveC(BridgedLargeValueType(value: 42)) as ClassA).value)

  expectEqual(42, (_bridgeToObjectiveC(ConditionallyBridgedValueType<Int>(value: 42)) as ClassA).value)

  expectEmpty(_bridgeToObjectiveC(ConditionallyBridgedValueType<String>(value: 42)))

  var bridgedVerbatimRef = BridgedVerbatimRefType()
  expectTrue(_bridgeToObjectiveC(bridgedVerbatimRef) === bridgedVerbatimRef)
}

Runtime.test("bridgeToObjectiveC/NoLeak") {
  withSwiftObjectCanary(
    { NotBridgedValueType() },
    { expectEmpty(_bridgeToObjectiveC($0)) })

  withSwiftObjectCanary(
    { BridgedValueType(value: 42) },
    { expectEqual(42, (_bridgeToObjectiveC($0) as ClassA).value) })

  withSwiftObjectCanary(
    { BridgedLargeValueType(value: 42) },
    { expectEqual(42, (_bridgeToObjectiveC($0) as ClassA).value) })

  withSwiftObjectCanary(
    { ConditionallyBridgedValueType<Int>(value: 42) },
    { expectEqual(42, (_bridgeToObjectiveC($0) as ClassA).value) })

  withSwiftObjectCanary(
    { ConditionallyBridgedValueType<String>(value: 42) },
    { expectEmpty(_bridgeToObjectiveC($0)) })

  withSwiftObjectCanary(
    { BridgedVerbatimRefType() },
    { expectTrue(_bridgeToObjectiveC($0) === $0) })
}

Runtime.test("forceBridgeFromObjectiveC") {
  // Bridge back using NotBridgedValueType.
  expectEmpty(_conditionallyBridgeFromObjectiveC(
      ClassA(value: 21), NotBridgedValueType.self))

  expectEmpty(_conditionallyBridgeFromObjectiveC(
      ClassA(value: 42), NotBridgedValueType.self))

  expectEmpty(_conditionallyBridgeFromObjectiveC(
      BridgedVerbatimRefType(), NotBridgedValueType.self))

  // Bridge back using BridgedValueType.
  expectEmpty(_conditionallyBridgeFromObjectiveC(
      ClassA(value: 21), BridgedValueType.self))

  expectEqual(42, _forceBridgeFromObjectiveC(
      ClassA(value: 42), BridgedValueType.self).value)
  expectEqual(42, _conditionallyBridgeFromObjectiveC(
      ClassA(value: 42), BridgedValueType.self)!.value)

  expectEmpty(_conditionallyBridgeFromObjectiveC(
      BridgedVerbatimRefType(), BridgedValueType.self))

  // Bridge back using BridgedLargeValueType.
  expectEmpty(_conditionallyBridgeFromObjectiveC(
      ClassA(value: 21), BridgedLargeValueType.self))

  expectEqual(42, _forceBridgeFromObjectiveC(
      ClassA(value: 42), BridgedLargeValueType.self).value)
  expectEqual(42, _conditionallyBridgeFromObjectiveC(
      ClassA(value: 42), BridgedLargeValueType.self)!.value)

  expectEmpty(_conditionallyBridgeFromObjectiveC(
      BridgedVerbatimRefType(), BridgedLargeValueType.self))

  // Bridge back using BridgedVerbatimRefType.
  expectEmpty(_conditionallyBridgeFromObjectiveC(
      ClassA(value: 21), BridgedVerbatimRefType.self))

  expectEmpty(_conditionallyBridgeFromObjectiveC(
      ClassA(value: 42), BridgedVerbatimRefType.self))

  var bridgedVerbatimRef = BridgedVerbatimRefType()
  expectTrue(_forceBridgeFromObjectiveC(
      bridgedVerbatimRef, BridgedVerbatimRefType.self) === bridgedVerbatimRef)
  expectTrue(_conditionallyBridgeFromObjectiveC(
      bridgedVerbatimRef, BridgedVerbatimRefType.self)! === bridgedVerbatimRef)
}

Runtime.test("isBridgedToObjectiveC") {
  expectFalse(_isBridgedToObjectiveC(NotBridgedValueType))
  expectTrue(_isBridgedToObjectiveC(BridgedValueType))
  expectTrue(_isBridgedToObjectiveC(BridgedVerbatimRefType))
}

Runtime.test("isBridgedVerbatimToObjectiveC") {
  expectFalse(_isBridgedVerbatimToObjectiveC(NotBridgedValueType))
  expectFalse(_isBridgedVerbatimToObjectiveC(BridgedValueType))
  expectTrue(_isBridgedVerbatimToObjectiveC(BridgedVerbatimRefType))
}

//===---------------------------------------------------------------------===//

// The protocol should be defined in the standard library, otherwise the cast
// does not work.
typealias P1 = BooleanType
typealias P2 = Printable
protocol Q1 {}

// A small struct that can be stored inline in an opaque buffer.
struct StructConformsToP1 : BooleanType, Q1 {
  var boolValue: Bool {
    return true
  }
}

// A small struct that can be stored inline in an opaque buffer.
struct Struct2ConformsToP1<T : BooleanType> : BooleanType, Q1 {
  init(_ value: T) {
    self.value = value
  }
  var boolValue: Bool {
    return value.boolValue
  }
  var value: T
}

// A large struct that can not be stored inline in an opaque buffer.
struct Struct3ConformsToP2 : Printable, Q1 {
  var a: UInt64 = 10
  var b: UInt64 = 20
  var c: UInt64 = 30
  var d: UInt64 = 40

  var description: String {
    // Don't rely on string interpolation, it uses the casts that we are trying
    // to test.
    var result = ""
    result += _uint64ToString(a) + " "
    result += _uint64ToString(b) + " "
    result += _uint64ToString(c) + " "
    result += _uint64ToString(d)
    return result
  }
}

// A large struct that can not be stored inline in an opaque buffer.
struct Struct4ConformsToP2<T : Printable> : Printable, Q1 {
  var value: T
  var e: UInt64 = 50
  var f: UInt64 = 60
  var g: UInt64 = 70
  var h: UInt64 = 80

  init(_ value: T) {
    self.value = value
  }

  var description: String {
    // Don't rely on string interpolation, it uses the casts that we are trying
    // to test.
    var result = value.description + " "
    result += _uint64ToString(e) + " "
    result += _uint64ToString(f) + " "
    result += _uint64ToString(g) + " "
    result += _uint64ToString(h)
    return result
  }
}

struct StructDoesNotConformToP1 : Q1 {}

class ClassConformsToP1 : BooleanType, Q1 {
  var boolValue: Bool {
    return true
  }
}

class Class2ConformsToP1<T : BooleanType> : BooleanType, Q1 {
  init(_ value: T) {
    self.value = [ value ]
  }
  var boolValue: Bool {
    return value[0].boolValue
  }
  // FIXME: should be "var value: T", but we don't support it now.
  var value: Array<T>
}

class ClassDoesNotConformToP1 : Q1 {}

Runtime.test("dynamicCastToExistential1") {
  var someP1Value = StructConformsToP1()
  var someP1Value2 = Struct2ConformsToP1(true)
  var someNotP1Value = StructDoesNotConformToP1()
  var someP2Value = Struct3ConformsToP2()
  var someP2Value2 = Struct4ConformsToP2(Struct3ConformsToP2())
  var someP1Ref = ClassConformsToP1()
  var someP1Ref2 = Class2ConformsToP1(true)
  var someNotP1Ref = ClassDoesNotConformToP1()

  expectTrue(_stdlib_conformsToProtocol(someP1Value, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Value2, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Value, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP2Value, P2.self))
  expectTrue(_stdlib_conformsToProtocol(someP2Value2, P2.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref2, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Ref, P1.self))

  expectTrue(_stdlib_conformsToProtocol(someP1Value as P1, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Value2 as P1, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP2Value as P2, P2.self))
  expectTrue(_stdlib_conformsToProtocol(someP2Value2 as P2, P2.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref as P1, P1.self))

  expectTrue(_stdlib_conformsToProtocol(someP1Value as Q1, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Value2 as Q1, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Value as Q1, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP2Value as Q1, P2.self))
  expectTrue(_stdlib_conformsToProtocol(someP2Value2 as Q1, P2.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref as Q1, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref2 as Q1, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Ref as Q1, P1.self))

  expectTrue(_stdlib_conformsToProtocol(someP1Value as Any, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Value2 as Any, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Value as Any, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP2Value as Any, P2.self))
  expectTrue(_stdlib_conformsToProtocol(someP2Value2 as Any, P2.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref as Any, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref2 as Any, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Ref as Any, P1.self))

  expectTrue(_stdlib_conformsToProtocol(someP1Ref as AnyObject, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref2 as AnyObject, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Ref as AnyObject, P1.self))

  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Value, P1.self).boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Value2, P1.self).boolValue)
  expectEqual("10 20 30 40",
      _stdlib_dynamicCastToExistential1Unconditional(someP2Value, P2.self).description)
  expectEqual("10 20 30 40 50 60 70 80",
      _stdlib_dynamicCastToExistential1Unconditional(someP2Value2, P2.self).description)

  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref, P1.self).boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref2, P1.self).boolValue)

  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Value as Q1, P1.self).boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Value2 as Q1, P1.self).boolValue)
  expectEqual("10 20 30 40",
      _stdlib_dynamicCastToExistential1Unconditional(someP2Value as Q1, P2.self).description)
  expectEqual("10 20 30 40 50 60 70 80",
      _stdlib_dynamicCastToExistential1Unconditional(someP2Value2 as Q1, P2.self).description)
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref as Q1, P1.self).boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref2 as Q1, P1.self).boolValue)

  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Value as Any, P1.self).boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Value2 as Any, P1.self).boolValue)
  expectEqual("10 20 30 40",
      _stdlib_dynamicCastToExistential1Unconditional(someP2Value as Any, P2.self).description)
  expectEqual("10 20 30 40 50 60 70 80",
      _stdlib_dynamicCastToExistential1Unconditional(someP2Value2 as Any, P2.self).description)
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref as Any, P1.self).boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref2 as Any, P1.self).boolValue)

  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref as AnyObject, P1.self).boolValue)

  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value, P1.self)!.boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value2, P1.self)!.boolValue)
  expectEmpty(_stdlib_dynamicCastToExistential1(someNotP1Value, P1.self))
  expectEqual("10 20 30 40",
      _stdlib_dynamicCastToExistential1(someP2Value, P2.self)!.description)
  expectEqual("10 20 30 40 50 60 70 80",
      _stdlib_dynamicCastToExistential1(someP2Value2, P2.self)!.description)
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref, P1.self)!.boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref2, P1.self)!.boolValue)
  expectEmpty(_stdlib_dynamicCastToExistential1(someNotP1Ref, P1.self))

  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value as P1, P1.self)!.boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value2 as P1, P1.self)!.boolValue)
  expectEqual("10 20 30 40",
      _stdlib_dynamicCastToExistential1(someP2Value as P2, P2.self)!.description)
  expectEqual("10 20 30 40 50 60 70 80",
      _stdlib_dynamicCastToExistential1(someP2Value2 as P2, P2.self)!.description)
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref as P1, P1.self)!.boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref2 as P1, P1.self)!.boolValue)

  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value as Q1, P1.self)!.boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value2 as Q1, P1.self)!.boolValue)
  expectEmpty(_stdlib_dynamicCastToExistential1(someNotP1Value as Q1, P1.self))
  expectEqual("10 20 30 40",
      _stdlib_dynamicCastToExistential1(someP2Value as Q1, P2.self)!.description)
  expectEqual("10 20 30 40 50 60 70 80",
      _stdlib_dynamicCastToExistential1(someP2Value2 as Q1, P2.self)!.description)
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref as Q1, P1.self)!.boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref2 as Q1, P1.self)!.boolValue)
  expectEmpty(_stdlib_dynamicCastToExistential1(someNotP1Ref as Q1, P1.self))

  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value as Any, P1.self)!.boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value2 as Any, P1.self)!.boolValue)
  expectEmpty(_stdlib_dynamicCastToExistential1(someNotP1Value as Any, P1.self))
  expectEqual("10 20 30 40",
      _stdlib_dynamicCastToExistential1(someP2Value as Any, P2.self)!.description)
  expectEqual("10 20 30 40 50 60 70 80",
      _stdlib_dynamicCastToExistential1(someP2Value2 as Any, P2.self)!.description)
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref as Any, P1.self)!.boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref2 as Any, P1.self)!.boolValue)
  expectEmpty(_stdlib_dynamicCastToExistential1(someNotP1Ref as Any, P1.self))

  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref as AnyObject, P1.self)!.boolValue)
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref2 as AnyObject, P1.self)!.boolValue)
  expectEmpty(_stdlib_dynamicCastToExistential1(someNotP1Ref as AnyObject, P1.self))
}

class SomeClass {}
@objc class SomeObjCClass {}
class SomeNSObjectSubclass : NSObject {}
struct SomeStruct {}
enum SomeEnum {
  case A
  init() { self = .A }
}

Runtime.test("getTypeName") {
  expectEqual("_TtC1a9SomeClass", _stdlib_getTypeName(SomeClass()))
  expectEqual("_TtC1a13SomeObjCClass", _stdlib_getTypeName(SomeObjCClass()))
  expectEqual("_TtC1a20SomeNSObjectSubclass", _stdlib_getTypeName(SomeNSObjectSubclass()))
  expectEqual("NSObject", _stdlib_getTypeName(NSObject()))
  expectEqual("_TtV1a10SomeStruct", _stdlib_getTypeName(SomeStruct()))
  expectEqual("_TtO1a8SomeEnum", _stdlib_getTypeName(SomeEnum()))

  var a: Any = SomeClass()
  expectEqual("_TtC1a9SomeClass", _stdlib_getTypeName(a))

  a = SomeObjCClass()
  expectEqual("_TtC1a13SomeObjCClass", _stdlib_getTypeName(a))

  a = SomeNSObjectSubclass()
  expectEqual("_TtC1a20SomeNSObjectSubclass", _stdlib_getTypeName(a))

  a = NSObject()
  expectEqual("NSObject", _stdlib_getTypeName(a))

  a = SomeStruct()
  expectEqual("_TtV1a10SomeStruct", _stdlib_getTypeName(a))

  a = SomeEnum()
  expectEqual("_TtO1a8SomeEnum", _stdlib_getTypeName(a))
}

Runtime.test("demangleName") {
  expectEqual("", _stdlib_demangleName(""))
  expectEqual("abc", _stdlib_demangleName("abc"))
  expectEqual("\0", _stdlib_demangleName("\0"))
  expectEqual("Swift.Double", _stdlib_demangleName("_TtSd"))
  expectEqual("x.a : x.Foo<x.Foo<x.Foo<Swift.Int, Swift.Int>, x.Foo<Swift.Int, Swift.Int>>, x.Foo<x.Foo<Swift.Int, Swift.Int>, x.Foo<Swift.Int, Swift.Int>>>",
      _stdlib_demangleName("_Tv1x1aGCS_3FooGS0_GS0_SiSi_GS0_SiSi__GS0_GS0_SiSi_GS0_SiSi___"))
}

Runtime.test("_stdlib_atomicCompareExchangeStrongPtr") {
  typealias IntPtr = UnsafeMutablePointer<Int>
  var origP1 = IntPtr(bitPattern: 0x10101010)
  var origP2 = IntPtr(bitPattern: 0x20202020)
  var origP3 = IntPtr(bitPattern: 0x30303030)

  if true {
    var object = origP1
    var expected = origP1
    let r = _stdlib_atomicCompareExchangeStrongPtr(
      object: &object, expected: &expected, desired: origP2)
    expectTrue(r)
    expectEqual(origP2, object)
    expectEqual(origP1, expected)
  }
  if true {
    var object = origP1
    var expected = origP2
    let r = _stdlib_atomicCompareExchangeStrongPtr(
      object: &object, expected: &expected, desired: origP3)
    expectFalse(r)
    expectEqual(origP1, object)
    expectEqual(origP1, expected)
  }

  struct FooStruct {
    var i: Int
    var object: IntPtr
    var expected: IntPtr

    init(_ object: IntPtr, _ expected: IntPtr) {
      self.i = 0
      self.object = object
      self.expected = expected
    }
  }
  if true {
    var foo = FooStruct(origP1, origP1)
    let r = _stdlib_atomicCompareExchangeStrongPtr(
      object: &foo.object, expected: &foo.expected, desired: origP2)
    expectTrue(r)
    expectEqual(origP2, foo.object)
    expectEqual(origP1, foo.expected)
  }
  if true {
    var foo = FooStruct(origP1, origP2)
    let r = _stdlib_atomicCompareExchangeStrongPtr(
      object: &foo.object, expected: &foo.expected, desired: origP3)
    expectFalse(r)
    expectEqual(origP1, foo.object)
    expectEqual(origP1, foo.expected)
  }
}

var RuntimeFoundationWrappers = TestCase("RuntimeFoundationWrappers")

RuntimeFoundationWrappers.test("_stdlib_NSObject_isEqual/NoLeak") {
  nsObjectCanaryCount = 0
  if true {
    let a = NSObjectCanary()
    let b = NSObjectCanary()
    expectEqual(2, nsObjectCanaryCount)
    _stdlib_NSObject_isEqual(a, b)
  }
  expectEqual(0, nsObjectCanaryCount)
}

var nsStringCanaryCount = 0
@objc class NSStringCanary : NSString {
  override init() {
    ++nsStringCanaryCount
    super.init()
  }
  required init(coder: NSCoder!) {
    fatalError("don't call this initializer")
  }
  deinit {
    --nsStringCanaryCount
  }
  @objc override var length: Int {
    return 0
  }
  @objc override func characterAtIndex(index: Int) -> unichar {
    fatalError("out-of-bounds access")
  }
}

RuntimeFoundationWrappers.test(
  "_stdlib_compareNSStringDeterministicUnicodeCollation/NoLeak"
) {
  nsStringCanaryCount = 0
  if true {
    let a = NSStringCanary()
    let b = NSStringCanary()
    expectEqual(2, nsStringCanaryCount)
    _stdlib_compareNSStringDeterministicUnicodeCollation(a, b)
  }
  expectEqual(0, nsStringCanaryCount)
}

RuntimeFoundationWrappers.test("_stdlib_NSStringNFDHashValue/NoLeak") {
  nsStringCanaryCount = 0
  if true {
    let a = NSStringCanary()
    expectEqual(1, nsStringCanaryCount)
    _stdlib_NSStringNFDHashValue(a)
  }
  expectEqual(0, nsStringCanaryCount)
}

RuntimeFoundationWrappers.test("_stdlib_NSStringHasPrefixNFD/NoLeak") {
  nsStringCanaryCount = 0
  if true {
    let a = NSStringCanary()
    let b = NSStringCanary()
    expectEqual(2, nsStringCanaryCount)
    _stdlib_NSStringHasPrefixNFD(a, b)
  }
  expectEqual(0, nsStringCanaryCount)
}

RuntimeFoundationWrappers.test("_stdlib_NSStringHasSuffixNFD/NoLeak") {
  nsStringCanaryCount = 0
  if true {
    let a = NSStringCanary()
    let b = NSStringCanary()
    expectEqual(2, nsStringCanaryCount)
    _stdlib_NSStringHasSuffixNFD(a, b)
  }
  expectEqual(0, nsStringCanaryCount)
}

var Reflection = TestCase("Reflection")

Reflection.test("dumpToAStream") {
  var output = ""
  dump([ 42, 4242 ], &output)
  expectEqual("▿ 2 elements\n  - [0]: 42\n  - [1]: 4242\n", output)
}

Reflection.test("String.UTF8View/Mirror") {
  // U+0061 LATIN SMALL LETTER A
  // U+304B HIRAGANA LETTER KA
  // U+3099 COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK
  var output = ""
  dump("\u{61}\u{304b}\u{3099}".utf8, &output)

  var expected = ""
  expected += "▿ \u{61}\u{304b}\u{3099}\n"
  expected += "  - [0]: 97\n"
  expected += "  - [1]: 227\n"
  expected += "  - [2]: 129\n"
  expected += "  - [3]: 139\n"
  expected += "  - [4]: 227\n"
  expected += "  - [5]: 130\n"
  expected += "  - [6]: 153\n"

  expectEqual(expected, output)
}

Reflection.test("String.UTF16View/Mirror") {
  // U+0061 LATIN SMALL LETTER A
  // U+304B HIRAGANA LETTER KA
  // U+3099 COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK
  // U+1F425 FRONT-FACING BABY CHICK
  var output = ""
  dump("\u{61}\u{304b}\u{3099}\u{1f425}".utf16, &output)

  var expected = ""
  expected += "▿ \u{61}\u{304b}\u{3099}\u{1f425}\n"
  expected += "  - [0]: 97\n"
  expected += "  - [1]: 12363\n"
  expected += "  - [2]: 12441\n"
  expected += "  - [3]: 55357\n"
  expected += "  - [4]: 56357\n"

  expectEqual(expected, output)
}

Reflection.test("String.UnicodeScalarView/Mirror") {
  // U+0061 LATIN SMALL LETTER A
  // U+304B HIRAGANA LETTER KA
  // U+3099 COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK
  // U+1F425 FRONT-FACING BABY CHICK
  var output = ""
  dump("\u{61}\u{304b}\u{3099}\u{1f425}".unicodeScalars, &output)

  var expected = ""
  expected += "▿ \u{61}\u{304b}\u{3099}\u{1f425}\n"
  expected += "  - [0]: \u{61}\n"
  expected += "  - [1]: \u{304b}\n"
  expected += "  - [2]: \u{3099}\n"
  expected += "  - [3]: \u{1f425}\n"

  expectEqual(expected, output)
}

Reflection.test("CGPoint") {
  var output = ""
  dump(CGPoint(x: 1.25, y: 2.75), &output)

  var expected = ""
  expected += "▿ (1.25,2.75)\n"
  expected += "  - x: 1.25\n"
  expected += "  - y: 2.75\n"

  expectEqual(expected, output)
}

Reflection.test("CGSize") {
  var output = ""
  dump(CGSize(width: 1.25, height: 2.75), &output)

  var expected = ""
  expected += "▿ (1.25,2.75)\n"
  expected += "  - width: 1.25\n"
  expected += "  - height: 2.75\n"

  expectEqual(expected, output)
}

Reflection.test("CGRect") {
  var output = ""
  dump(
    CGRect(
      origin: CGPoint(x: 1.25, y: 2.25),
      size: CGSize(width: 10.25, height: 11.75)),
    &output)

  var expected = ""
  expected += "▿ (1.25,2.25,10.25,11.75)\n"
  expected += "  ▿ origin: (1.25,2.25)\n"
  expected += "    - x: 1.25\n"
  expected += "    - y: 2.25\n"
  expected += "  ▿ size: (10.25,11.75)\n"
  expected += "    - width: 10.25\n"
  expected += "    - height: 11.75\n"

  expectEqual(expected, output)
}

Reflection.test("TupleMirror/NoLeak") {
  if true {
    nsObjectCanaryCount = 0
    if true {
      var tuple = (1, NSObjectCanary())
      expectEqual(1, nsObjectCanaryCount)
      var output = ""
      dump(tuple, &output)
    }
    expectEqual(0, nsObjectCanaryCount)
  }
  if true {
    nsObjectCanaryCount = 0
    if true {
      var tuple = (1, NSObjectCanaryStruct())
      expectEqual(1, nsObjectCanaryCount)
      var output = ""
      dump(tuple, &output)
    }
    expectEqual(0, nsObjectCanaryCount)
  }
  if true {
    swiftObjectCanaryCount = 0
    if true {
      var tuple = (1, SwiftObjectCanary())
      expectEqual(1, swiftObjectCanaryCount)
      var output = ""
      dump(tuple, &output)
    }
    expectEqual(0, swiftObjectCanaryCount)
  }
  if true {
    swiftObjectCanaryCount = 0
    if true {
      var tuple = (1, SwiftObjectCanaryStruct())
      expectEqual(1, swiftObjectCanaryCount)
      var output = ""
      dump(tuple, &output)
    }
    expectEqual(0, swiftObjectCanaryCount)
  }
}

runAllTests()


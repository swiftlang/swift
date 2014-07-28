// RUN: %target-build-swift -parse-stdlib -Xfrontend -disable-access-control -module-name a %s -o %t.out
// RUN: %target-run %t.out | FileCheck %s

import Swift
import StdlibUnittest
import Foundation

@objc class ClassA {
  init(value: Int) {
    self.value = value
  }

  var value: Int
}

struct NotBridgedValueType {
  // Keep it pointer-sized.
  var a: ClassA = ClassA(value: 4242)
}

struct BridgedValueType : _ConditionallyBridgedToObjectiveCType {
  static func _getObjectiveCType() -> Any.Type {
    return ClassA.self
  }

  func _bridgeToObjectiveC() -> ClassA {
    return ClassA(value: value)
  }

  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }

  static func _bridgeFromObjectiveC(x: ClassA) -> BridgedValueType {
    assert(x.value % 2 == 0, "not bridged to Objective-C")
    return BridgedValueType(value: x.value)
  }

  static func _bridgeFromObjectiveCConditional(x: ClassA) -> BridgedValueType? {
    if x.value % 2 == 0 {
      return BridgedValueType(value: x.value)
    }
    return .None
  }

  var value: Int
}

struct BridgedLargeValueType : _ConditionallyBridgedToObjectiveCType {
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

  static func _bridgeFromObjectiveC(x: ClassA) -> BridgedLargeValueType {
    assert(x.value % 2 == 0, "not bridged to Objective-C")
    return BridgedLargeValueType(value: x.value)
  }

  static func _bridgeFromObjectiveCConditional(x: ClassA) -> BridgedLargeValueType? {
    if x.value % 2 == 0 {
      return BridgedLargeValueType(value: x.value)
    }
    return .None
  }

  var value: Int {
    let x = value0
    assert(value0 == x && value1 == x && value2 == x && value3 == x &&
           value4 == x && value5 == x && value6 == x && value7 == x)
    return x
  }

  var (value0, value1, value2, value3): (Int, Int, Int, Int)
  var (value4, value5, value6, value7): (Int, Int, Int, Int)
}


struct ConditionallyBridgedValueType<T>
  : _ConditionallyBridgedToObjectiveCType {
  static func _getObjectiveCType() -> Any.Type {
    return ClassA.self
  }

  func _bridgeToObjectiveC() -> ClassA {
    return ClassA(value: value)
  }

  static func _bridgeFromObjectiveC(x: ClassA) -> ConditionallyBridgedValueType {
    assert(x.value % 2 == 0, "not bridged from Objective-C")
    return ConditionallyBridgedValueType(value: x.value)
  }

  static func _bridgeFromObjectiveCConditional(x: ClassA)
      -> ConditionallyBridgedValueType? {
    if x.value % 2 == 0 {
      return ConditionallyBridgedValueType(value: x.value)
    }
    return .None
  }

  static func _isBridgedToObjectiveC() -> Bool {
    return ((T.self as Any) as? String.Type) == nil
  }

  var value: Int
}

class BridgedVerbatimRefType {}

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

Runtime.test("bridgeFromObjectiveC") {
  // Bridge back using NotBridgedValueType.
  expectEmpty(_bridgeFromObjectiveCConditional(
      ClassA(value: 21), NotBridgedValueType.self))

  expectEmpty(_bridgeFromObjectiveCConditional(
      ClassA(value: 42), NotBridgedValueType.self))

  expectEmpty(_bridgeFromObjectiveCConditional(
      BridgedVerbatimRefType(), NotBridgedValueType.self))

  // Bridge back using BridgedValueType.
  expectEmpty(_bridgeFromObjectiveCConditional(
      ClassA(value: 21), BridgedValueType.self))

  expectEqual(42, _bridgeFromObjectiveC(
      ClassA(value: 42), BridgedValueType.self).value)
  expectEqual(42, _bridgeFromObjectiveCConditional(
      ClassA(value: 42), BridgedValueType.self)!.value)

  expectEmpty(_bridgeFromObjectiveCConditional(
      BridgedVerbatimRefType(), BridgedValueType.self))

  // Bridge back using BridgedLargeValueType.
  expectEmpty(_bridgeFromObjectiveCConditional(
      ClassA(value: 21), BridgedLargeValueType.self))

  expectEqual(42, _bridgeFromObjectiveC(
      ClassA(value: 42), BridgedLargeValueType.self).value)
  expectEqual(42, _bridgeFromObjectiveCConditional(
      ClassA(value: 42), BridgedLargeValueType.self)!.value)

  expectEmpty(_bridgeFromObjectiveCConditional(
      BridgedVerbatimRefType(), BridgedLargeValueType.self))

  // Bridge back using BridgedVerbatimRefType.
  expectEmpty(_bridgeFromObjectiveCConditional(
      ClassA(value: 21), BridgedVerbatimRefType.self))

  expectEmpty(_bridgeFromObjectiveCConditional(
      ClassA(value: 42), BridgedVerbatimRefType.self))

  var bridgedVerbatimRef = BridgedVerbatimRefType()
  expectTrue(_bridgeFromObjectiveC(
      bridgedVerbatimRef, BridgedVerbatimRefType.self) === bridgedVerbatimRef)
  expectTrue(_bridgeFromObjectiveCConditional(
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

Runtime.run()
// CHECK: {{^}}Runtime: All tests passed

var RuntimeFoundationWrappers = TestCase("RuntimeFoundationWrappers")

var nsObjectCanaryCount = 0
@objc class NSObjectCanary : NSObject {
  override init() {
    ++nsObjectCanaryCount
  }
  deinit {
    --nsObjectCanaryCount
  }
}

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
    debugTrap("don't call this initializer")
  }
  deinit {
    --nsStringCanaryCount
  }
  @objc override var length: Int {
    return 0
  }
  @objc override func characterAtIndex(index: Int) -> unichar {
    debugTrap("out-of-bounds access")
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

RuntimeFoundationWrappers.run()
// CHECK: {{^}}RuntimeFoundationWrappers: All tests passed


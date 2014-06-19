// RUN: %target-run-simple-swift | FileCheck %s

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

struct BridgedValueType : _BridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return ClassA.self
  }

  func bridgeToObjectiveC() -> ClassA {
    return ClassA(value: value)
  }

  static func bridgeFromObjectiveC(x: ClassA) -> BridgedValueType? {
    if x.value % 2 == 0 {
      return BridgedValueType(value: x.value)
    }
    return .None
  }

  var value: Int
}

struct BridgedLargeValueType : _BridgedToObjectiveC {
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

  static func getObjectiveCType() -> Any.Type {
    return ClassA.self
  }

  func bridgeToObjectiveC() -> ClassA {
    assert(value == value0)
    return ClassA(value: value0)
  }

  static func bridgeFromObjectiveC(x: ClassA) -> BridgedLargeValueType? {
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


struct ConditionallyBridgedValueType<T> : _ConditionallyBridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return ClassA.self
  }

  func bridgeToObjectiveC() -> ClassA {
    return ClassA(value: value)
  }

  static func bridgeFromObjectiveC(x: ClassA) -> ConditionallyBridgedValueType? {
    if x.value % 2 == 0 {
      return ConditionallyBridgedValueType(value: x.value)
    }
    return .None
  }

  static func bridgeFromObjectiveCConditional(x: ClassA) 
                -> ConditionallyBridgedValueType? {
    if x.value % 2 == 0 {
      return ConditionallyBridgedValueType(value: x.value)
    }
    return .None
  }

  static func isBridgedToObjectiveC() -> Bool {
    return !((T.self as Any) as? String.Type)
  }

  var value: Int
}

class BridgedVerbatimRefType {}

var RuntimeBridging = TestCase("RuntimeBridging")

RuntimeBridging.test("bridgeToObjectiveC") {
  expectEmpty(bridgeToObjectiveC(NotBridgedValueType()))

  expectEqual(42, (bridgeToObjectiveC(BridgedValueType(value: 42)) as ClassA).value)

  expectEqual(42, (bridgeToObjectiveC(BridgedLargeValueType(value: 42)) as ClassA).value)

  expectEqual(42, (bridgeToObjectiveC(ConditionallyBridgedValueType<Int>(value: 42)) as ClassA).value)

  expectEmpty(bridgeToObjectiveC(ConditionallyBridgedValueType<String>(value: 42)))

  var bridgedVerbatimRef = BridgedVerbatimRefType()
  expectTrue(bridgeToObjectiveC(bridgedVerbatimRef) === bridgedVerbatimRef)
}

RuntimeBridging.test("bridgeFromObjectiveC") {
  // Bridge back using NotBridgedValueType.
  expectEmpty(bridgeFromObjectiveC(
      ClassA(value: 21), NotBridgedValueType.self))

  expectEmpty(bridgeFromObjectiveC(
      ClassA(value: 42), NotBridgedValueType.self))

  expectEmpty(bridgeFromObjectiveC(
      BridgedVerbatimRefType(), NotBridgedValueType.self))

  // Bridge back using BridgedValueType.
  expectEmpty(bridgeFromObjectiveC(
      ClassA(value: 21), BridgedValueType.self))

  expectEqual(42, bridgeFromObjectiveC(
      ClassA(value: 42), BridgedValueType.self)!.value)

  expectEmpty(bridgeFromObjectiveC(
      BridgedVerbatimRefType(), BridgedValueType.self))

  // Bridge back using BridgedLargeValueType.
  expectEmpty(bridgeFromObjectiveC(
      ClassA(value: 21), BridgedLargeValueType.self))

  expectEqual(42, bridgeFromObjectiveC(
      ClassA(value: 42), BridgedLargeValueType.self)!.value)

  expectEmpty(bridgeFromObjectiveC(
      BridgedVerbatimRefType(), BridgedLargeValueType.self))

  // Bridge back using BridgedVerbatimRefType.
  expectEmpty(bridgeFromObjectiveC(
      ClassA(value: 21), BridgedVerbatimRefType.self))

  expectEmpty(bridgeFromObjectiveC(
      ClassA(value: 42), BridgedVerbatimRefType.self))

  var bridgedVerbatimRef = BridgedVerbatimRefType()
  expectTrue(bridgeFromObjectiveC(
      bridgedVerbatimRef, BridgedVerbatimRefType.self)! === bridgedVerbatimRef)
}

RuntimeBridging.test("isBridgedToObjectiveC") {
  expectFalse(isBridgedToObjectiveC(NotBridgedValueType))
  expectTrue(isBridgedToObjectiveC(BridgedValueType))
  expectTrue(isBridgedToObjectiveC(BridgedVerbatimRefType))
}

RuntimeBridging.test("isBridgedVerbatimToObjectiveC") {
  expectFalse(isBridgedVerbatimToObjectiveC(NotBridgedValueType))
  expectFalse(isBridgedVerbatimToObjectiveC(BridgedValueType))
  expectTrue(isBridgedVerbatimToObjectiveC(BridgedVerbatimRefType))
}

//===---------------------------------------------------------------------===//

// The protocol should be defined in the standard library, otherwise the cast
// does not work.
typealias P1 = LogicValue
protocol P2 {}

struct StructConformsToP1 : LogicValue, P2 {
  func getLogicValue() -> Bool {
    return true
  }
}

struct Struct2ConformsToP1<T : LogicValue> : LogicValue, P2 {
  init(_ value: T) {
    self.value = value
  }
  func getLogicValue() -> Bool {
    return value.getLogicValue()
  }
  var value: T
}

struct StructDoesNotConformToP1 : P2 {}

class ClassConformsToP1 : LogicValue, P2 {
  func getLogicValue() -> Bool {
    return true
  }
}

class Class2ConformsToP1<T : LogicValue> : LogicValue, P2 {
  init(_ value: T) {
    self.value = [ value ]
  }
  func getLogicValue() -> Bool {
    return value[0].getLogicValue()
  }
  // FIXME: should be "var value: T", but we don't support it now.
  var value: Array<T>
}

class ClassDoesNotConformToP1 : P2 {}

RuntimeBridging.test("dynamicCastToExistential1") {
  var someP1Value = StructConformsToP1()
  var someP1Value2 = Struct2ConformsToP1(true)
  var someNotP1Value = StructDoesNotConformToP1()
  var someP1Ref = ClassConformsToP1()
  var someP1Ref2 = Class2ConformsToP1(true)
  var someNotP1Ref = ClassDoesNotConformToP1()

  expectTrue(_stdlib_conformsToProtocol(someP1Value, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Value2, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Value, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref2, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Ref, P1.self))

  expectTrue(_stdlib_conformsToProtocol(someP1Value as P1, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Value2 as P1, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref as P1, P1.self))

  expectTrue(_stdlib_conformsToProtocol(someP1Value as P2, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Value2 as P2, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Value as P2, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref as P2, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref2 as P2, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Ref as P2, P1.self))

  expectTrue(_stdlib_conformsToProtocol(someP1Value as Any, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Value2 as Any, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Value as Any, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref as Any, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref2 as Any, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Ref as Any, P1.self))

  expectTrue(_stdlib_conformsToProtocol(someP1Ref as AnyObject, P1.self))
  expectTrue(_stdlib_conformsToProtocol(someP1Ref2 as AnyObject, P1.self))
  expectFalse(_stdlib_conformsToProtocol(someNotP1Ref as AnyObject, P1.self))

  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Value, P1.self).getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Value2, P1.self).getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref, P1.self).getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref2, P1.self).getLogicValue())

  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Value as P2, P1.self).getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Value2 as P2, P1.self).getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref as P2, P1.self).getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref2 as P2, P1.self).getLogicValue())

  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Value as Any, P1.self).getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Value2 as Any, P1.self).getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref as Any, P1.self).getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref2 as Any, P1.self).getLogicValue())

  expectTrue(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref as AnyObject, P1.self).getLogicValue())

  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value, P1.self)!.getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value2, P1.self)!.getLogicValue())
  expectEmpty(_stdlib_dynamicCastToExistential1(someNotP1Value, P1.self))
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref, P1.self)!.getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref2, P1.self)!.getLogicValue())
  expectEmpty(_stdlib_dynamicCastToExistential1(someNotP1Ref, P1.self))

  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value as P1, P1.self)!.getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value2 as P1, P1.self)!.getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref as P1, P1.self)!.getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref2 as P1, P1.self)!.getLogicValue())

  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value as P2, P1.self)!.getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value2 as P2, P1.self)!.getLogicValue())
  expectEmpty(_stdlib_dynamicCastToExistential1(someNotP1Value as P2, P1.self))
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref as P2, P1.self)!.getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref2 as P2, P1.self)!.getLogicValue())
  expectEmpty(_stdlib_dynamicCastToExistential1(someNotP1Ref as P2, P1.self))

  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value as Any, P1.self)!.getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Value2 as Any, P1.self)!.getLogicValue())
  expectEmpty(_stdlib_dynamicCastToExistential1(someNotP1Value as Any, P1.self))
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref as Any, P1.self)!.getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref2 as Any, P1.self)!.getLogicValue())
  expectEmpty(_stdlib_dynamicCastToExistential1(someNotP1Ref as Any, P1.self))

  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref as AnyObject, P1.self)!.getLogicValue())
  expectTrue(_stdlib_dynamicCastToExistential1(someP1Ref2 as AnyObject, P1.self)!.getLogicValue())
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

RuntimeBridging.test("getTypeName") {
  expectEqual("C1a9SomeClass", _stdlib_getTypeName(SomeClass()))
  expectEqual("C1a13SomeObjCClass", _stdlib_getTypeName(SomeObjCClass()))
  expectEqual("C1a20SomeNSObjectSubclass", _stdlib_getTypeName(SomeNSObjectSubclass()))
  expectEqual("NSObject", _stdlib_getTypeName(NSObject()))
  expectEqual("V1a10SomeStruct", _stdlib_getTypeName(SomeStruct()))
  expectEqual("O1a8SomeEnum", _stdlib_getTypeName(SomeEnum()))

  var a: Any = SomeClass()
  expectEqual("C1a9SomeClass", _stdlib_getTypeName(a))

  a = SomeObjCClass()
  expectEqual("C1a13SomeObjCClass", _stdlib_getTypeName(a))

  a = SomeNSObjectSubclass()
  expectEqual("C1a20SomeNSObjectSubclass", _stdlib_getTypeName(a))

  a = NSObject()
  expectEqual("NSObject", _stdlib_getTypeName(a))

  a = SomeStruct()
  expectEqual("V1a10SomeStruct", _stdlib_getTypeName(a))

  a = SomeEnum()
  expectEqual("O1a8SomeEnum", _stdlib_getTypeName(a))
}

RuntimeBridging.run()
// CHECK: {{^}}RuntimeBridging: All tests passed


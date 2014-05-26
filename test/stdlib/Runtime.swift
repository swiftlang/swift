// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

func assertEquals(
    expected: String, actual: String,
    file: StaticString = __FILE__, line: UWord = __LINE__
) {
  if expected != actual {
    println("expected: \"\(expected)\"")
    println("actual: \"\(actual)\"")
    assert(expected == actual, file: file, line: line)
  }
}

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

  static func isBridgedToObjectiveC() -> Bool {
    return !((T.self as Any) as? String.Type)
  }

  var value: Int
}

class BridgedVerbatimRefType {}

func test_bridgeToObjectiveC() {
  assert(!bridgeToObjectiveC(NotBridgedValueType()))

  assert((bridgeToObjectiveC(BridgedValueType(value: 42)) as ClassA).value == 42)

  assert((bridgeToObjectiveC(BridgedLargeValueType(value: 42)) as ClassA).value == 42)

  assert((bridgeToObjectiveC(ConditionallyBridgedValueType<Int>(value: 42)) as ClassA).value == 42)

  assert(!bridgeToObjectiveC(ConditionallyBridgedValueType<String>(value: 42)))

  var bridgedVerbatimRef = BridgedVerbatimRefType()
  assert(bridgeToObjectiveC(bridgedVerbatimRef) === bridgedVerbatimRef)

  println("test_bridgeToObjectiveC done")
}
test_bridgeToObjectiveC()
// CHECK: test_bridgeToObjectiveC done

func test_bridgeFromObjectiveC() {

  // Bridge back using NotBridgedValueType.
  assert(!bridgeFromObjectiveC(
      ClassA(value: 21), NotBridgedValueType.self))

  assert(!bridgeFromObjectiveC(
      ClassA(value: 42), NotBridgedValueType.self))

  assert(!bridgeFromObjectiveC(
      BridgedVerbatimRefType(), NotBridgedValueType.self))

  // Bridge back using BridgedValueType.
  assert(!bridgeFromObjectiveC(
      ClassA(value: 21), BridgedValueType.self))

  assert(bridgeFromObjectiveC(
      ClassA(value: 42), BridgedValueType.self)!.value == 42)

  assert(!bridgeFromObjectiveC(
      BridgedVerbatimRefType(), BridgedValueType.self))

  // Bridge back using BridgedLargeValueType.
  assert(!bridgeFromObjectiveC(
      ClassA(value: 21), BridgedLargeValueType.self))

  assert(bridgeFromObjectiveC(
      ClassA(value: 42), BridgedLargeValueType.self)!.value == 42)

  assert(!bridgeFromObjectiveC(
      BridgedVerbatimRefType(), BridgedLargeValueType.self))

  // Bridge back using BridgedVerbatimRefType.
  assert(!bridgeFromObjectiveC(
      ClassA(value: 21), BridgedVerbatimRefType.self))

  assert(!bridgeFromObjectiveC(
      ClassA(value: 42), BridgedVerbatimRefType.self))

  var bridgedVerbatimRef = BridgedVerbatimRefType()
  assert(bridgeFromObjectiveC(
      bridgedVerbatimRef, BridgedVerbatimRefType.self)! === bridgedVerbatimRef)

  println("test_bridgeFromObjectiveC done")
}
test_bridgeFromObjectiveC()
// CHECK: test_bridgeFromObjectiveC done

func test_isBridgedToObjectiveC() {
  assert(!isBridgedToObjectiveC(NotBridgedValueType))
  assert(isBridgedToObjectiveC(BridgedValueType))
  assert(isBridgedToObjectiveC(BridgedVerbatimRefType))

  println("test_isBridgedToObjectiveC done")
}
test_isBridgedToObjectiveC()
// CHECK: test_isBridgedToObjectiveC done

func test_isBridgedVerbatimToObjectiveC() {
  assert(!isBridgedVerbatimToObjectiveC(NotBridgedValueType))
  assert(!isBridgedVerbatimToObjectiveC(BridgedValueType))
  assert(isBridgedVerbatimToObjectiveC(BridgedVerbatimRefType))

  println("test_isBridgedVerbatimToObjectiveC done")
}
test_isBridgedVerbatimToObjectiveC()
// CHECK: test_isBridgedVerbatimToObjectiveC done

//===---------------------------------------------------------------------===//

protocol P2 {}
func test_dynamicCastToExistential1() {
  // The protocol should be defined in the standard library, otherwise the cast
  // does not work.

  typealias P1 = LogicValue

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

  var someP1Value = StructConformsToP1()
  var someP1Value2 = Struct2ConformsToP1(true)
  var someNotP1Value = StructDoesNotConformToP1()
  var someP1Ref = ClassConformsToP1()
  var someP1Ref2 = Class2ConformsToP1(true)
  var someNotP1Ref = ClassDoesNotConformToP1()

  assert(_stdlib_conformsToProtocol(someP1Value, P1.self))
  assert(_stdlib_conformsToProtocol(someP1Value2, P1.self))
  assert(!_stdlib_conformsToProtocol(someNotP1Value, P1.self))
  assert(_stdlib_conformsToProtocol(someP1Ref, P1.self))
  assert(_stdlib_conformsToProtocol(someP1Ref2, P1.self))
  assert(!_stdlib_conformsToProtocol(someNotP1Ref, P1.self))

  assert(_stdlib_conformsToProtocol(someP1Value as P1, P1.self))
  assert(_stdlib_conformsToProtocol(someP1Value2 as P1, P1.self))
  assert(_stdlib_conformsToProtocol(someP1Ref as P1, P1.self))

  assert(_stdlib_conformsToProtocol(someP1Value as P2, P1.self))
  assert(_stdlib_conformsToProtocol(someP1Value2 as P2, P1.self))
  assert(!_stdlib_conformsToProtocol(someNotP1Value as P2, P1.self))
  assert(_stdlib_conformsToProtocol(someP1Ref as P2, P1.self))
  assert(_stdlib_conformsToProtocol(someP1Ref2 as P2, P1.self))
  assert(!_stdlib_conformsToProtocol(someNotP1Ref as P2, P1.self))

  assert(_stdlib_conformsToProtocol(someP1Value as Any, P1.self))
  assert(_stdlib_conformsToProtocol(someP1Value2 as Any, P1.self))
  assert(!_stdlib_conformsToProtocol(someNotP1Value as Any, P1.self))
  assert(_stdlib_conformsToProtocol(someP1Ref as Any, P1.self))
  assert(_stdlib_conformsToProtocol(someP1Ref2 as Any, P1.self))
  assert(!_stdlib_conformsToProtocol(someNotP1Ref as Any, P1.self))

  assert(_stdlib_conformsToProtocol(someP1Ref as AnyObject, P1.self))
  assert(_stdlib_conformsToProtocol(someP1Ref2 as AnyObject, P1.self))
  assert(!_stdlib_conformsToProtocol(someNotP1Ref as AnyObject, P1.self))

  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Value, P1.self).getLogicValue())
  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Value2, P1.self).getLogicValue())
  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref, P1.self).getLogicValue())
  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref2, P1.self).getLogicValue())

  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Value as P2, P1.self).getLogicValue())
  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Value2 as P2, P1.self).getLogicValue())
  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref as P2, P1.self).getLogicValue())
  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref2 as P2, P1.self).getLogicValue())

  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Value as Any, P1.self).getLogicValue())
  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Value2 as Any, P1.self).getLogicValue())
  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref as Any, P1.self).getLogicValue())
  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref2 as Any, P1.self).getLogicValue())

  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref as AnyObject, P1.self).getLogicValue())

  assert(_stdlib_dynamicCastToExistential1(someP1Value, P1.self)!.getLogicValue())
  assert(_stdlib_dynamicCastToExistential1(someP1Value2, P1.self)!.getLogicValue())
  assert(!_stdlib_dynamicCastToExistential1(someNotP1Value, P1.self))
  assert(_stdlib_dynamicCastToExistential1(someP1Ref, P1.self)!.getLogicValue())
  assert(_stdlib_dynamicCastToExistential1(someP1Ref2, P1.self)!.getLogicValue())
  assert(!_stdlib_dynamicCastToExistential1(someNotP1Ref, P1.self))

  assert(_stdlib_dynamicCastToExistential1(someP1Value as P1, P1.self)!.getLogicValue())
  assert(_stdlib_dynamicCastToExistential1(someP1Value2 as P1, P1.self)!.getLogicValue())
  assert(_stdlib_dynamicCastToExistential1(someP1Ref as P1, P1.self)!.getLogicValue())
  assert(_stdlib_dynamicCastToExistential1(someP1Ref2 as P1, P1.self)!.getLogicValue())

  assert(_stdlib_dynamicCastToExistential1(someP1Value as P2, P1.self)!.getLogicValue())
  assert(_stdlib_dynamicCastToExistential1(someP1Value2 as P2, P1.self)!.getLogicValue())
  assert(!_stdlib_dynamicCastToExistential1(someNotP1Value as P2, P1.self))
  assert(_stdlib_dynamicCastToExistential1(someP1Ref as P2, P1.self)!.getLogicValue())
  assert(_stdlib_dynamicCastToExistential1(someP1Ref2 as P2, P1.self)!.getLogicValue())
  assert(!_stdlib_dynamicCastToExistential1(someNotP1Ref as P2, P1.self))

  assert(_stdlib_dynamicCastToExistential1(someP1Value as Any, P1.self)!.getLogicValue())
  assert(_stdlib_dynamicCastToExistential1(someP1Value2 as Any, P1.self)!.getLogicValue())
  assert(!_stdlib_dynamicCastToExistential1(someNotP1Value as Any, P1.self))
  assert(_stdlib_dynamicCastToExistential1(someP1Ref as Any, P1.self)!.getLogicValue())
  assert(_stdlib_dynamicCastToExistential1(someP1Ref2 as Any, P1.self)!.getLogicValue())
  assert(!_stdlib_dynamicCastToExistential1(someNotP1Ref as Any, P1.self))

  assert(_stdlib_dynamicCastToExistential1(someP1Ref as AnyObject, P1.self)!.getLogicValue())
  assert(_stdlib_dynamicCastToExistential1(someP1Ref2 as AnyObject, P1.self)!.getLogicValue())
  assert(!_stdlib_dynamicCastToExistential1(someNotP1Ref as AnyObject, P1.self))

  println("test_dynamicCastToExistential1 done")
}
test_dynamicCastToExistential1()
// CHECK: test_dynamicCastToExistential1 done

class SomeClass {}
@objc class SomeObjCClass {}
class SomeNSObjectSubclass : NSObject {}
struct SomeStruct {}
enum SomeEnum {
  case A
  init() { self = .A }
}

func test_getTypeName() {
  assertEquals("C1a9SomeClass", _stdlib_getTypeName(SomeClass()))
  assertEquals("C1a13SomeObjCClass", _stdlib_getTypeName(SomeObjCClass()))
  assertEquals("C1a20SomeNSObjectSubclass", _stdlib_getTypeName(SomeNSObjectSubclass()))
  assertEquals("NSObject", _stdlib_getTypeName(NSObject()))
  assertEquals("V1a10SomeStruct", _stdlib_getTypeName(SomeStruct()))
  assertEquals("O1a8SomeEnum", _stdlib_getTypeName(SomeEnum()))

  var a: Any = SomeClass()
  assertEquals("C1a9SomeClass", _stdlib_getTypeName(a))

  a = SomeObjCClass()
  assertEquals("C1a13SomeObjCClass", _stdlib_getTypeName(a))

  a = SomeNSObjectSubclass()
  assertEquals("C1a20SomeNSObjectSubclass", _stdlib_getTypeName(a))

  a = NSObject()
  assertEquals("NSObject", _stdlib_getTypeName(a))

  a = SomeStruct()
  assertEquals("V1a10SomeStruct", _stdlib_getTypeName(a))

  a = SomeEnum()
  assertEquals("O1a8SomeEnum", _stdlib_getTypeName(a))

  println("test_getTypeName done")
}
test_getTypeName()
// CHECK: test_getTypeName done


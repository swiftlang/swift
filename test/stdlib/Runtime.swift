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

  static func bridgeFromObjectiveC(x: ClassA) -> BridgedValueType {
    return BridgedValueType(value: x.value)
  }

  var value: Int
}

struct ConditionallyBridgedValueType<T> : _ConditionallyBridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type {
    return ClassA.self
  }

  func bridgeToObjectiveC() -> ClassA {
    return ClassA(value: value)
  }

  static func bridgeFromObjectiveC(x: ClassA) -> ConditionallyBridgedValueType {
    return ConditionallyBridgedValueType(value: x.value)
  }

  static func isBridgedToObjectiveC() -> Bool {
    return !((T.self as Any) as String.Type)
  }

  var value: Int
}

class BridgedVerbatimRefType {}

class BridgedRefType : _BridgedToObjectiveC {
  init(value: Int) {
    self.value = value
  }

  class func getObjectiveCType() -> Any.Type {
    return ClassA.self
  }

  func bridgeToObjectiveC() -> ClassA {
    return ClassA(value: value)
  }

  class func bridgeFromObjectiveC(x: ClassA) -> BridgedRefType {
    return BridgedRefType(value: x.value)
  }

  var value: Int
}

class ConditionallyBridgedRefType<T> : _ConditionallyBridgedToObjectiveC {
  init(value: Int) {
    self.value = value
  }

  class func getObjectiveCType() -> Any.Type {
    return ClassA.self
  }

  func bridgeToObjectiveC() -> ClassA {
    return ClassA(value: value)
  }

  class func bridgeFromObjectiveC(x: ClassA) -> ConditionallyBridgedRefType {
    return ConditionallyBridgedRefType(value: x.value)
  }

  class func isBridgedToObjectiveC() -> Bool {
    return !((T.self as Any) as String.Type)
  }

  var value: Int
}

func test_bridgeToObjectiveC() {
  assert(!bridgeToObjectiveC(NotBridgedValueType()))

  assert((bridgeToObjectiveC(BridgedValueType(value: 42)) as ClassA)!.value == 42)

  assert((bridgeToObjectiveC(ConditionallyBridgedValueType<Int>(value: 42)) as ClassA)!.value == 42)

  assert(!bridgeToObjectiveC(ConditionallyBridgedValueType<String>(value: 42)))

  var bridgedVerbatimRef = BridgedVerbatimRefType()
  assert(bridgeToObjectiveC(bridgedVerbatimRef) === bridgedVerbatimRef)

  assert((bridgeToObjectiveC(BridgedRefType(value: 42)) as ClassA)!.value == 42)

  assert((bridgeToObjectiveC(ConditionallyBridgedRefType<Int>(value: 42)) as ClassA)!.value == 42)

  assert(!bridgeToObjectiveC(ConditionallyBridgedRefType<String>(value: 42)))

  println("test_bridgeToObjectiveC done")
}
test_bridgeToObjectiveC()
// CHECK: test_bridgeToObjectiveC done

func test_bridgeFromObjectiveC() {

  // Bridge back using NotBridgedValueType.
  assert(!bridgeFromObjectiveC(
      ClassA(value: 42), NotBridgedValueType.self))

  assert(!bridgeFromObjectiveC(
      BridgedVerbatimRefType(), NotBridgedValueType.self))

  assert(!bridgeFromObjectiveC(
      BridgedRefType(value: 42), NotBridgedValueType.self))

  assert(!bridgeFromObjectiveC(
      ConditionallyBridgedRefType<Int>(value: 42), NotBridgedValueType.self))

  assert(!bridgeFromObjectiveC(
      ConditionallyBridgedRefType<String>(value: 42), NotBridgedValueType.self))

  // Bridge back using BridgedValueType.
  assert(bridgeFromObjectiveC(
      ClassA(value: 42), BridgedValueType.self)!.value == 42)

  assert(!bridgeFromObjectiveC(
      BridgedVerbatimRefType(), BridgedValueType.self))

  assert(!bridgeFromObjectiveC(
      BridgedRefType(value: 42), BridgedValueType.self))

  assert(!bridgeFromObjectiveC(
      ConditionallyBridgedRefType<Int>(value: 42), BridgedValueType.self))

  assert(!bridgeFromObjectiveC(
      ConditionallyBridgedRefType<String>(value: 42), BridgedValueType.self))

  // Bridge back using BridgedVerbatimRefType.
  assert(!bridgeFromObjectiveC(
      ClassA(value: 42), BridgedVerbatimRefType.self))

  var bridgedVerbatimRef = BridgedVerbatimRefType()
  assert(bridgeFromObjectiveC(
      bridgedVerbatimRef, BridgedVerbatimRefType.self)! === bridgedVerbatimRef)

  assert(!bridgeFromObjectiveC(
      BridgedRefType(value: 42), BridgedVerbatimRefType.self))

  assert(!bridgeFromObjectiveC(
      ConditionallyBridgedRefType<Int>(value: 42), BridgedVerbatimRefType.self))

  assert(!bridgeFromObjectiveC(
      ConditionallyBridgedRefType<String>(value: 42), BridgedVerbatimRefType.self))

  // Bridge back using BridgedRefType.
  assert(bridgeFromObjectiveC(
      ClassA(value: 42), BridgedRefType.self)!.value == 42)

  assert(!bridgeFromObjectiveC(
      BridgedVerbatimRefType(), BridgedRefType.self))

  assert(!bridgeFromObjectiveC(
      BridgedRefType(value: 42), BridgedRefType.self))

  assert(!bridgeFromObjectiveC(
      ConditionallyBridgedRefType<Int>(value: 42), BridgedRefType.self))

  assert(!bridgeFromObjectiveC(
      ConditionallyBridgedRefType<String>(value: 42), BridgedRefType.self))

  // Bridge back using ConditionallyBridgedRefType<Int>.
  assert(bridgeFromObjectiveC(
      ClassA(value: 42), ConditionallyBridgedRefType<Int>.self)!.value == 42)

  assert(!bridgeFromObjectiveC(
      BridgedVerbatimRefType(), ConditionallyBridgedRefType<Int>.self))

  assert(!bridgeFromObjectiveC(
      BridgedRefType(value: 42), ConditionallyBridgedRefType<Int>.self))

  assert(!bridgeFromObjectiveC(
      ConditionallyBridgedRefType<Int>(value: 42), ConditionallyBridgedRefType<Int>.self))

  assert(!bridgeFromObjectiveC(
      ConditionallyBridgedRefType<String>(value: 42), ConditionallyBridgedRefType<Int>.self))

  // Bridge back using ConditionallyBridgedRefType<String>.
  assert(!bridgeFromObjectiveC(
      ClassA(value: 42), ConditionallyBridgedRefType<String>.self))

  assert(!bridgeFromObjectiveC(
      BridgedVerbatimRefType(), ConditionallyBridgedRefType<String>.self))

  assert(!bridgeFromObjectiveC(
      BridgedRefType(value: 42), ConditionallyBridgedRefType<String>.self))

  assert(!bridgeFromObjectiveC(
      ConditionallyBridgedRefType<String>(value: 42), ConditionallyBridgedRefType<String>.self))

  assert(!bridgeFromObjectiveC(
      ConditionallyBridgedRefType<String>(value: 42), ConditionallyBridgedRefType<String>.self))

  println("test_bridgeFromObjectiveC done")
}
test_bridgeFromObjectiveC()
// CHECK: test_bridgeFromObjectiveC done

func test_isBridgedToObjectiveC() {
  assert(!isBridgedToObjectiveC(NotBridgedValueType))
  assert(isBridgedToObjectiveC(BridgedValueType))
  assert(isBridgedToObjectiveC(BridgedVerbatimRefType))
  assert(isBridgedToObjectiveC(BridgedRefType))

  println("test_isBridgedToObjectiveC done")
}
test_isBridgedToObjectiveC()
// CHECK: test_isBridgedToObjectiveC done

func test_isBridgedVerbatimToObjectiveC() {
  assert(!isBridgedVerbatimToObjectiveC(NotBridgedValueType))
  assert(!isBridgedVerbatimToObjectiveC(BridgedValueType))
  assert(isBridgedVerbatimToObjectiveC(BridgedVerbatimRefType))
  assert(!isBridgedVerbatimToObjectiveC(BridgedRefType))

  println("test_isBridgedVerbatimToObjectiveC done")
}
test_isBridgedVerbatimToObjectiveC()
// CHECK: test_isBridgedVerbatimToObjectiveC done

//===---------------------------------------------------------------------===//

func test_dynamicCastToExistential1() {
  // The protocol should be defined in the standard library, otherwise the cast
  // does not work.

  typealias P1 = LogicValue

  struct StructConformsToP1 : LogicValue {
    func getLogicValue() -> Bool {
      return true
    }
  }

  struct StructDoesNotConformToP1 {}

  class ClassConformsToP1 : LogicValue {
    func getLogicValue() -> Bool {
      return true
    }
  }

  class ClassDoesNotConformToP1 {}

  var someP1Value = StructConformsToP1()
  var someNotP1Value = StructDoesNotConformToP1()
  var someP1Ref = ClassConformsToP1()
  var someNotP1Ref = ClassDoesNotConformToP1()

  assert(_stdlib_conformsToProtocol(someP1Value, P1.self))
  assert(!_stdlib_conformsToProtocol(someNotP1Value, P1.self))
  assert(_stdlib_conformsToProtocol(someP1Ref, P1.self))
  assert(!_stdlib_conformsToProtocol(someNotP1Ref, P1.self))

  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Value, P1.self).getLogicValue())
  assert(_stdlib_dynamicCastToExistential1Unconditional(someP1Ref, P1.self).getLogicValue())

  assert(_stdlib_dynamicCastToExistential1(someP1Value, P1.self)!.getLogicValue())
  assert(!_stdlib_dynamicCastToExistential1(someNotP1Value, P1.self))
  assert(_stdlib_dynamicCastToExistential1(someP1Ref, P1.self)!.getLogicValue())
  assert(!_stdlib_dynamicCastToExistential1(someNotP1Ref, P1.self))

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


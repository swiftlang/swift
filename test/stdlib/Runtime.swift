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

@objc class ClassA {}

struct NotBridgedValueType {
  // Keep it pointer-sized.
  var a: ClassA = ClassA()
}

struct BridgedValueType : _BridgedToObjectiveC {
  func bridgeToObjectiveC() -> ClassA {
    return ClassA()
  }
}

class BridgedVerbatimRefType {}

class BridgedRefType : _BridgedToObjectiveC {
  func bridgeToObjectiveC() -> ClassA {
    return ClassA()
  }
}

func test_bridgeToObjectiveC() {
  assert(!bridgeToObjectiveC(NotBridgedValueType()))

  assert(bridgeToObjectiveC(BridgedValueType()) as ClassA)

  var bridgedVerbatimRef = BridgedVerbatimRefType()
  assert(bridgeToObjectiveC(bridgedVerbatimRef) === bridgedVerbatimRef)

  assert(bridgeToObjectiveC(BridgedRefType()) as ClassA)

  println("test_bridgeToObjectiveC done")
}
test_bridgeToObjectiveC()
// CHECK: test_bridgeToObjectiveC done

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


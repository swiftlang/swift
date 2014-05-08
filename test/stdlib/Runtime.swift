// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

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


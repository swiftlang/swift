// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import ObjectiveC
import StdlibUnittest


var ObjectiveCTests = TestSuite("ObjectiveC")

class NSObjectWithCustomHashable : NSObject {
  var _value: Int
  var _hashValue: Int

  init(value: Int, hashValue: Int) {
    self._value = value
    self._hashValue = hashValue
  }

  override func isEqual(_ other: Any?) -> Bool {
    let other_ = other as! NSObjectWithCustomHashable
    return self._value == other_._value
  }

  override var hash: Int {
    return _hashValue
  }
}

ObjectiveCTests.test("NSObject/Hashable") {
  let instances: [(order: Int, hashOrder: Int, object: NSObject)] = [
    (10, 1, NSObjectWithCustomHashable(value: 10, hashValue: 100)),
    (10, 1, NSObjectWithCustomHashable(value: 10, hashValue: 100)),
    (20, 1, NSObjectWithCustomHashable(value: 20, hashValue: 100)),
    (30, 2, NSObjectWithCustomHashable(value: 30, hashValue: 300)),
  ]
  checkHashable(
    instances.map { $0.object },
    equalityOracle: { instances[$0].order == instances[$1].order },
    hashEqualityOracle: { instances[$0].hashOrder == instances[$1].hashOrder })
}

runAllTests()


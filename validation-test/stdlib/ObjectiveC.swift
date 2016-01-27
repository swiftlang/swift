// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import ObjectiveC
import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

var ObjectiveCTests = TestSuite("ObjectiveC")

class NSObjectWithCustomHashable : NSObject {
  init(value: Int, hashValue: Int) {
    self._value = value
    self._hashValue = hashValue
  }

  override func isEqual(other: AnyObject?) -> Bool {
    let other_ = other as! NSObjectWithCustomHashable
    return self._value == other_._value
  }

  override var hashValue: Int {
    return _hashValue
  }

  var _value: Int
  var _hashValue: Int
}

ObjectiveCTests.test("NSObject/Hashable") {
  let objects = [
    NSObjectWithCustomHashable(value: 10, hashValue: 100),
    NSObjectWithCustomHashable(value: 10, hashValue: 100),
    NSObjectWithCustomHashable(value: 20, hashValue: 100),
    NSObjectWithCustomHashable(value: 30, hashValue: 300),
  ]
  for (i, object1) in objects.enumerate() {
    for (j, object2) in objects.enumerate() {
      checkHashable(
        object1._value == object2._value,
        object1,
        object2,
        "i=\(i), j=\(j)")
    }
  }
}

runAllTests()


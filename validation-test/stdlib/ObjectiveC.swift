// RUN: %target-run-simple-swift

// REQUIRES: objc_interop

import ObjectiveC
import StdlibUnittest

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
        SourceLocStack().withCurrentLoc()) {
        "i=\(i), j=\(j)"
      }
    }
  }
}

runAllTests()


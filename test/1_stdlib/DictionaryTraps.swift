// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out_Debug
// RUN: %target-build-swift %s -o %t/a.out_Release -O
//
// RUN: %target-run %t/a.out_Debug
// RUN: %target-run %t/a.out_Release
//
// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import StdlibUnittest
import Foundation

struct NotBridgedKeyTy : Equatable, Hashable {
  init(_ value: Int) {
    self.value = value
  }
  var hashValue: Int {
    return value
  }
  var value: Int
}

func == (lhs: NotBridgedKeyTy, rhs: NotBridgedKeyTy) -> Bool {
  return lhs.value == rhs.value
}

assert(!_isBridgedToObjectiveC(NotBridgedKeyTy.self))

struct NotBridgedValueTy {}

assert(!_isBridgedToObjectiveC(NotBridgedValueTy.self))

class BridgedVerbatimRefTy : Equatable, Hashable {
  init(_ value: Int) {
    self.value = value
  }
  var hashValue: Int {
    return value
  }
  var value: Int
}

func == (lhs: BridgedVerbatimRefTy, rhs: BridgedVerbatimRefTy) -> Bool {
  return lhs.value == rhs.value
}

assert(_isBridgedToObjectiveC(BridgedVerbatimRefTy.self))
assert(_isBridgedVerbatimToObjectiveC(BridgedVerbatimRefTy.self))

var DictionaryTraps = TestSuite("DictionaryTraps")

DictionaryTraps.test("sanity") {
  // Sanity checks.  This code should not trap.
  var d = Dictionary<BridgedVerbatimRefTy, BridgedVerbatimRefTy>()
  var nsd = d as NSDictionary
}

DictionaryTraps.test("DuplicateKeys1") {
  expectCrashLater()
  let d = Dictionary(dictionaryLiteral:
    (10, 1010), (20, 1020), (30, 1030), (10, 0))
  _blackHole(d)
}

DictionaryTraps.test("DuplicateKeys2") {
  expectCrashLater()
  let d = Dictionary(dictionaryLiteral:
    (10, 1010), (20, 1020), (30, 1030), (10, 1010))
  _blackHole(d)
}

DictionaryTraps.test("DuplicateKeys3") {
  expectCrashLater()
  let d = [ 10: 1010, 10: 0 ]
  _blackHole(d)
}

DictionaryTraps.test("RemoveInvalidIndex1") {
  var d = Dictionary<Int, Int>()
  let index = d.startIndex
  expectCrashLater()
  d.removeAtIndex(index)
}

DictionaryTraps.test("RemoveInvalidIndex2") {
  var d = Dictionary<Int, Int>()
  let index = d.endIndex
  expectCrashLater()
  d.removeAtIndex(index)
}

DictionaryTraps.test("RemoveInvalidIndex3") {
  var d = [ 10: 1010, 20: 1020, 30: 1030 ]
  let index = d.endIndex
  expectCrashLater()
  d.removeAtIndex(index)
}

DictionaryTraps.test("RemoveInvalidIndex4") {
  var d = [ 10: 1010 ]
  let index = d.indexForKey(10)!
  d.removeAtIndex(index)
  expectEmpty(d[10])
  expectCrashLater()
  d.removeAtIndex(index)
}

class TestObjCKeyTy : NSObject {
  init(_ value: Int) {
    self.value = value
  }

  override func isEqual(object: AnyObject!) -> Bool {
    if let other: AnyObject = object {
      if let otherObjcKey = other as? TestObjCKeyTy {
        return self.value == otherObjcKey.value
      }
    }
    return false
  }

  override var hash : Int {
    return value
  }

  var value: Int
}

struct TestBridgedKeyTy : Hashable, _ObjectiveCBridgeable {
  static func _isBridgedToObjectiveC() -> Bool {
    return true
  }

  init(_ value: Int) { self.value = value }

  var hashValue: Int { return value }

  static func _getObjectiveCType() -> Any.Type {
    return TestObjCKeyTy.self
  }

  func _bridgeToObjectiveC() -> TestObjCKeyTy {
    return TestObjCKeyTy(value)
  }

  static func _forceBridgeFromObjectiveC(
    x: TestObjCKeyTy,
    inout result: TestBridgedKeyTy?
  ) {
    result = TestBridgedKeyTy(x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    x: TestObjCKeyTy,
    inout result: TestBridgedKeyTy?
  ) -> Bool {
    result = TestBridgedKeyTy(x.value)
    return true
  }

  var value: Int
}

func ==(x: TestBridgedKeyTy, y: TestBridgedKeyTy) -> Bool {
  return x.value == y.value
}

DictionaryTraps.test("BridgedKeyIsNotNSCopyable1")
  .crashOutputMatches("unrecognized selector sent to instance").code {
  // This Dictionary is bridged in O(1).
  var d = [ TestObjCKeyTy(10): NSObject() ]
  var nsd = d as NSDictionary
  expectCrashLater()
  nsd.mutableCopy()
}

DictionaryTraps.test("BridgedKeyIsNotNSCopyable2") {
  // This Dictionary is bridged in O(1).
  var d = [ TestObjCKeyTy(10): 10 ]
  var nsd = d as NSDictionary
  expectCrashLater()
  nsd.mutableCopy()
}

DictionaryTraps.test("Downcast1") {
  let d: Dictionary<NSObject, NSObject> = [ TestObjCKeyTy(10): NSObject(),
                                            NSObject() : NSObject() ]
  let d2: Dictionary<TestObjCKeyTy, NSObject> = _dictionaryDownCast(d)
  expectCrashLater()
  let v1 = d2[TestObjCKeyTy(10)]
  let v2 = d2[TestObjCKeyTy(20)]

  // This triggers failure.
  for (k, v) in d2 { }
}

DictionaryTraps.test("Downcast2") {
  let d: Dictionary<NSObject, NSObject> = [ TestObjCKeyTy(10): NSObject(),
                                            NSObject() : NSObject() ]

  expectCrashLater()
  println("OK")
  let d2: Dictionary<TestBridgedKeyTy, NSObject>
    = _dictionaryBridgeFromObjectiveC(d)
  let v1 = d2[TestBridgedKeyTy(10)]
}

runAllTests()


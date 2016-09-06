// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out_Debug
// RUN: %target-build-swift %s -o %t/a.out_Release -O
//
// RUN: %target-run %t/a.out_Debug
// RUN: %target-run %t/a.out_Release
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

let testSuiteSuffix = _isDebugAssertConfiguration() ? "_debug" : "_release"

var DictionaryTraps = TestSuite("DictionaryTraps" + testSuiteSuffix)

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

DictionaryTraps.test("sanity") {
  // Sanity checks.  This code should not trap.
  var d = Dictionary<BridgedVerbatimRefTy, BridgedVerbatimRefTy>()
  var nsd = d as NSDictionary
}

class TestObjCKeyTy : NSObject {
  init(_ value: Int) {
    self.value = value
  }

  override func isEqual(_ object: Any?) -> Bool {
    if let other = object {
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
  init(_ value: Int) { self.value = value }

  var hashValue: Int { return value }

  func _bridgeToObjectiveC() -> TestObjCKeyTy {
    return TestObjCKeyTy(value)
  }

  static func _forceBridgeFromObjectiveC(
    _ x: TestObjCKeyTy,
    result: inout TestBridgedKeyTy?
  ) {
    result = TestBridgedKeyTy(x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    _ x: TestObjCKeyTy,
    result: inout TestBridgedKeyTy?
  ) -> Bool {
    result = TestBridgedKeyTy(x.value)
    return true
  }

  static func _unconditionallyBridgeFromObjectiveC(_ source: TestObjCKeyTy?)
      -> TestBridgedKeyTy {
    var result: TestBridgedKeyTy? = nil
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }

  var value: Int
}

func ==(x: TestBridgedKeyTy, y: TestBridgedKeyTy) -> Bool {
  return x.value == y.value
}

DictionaryTraps.test("BridgedKeyIsNotNSCopyable1")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches("unrecognized selector sent to instance").code {
  // This Dictionary is bridged in O(1).
  var d = [ TestObjCKeyTy(10): NSObject() ]
  var nsd = d as NSDictionary
  expectCrashLater()
  nsd.mutableCopy()
}

DictionaryTraps.test("BridgedKeyIsNotNSCopyable2")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
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

DictionaryTraps.test("Downcast2")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let d: Dictionary<NSObject, NSObject> = [ TestObjCKeyTy(10): NSObject(),
                                            NSObject() : NSObject() ]

  expectCrashLater()
  let d2: Dictionary<TestBridgedKeyTy, NSObject>
    = _dictionaryBridgeFromObjectiveC(d)
  let v1 = d2[TestBridgedKeyTy(10)]
}

runAllTests()

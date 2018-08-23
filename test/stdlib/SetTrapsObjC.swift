// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out_Debug -Onone
// RUN: %target-build-swift %s -o %t/a.out_Release -O
//
// RUN: %target-codesign %t/a.out_Debug
// RUN: %target-codesign %t/a.out_Release
// RUN: %target-run %t/a.out_Debug
// RUN: %target-run %t/a.out_Release
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

let testSuiteSuffix = _isDebugAssertConfiguration() ? "_debug" : "_release"

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

var SetTraps = TestSuite("SetTraps" + testSuiteSuffix)

SetTraps.test("sanity") {
  // Sanity checks.  This code should not trap.
  let s = Set<BridgedVerbatimRefTy>()
  _ = s as NSSet
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
    var result: TestBridgedKeyTy?
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }

  var value: Int
}

func ==(x: TestBridgedKeyTy, y: TestBridgedKeyTy) -> Bool {
  return x.value == y.value
}

SetTraps.test("BridgedKeyIsNotNSCopyable1") {
  // This Set is bridged in O(1).
  let s: Set<TestObjCKeyTy> = [ TestObjCKeyTy(10) ]
  let nss = s as NSSet

  // Unlike NSDictionary, NSSet does not require NSCopying from its element
  // type.
  let copiedSet = nss.mutableCopy() as! NSMutableSet
  expectEqual(10, (copiedSet.anyObject() as! TestObjCKeyTy).value)
}

SetTraps.test("Downcast1")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let s: Set<NSObject> = [ NSObject(), NSObject() ]
  let s2: Set<TestObjCKeyTy> = _setDownCast(s)
  expectCrashLater()
  _ = s2.contains(TestObjCKeyTy(10))
  _ = s2.contains(TestObjCKeyTy(20))

  // This triggers failure.
  for _ in s2 { }
}

SetTraps.test("Downcast2")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  let s: Set<NSObject> = [ TestObjCKeyTy(10), NSObject() ]
  expectCrashLater()
  let s2 = s as! Set<TestBridgedKeyTy>
  _ = s2.contains(TestBridgedKeyTy(10))
}

runAllTests()

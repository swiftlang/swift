// RUN: %target-run-simple-swift(-Onone)
// RUN: %target-run-simple-swift(-O)
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

let testSuiteSuffix = _isDebugAssertConfiguration() ? "_debug" : "_release"

var DictionaryTraps = TestSuite("DictionaryTraps" + testSuiteSuffix)

struct NotBridgedKeyTy : Equatable, Hashable {
  var value: Int

  init(_ value: Int) {
    self.value = value
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }
}

func == (lhs: NotBridgedKeyTy, rhs: NotBridgedKeyTy) -> Bool {
  return lhs.value == rhs.value
}

assert(!_isBridgedToObjectiveC(NotBridgedKeyTy.self))

struct NotBridgedValueTy {}

assert(!_isBridgedToObjectiveC(NotBridgedValueTy.self))

class BridgedVerbatimRefTy : Equatable, Hashable {
  var value: Int

  init(_ value: Int) {
    self.value = value
  }
  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }
}

func == (lhs: BridgedVerbatimRefTy, rhs: BridgedVerbatimRefTy) -> Bool {
  return lhs.value == rhs.value
}

assert(_isBridgedToObjectiveC(BridgedVerbatimRefTy.self))
assert(_isBridgedVerbatimToObjectiveC(BridgedVerbatimRefTy.self))

DictionaryTraps.test("sanity") {
  // Soundness checks.  This code should not trap.
  let d = Dictionary<BridgedVerbatimRefTy, BridgedVerbatimRefTy>()
  _ = d as NSDictionary
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
  var value: Int

  init(_ value: Int) { self.value = value }

  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }

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
  let d = [ TestObjCKeyTy(10): NSObject() ]
  let nsd = d as NSDictionary
  expectCrashLater()
  nsd.mutableCopy()
}

DictionaryTraps.test("BridgedKeyIsNotNSCopyable2")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  // This Dictionary is bridged in O(1).
  let d = [ TestObjCKeyTy(10): 10 ]
  let nsd = d as NSDictionary
  expectCrashLater()
  nsd.mutableCopy()
}

if #available(macOS 15, iOS 13, watchOS 6, tvOS 13, *) {
  // The behavior tested here was introduced in
  // https://github.com/apple/swift/pull/23174

  DictionaryTraps.test("ForcedNonverbatimBridge.StringKey")
  .skip(.custom(
      { _isFastAssertConfiguration() },
      reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches("Could not cast value of type")
  .code {
    let d1: NSDictionary = [
      "Gordon" as NSString: NSObject(),
      "William" as NSString: NSObject(),
      "Katherine" as NSString: NSObject(),
      "Lynn" as NSString: NSObject(),
      "Brian" as NSString: NSObject(),
      1756 as NSNumber: NSObject()]

    expectCrashLater()
    _ = d1 as! Dictionary<String, Any>
  }

  DictionaryTraps.test("ForcedNonverbatimBridge.IntKey")
  .skip(.custom(
      { _isFastAssertConfiguration() },
      reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches("Could not cast value of type")
  .code {

    let d1: NSDictionary = [
      4 as NSNumber: NSObject(),
      8 as NSNumber: NSObject(),
      15 as NSNumber: NSObject(),
      16 as NSNumber: NSObject(),
      23 as NSNumber: NSObject(),
      42 as NSNumber: NSObject(),
      "John" as NSString: NSObject()]

    expectCrashLater()
    _ = d1 as! Dictionary<Int, Any>
  }

  DictionaryTraps.test("ForcedNonverbatimBridge.Value")
  .skip(.custom(
      { _isFastAssertConfiguration() },
      reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches("Could not cast value of type")
  .code {

    let d1: NSDictionary = [
      4 as NSNumber: "Jack" as NSString,
      8 as NSNumber: "Kate" as NSString,
      15 as NSNumber: "Hurley" as NSString,
      16 as NSNumber: "Sawyer" as NSString,
      23 as NSNumber: "John" as NSString,
      42 as NSNumber: NSObject()]

    expectCrashLater()
    _ = d1 as! Dictionary<NSObject, String>
  }

  DictionaryTraps.test("ForcedVerbatimBridge.StringKey")
  .skip(.custom(
      { _isFastAssertConfiguration() },
      reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches("Could not cast value of type")
  .code {
    let d1: NSDictionary = [
      "Gordon" as NSString: NSObject(),
      "William" as NSString: NSObject(),
      "Katherine" as NSString: NSObject(),
      "Lynn" as NSString: NSObject(),
      "Brian" as NSString: NSObject(),
      1756 as NSNumber: NSObject()]

    // With the ObjC runtime, the verbatim downcast is O(1); it performs no
    // runtime checks.
    let d2 = d1 as! Dictionary<NSString, NSObject>
    // Element access goes through the bridged path and performs forced downcasts.
    // This is where the odd numeric value is caught.
    expectCrashLater()
    for (key, value) in d2 {
      _ = (key, value)
    }
  }

  DictionaryTraps.test("ForcedVerbatimBridge.IntKey")
  .skip(.custom(
      { _isFastAssertConfiguration() },
      reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches("Could not cast value of type")
  .code {

    let d1: NSDictionary = [
      4 as NSNumber: NSObject(),
      8 as NSNumber: NSObject(),
      15 as NSNumber: NSObject(),
      16 as NSNumber: NSObject(),
      23 as NSNumber: NSObject(),
      42 as NSNumber: NSObject(),
      "John" as NSString: NSObject()]

    // With the ObjC runtime, the verbatim downcast is O(1); it performs no
    // runtime checks.
    let d2 = d1 as! Dictionary<NSNumber, NSObject>
    // Element access goes through the bridged path and performs forced downcasts.
    // This is where the odd numeric value is caught.
    expectCrashLater()
    for (key, value) in d2 {
      _ = (key, value)
    }
  }

  DictionaryTraps.test("ForcedVerbatimBridge.Value")
  .skip(.custom(
      { _isFastAssertConfiguration() },
      reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches("Could not cast value of type")
  .code {

    let d1: NSDictionary = [
      4 as NSNumber: "Jack" as NSString,
      8 as NSNumber: "Kate" as NSString,
      15 as NSNumber: "Hurley" as NSString,
      16 as NSNumber: "Sawyer" as NSString,
      23 as NSNumber: "John" as NSString,
      42 as NSNumber: NSObject()]

    // With the ObjC runtime, the verbatim downcast is O(1); it performs no
    // runtime checks.
    let d2 = d1 as! Dictionary<NSObject, NSString>
    // Element access goes through the bridged path and performs forced downcasts.
    // This is where the odd numeric value is caught.
    expectCrashLater()
    for (key, value) in d2 {
      _ = (key, value)
    }
  }

  DictionaryTraps.test("Downcast.Verbatim")
  .skip(.custom(
      { _isFastAssertConfiguration() },
      reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches("Could not cast value of type")
  .code {
    let d: Dictionary<NSObject, NSObject> = [ TestObjCKeyTy(10): NSObject(),
      NSObject() : NSObject() ]
    let d2: Dictionary<TestObjCKeyTy, NSObject> = _dictionaryDownCast(d)
    expectCrashLater()
    _ = d2[TestObjCKeyTy(10)]
    _ = d2[TestObjCKeyTy(20)]

    // This triggers failure.
    for (_, _) in d2 { }
  }

  DictionaryTraps.test("Downcast.NonVerbatimBridged")
  .skip(.custom(
      { _isFastAssertConfiguration() },
      reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .crashOutputMatches("Could not cast value of type")
  .code {
    let d: Dictionary<NSObject, NSObject> = [ TestObjCKeyTy(10): NSObject(),
      NSObject() : NSObject() ]

    expectCrashLater()
    let d2 = d as! Dictionary<TestBridgedKeyTy, NSObject>
    _ = d2[TestBridgedKeyTy(10)]
  }
}

runAllTests()

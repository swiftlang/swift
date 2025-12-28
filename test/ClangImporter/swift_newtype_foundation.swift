// RUN: %empty-directory(%t)

// Test that swift_newtype works on all platforms when Foundation is imported.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules %s -verify

// REQUIRES: OS=linux-gnu || OS=windows-msvc

// This test verifies the fix for https://github.com/swiftlang/swift/issues/71086
// Previously, importing swift_newtype types with Foundation would crash on non-ObjC
// platforms because the _ObjectiveCBridgeable extension in NewtypeWrapper.swift was
// incorrectly guarded by #if _runtime(_ObjC), preventing it from being available on
// Linux even though _ObjectiveCBridgeable is available via swift-corelibs-foundation.

import NewtypeFoundation

// Stub Foundation types for non-Darwin platforms since the mock SDK doesn't include them
// Minimal NSObject stub
class NSObject {
  init() {}

  func isEqual(_ object: Any?) -> Bool {
    return false
  }
}

// Minimal NSNumber stub for testing
class NSNumber: NSObject {
  private var _value: UInt64

  init(value: UInt32) {
    self._value = UInt64(value)
    super.init()
  }

  var uint32Value: UInt32 {
    return UInt32(truncatingIfNeeded: _value)
  }

  override func isEqual(_ object: Any?) -> Bool {
    guard let other = object as? NSNumber else { return false }
    return self._value == other._value
  }
}

// UInt32 bridging conformance for non-Darwin platforms
extension UInt32: _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber(value: self)
  }

  public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout UInt32?) {
    if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
      fatalError("Unable to bridge \(type(of: x)) to \(self)")
    }
  }

  public static func _conditionallyBridgeFromObjectiveC(_ x: NSNumber, result: inout UInt32?) -> Bool {
    result = x.uint32Value
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNumber?) -> UInt32 {
    guard let src = source else { return 0 }
    return src.uint32Value
  }
}

// Test that the newtype wrapper conforms to _ObjectiveCBridgeable when the
// underlying type does
func acceptObjectiveCBridgeable<T: _ObjectiveCBridgeable>(_ t: T) {}

func testUInt32Newtype(attr: MyUInt32Newtype) {
  // UInt32 conforms to _ObjectiveCBridgeable on all platforms (via Foundation)
  // so MyUInt32Newtype should also conform
  acceptObjectiveCBridgeable(attr)
  
  // Test that we can use the newtype
  let _ = attr.rawValue
  let copy = MyUInt32Newtype(rawValue: 42)
  let _ = copy
}

// Test that Equatable and Hashable work
func testEquatableHashable(a: MyUInt32Newtype, b: MyUInt32Newtype) {
  let _ = a == b
  let _ = a != b
  let _ = a.hashValue
  
  var dict: [MyUInt32Newtype: String] = [:]
  dict[a] = "test"
  let _ = dict[b]
}

// Test that the type can be used in collections
func testCollections() {
  let array: [MyUInt32Newtype] = [
    MyUInt32Newtype(rawValue: 1),
    MyUInt32Newtype(rawValue: 2),
    MyUInt32Newtype(rawValue: 3)
  ]
  let _ = array.count
  
  let set: Set<MyUInt32Newtype> = [
    MyUInt32Newtype(rawValue: 1),
    MyUInt32Newtype(rawValue: 2)
  ]
  let _ = set.count
}

// Test that the type can be used in a struct with Equatable
struct Container: Equatable {
  var value: MyUInt32Newtype
}

func testContainerEquatable(a: Container, b: Container) {
  let _ = a == b
}


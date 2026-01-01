// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the FoundationStub module first
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/FoundationStub.swiftmodule -module-name FoundationStub %t/FoundationStub.swift

// Test that swift_newtype works on all platforms when _ObjectiveCBridgeable is available.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules -I %t %t/main.swift -verify

// This test verifies the fix for https://github.com/swiftlang/swift/issues/71086
// Previously, importing swift_newtype types would crash on non-ObjC platforms
// because the _ObjectiveCBridgeable extension in NewtypeWrapper.swift was
// incorrectly guarded by #if _runtime(_ObjC), preventing it from being available
// on Linux even though _ObjectiveCBridgeable is available via swift-corelibs-foundation.

//--- FoundationStub.swift

// Minimal NSNumber stub for testing
public class NSNumber {
  private var _value: UInt64

  public init(value: UInt32) {
    self._value = UInt64(value)
  }

  public var uint32Value: UInt32 {
    return UInt32(truncatingIfNeeded: _value)
  }
}

// UInt32 bridging conformance
extension UInt32: @retroactive _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> NSNumber {
    return NSNumber(value: self)
  }

  public static func _forceBridgeFromObjectiveC(_ x: NSNumber, result: inout UInt32?) {
    result = x.uint32Value
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

//--- main.swift

import NewtypeFoundation
import FoundationStub

// Test that the newtype wrapper conforms to _ObjectiveCBridgeable when the
// underlying type does
func acceptObjectiveCBridgeable<T: _ObjectiveCBridgeable>(_ t: T) {}

func testUInt32Newtype(attr: MyUInt32Newtype) {
  // UInt32 conforms to _ObjectiveCBridgeable (via FoundationStub)
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

// Helper for newtype_conformance.swift

@_exported import MoreSwiftNewtypes
import Foundation

extension UnbridgedNonNSObject: Equatable /* but not Hashable */ {
  public static func ==(lhs: UnbridgedNonNSObject, rhs: UnbridgedNonNSObject) -> Bool { return true }
}

// Pick something other than "Equatable" to test that we're not just looking at
// immediately inherited protocols.
protocol EquatablePlus: Equatable {}

public struct BridgedValue : EquatablePlus /* but not Hashable */ {
  public static func ==(lhs: BridgedValue, rhs: BridgedValue) -> Bool { return true }
}

extension BridgedValue: _ObjectiveCBridgeable {
  public func _bridgeToObjectiveC() -> BridgedNonNSObject {
    return BridgedNonNSObject()
  }

  public static func _forceBridgeFromObjectiveC(
    _ x: BridgedNonNSObject,
    result: inout BridgedValue?) {
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ x: BridgedNonNSObject,
    result: inout BridgedValue?
  ) -> Bool {
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(_ source: BridgedNonNSObject?)
      -> BridgedValue {
    var result: BridgedValue?
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }
}

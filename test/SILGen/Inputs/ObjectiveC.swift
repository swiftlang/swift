// Fake ObjectiveC module for testing String/NSString bridging.

@exported import ObjectiveC

public struct ObjCBool : BooleanType {
  var value : UInt8

  /// \brief Allow use in a Boolean context.
  public var boolValue: Bool {
    if value == 0 { return false }
    return true
  }
}

@asmname("swift_BoolToObjCBool")
func _convertBoolToObjCBool(x: Bool) -> ObjCBool

@asmname("swift_ObjCBoolToBool")
func _convertObjCBoolToBool(x: ObjCBool) -> Bool

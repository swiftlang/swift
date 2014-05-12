// Fake ObjectiveC module for testing String/NSString bridging.

@exported import ObjectiveC

struct ObjCBool : LogicValue {
  var value : UInt8

  /// \brief Allow use in a Boolean context.
  func getLogicValue() -> Bool {
    if value == 0 { return false }
    return true
  }

  /// \brief Implicit conversion from C Boolean type to Swift Boolean
  /// type.
  @conversion func __conversion() -> Bool {
    return self
  }
}

@asmname("swift_BoolToObjCBool")
func _convertBoolToObjCBool(x: Bool) -> ObjCBool

@asmname("swift_ObjCBoolToBool")
func _convertObjCBoolToBool(x: ObjCBool) -> Bool

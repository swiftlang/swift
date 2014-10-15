// This is an overlay Swift module.
@exported import ObjectiveC

public struct ObjCBool : Printable {
  private var value : UInt8

  /// \brief Allow use in a Boolean context.
  public var boolValue: Bool {
    return value != 0
  }

  public var description: String {
    // Dispatch to Bool.
    return self.boolValue.description
  }
}

public struct Selector {
  private var ptr : COpaquePointer
}

// Functions used to implicitly bridge ObjCBool types to Swift's Bool type.

internal func _convertBoolToObjCBool(x: Bool) -> ObjCBool {
  return ObjCBool(value: x ? 1 : 0)
}
internal func _convertObjCBoolToBool(x: ObjCBool) -> Bool {
  return x.boolValue
}



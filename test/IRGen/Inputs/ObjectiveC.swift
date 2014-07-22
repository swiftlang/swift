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

public struct Selector : StringLiteralConvertible {
  private var ptr : COpaquePointer

  public static func convertFromStringLiteral(str: String) -> Selector {
    return sel_registerName(str)
  }
}

// Functions used to implicitly bridge ObjCBool types to Swift's Bool type.

internal func _convertBoolToObjCBool(x: Bool) -> ObjCBool {
  return x
}
internal func _convertObjCBoolToBool(x: ObjCBool) -> Bool {
  return x
}



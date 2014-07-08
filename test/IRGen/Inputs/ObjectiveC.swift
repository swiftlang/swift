// This is an overlay Swift module.
@exported import ObjectiveC

public struct ObjCBool : Printable {
  private var value : UInt8

  /// \brief Allow use in a Boolean context.
  public func getLogicValue() -> Bool {
    return value != 0
  }

  /// \brief Implicit conversion from C Boolean type to Swift Boolean
  /// type.
  @conversion public func __conversion() -> Bool {
    return self.getLogicValue()
  }

  public var description: String {
    // Dispatch to Bool.
    return self.getLogicValue().description
  }
}

extension Bool {
  /// \brief Implicit conversion from Swift Boolean type to
  /// Objective-C Boolean type.
  @conversion public func __conversion() -> ObjCBool {
    return ObjCBool(self ? 1 : 0)
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



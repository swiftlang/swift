// This is an overlay Swift module.
@exported import ObjectiveC

struct ObjCBool : ReplPrintable {
  var value : UInt8

  /// \brief Allow use in a Boolean context.
  func getLogicValue() -> Bool {
    return value != 0
  }

  /// \brief Implicit conversion from C Boolean type to Swift Boolean
  /// type.
  @conversion func __conversion() -> Bool {
    return self.getLogicValue()
  }

  func replPrint() {
    // Dispatch to Bool.
    self.getLogicValue().replPrint()
  }
}

extension Bool {
  /// \brief Implicit conversion from Swift Boolean type to
  /// Objective-C Boolean type.
  @conversion func __conversion() -> ObjCBool {
    return ObjCBool(self ? 1 : 0)
  }
}

struct Selector : StringLiteralConvertible {
  var ptr : COpaquePointer

  static func convertFromStringLiteral(str: CString) -> Selector {
    return sel_registerName(str)
  }
}

// Functions used to implicitly bridge ObjCBool types to Swift's Bool type.

func _convertBoolToObjCBool(x: Bool) -> ObjCBool {
  return x
}
func _convertObjCBoolToBool(x: ObjCBool) -> Bool {
  return x
}



@exported import ObjectiveC // Clang module

// The iOS/arm64 target uses _Bool for Objective C's BOOL.  We include
// x86_64 here as well because the iOS simulator also uses _Bool.
#if os(iOS) && (arch(arm64) || arch(x86_64))
struct ObjCBool : LogicValue {
  var value : Bool

  init(_ value: Bool) {
    self.value = value
  }

  /// \brief Allow use in a Boolean context.
  func getLogicValue() -> Bool {
    return value
  }

  /// \brief Implicit conversion from C Boolean type to Swift Boolean
  /// type.
  @conversion func __conversion() -> Bool {
    return self.getLogicValue()
  }
}

extension Bool {
  /// \brief Implicit conversion from Swift Boolean type to
  /// Objective-C Boolean type.
  @conversion func __conversion() -> ObjCBool {
    return ObjCBool(self ? Bool.true : Bool.false)
  }
}
#else
struct ObjCBool : LogicValue {
  var value : UInt8

  init(_ value: UInt8) {
    self.value = value
  }

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

extension Bool {
  /// \brief Implicit conversion from Swift Boolean type to
  /// Objective-C Boolean type.
  @conversion func __conversion() -> ObjCBool {
    return ObjCBool(self ? 1 : 0)
  }
}
#endif

struct Selector : StringLiteralConvertible {
  var ptr : COpaquePointer

  static func convertFromExtendedGraphemeClusterLiteral(
    value: CString) -> Selector {

    return convertFromStringLiteral(value)
  }

  static func convertFromStringLiteral(value: CString) -> Selector {
    return sel_registerName(value)
  }
}

func _convertBoolToObjCBool(x: Bool) -> ObjCBool {
  return x
}

func _convertObjCBoolToBool(x: ObjCBool) -> Bool {
  return x
}

func ~=(x: NSObject, y: NSObject) -> Bool {
  return true
}

extension NSObject : Equatable, Hashable {
  var hashValue: Int {
    return hash
  }
}

func == (lhs: NSObject, rhs: NSObject) -> Bool {
  return lhs.isEqual(rhs)
}

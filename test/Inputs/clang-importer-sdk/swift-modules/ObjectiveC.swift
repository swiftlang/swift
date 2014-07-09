@exported import ObjectiveC // Clang module

// The iOS/arm64 target uses _Bool for Objective C's BOOL.  We include
// x86_64 here as well because the iOS simulator also uses _Bool.
#if os(iOS) && (arch(arm64) || arch(x86_64))
public struct ObjCBool : LogicValue {
  private var value : Bool

  public init(_ value: Bool) {
    self.value = value
  }

  /// \brief Allow use in a Boolean context.
  public func getLogicValue() -> Bool {
    return value
  }

  /// \brief Implicit conversion from C Boolean type to Swift Boolean
  /// type.
  @conversion public func __conversion() -> Bool {
    return self.getLogicValue()
  }
}

extension Bool {
  /// \brief Implicit conversion from Swift Boolean type to
  /// Objective-C Boolean type.
  @conversion public func __conversion() -> ObjCBool {
    return ObjCBool(self ? true : false)
  }
}
#else
public struct ObjCBool : LogicValue {
  private var value : UInt8

  public init(_ value: Bool) {
    self.value = value ? 1 : 0
  }

  public init(_ value: UInt8) {
    self.value = value
  }

  /// \brief Allow use in a Boolean context.
  public func getLogicValue() -> Bool {
    if value == 0 { return false }
    return true
  }

  /// \brief Implicit conversion from C Boolean type to Swift Boolean
  /// type.
  @conversion public func __conversion() -> Bool {
    return self  
  }
}

extension Bool {
  /// \brief Implicit conversion from Swift Boolean type to
  /// Objective-C Boolean type.
  @conversion public func __conversion() -> ObjCBool {
    return ObjCBool(self ? 1 : 0)
  }
}
#endif

public struct Selector : StringLiteralConvertible {
  private var ptr : COpaquePointer

  public static func convertFromExtendedGraphemeClusterLiteral(
    value: String) -> Selector {

    return convertFromStringLiteral(value)
  }

  public static func convertFromStringLiteral(value: String) -> Selector {
    return sel_registerName(value)
  }
}

internal func _convertBoolToObjCBool(x: Bool) -> ObjCBool {
  return x
}

internal func _convertObjCBoolToBool(x: ObjCBool) -> Bool {
  return x
}

public func ~=(x: NSObject, y: NSObject) -> Bool {
  return true
}

extension NSObject : Equatable, Hashable {
  public var hashValue: Int {
    return hash
  }
}

public func == (lhs: NSObject, rhs: NSObject) -> Bool {
  return lhs.isEqual(rhs)
}

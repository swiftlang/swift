@_exported import ObjectiveC // Clang module

// The iOS/arm64 target uses _Bool for Objective-C's BOOL.  We include
// x86_64 here as well because the iOS simulator also uses _Bool.
#if ((os(iOS) || os(tvOS)) && (arch(arm64) || arch(x86_64))) || os(watchOS)
public struct ObjCBool : Boolean {
  private var value : Bool

  public init(_ value: Bool) {
    self.value = value
  }

  /// \brief Allow use in a Boolean context.
  public var boolValue: Bool {
    return value
  }
}

#else

public struct ObjCBool : Boolean {
  private var value : UInt8

  public init(_ value: Bool) {
    self.value = value ? 1 : 0
  }

  public init(_ value: UInt8) {
    self.value = value
  }

  /// \brief Allow use in a Boolean context.
  public var boolValue: Bool {
    if value == 0 { return false }
    return true
  }
}
#endif

extension ObjCBool : BooleanLiteralConvertible {
  public init(booleanLiteral: Bool) {
    self.init(booleanLiteral)
  }
}

public struct Selector : StringLiteralConvertible {
  private var ptr : OpaquePointer

  public init(_ value: String) {
    self.init(stringLiteral: value)
  }

  public init(unicodeScalarLiteral value: String) {
    self.init(stringLiteral: value)
  }

  public init(extendedGraphemeClusterLiteral value: String) {
    self.init(stringLiteral: value)
  }

  public init (stringLiteral value: String) {
    self = sel_registerName(value)
  }
}

public struct NSZone {
  public var pointer : OpaquePointer
}

internal func _convertBoolToObjCBool(_ x: Bool) -> ObjCBool {
  return ObjCBool(x)
}

internal func _convertObjCBoolToBool(_ x: ObjCBool) -> Bool {
  return Bool(x)
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

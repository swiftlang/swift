@_exported import ObjectiveC // Clang module

// The iOS/arm64 target uses _Bool for Objective-C's BOOL.  We include
// x86_64 here as well because the iOS simulator also uses _Bool.
public struct ObjCBool {
#if (os(macOS) && arch(x86_64)) || (os(iOS) && (arch(i386) || arch(arm) || targetEnvironment(macCatalyst)))

  // On macOS and 32-bit iOS, Objective-C's BOOL type is a "signed char".
  private var value: UInt8

  public init(_ value: Bool) {
    self.value = value ? 1 : 0
  }

  /// Allow use in a Boolean context.
  public var boolValue: Bool {
    return value != 0
  }
#else
  // Everywhere else it is C/C++'s "Bool"
  private var value: Bool

  public init(_ value: Bool) {
    self.value = value
  }

  public var boolValue: Bool {
    return value
  }
#endif
}

extension ObjCBool : ExpressibleByBooleanLiteral {
  public init(booleanLiteral: Bool) {
    self.init(booleanLiteral)
  }
}

public struct Selector : ExpressibleByStringLiteral {
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

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ptr)
  }
}

extension Selector : Equatable, Hashable {}

public func ==(lhs: Selector, rhs: Selector) -> Bool {
  return sel_isEqual(lhs, rhs)
}

public struct NSZone {
  public var pointer : OpaquePointer
}

public func _convertBoolToObjCBool(_ x: Bool) -> ObjCBool {
  return ObjCBool(x)
}

public func _convertObjCBoolToBool(_ x: ObjCBool) -> Bool {
  return x.boolValue
}

public func ~=(x: NSObject, y: NSObject) -> Bool {
  return true
}

extension NSObject : Equatable, Hashable {
  public static func == (lhs: NSObject, rhs: NSObject) -> Bool {
    return lhs.isEqual(rhs)
  }

  public var hashValue: Int {
    return hash
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(hash)
  }
}


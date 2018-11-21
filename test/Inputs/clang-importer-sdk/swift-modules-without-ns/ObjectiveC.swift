@_exported import ObjectiveC // Clang module

// The iOS/arm64 target uses _Bool for Objective-C's BOOL.  We include
// x86_64 here as well because the iOS simulator also uses _Bool.
#if ((os(iOS) || os(tvOS)) && (arch(arm64) || arch(x86_64))) || os(watchOS)
public struct ObjCBool {
  private var value : Bool

  public init(_ value: Bool) {
    self.value = value
  }

  public var boolValue: Bool {
    return value
  }
}

#else

public struct ObjCBool {
  private var value : UInt8

  public init(_ value: Bool) {
    self.value = value ? 1 : 0
  }

  public init(_ value: UInt8) {
    self.value = value
  }

  public var boolValue: Bool {
    if value == 0 { return false }
    return true
  }
}
#endif

extension ObjCBool : ExpressibleByBooleanLiteral {
  public init(booleanLiteral: Bool) {
    self.init(booleanLiteral)
  }
}

public struct Selector : ExpressibleByStringLiteral {
  private var ptr : OpaquePointer

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
  public var hashValue: Int {
    return hash
  }
  public func hash(into hasher: inout Hasher) {
    hasher.combine(hash)
  }
}

public func == (lhs: NSObject, rhs: NSObject) -> Bool {
  return lhs.isEqual(rhs)
}

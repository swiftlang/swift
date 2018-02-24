// Fake ObjectiveC module for testing String/NSString bridging.

@_exported import ObjectiveC

public struct ObjCBool {
  var value : UInt8

  public var boolValue: Bool {
    if value == 0 { return false }
    return true
  }
}

public func _convertBoolToObjCBool(_ x: Bool) -> ObjCBool {
  return x ? ObjCBool(value: 1) : ObjCBool(value: 0)
}

public func _convertObjCBoolToBool(_ x: ObjCBool) -> Bool {
  return x.boolValue
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

// Fake ObjectiveC module for testing String/NSString bridging.

@_exported import ObjectiveC

public struct ObjCBool {
  var value : UInt8

  public var boolValue: Bool {
    if value == 0 { return false }
    return true
  }
}

@_silgen_name("swift_BoolToObjCBool")
public func _convertBoolToObjCBool(_ x: Bool) -> ObjCBool

@_silgen_name("swift_ObjCBoolToBool")
public func _convertObjCBoolToBool(_ x: ObjCBool) -> Bool


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

extension NSObject : Hashable {
  public func hash(into hasher: inout Hasher) {}
  public static func == (x: NSObject, y: NSObject) -> Bool { return true }
}


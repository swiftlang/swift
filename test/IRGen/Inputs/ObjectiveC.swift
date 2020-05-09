// This is an overlay Swift module.
@_exported import ObjectiveC

public struct ObjCBool : CustomStringConvertible {
#if os(macOS) || (os(iOS) && (arch(i386) || arch(arm)))
  // On macOS and 32-bit iOS, Objective-C's BOOL type is a "signed char".
  private var value: Int8

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

  public var description: String {
    // Dispatch to Bool.
    return self.boolValue.description
  }
}

public struct Selector {
  private var ptr : OpaquePointer
}

// Functions used to implicitly bridge ObjCBool types to Swift's Bool type.

public func _convertBoolToObjCBool(_ x: Bool) -> ObjCBool {
  return ObjCBool(x)
}
public func _convertObjCBoolToBool(_ x: ObjCBool) -> Bool {
  return x.boolValue
}

extension NSObject : Hashable {
  public func hash(into hasher: inout Hasher) {}
}
public func ==(x: NSObject, y: NSObject) -> Bool { return x === y }


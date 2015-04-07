// This is an overlay Swift module.
@exported import ObjectiveC

public struct ObjCBool : CustomStringConvertible {
#if os(OSX) || (os(iOS) && (arch(i386) || arch(arm)))
  // On OS X and 32-bit iOS, Objective-C's BOOL type is a "signed char".
  private var value: Int8

  public init(_ value: Bool) {
    self.value = value ? 1 : 0
  }

  /// \brief Allow use in a Boolean context.
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
  private var ptr : COpaquePointer
}

// Functions used to implicitly bridge ObjCBool types to Swift's Bool type.

public func _convertBoolToObjCBool(x: Bool) -> ObjCBool {
  return ObjCBool(x)
}
public func _convertObjCBoolToBool(x: ObjCBool) -> Bool {
  return x.boolValue
}


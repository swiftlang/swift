@_exported import Darwin // Clang module


//===----------------------------------------------------------------------===//
// MacTypes.h
//===----------------------------------------------------------------------===//
public let noErr: OSStatus = 0

/// The `Boolean` type declared in MacTypes.h and used throughout Core
/// Foundation.
///
/// The C type is a typedef for `unsigned char`.
public struct DarwinBoolean : ExpressibleByBooleanLiteral {
  var value: UInt8

  public init(_ value: Bool) {
    self.value = value ? 1 : 0
  }

  /// The value of `self`, expressed as a `Bool`.
  public var boolValue: Bool {
    return value != 0
  }

  /// Create an instance initialized to `value`.
  @_transparent
  public init(booleanLiteral value: Bool) {
    self.init(value)
  }
}

public // COMPILER_INTRINSIC
func _convertBoolToDarwinBoolean(_ x: Bool) -> DarwinBoolean {
  return DarwinBoolean(x)
}
public // COMPILER_INTRINSIC
func _convertDarwinBooleanToBool(_ x: DarwinBoolean) -> Bool {
  return x.boolValue
}

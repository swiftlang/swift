//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// A signed integer value type.
///
/// On 32-bit platforms, `Int` is the same size as `Int32`, and
/// on 64-bit platforms, `Int` is the same size as `Int64`.
@frozen
public struct Int : FixedWidthInteger, SignedInteger {
  public typealias Magnitude = UInt
  public typealias Words = UInt.Words
  public typealias IntegerLiteralType = Int

#if arch(i386) || arch(arm) || arch(arm64_32) || arch(wasm32)
  public typealias _StorageType = Int32
#else
  public typealias _StorageType = Int64
#endif

  @usableFromInline
  internal var _value: _StorageType

  /// Creates a new instance with the same memory representation as the given
  /// value.
  @_transparent
  public init(bitPattern: UInt) {
    _value = _StorageType(bitPattern: bitPattern._value)
  }

  /// Creates an integer from the given floating-point value, rounding toward
  /// zero.
  @_transparent
  public init(_ source: Float) {
    _value = _StorageType(source)
  }

  /// Creates an integer from the given floating-point value, rounding toward
  /// zero.
  @_transparent
  public init(_ source: Double) {
    _value = _StorageType(source)
  }

  /// Creates a new instance from the given integer, if it can be represented
  /// exactly.
  @_transparent
  public init?<T>(exactly source: T) where T : BinaryInteger {
    guard let value = _StorageType(exactly: source) else { return nil }
    _value = value
  }

  /// Creates a new instance by clamping the given value to this type's range.
  @_transparent
  public init<T>(clamping source: T) where T : BinaryInteger {
    _value = _StorageType(clamping: source)
  }

  /// Creates a new instance with the representable value that's closest to the
  /// given integer.
  @_transparent
  public init<T>(_ source: T) where T : BinaryInteger {
    _value = _StorageType(source)
  }

  /// Creates a new instance from the bit pattern of the given instance by
  /// sign-extending or truncating to fit this type.
  @_transparent
  public init<T>(truncatingIfNeeded source: T) where T : BinaryInteger {
    _value = _StorageType(truncatingIfNeeded: source)
  }

  @_transparent
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
#if arch(i386) || arch(arm) || arch(arm64_32) || arch(wasm32)
    _value = Int32(_builtinIntegerLiteral: value)
#else
    _value = Int64(_builtinIntegerLiteral: value)
#endif
  }
}

// MARK: - Integer literal support

extension Int: ExpressibleByIntegerLiteral {
  /// Creates an instance initialized to the specified integer value.
  @_transparent
  public init(integerLiteral value: Int) {
    self = value
  }
}

// MARK: - Basic arithmetic operations

extension Int {
  /// Adds two values and produces their sum.
  @_transparent
  public static func + (lhs: Int, rhs: Int) -> Int {
    let (result, overflow) = lhs.addingReportingOverflow(rhs)
    _precondition(!overflow, "Arithmetic overflow")
    return result
  }

  /// Subtracts one value from another and produces their difference.
  @_transparent
  public static func - (lhs: Int, rhs: Int) -> Int {
    let (result, overflow) = lhs.subtractingReportingOverflow(rhs)
    _precondition(!overflow, "Arithmetic overflow")
    return result
  }

  /// Multiplies two values and produces their product.
  @_transparent
  public static func * (lhs: Int, rhs: Int) -> Int {
    let (result, overflow) = lhs.multipliedReportingOverflow(by: rhs)
    _precondition(!overflow, "Arithmetic overflow")
    return result
  }

  /// Returns the quotient of dividing the first value by the second.
  @_transparent
  public static func / (lhs: Int, rhs: Int) -> Int {
    _precondition(rhs != 0, "Division by zero")
    let (result, overflow) = lhs.dividedReportingOverflow(by: rhs)
    _precondition(!overflow, "Arithmetic overflow")
    return result
  }

  /// Returns the remainder of dividing the first value by the second.
  @_transparent
  public static func % (lhs: Int, rhs: Int) -> Int {
    _precondition(rhs != 0, "Division by zero in remainder operation")
    let (result, overflow) = lhs.remainderReportingOverflow(dividingBy: rhs)
    _precondition(!overflow, "Arithmetic overflow")
    return result
  }
}

// MARK: - Compound assignment operators

extension Int {
  /// Adds the given value to this value in place.
  @_transparent
  public static func += (lhs: inout Int, rhs: Int) {
    lhs = lhs + rhs
  }

  /// Subtracts the given value from this value in place.
  @_transparent
  public static func -= (lhs: inout Int, rhs: Int) {
    lhs = lhs - rhs
  }

  /// Multiplies this value by the given value in place.
  @_transparent
  public static func *= (lhs: inout Int, rhs: Int) {
    lhs = lhs * rhs
  }

  /// Divides this value by the given value in place.
  @_transparent
  public static func /= (lhs: inout Int, rhs: Int) {
    lhs = lhs / rhs
  }

  /// Divides this value by the given value and stores the remainder in place.
  @_transparent
  public static func %= (lhs: inout Int, rhs: Int) {
    lhs = lhs % rhs
  }
}

// MARK: - Comparison operations

extension Int: Equatable {
  /// Returns a Boolean value indicating whether two values are equal.
  @_transparent
  public static func == (lhs: Int, rhs: Int) -> Bool {
    return lhs._value == rhs._value
  }
}

extension Int: Comparable {
  /// Returns a Boolean value indicating whether the first value is less than
  /// the second value.
  @_transparent
  public static func < (lhs: Int, rhs: Int) -> Bool {
    return lhs._value < rhs._value
  }
}

// MARK: - Hashable conformance

extension Int: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  public func hash(into hasher: inout Hasher) {
    hasher.combine(_value)
  }
}

// MARK: - String representation

extension Int: CustomStringConvertible {
  /// A textual representation of this instance.
  public var description: String {
    return String(_value)
  }
}

// MARK: - Numeric protocol conformance

extension Int: AdditiveArithmetic {
  /// The zero value.
  public static var zero: Int { return 0 }
}

extension Int: Numeric {
  /// The magnitude of this value.
  public var magnitude: UInt {
    return UInt(bitPattern: self < 0 ? -self : self)
  }

  /// Creates a new instance from the given integer, if it can be represented
  /// exactly.
  public init?<T>(exactly source: T) where T : BinaryInteger {
    guard let value = _StorageType(exactly: source) else { return nil }
    _value = value
  }
}

// MARK: - Constants

extension Int {
  /// The maximum representable integer in this type.
  public static var max: Int {
#if arch(i386) || arch(arm) || arch(arm64_32) || arch(wasm32)
    return Int(Int32.max)
#else
    return Int(Int64.max)
#endif
  }

  /// The minimum representable integer in this type.
  public static var min: Int {
#if arch(i386) || arch(arm) || arch(arm64_32) || arch(wasm32)
    return Int(Int32.min)
#else
    return Int(Int64.min)
#endif
  }
}

// MARK: - Bitwise operations

extension Int {
  /// Returns the bitwise NOT of the given value.
  @_transparent
  public static prefix func ~ (x: Int) -> Int {
    return Int(bitPattern: ~x.magnitude)
  }

  /// Returns the result of performing a bitwise AND operation on the two given
  /// values.
  @_transparent
  public static func & (lhs: Int, rhs: Int) -> Int {
    return Int(bitPattern: lhs.magnitude & rhs.magnitude)
  }

  /// Returns the result of performing a bitwise OR operation on the two given
  /// values.
  @_transparent
  public static func | (lhs: Int, rhs: Int) -> Int {
    return Int(bitPattern: lhs.magnitude | rhs.magnitude)
  }

  /// Returns the result of performing a bitwise XOR operation on the two given
  /// values.
  @_transparent
  public static func ^ (lhs: Int, rhs: Int) -> Int {
    return Int(bitPattern: lhs.magnitude ^ rhs.magnitude)
  }
}

// MARK: - Overflow reporting arithmetic

extension Int {
  /// Returns a tuple containing the result of adding the two given values and
  /// a Boolean value indicating whether overflow occurred in the operation.
  @_transparent
  public func addingReportingOverflow(_ rhs: Int) -> (partialValue: Int, overflow: Bool) {
    let (result, overflow) = _value.addingReportingOverflow(rhs._value)
    return (Int(result), overflow)
  }

  /// Returns a tuple containing the result of subtracting the second value from
  /// the first and a Boolean value indicating whether overflow occurred.
  @_transparent
  public func subtractingReportingOverflow(_ rhs: Int) -> (partialValue: Int, overflow: Bool) {
    let (result, overflow) = _value.subtractingReportingOverflow(rhs._value)
    return (Int(result), overflow)
  }

  /// Returns a tuple containing the result of multiplying the two given values
  /// and a Boolean value indicating whether overflow occurred.
  @_transparent
  public func multipliedReportingOverflow(by rhs: Int) -> (partialValue: Int, overflow: Bool) {
    let (result, overflow) = _value.multipliedReportingOverflow(by: rhs._value)
    return (Int(result), overflow)
  }

  /// Returns a tuple containing the result of dividing the first value by the
  /// second and a Boolean value indicating whether overflow occurred.
  @_transparent
  public func dividedReportingOverflow(by rhs: Int) -> (partialValue: Int, overflow: Bool) {
    let (result, overflow) = _value.dividedReportingOverflow(by: rhs._value)
    return (Int(result), overflow)
  }

  /// Returns a tuple containing the remainder of dividing the first value by
  /// the second and a Boolean value indicating whether overflow occurred.
  @_transparent
  public func remainderReportingOverflow(dividingBy rhs: Int) -> (partialValue: Int, overflow: Bool) {
    let (result, overflow) = _value.remainderReportingOverflow(dividingBy: rhs._value)
    return (Int(result), overflow)
  }
}
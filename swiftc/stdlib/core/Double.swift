//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// A double-precision, floating-point value type.
///
/// The `Double` type conforms to the `FloatingPoint` protocol and is the
/// default type for floating-point literals that don't require the precision
/// of a `Float80`.
@frozen
public struct Double {
  @usableFromInline
  internal var _value: Builtin.FPIEEE64

  /// Creates a value initialized to zero.
  @_transparent
  public init() {
    _value = Builtin.fpext_FPIEEE32_FPIEEE64(0.0._value)
  }

  /// Creates a new instance from the given value.
  @_transparent
  public init(_ value: Double) {
    self._value = value._value
  }

  /// Creates a new instance from the given value.
  @_transparent
  public init(_ value: Float) {
    _value = Builtin.fpext_FPIEEE32_FPIEEE64(value._value)
  }

  /// Creates a new instance from the exact given value.
  @_transparent
  public init(_ value: Int) {
    _value = Builtin.sitofp_Int64_FPIEEE64(value._value)
  }

  /// Creates a new instance from the exact given value.
  @_transparent
  public init(_ value: UInt) {
    _value = Builtin.uitofp_Int64_FPIEEE64(value._value)
  }

  /// Creates a new instance, if the given value can be represented exactly.
  @_transparent
  public init?<T>(exactly source: T) where T : BinaryInteger {
    guard let intValue = Int(exactly: source) else { return nil }
    self.init(intValue)
  }

  /// Creates a new instance with the representable value closest to the given
  /// integer.
  @_transparent
  public init<T>(_ source: T) where T : BinaryInteger {
    self.init(Int(source))
  }

  /// Creates a new instance from the given floating-point literal.
  @_transparent
  public init(_builtinFloatLiteral value: Builtin.FPIEEE64) {
    _value = value
  }
}

// MARK: - ExpressibleByFloatLiteral

extension Double: ExpressibleByFloatLiteral {
  /// The type used to represent a floating-point literal.
  public typealias FloatLiteralType = Double

  /// Creates an instance initialized to the specified floating-point value.
  @_transparent
  public init(floatLiteral value: Double) {
    self = value
  }
}

// MARK: - ExpressibleByIntegerLiteral

extension Double: ExpressibleByIntegerLiteral {
  /// The type used to represent an integer literal.
  public typealias IntegerLiteralType = Double

  /// Creates an instance initialized to the specified integer value.
  @_transparent
  public init(integerLiteral value: Double) {
    self = value
  }
}

// MARK: - Basic arithmetic operations

extension Double {
  /// Adds two values and produces their sum.
  @_transparent
  public static func + (lhs: Double, rhs: Double) -> Double {
    return Double(Builtin.fadd_FPIEEE64(lhs._value, rhs._value))
  }

  /// Subtracts one value from another and produces their difference.
  @_transparent
  public static func - (lhs: Double, rhs: Double) -> Double {
    return Double(Builtin.fsub_FPIEEE64(lhs._value, rhs._value))
  }

  /// Multiplies two values and produces their product.
  @_transparent
  public static func * (lhs: Double, rhs: Double) -> Double {
    return Double(Builtin.fmul_FPIEEE64(lhs._value, rhs._value))
  }

  /// Returns the quotient of dividing the first value by the second.
  @_transparent
  public static func / (lhs: Double, rhs: Double) -> Double {
    return Double(Builtin.fdiv_FPIEEE64(lhs._value, rhs._value))
  }

  /// Returns the remainder of dividing the first value by the second.
  @_transparent
  public static func % (lhs: Double, rhs: Double) -> Double {
    return Double(Builtin.frem_FPIEEE64(lhs._value, rhs._value))
  }
}

// MARK: - Compound assignment operators

extension Double {
  /// Adds the given value to this value in place.
  @_transparent
  public static func += (lhs: inout Double, rhs: Double) {
    lhs = lhs + rhs
  }

  /// Subtracts the given value from this value in place.
  @_transparent
  public static func -= (lhs: inout Double, rhs: Double) {
    lhs = lhs - rhs
  }

  /// Multiplies this value by the given value in place.
  @_transparent
  public static func *= (lhs: inout Double, rhs: Double) {
    lhs = lhs * rhs
  }

  /// Divides this value by the given value in place.
  @_transparent
  public static func /= (lhs: inout Double, rhs: Double) {
    lhs = lhs / rhs
  }

  /// Divides this value by the given value and stores the remainder in place.
  @_transparent
  public static func %= (lhs: inout Double, rhs: Double) {
    lhs = lhs % rhs
  }
}

// MARK: - Unary operators

extension Double {
  /// Returns the given number unchanged.
  @_transparent
  public static prefix func + (x: Double) -> Double {
    return x
  }

  /// Returns the additive inverse of the given number.
  @_transparent
  public static prefix func - (x: Double) -> Double {
    return Double(Builtin.fneg_FPIEEE64(x._value))
  }
}

// MARK: - Comparison operations

extension Double: Equatable {
  /// Returns a Boolean value indicating whether two values are equal.
  @_transparent
  public static func == (lhs: Double, rhs: Double) -> Bool {
    return Bool(Builtin.fcmp_oeq_FPIEEE64(lhs._value, rhs._value))
  }
}

extension Double: Comparable {
  /// Returns a Boolean value indicating whether the first value is less than
  /// the second value.
  @_transparent
  public static func < (lhs: Double, rhs: Double) -> Bool {
    return Bool(Builtin.fcmp_olt_FPIEEE64(lhs._value, rhs._value))
  }

  /// Returns a Boolean value indicating whether the first value is less than
  /// or equal to the second value.
  @_transparent
  public static func <= (lhs: Double, rhs: Double) -> Bool {
    return Bool(Builtin.fcmp_ole_FPIEEE64(lhs._value, rhs._value))
  }

  /// Returns a Boolean value indicating whether the first value is greater
  /// than the second value.
  @_transparent
  public static func > (lhs: Double, rhs: Double) -> Bool {
    return Bool(Builtin.fcmp_ogt_FPIEEE64(lhs._value, rhs._value))
  }

  /// Returns a Boolean value indicating whether the first value is greater
  /// than or equal to the second value.
  @_transparent
  public static func >= (lhs: Double, rhs: Double) -> Bool {
    return Bool(Builtin.fcmp_oge_FPIEEE64(lhs._value, rhs._value))
  }
}

// MARK: - Hashable conformance

extension Double: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  public func hash(into hasher: inout Hasher) {
    // Convert to bit representation for hashing
    let bits = bitPattern
    hasher.combine(bits)
  }
}

// MARK: - String representation

extension Double: CustomStringConvertible {
  /// A textual representation of this instance.
  public var description: String {
    return _doubleToString(self)
  }
}

extension Double: CustomDebugStringConvertible {
  /// A textual representation of this instance, suitable for debugging.
  public var debugDescription: String {
    return description
  }
}

// MARK: - Numeric protocol conformance

extension Double: AdditiveArithmetic {
  /// The zero value.
  public static var zero: Double { return 0.0 }
}

extension Double: Numeric {
  /// The magnitude of this value.
  public var magnitude: Double {
    return abs(self)
  }

  /// Creates a new instance from the given integer, if it can be represented
  /// exactly.
  public init?<T>(exactly source: T) where T : BinaryInteger {
    guard let intValue = Int(exactly: source) else { return nil }
    self.init(intValue)
  }
}

// MARK: - FloatingPoint protocol conformance

extension Double: FloatingPoint {
  /// The radix, or base of exponentiation, for this floating-point type.
  public static var radix: Int { return 2 }

  /// The number of digits that can be represented in this floating-point type
  /// without losing precision.
  public static var significandBitCount: Int { return 52 }

  /// The available number of fractional significand bits.
  public static var significandBitPattern: UInt64 { return 0x000fffffffffffff }

  /// The bit pattern of the value's exponent field.
  public var exponentBitPattern: UInt {
    return UInt((bitPattern & 0x7ff0000000000000) >> 52)
  }

  /// The bit pattern of the value's significand field.
  public var significandBitPattern: UInt64 {
    return bitPattern & Double.significandBitPattern
  }

  /// The IEEE 754 bit pattern of this value as a `UInt64`.
  public var bitPattern: UInt64 {
    return UInt64(Builtin.bitcast_FPIEEE64_Int64(_value))
  }

  /// Creates a new value from the given bit pattern.
  public init(bitPattern: UInt64) {
    _value = Builtin.bitcast_Int64_FPIEEE64(bitPattern._value)
  }

  /// The unit in the last place of this value.
  public var ulp: Double {
    if !isFinite { return .nan }
    if isZero { return .leastNormalMagnitude }
    
    let magnitude = self.magnitude
    if magnitude >= .greatestFiniteMagnitude {
      return magnitude - magnitude.nextDown
    }
    
    // For normal numbers, ULP is 2^(exponent - significandBitCount)
    let exp = exponent
    return Double(sign: .plus, exponent: exp - Double.significandBitCount, significand: 1.0)
  }

  /// The least representable value that compares greater than this value.
  public var nextUp: Double {
    if isNaN { return self }
    if self == .infinity { return self }
    if self == -.greatestFiniteMagnitude { return -.infinity }
    
    let bits = bitPattern
    if self >= 0 {
      return Double(bitPattern: bits + 1)
    } else {
      return Double(bitPattern: bits - 1)
    }
  }

  /// The greatest representable value that compares less than this value.
  public var nextDown: Double {
    if isNaN { return self }
    if self == -.infinity { return self }
    if self == .greatestFiniteMagnitude { return .infinity }
    
    let bits = bitPattern
    if self > 0 {
      return Double(bitPattern: bits - 1)
    } else {
      return Double(bitPattern: bits + 1)
    }
  }

  /// The exponent of this value.
  public var exponent: Int {
    let exp = Int(exponentBitPattern)
    if exp == 0 {
      // Subnormal or zero
      return significandBitPattern == 0 ? 0 : 1 - 1023
    } else if exp == 0x7ff {
      // Infinity or NaN
      return .max
    } else {
      // Normal
      return exp - 1023
    }
  }

  /// The significand of this value.
  public var significand: Double {
    if !isFinite { return self }
    if isZero { return self }
    
    let significandBits = significandBitPattern
    if exponentBitPattern == 0 {
      // Subnormal
      return Double(bitPattern: significandBits | 0x3ff0000000000000)
    } else {
      // Normal - add implicit leading 1
      return Double(bitPattern: (significandBits | 0x0010000000000000) | 0x3ff0000000000000)
    }
  }

  /// Creates a floating-point value from components.
  public init(sign: FloatingPointSign, exponent: Int, significand: Double) {
    let signBit: UInt64 = sign == .minus ? 0x8000000000000000 : 0
    
    if !significand.isFinite || significand.isZero {
      self = significand
      if sign == .minus { self = -self }
      return
    }
    
    var exp = exponent + 1023
    var sig = significand.significandBitPattern
    
    if exp <= 0 {
      // Subnormal
      sig >>= (1 - exp)
      exp = 0
    } else if exp >= 0x7ff {
      // Infinity
      exp = 0x7ff
      sig = 0
    }
    
    let bits = signBit | (UInt64(exp) << 52) | sig
    self.init(bitPattern: bits)
  }

  /// A Boolean value indicating whether this instance is equal to zero.
  public var isZero: Bool {
    return (bitPattern & 0x7fffffffffffffff) == 0
  }

  /// A Boolean value indicating whether this instance is finite.
  public var isFinite: Bool {
    return exponentBitPattern != 0x7ff
  }

  /// A Boolean value indicating whether the instance is infinite.
  public var isInfinite: Bool {
    return !isFinite && significandBitPattern == 0
  }

  /// A Boolean value indicating whether the instance is NaN ("not a number").
  public var isNaN: Bool {
    return !isFinite && significandBitPattern != 0
  }

  /// A Boolean value indicating whether the instance is a signaling NaN.
  public var isSignalingNaN: Bool {
    return isNaN && (significandBitPattern & 0x0008000000000000) == 0
  }

  /// The sign of the floating-point value.
  public var sign: FloatingPointSign {
    return (bitPattern & 0x8000000000000000) != 0 ? .minus : .plus
  }
}

// MARK: - Constants

extension Double {
  /// Positive infinity.
  public static var infinity: Double {
    return Double(bitPattern: 0x7ff0000000000000)
  }

  /// A quiet NaN ("not a number").
  public static var nan: Double {
    return Double(bitPattern: 0x7ff8000000000000)
  }

  /// A signaling NaN ("not a number").
  public static var signalingNaN: Double {
    return Double(bitPattern: 0x7ff0000000000001)
  }

  /// The greatest finite number representable by this type.
  public static var greatestFiniteMagnitude: Double {
    return Double(bitPattern: 0x7fefffffffffffff)
  }

  /// The mathematical constant pi (π), approximately 3.14159.
  public static var pi: Double {
    return 3.1415926535897932384626433832795028841971693993751
  }

  /// The smallest positive normal number.
  public static var leastNormalMagnitude: Double {
    return Double(bitPattern: 0x0010000000000000)
  }

  /// The smallest positive number.
  public static var leastNonzeroMagnitude: Double {
    return Double(bitPattern: 0x0000000000000001)
  }
}

// MARK: - Math functions

extension Double {
  /// Returns the absolute value of the given number.
  @_transparent
  public static func abs(_ x: Double) -> Double {
    return x < 0 ? -x : x
  }
}

/// Returns the absolute value of the given floating-point value.
@_transparent
public func abs(_ x: Double) -> Double {
  return Double.abs(x)
}

// MARK: - Helper protocols

/// A type that represents a floating-point sign.
public enum FloatingPointSign: Int, CaseIterable {
  /// The positive sign.
  case plus = 1
  
  /// The negative sign.
  case minus = -1
}

/// A protocol for floating-point types.
public protocol FloatingPoint: Numeric, Comparable {
  /// The radix, or base of exponentiation, for this floating-point type.
  static var radix: Int { get }

  /// The number of digits that can be represented without losing precision.
  static var significandBitCount: Int { get }

  /// Positive infinity.
  static var infinity: Self { get }

  /// A quiet NaN ("not a number").
  static var nan: Self { get }

  /// A signaling NaN ("not a number").
  static var signalingNaN: Self { get }

  /// The greatest finite number representable by this type.
  static var greatestFiniteMagnitude: Self { get }

  /// The mathematical constant pi.
  static var pi: Self { get }

  /// The unit in the last place of this value.
  var ulp: Self { get }

  /// The least representable value that compares greater than this value.
  var nextUp: Self { get }

  /// The greatest representable value that compares less than this value.
  var nextDown: Self { get }

  /// The exponent of this value.
  var exponent: Int { get }

  /// The significand of this value.
  var significand: Self { get }

  /// Creates a floating-point value from components.
  init(sign: FloatingPointSign, exponent: Int, significand: Self)

  /// A Boolean value indicating whether this instance is equal to zero.
  var isZero: Bool { get }

  /// A Boolean value indicating whether this instance is finite.
  var isFinite: Bool { get }

  /// A Boolean value indicating whether the instance is infinite.
  var isInfinite: Bool { get }

  /// A Boolean value indicating whether the instance is NaN.
  var isNaN: Bool { get }

  /// A Boolean value indicating whether the instance is a signaling NaN.
  var isSignalingNaN: Bool { get }

  /// The sign of the floating-point value.
  var sign: FloatingPointSign { get }
}

// MARK: - Runtime support functions

/// Converts a Double to its string representation.
@usableFromInline
internal func _doubleToString(_ value: Double) -> String {
  // This would be implemented in the runtime
  // For now, provide a simplified version
  if value.isNaN { return "nan" }
  if value.isInfinite { return value.sign == .minus ? "-inf" : "inf" }
  if value.isZero { return value.sign == .minus ? "-0.0" : "0.0" }
  
  // Simplified string conversion
  return _swift_double_to_string(value)
}

/// Runtime function to convert double to string (implemented in C++).
@_silgen_name("_swift_double_to_string")
internal func _swift_double_to_string(_ value: Double) -> String
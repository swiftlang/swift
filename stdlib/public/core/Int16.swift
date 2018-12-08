//===--- Int16.swift ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A 16-bit signed integer value
/// type.
@_fixed_layout
public struct Int16
  : FixedWidthInteger, SignedInteger,
    _ExpressibleByBuiltinIntegerLiteral {

  /// A type that represents an integer literal.
  public typealias IntegerLiteralType = Int16

  @_transparent
  public init(_builtinIntegerLiteral x: Builtin.IntLiteral) {
    _value = Builtin.s_to_s_checked_trunc_IntLiteral_Int16(x).0
  }

  /// Creates a new instance with the same memory representation as the given
  /// value.
  ///
  /// This initializer does not perform any range or overflow checking. The
  /// resulting instance may not have the same numeric value as
  /// `bitPattern`---it is only guaranteed to use the same pattern of bits in
  /// its binary representation.
  ///
  /// - Parameter x: A value to use as the source of the new instance's binary
  ///   representation.
  @_transparent
  public init(bitPattern x: UInt16) {
    _value = x._value
  }

  /// Creates an integer from the given floating-point value, rounding toward
  /// zero.
  ///
  /// Any fractional part of the value passed as `source` is removed, rounding
  /// the value toward zero.
  ///
  ///     let x = Int(21.5)
  ///     // x == 21
  ///     let y = Int(-21.5)
  ///     // y == -21
  ///
  /// If `source` is outside the bounds of this type after rounding toward
  /// zero, a runtime error may occur.
  ///
  ///     let z = UInt(-21.5)
  ///     // Error: ...the result would be less than UInt.min
  ///
  /// - Parameter source: A floating-point value to convert to an integer.
  ///   `source` must be representable in this type after rounding toward
  ///   zero.
  @_transparent
  public init(_ source: Float) {
    _precondition(source.isFinite,
      "Float value cannot be converted to Int16 because it is either infinite or NaN")
    _precondition(source > -32769.0,
      "Float value cannot be converted to Int16 because the result would be less than Int16.min")
    _precondition(source < 32768.0,
      "Float value cannot be converted to Int16 because the result would be greater than Int16.max")
    self._value = Builtin.fptosi_FPIEEE32_Int16(source._value)
  }

  /// Creates an integer from the given floating-point value, if it can be
  /// represented exactly.
  ///
  /// If the value passed as `source` is not representable exactly, the result
  /// is `nil`. In the following example, the constant `x` is successfully
  /// created from a value of `21.0`, while the attempt to initialize the
  /// constant `y` from `21.5` fails:
  ///
  ///     let x = Int(exactly: 21.0)
  ///     // x == Optional(21)
  ///     let y = Int(exactly: 21.5)
  ///     // y == nil
  ///
  /// - Parameter source: A floating-point value to convert to an integer.
  @_transparent
  public init?(exactly source: Float) {
    // The value passed as `source` must not be infinite, NaN, or exceed the
    // bounds of the integer type; the result of `fptosi` or `fptoui` is
    // undefined if it overflows.
    guard source > -32769.0 && source < 32768.0 else {
      return nil
    }
    guard source == source.rounded(.towardZero) else {
      return nil
    }
    self._value = Builtin.fptosi_FPIEEE32_Int16(source._value)
  }

  /// Creates an integer from the given floating-point value, rounding toward
  /// zero.
  ///
  /// Any fractional part of the value passed as `source` is removed, rounding
  /// the value toward zero.
  ///
  ///     let x = Int(21.5)
  ///     // x == 21
  ///     let y = Int(-21.5)
  ///     // y == -21
  ///
  /// If `source` is outside the bounds of this type after rounding toward
  /// zero, a runtime error may occur.
  ///
  ///     let z = UInt(-21.5)
  ///     // Error: ...the result would be less than UInt.min
  ///
  /// - Parameter source: A floating-point value to convert to an integer.
  ///   `source` must be representable in this type after rounding toward
  ///   zero.
  @_transparent
  public init(_ source: Double) {
    _precondition(source.isFinite,
      "Double value cannot be converted to Int16 because it is either infinite or NaN")
    _precondition(source > -32769.0,
      "Double value cannot be converted to Int16 because the result would be less than Int16.min")
    _precondition(source < 32768.0,
      "Double value cannot be converted to Int16 because the result would be greater than Int16.max")
    self._value = Builtin.fptosi_FPIEEE64_Int16(source._value)
  }

  /// Creates an integer from the given floating-point value, if it can be
  /// represented exactly.
  ///
  /// If the value passed as `source` is not representable exactly, the result
  /// is `nil`. In the following example, the constant `x` is successfully
  /// created from a value of `21.0`, while the attempt to initialize the
  /// constant `y` from `21.5` fails:
  ///
  ///     let x = Int(exactly: 21.0)
  ///     // x == Optional(21)
  ///     let y = Int(exactly: 21.5)
  ///     // y == nil
  ///
  /// - Parameter source: A floating-point value to convert to an integer.
  @_transparent
  public init?(exactly source: Double) {
    // The value passed as `source` must not be infinite, NaN, or exceed the
    // bounds of the integer type; the result of `fptosi` or `fptoui` is
    // undefined if it overflows.
    guard source > -32769.0 && source < 32768.0 else {
      return nil
    }
    guard source == source.rounded(.towardZero) else {
      return nil
    }
    self._value = Builtin.fptosi_FPIEEE64_Int16(source._value)
  }

#if !os(Windows) && (arch(i386) || arch(x86_64))

  /// Creates an integer from the given floating-point value, rounding toward
  /// zero.
  ///
  /// Any fractional part of the value passed as `source` is removed, rounding
  /// the value toward zero.
  ///
  ///     let x = Int(21.5)
  ///     // x == 21
  ///     let y = Int(-21.5)
  ///     // y == -21
  ///
  /// If `source` is outside the bounds of this type after rounding toward
  /// zero, a runtime error may occur.
  ///
  ///     let z = UInt(-21.5)
  ///     // Error: ...the result would be less than UInt.min
  ///
  /// - Parameter source: A floating-point value to convert to an integer.
  ///   `source` must be representable in this type after rounding toward
  ///   zero.
  @_transparent
  public init(_ source: Float80) {
    _precondition(source.isFinite,
      "Float80 value cannot be converted to Int16 because it is either infinite or NaN")
    _precondition(source > -32769.0,
      "Float80 value cannot be converted to Int16 because the result would be less than Int16.min")
    _precondition(source < 32768.0,
      "Float80 value cannot be converted to Int16 because the result would be greater than Int16.max")
    self._value = Builtin.fptosi_FPIEEE80_Int16(source._value)
  }

  /// Creates an integer from the given floating-point value, if it can be
  /// represented exactly.
  ///
  /// If the value passed as `source` is not representable exactly, the result
  /// is `nil`. In the following example, the constant `x` is successfully
  /// created from a value of `21.0`, while the attempt to initialize the
  /// constant `y` from `21.5` fails:
  ///
  ///     let x = Int(exactly: 21.0)
  ///     // x == Optional(21)
  ///     let y = Int(exactly: 21.5)
  ///     // y == nil
  ///
  /// - Parameter source: A floating-point value to convert to an integer.
  @_transparent
  public init?(exactly source: Float80) {
    // The value passed as `source` must not be infinite, NaN, or exceed the
    // bounds of the integer type; the result of `fptosi` or `fptoui` is
    // undefined if it overflows.
    guard source > -32769.0 && source < 32768.0 else {
      return nil
    }
    guard source == source.rounded(.towardZero) else {
      return nil
    }
    self._value = Builtin.fptosi_FPIEEE80_Int16(source._value)
  }

#endif

  @_transparent
  public static func == (lhs: Int16, rhs: Int16) -> Bool {
    return Bool(Builtin.cmp_eq_Int16(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: Int16, rhs: Int16) -> Bool {
    return Bool(Builtin.cmp_slt_Int16(lhs._value, rhs._value))
  }

  /// Adds two values and stores the result in the left-hand-side variable.
  ///
  /// The sum of the two arguments must be representable in the arguments'
  /// type. In the following example, the result of `21 + 120` is greater than
  /// the maximum representable `Int8` value:
  ///
  ///     var x: Int8 = 21
  ///     x += 120
  ///     // Overflow error
  ///
  /// - Note: Overflow checking is not performed in `-Ounchecked` builds.
  ///
  /// - Parameters:
  ///   - lhs: The first value to add.
  ///   - rhs: The second value to add.
  @_transparent
  public static func +=(lhs: inout Int16, rhs: Int16) {
    let (result, overflow) =
      Builtin.sadd_with_overflow_Int16(
        lhs._value, rhs._value, true._value)
    Builtin.condfail(overflow)
    lhs = Int16(result)
  }

  /// Subtracts the second value from the first and stores the difference in the
  /// left-hand-side variable.
  ///
  /// The difference of the two arguments must be representable in the
  /// arguments' type. In the following example, the result of `21 - 50` is
  /// less than zero, the minimum representable `UInt8` value:
  ///
  ///     var x: UInt8 = 21
  ///     x - 50
  ///     // Overflow error
  ///
  /// - Note: Overflow checking is not performed in `-Ounchecked` builds.
  ///
  /// - Parameters:
  ///   - lhs: A numeric value.
  ///   - rhs: The value to subtract from `lhs`.
  @_transparent
  public static func -=(lhs: inout Int16, rhs: Int16) {
    let (result, overflow) =
      Builtin.ssub_with_overflow_Int16(
        lhs._value, rhs._value, true._value)
    Builtin.condfail(overflow)
    lhs = Int16(result)
  }

  /// Multiplies two values and stores the result in the left-hand-side
  /// variable.
  ///
  /// The product of the two arguments must be representable in the arguments'
  /// type. In the following example, the result of `21 * 21` is greater than
  /// the maximum representable `Int8` value:
  ///
  ///     var x: Int8 = 21
  ///     x * 21
  ///     // Overflow error
  ///
  /// - Note: Overflow checking is not performed in `-Ounchecked` builds.
  ///
  /// - Parameters:
  ///   - lhs: The first value to multiply.
  ///   - rhs: The second value to multiply.
  @_transparent
  public static func *=(lhs: inout Int16, rhs: Int16) {
    let (result, overflow) =
      Builtin.smul_with_overflow_Int16(
        lhs._value, rhs._value, true._value)
    Builtin.condfail(overflow)
    lhs = Int16(result)
  }

  /// Divides the first value by the second and stores the quotient in the
  /// left-hand-side variable.
  ///
  /// For integer types, any remainder of the division is discarded.
  ///
  ///     var x = 21
  ///     x /= 5
  ///     // x == 4
  ///
  /// - Parameters:
  ///   - lhs: The value to divide.
  ///   - rhs: The value to divide `lhs` by. `rhs` must not be zero.
  @_transparent
  public static func /=(lhs: inout Int16, rhs: Int16) {
    // No LLVM primitives for checking overflow of division operations, so we
    // check manually.
    if _slowPath(rhs == (0 as Int16)) {
      _preconditionFailure(
        "Division by zero")
    }

    if _slowPath(
      lhs == Int16.min && rhs == (-1 as Int16)
    ) {
      _preconditionFailure(
        "Division results in an overflow")
    }

    let (result, overflow) =
      (Builtin.sdiv_Int16(lhs._value, rhs._value),
      false._value)

    Builtin.condfail(overflow)
    lhs = Int16(result)
  }

  /// Returns the quotient obtained by dividing this value by the given value,
  /// along with a Boolean value indicating whether overflow occurred in the
  /// operation.
  ///
  /// Dividing by zero is not an error when using this method. For a value `x`,
  /// the result of `x.dividedReportingOverflow(by: 0)` is `(x, true)`.
  ///
  /// - Parameter rhs: The value to divide this value by.
  /// - Returns: A tuple containing the result of the division along with a
  ///   Boolean value indicating whether overflow occurred. If the `overflow`
  ///   component is `false`, the `partialValue` component contains the entire
  ///   quotient. If the `overflow` component is `true`, an overflow occurred
  ///   and the `partialValue` component contains either the truncated quotient
  ///   or, if the quotient is undefined, the dividend.
  @_transparent
  public func dividedReportingOverflow(
    by other: Int16
  ) -> (partialValue: Int16, overflow: Bool) {
    // No LLVM primitives for checking overflow of division operations, so we
    // check manually.
    if _slowPath(other == (0 as Int16)) {
      return (partialValue: self, overflow: true)
    }

    if _slowPath(self == Int16.min && other == (-1 as Int16)) {

      return (partialValue: self, overflow: true)
    }

    let (newStorage, overflow) = (
      Builtin.sdiv_Int16(self._value, other._value),
      false._value)

    return (
      partialValue: Int16(newStorage),
      overflow: Bool(overflow))
  }

  /// Returns the remainder after dividing this value by the given value, along
  /// with a Boolean value indicating whether overflow occurred during division.
  ///
  /// Dividing by zero is not an error when using this method. For a value `x`,
  /// the result of `x.remainderReportingOverflow(dividingBy: 0)` is
  /// `(x, true)`.
  ///
  /// - Parameter rhs: The value to divide this value by.
  /// - Returns: A tuple containing the result of the operation along with a
  ///   Boolean value indicating whether overflow occurred. If the `overflow`
  ///   component is `false`, the `partialValue` component contains the entire
  ///   remainder. If the `overflow` component is `true`, an overflow occurred
  ///   during division and the `partialValue` component contains either the
  ///   entire remainder or, if the remainder is undefined, the dividend.
  @_transparent
  public func remainderReportingOverflow(
    dividingBy other: Int16
  ) -> (partialValue: Int16, overflow: Bool) {
    // No LLVM primitives for checking overflow of division operations, so we
    // check manually.
    if _slowPath(other == (0 as Int16)) {
      return (partialValue: self, overflow: true)
    }

    if _slowPath(self == Int16.min && other == (-1 as Int16)) {

      return (partialValue: 0, overflow: true)
    }

    let (newStorage, overflow) = (
      Builtin.srem_Int16(self._value, other._value),
      false._value)

    return (
      partialValue: Int16(newStorage),
      overflow: Bool(overflow))
  }

  /// Returns the sum of this value and the given value, along with a Boolean
  /// value indicating whether overflow occurred in the operation.
  ///
  /// - Parameter rhs: The value to add to this value.
  /// - Returns: A tuple containing the result of the addition along with a
  ///   Boolean value indicating whether overflow occurred. If the `overflow`
  ///   component is `false`, the `partialValue` component contains the entire
  ///   sum. If the `overflow` component is `true`, an overflow occurred and
  ///   the `partialValue` component contains the truncated sum of this value
  ///   and `rhs`.
  @_transparent
  public func addingReportingOverflow(
    _ other: Int16
  ) -> (partialValue: Int16, overflow: Bool) {
    let (newStorage, overflow) =
      Builtin.sadd_with_overflow_Int16(
        self._value, other._value, false._value)

    return (
      partialValue: Int16(newStorage),
      overflow: Bool(overflow))
  }

  /// Returns the difference obtained by subtracting the given value from this
  /// value, along with a Boolean value indicating whether overflow occurred in
  /// the operation.
  ///
  /// - Parameter rhs: The value to subtract from this value.
  /// - Returns: A tuple containing the result of the subtraction along with a
  ///   Boolean value indicating whether overflow occurred. If the `overflow`
  ///   component is `false`, the `partialValue` component contains the entire
  ///   difference. If the `overflow` component is `true`, an overflow occurred
  ///   and the `partialValue` component contains the truncated result of `rhs`
  ///   subtracted from this value.
  @_transparent
  public func subtractingReportingOverflow(
    _ other: Int16
  ) -> (partialValue: Int16, overflow: Bool) {
    let (newStorage, overflow) =
      Builtin.ssub_with_overflow_Int16(
        self._value, other._value, false._value)

    return (
      partialValue: Int16(newStorage),
      overflow: Bool(overflow))
  }

  /// Returns the product of this value and the given value, along with a
  /// Boolean value indicating whether overflow occurred in the operation.
  ///
  /// - Parameter rhs: The value to multiply by this value.
  /// - Returns: A tuple containing the result of the multiplication along with
  ///   a Boolean value indicating whether overflow occurred. If the `overflow`
  ///   component is `false`, the `partialValue` component contains the entire
  ///   product. If the `overflow` component is `true`, an overflow occurred and
  ///   the `partialValue` component contains the truncated product of this
  ///   value and `rhs`.
  @_transparent
  public func multipliedReportingOverflow(
    by other: Int16
  ) -> (partialValue: Int16, overflow: Bool) {
    let (newStorage, overflow) =
      Builtin.smul_with_overflow_Int16(
        self._value, other._value, false._value)

    return (
      partialValue: Int16(newStorage),
      overflow: Bool(overflow))
  }

  /// Divides the first value by the second and stores the remainder in the
  /// left-hand-side variable.
  ///
  /// The result has the same sign as `lhs` and has a magnitude less than
  /// `rhs.magnitude`.
  ///
  ///     var x = 22
  ///     x %= 5
  ///     // x == 2
  ///
  ///     var y = 22
  ///     y %= -5
  ///     // y == 2
  ///
  ///     var z = -22
  ///     z %= -5
  ///     // z == -2
  ///
  /// - Parameters:
  ///   - lhs: The value to divide.
  ///   - rhs: The value to divide `lhs` by. `rhs` must not be zero.
  @_transparent
  public static func %=(lhs: inout Int16, rhs: Int16) {
    // No LLVM primitives for checking overflow of division operations, so we
    // check manually.
    if _slowPath(rhs == (0 as Int16)) {
      _preconditionFailure(
        "Division by zero in remainder operation")
    }

    if _slowPath(lhs == Int16.min && rhs == (-1 as Int16)) {
      _preconditionFailure(
        "Division results in an overflow in remainder operation")
    }

    let (newStorage, _) = (
      Builtin.srem_Int16(lhs._value, rhs._value),
      false._value)
    lhs = Int16(newStorage)
  }

  @_transparent
  public init(_ _value: Builtin.Int16) {
    self._value = _value
  }

  /// Stores the result of performing a bitwise AND operation on the two given
  /// values in the left-hand-side variable.
  ///
  /// A bitwise AND operation results in a value that has each bit set to `1`
  /// where *both* of its arguments have that bit set to `1`. For example:
  ///
  ///     var x: UInt8 = 5          // 0b00000101
  ///     let y: UInt8 = 14         // 0b00001110
  ///     x &= y                    // 0b00000100
  ///
  /// - Parameters:
  ///   - lhs: An integer value.
  ///   - rhs: Another integer value.
  @_transparent
  public static func &=(lhs: inout Int16, rhs: Int16) {
    lhs = Int16(Builtin.and_Int16(lhs._value, rhs._value))
  }

  /// Stores the result of performing a bitwise OR operation on the two given
  /// values in the left-hand-side variable.
  ///
  /// A bitwise OR operation results in a value that has each bit set to `1`
  /// where *one or both* of its arguments have that bit set to `1`. For
  /// example:
  ///
  ///     var x: UInt8 = 5          // 0b00000101
  ///     let y: UInt8 = 14         // 0b00001110
  ///     x |= y                    // 0b00001111
  ///
  /// - Parameters:
  ///   - lhs: An integer value.
  ///   - rhs: Another integer value.
  @_transparent
  public static func |=(lhs: inout Int16, rhs: Int16) {
    lhs = Int16(Builtin.or_Int16(lhs._value, rhs._value))
  }

  /// Stores the result of performing a bitwise XOR operation on the two given
  /// values in the left-hand-side variable.
  ///
  /// A bitwise XOR operation, also known as an exclusive OR operation, results
  /// in a value that has each bit set to `1` where *one or the other but not
  /// both* of its arguments had that bit set to `1`. For example:
  ///
  ///     var x: UInt8 = 5          // 0b00000101
  ///     let y: UInt8 = 14         // 0b00001110
  ///     x ^= y                    // 0b00001011
  ///
  /// - Parameters:
  ///   - lhs: An integer value.
  ///   - rhs: Another integer value.
  @_transparent
  public static func ^=(lhs: inout Int16, rhs: Int16) {
    lhs = Int16(Builtin.xor_Int16(lhs._value, rhs._value))
  }

  /// Calculates the result of shifting a value's binary representation the
  /// specified number of digits to the right, masking the shift amount to the
  /// type's bit width, and stores the result in the left-hand-side variable.
  ///
  /// The `&>>=` operator performs a *masking shift*, where the value passed as
  /// `rhs` is masked to produce a value in the range `0..<lhs.bitWidth`. The
  /// shift is performed using this masked value.
  ///
  /// The following example defines `x` as an instance of `UInt8`, an 8-bit,
  /// unsigned integer type. If you use `2` as the right-hand-side value in an
  /// operation on `x`, the shift amount requires no masking.
  ///
  ///     var x: UInt8 = 30                 // 0b00011110
  ///     x &>>= 2
  ///     // x == 7                         // 0b00000111
  ///
  /// However, if you use `19` as `rhs`, the operation first bitmasks `rhs` to
  /// `3`, and then uses that masked value as the number of bits to shift `lhs`.
  ///
  ///     var y: UInt8 = 30                 // 0b00011110
  ///     y &>>= 19
  ///     // y == 3                         // 0b00000011
  ///
  /// - Parameters:
  ///   - lhs: The value to shift.
  ///   - rhs: The number of bits to shift `lhs` to the right. If `rhs` is
  ///     outside the range `0..<lhs.bitWidth`, it is masked to produce a
  ///     value within that range.
  @_transparent
  public static func &>>=(lhs: inout Int16, rhs: Int16) {
    let rhs_ = rhs & 15
    lhs = Int16(
      Builtin.ashr_Int16(lhs._value, rhs_._value))
  }

  /// Returns the result of shifting a value's binary representation the
  /// specified number of digits to the left, masking the shift amount to the
  /// type's bit width, and stores the result in the left-hand-side variable.
  ///
  /// The `&<<=` operator performs a *masking shift*, where the value used as
  /// `rhs` is masked to produce a value in the range `0..<lhs.bitWidth`. The
  /// shift is performed using this masked value.
  ///
  /// The following example defines `x` as an instance of `UInt8`, an 8-bit,
  /// unsigned integer type. If you use `2` as the right-hand-side value in an
  /// operation on `x`, the shift amount requires no masking.
  ///
  ///     var x: UInt8 = 30                 // 0b00011110
  ///     x &<<= 2
  ///     // x == 120                       // 0b01111000
  ///
  /// However, if you pass `19` as `rhs`, the method first bitmasks `rhs` to
  /// `3`, and then uses that masked value as the number of bits to shift `lhs`.
  ///
  ///     var y: UInt8 = 30                 // 0b00011110
  ///     y &<<= 19
  ///     // y == 240                       // 0b11110000
  ///
  /// - Parameters:
  ///   - lhs: The value to shift.
  ///   - rhs: The number of bits to shift `lhs` to the left. If `rhs` is
  ///     outside the range `0..<lhs.bitWidth`, it is masked to produce a
  ///     value within that range.
  @_transparent
  public static func &<<=(lhs: inout Int16, rhs: Int16) {
    let rhs_ = rhs & 15
    lhs = Int16(
      Builtin.shl_Int16(lhs._value, rhs_._value))
  }

  /// The number of bits used for the underlying binary representation of
  /// values of this type.
  ///
  /// The bit width of a `Int16` instance is 16.
  @_transparent
  public static var bitWidth : Int { return 16 }

  /// The number of leading zeros in this value's binary representation.
  ///
  /// For example, in an integer type with a `bitWidth` value of 8,
  /// the number *31* has three leading zeros.
  ///
  ///     let x: Int8 = 0b0001_1111
  ///     // x == 31
  ///     // x.leadingZeroBitCount == 3
  @_transparent
  public var leadingZeroBitCount: Int {
    return Int(
      Int16(
        Builtin.int_ctlz_Int16(self._value, false._value)
      )._lowWord._value)
  }

  /// The number of trailing zeros in this value's binary representation.
  ///
  /// For example, the number *-8* has three trailing zeros.
  ///
  ///     let x = Int8(bitPattern: 0b1111_1000)
  ///     // x == -8
  ///     // x.trailingZeroBitCount == 3
  @_transparent
  public var trailingZeroBitCount: Int {
    return Int(
      Int16(
        Builtin.int_cttz_Int16(self._value, false._value)
      )._lowWord._value)
  }

  /// The number of bits equal to 1 in this value's binary representation.
  ///
  /// For example, in a fixed-width integer type with a `bitWidth` value of 8,
  /// the number *31* has five bits equal to *1*.
  ///
  ///     let x: Int8 = 0b0001_1111
  ///     // x == 31
  ///     // x.nonzeroBitCount == 5
  @_transparent
  public var nonzeroBitCount: Int {
    return Int(
      Int16(
        Builtin.int_ctpop_Int16(self._value)
      )._lowWord._value)
  }

  /// A type that represents the words of this integer.
  @_fixed_layout
  public struct Words : RandomAccessCollection {
    public typealias Indices = Range<Int>
    public typealias SubSequence = Slice<Int16.Words>

    @usableFromInline
    internal var _value: Int16

    @inlinable
    public init(_ value: Int16) {
      self._value = value
    }

    @inlinable
    public var count: Int {
      return (16 + 64 - 1) / 64
    }

    @inlinable
    public var startIndex: Int { return 0 }

    @inlinable
    public var endIndex: Int { return count }

    @inlinable
    public var indices: Indices { return startIndex ..< endIndex }

    @_transparent
    public func index(after i: Int) -> Int { return i + 1 }

    @_transparent
    public func index(before i: Int) -> Int { return i - 1 }

    @inlinable
    public subscript(position: Int) -> UInt {
      get {
        _precondition(position >= 0, "Negative word index")
        _precondition(position < endIndex, "Word index out of range")
        let shift = UInt(position._value) &* 64
        _internalInvariant(shift < UInt(_value.bitWidth._value))
        return (_value &>> Int16(_truncatingBits: shift))._lowWord
      }
    }
  }

  /// A collection containing the words of this value's binary
  /// representation, in order from the least significant to most significant.
  ///
  /// Negative values are returned in two's complement representation,
  /// regardless of the type's underlying implementation.
  @_transparent
  public var words: Words {
    return Words(self)
  }

  @_transparent
  public // transparent
  var _lowWord: UInt {
    return UInt(
      Builtin.sextOrBitCast_Int16_Int64(_value)
    )
  }

  @_transparent
  public // transparent
  init(_truncatingBits bits: UInt) {
    self.init(
      Builtin.truncOrBitCast_Int64_Int16(bits._value))
  }

  /// A type that can represent the absolute value of any possible value of
  /// this type.
  public typealias Magnitude = UInt16

  /// The magnitude of this value.
  ///
  /// For any numeric value `x`, `x.magnitude` is the absolute value of `x`.
  /// You can use the `magnitude` property in operations that are simpler to
  /// implement in terms of unsigned values, such as printing the value of an
  /// integer, which is just printing a '-' character in front of an absolute
  /// value.
  ///
  ///     let x = -200
  ///     // x.magnitude == 200
  ///
  /// The global `abs(_:)` function provides more familiar syntax when you need
  /// to find an absolute value. In addition, because `abs(_:)` always returns
  /// a value of the same type, even in a generic context, using the function
  /// instead of the `magnitude` property is encouraged.
  @_transparent
  public var magnitude: UInt16 {
    let base = UInt16(_value)
    return self < (0 as Int16) ? ~base &+ 1 : base
  }

  /// Returns a tuple containing the high and low parts of the result of
  /// multiplying this value by the given value.
  ///
  /// Use this method to calculate the full result of a product that would
  /// otherwise overflow. Unlike traditional truncating multiplication, the
  /// `multipliedFullWidth(by:)` method returns a tuple
  /// containing both the `high` and `low` parts of the product of this value and
  /// `other`. The following example uses this method to multiply two `UInt8`
  /// values that normally overflow when multiplied:
  ///
  ///     let x: UInt8 = 100
  ///     let y: UInt8 = 20
  ///     let result = x.multipliedFullWidth(by: y)
  ///     // result.high == 0b00000111
  ///     // result.low  == 0b11010000
  ///
  /// The product of `x` and `y` is 2000, which is too large to represent in a
  /// `UInt8` instance. The `high` and `low` properties of the `result` value
  /// represent 2000 when concatenated to form a double-width integer; that
  /// is, using `result.high` as the high byte and `result.low` as the low byte
  /// of a `UInt16` instance.
  ///
  ///     let z = UInt16(result.high) << 8 | UInt16(result.low)
  ///     // z == 2000
  ///
  /// - Parameter other: The value to multiply this value by.
  /// - Returns: A tuple containing the high and low parts of the result of
  ///   multiplying this value and `other`.
  @inlinable
  public func multipliedFullWidth(
    by other: Int16
  ) -> (high: Int16, low: Int16.Magnitude) {
    // FIXME(integers): tests
    let lhs_ = Builtin.sext_Int16_Int32(self._value)
    let rhs_ = Builtin.sext_Int16_Int32(other._value)
    let res = Builtin.mul_Int32(lhs_, rhs_)
    let low = Int16.Magnitude(Builtin.truncOrBitCast_Int32_Int16(res))
    let shift = Builtin.zextOrBitCast_Int8_Int32(UInt8(16)._value)
    let shifted = Builtin.ashr_Int32(res, shift)
    let high = Int16(Builtin.truncOrBitCast_Int32_Int16(shifted))
    return (high: high, low: low)
  }

  /// Returns a tuple containing the quotient and remainder of dividing the
  /// given value by this value.
  ///
  /// The resulting quotient must be representable within the bounds of the
  /// type. If the quotient of dividing `dividend` by this value is too large
  /// to represent in the type, a runtime error may occur.
  ///
  /// - Parameter dividend: A tuple containing the high and low parts of a
  ///   double-width integer. The `high` component of the value carries the
  ///   sign, if the type is signed.
  /// - Returns: A tuple containing the quotient and remainder of `dividend`
  ///   divided by this value.
  @inlinable
  public func dividingFullWidth(
    _ dividend: (high: Int16, low: Int16.Magnitude)
  ) -> (quotient: Int16, remainder: Int16) {
    // FIXME(integers): tests
    // FIXME(integers): handle division by zero and overflows
    _precondition(self != 0, "Division by zero")
    let lhsHigh = Builtin.sext_Int16_Int32(dividend.high._value)
    let shift = Builtin.zextOrBitCast_Int8_Int32(UInt8(16)._value)
    let lhsHighShifted = Builtin.shl_Int32(lhsHigh, shift)
    let lhsLow = Builtin.zext_Int16_Int32(dividend.low._value)
    let lhs_ = Builtin.or_Int32(lhsHighShifted, lhsLow)
    let rhs_ = Builtin.sext_Int16_Int32(self._value)
    let quotient_ = Builtin.sdiv_Int32(lhs_, rhs_)
    let remainder_ = Builtin.srem_Int32(lhs_, rhs_)
    let quotient = Int16(
      Builtin.truncOrBitCast_Int32_Int16(quotient_))
    let remainder = Int16(
      Builtin.truncOrBitCast_Int32_Int16(remainder_))
    return (quotient: quotient, remainder: remainder)
  }

  /// A representation of this integer with the byte order swapped.
  @_transparent
  public var byteSwapped: Int16 {
    return Int16(Builtin.int_bswap_Int16(_value))
  }

  // Implementation details
  public var _value: Builtin.Int16

  @available(swift, obsoleted: 4.0, message: "Use initializers instead")
  public func toUIntMax() -> UInt64 {
    return numericCast(self)
  }

  @available(swift, obsoleted: 4, message: "Use bitWidth instead.")
  public static var _sizeInBits: Int16 { return 16 }

  @available(swift, obsoleted: 4)
  public static var _sizeInBytes: Int16 { return 16/8 }

  /// Returns `-1` if this value is negative and `1` if it's positive;
  /// otherwise, `0`.
  ///
  /// - Returns: The sign of this number, expressed as an integer of the same
  ///   type.
  @inlinable
  @inline(__always)
  public func signum() -> Int16 {
    let isPositive = Int16(Builtin.zext_Int1_Int16(
      (self > (0 as Int16))._value))
    return isPositive | (self &>> 15)
  }
}

extension Int16 : Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher._combine(UInt16(_value))
  }

  @inlinable
  public func _rawHashValue(seed: Int) -> Int {
    return Hasher._hash(
      seed: seed,
      bytes: UInt64(truncatingIfNeeded: UInt16(_value)),
      count: 2)
  }
}

extension Int16 : _HasCustomAnyHashableRepresentation {
  // Not @inlinable
  public func _toCustomAnyHashable() -> AnyHashable? {
    return AnyHashable(_box: _IntegerAnyHashableBox(self))
  }
}

extension Int16 {
  /// Creates a new instance with the same bitwise representation as the least
  /// significant bits of the given value.
  ///
  /// This initializer performs no range or overflow checking. The resulting
  /// instance may have a different numeric value from `source`.
  ///
  /// - Parameter source: An integer to use as the source of the new value's
  ///   bit pattern.
  @available(swift, obsoleted: 4.0, renamed: "init(truncatingIfNeeded:)")
  @_transparent
  public init(truncatingBitPattern source: UInt32) {
    let src = source._value
    let dstNotWord = Builtin.trunc_Int32_Int16(src)
    self._value = dstNotWord
  }
}

extension Int16 {
  /// Creates a new instance with the same bitwise representation as the least
  /// significant bits of the given value.
  ///
  /// This initializer performs no range or overflow checking. The resulting
  /// instance may have a different numeric value from `source`.
  ///
  /// - Parameter source: An integer to use as the source of the new value's
  ///   bit pattern.
  @available(swift, obsoleted: 4.0, renamed: "init(truncatingIfNeeded:)")
  @_transparent
  public init(truncatingBitPattern source: Int32) {
    let src = source._value
    let dstNotWord = Builtin.trunc_Int32_Int16(src)
    self._value = dstNotWord
  }
}

extension Int16 {
  /// Creates a new instance with the same bitwise representation as the least
  /// significant bits of the given value.
  ///
  /// This initializer performs no range or overflow checking. The resulting
  /// instance may have a different numeric value from `source`.
  ///
  /// - Parameter source: An integer to use as the source of the new value's
  ///   bit pattern.
  @available(swift, obsoleted: 4.0, renamed: "init(truncatingIfNeeded:)")
  @_transparent
  public init(truncatingBitPattern source: UInt64) {
    let src = source._value
    let dstNotWord = Builtin.trunc_Int64_Int16(src)
    self._value = dstNotWord
  }
}

extension Int16 {
  /// Creates a new instance with the same bitwise representation as the least
  /// significant bits of the given value.
  ///
  /// This initializer performs no range or overflow checking. The resulting
  /// instance may have a different numeric value from `source`.
  ///
  /// - Parameter source: An integer to use as the source of the new value's
  ///   bit pattern.
  @available(swift, obsoleted: 4.0, renamed: "init(truncatingIfNeeded:)")
  @_transparent
  public init(truncatingBitPattern source: Int64) {
    let src = source._value
    let dstNotWord = Builtin.trunc_Int64_Int16(src)
    self._value = dstNotWord
  }
}

extension Int16 {
  /// Creates a new instance with the same bitwise representation as the least
  /// significant bits of the given value.
  ///
  /// This initializer performs no range or overflow checking. The resulting
  /// instance may have a different numeric value from `source`.
  ///
  /// - Parameter source: An integer to use as the source of the new value's
  ///   bit pattern.
  @available(swift, obsoleted: 4.0, renamed: "init(truncatingIfNeeded:)")
  @_transparent
  public init(truncatingBitPattern source: UInt) {
    let src = source._value
    let dstNotWord = Builtin.trunc_Int64_Int16(src)
    self._value = dstNotWord
  }
}

extension Int16 {
  /// Creates a new instance with the same bitwise representation as the least
  /// significant bits of the given value.
  ///
  /// This initializer performs no range or overflow checking. The resulting
  /// instance may have a different numeric value from `source`.
  ///
  /// - Parameter source: An integer to use as the source of the new value's
  ///   bit pattern.
  @available(swift, obsoleted: 4.0, renamed: "init(truncatingIfNeeded:)")
  @_transparent
  public init(truncatingBitPattern source: Int) {
    let src = source._value
    let dstNotWord = Builtin.trunc_Int64_Int16(src)
    self._value = dstNotWord
  }
}

// FIXME(integers): this section here is to help the typechecker,
// as it seems to have problems with a pattern where the nonmutating operation
// is defined on a protocol in terms of a mutating one that is itself defined
// on concrete types.
extension Int16 {
  /// Returns the result of performing a bitwise AND operation on the two given
  /// values.
  ///
  /// A bitwise AND operation results in a value that has each bit set to `1`
  /// where *both* of its arguments have that bit set to `1`. For example:
  ///
  ///     let x: UInt8 = 5          // 0b00000101
  ///     let y: UInt8 = 14         // 0b00001110
  ///     let z = x & y             // 0b00000100
  ///     // z == 4
  ///
  /// - Parameters:
  ///   - lhs: An integer value.
  ///   - rhs: Another integer value.
  @_transparent
  public static func &(lhs: Int16, rhs: Int16) -> Int16 {
    var lhs = lhs
    lhs &= rhs
    return lhs
  }

  /// Returns the result of performing a bitwise OR operation on the two given
  /// values.
  ///
  /// A bitwise OR operation results in a value that has each bit set to `1`
  /// where *one or both* of its arguments have that bit set to `1`. For
  /// example:
  ///
  ///     let x: UInt8 = 5          // 0b00000101
  ///     let y: UInt8 = 14         // 0b00001110
  ///     let z = x | y             // 0b00001111
  ///     // z == 15
  ///
  /// - Parameters:
  ///   - lhs: An integer value.
  ///   - rhs: Another integer value.
  @_transparent
  public static func |(lhs: Int16, rhs: Int16) -> Int16 {
    var lhs = lhs
    lhs |= rhs
    return lhs
  }

  /// Returns the result of performing a bitwise XOR operation on the two given
  /// values.
  ///
  /// A bitwise XOR operation, also known as an exclusive OR operation, results
  /// in a value that has each bit set to `1` where *one or the other but not
  /// both* of its arguments had that bit set to `1`. For example:
  ///
  ///     let x: UInt8 = 5          // 0b00000101
  ///     let y: UInt8 = 14         // 0b00001110
  ///     let z = x ^ y             // 0b00001011
  ///     // z == 11
  ///
  /// - Parameters:
  ///   - lhs: An integer value.
  ///   - rhs: Another integer value.
  @_transparent
  public static func ^(lhs: Int16, rhs: Int16) -> Int16 {
    var lhs = lhs
    lhs ^= rhs
    return lhs
  }

  /// Returns the result of shifting a value's binary representation the
  /// specified number of digits to the right, masking the shift amount to the
  /// type's bit width.
  ///
  /// Use the masking right shift operator (`&>>`) when you need to perform a
  /// shift and are sure that the shift amount is in the range
  /// `0..<lhs.bitWidth`. Before shifting, the masking right shift operator
  /// masks the shift to this range. The shift is performed using this masked
  /// value.
  ///
  /// The following example defines `x` as an instance of `UInt8`, an 8-bit,
  /// unsigned integer type. If you use `2` as the right-hand-side value in an
  /// operation on `x`, the shift amount requires no masking.
  ///
  ///     let x: UInt8 = 30                 // 0b00011110
  ///     let y = x &>> 2
  ///     // y == 7                         // 0b00000111
  ///
  /// However, if you use `8` as the shift amount, the method first masks the
  /// shift amount to zero, and then performs the shift, resulting in no change
  /// to the original value.
  ///
  ///     let z = x &>> 8
  ///     // z == 30                        // 0b00011110
  ///
  /// If the bit width of the shifted integer type is a power of two, masking
  /// is performed using a bitmask; otherwise, masking is performed using a
  /// modulo operation.
  ///
  /// - Parameters:
  ///   - lhs: The value to shift.
  ///   - rhs: The number of bits to shift `lhs` to the right. If `rhs` is
  ///     outside the range `0..<lhs.bitWidth`, it is masked to produce a
  ///     value within that range.
  @_transparent
  public static func &>>(lhs: Int16, rhs: Int16) -> Int16 {
    var lhs = lhs
    lhs &>>= rhs
    return lhs
  }

  /// Returns the result of shifting a value's binary representation the
  /// specified number of digits to the left, masking the shift amount to the
  /// type's bit width.
  ///
  /// Use the masking left shift operator (`&<<`) when you need to perform a
  /// shift and are sure that the shift amount is in the range
  /// `0..<lhs.bitWidth`. Before shifting, the masking left shift operator
  /// masks the shift to this range. The shift is performed using this masked
  /// value.
  ///
  /// The following example defines `x` as an instance of `UInt8`, an 8-bit,
  /// unsigned integer type. If you use `2` as the right-hand-side value in an
  /// operation on `x`, the shift amount requires no masking.
  ///
  ///     let x: UInt8 = 30                 // 0b00011110
  ///     let y = x &<< 2
  ///     // y == 120                       // 0b01111000
  ///
  /// However, if you use `8` as the shift amount, the method first masks the
  /// shift amount to zero, and then performs the shift, resulting in no change
  /// to the original value.
  ///
  ///     let z = x &<< 8
  ///     // z == 30                        // 0b00011110
  ///
  /// If the bit width of the shifted integer type is a power of two, masking
  /// is performed using a bitmask; otherwise, masking is performed using a
  /// modulo operation.
  ///
  /// - Parameters:
  ///   - lhs: The value to shift.
  ///   - rhs: The number of bits to shift `lhs` to the left. If `rhs` is
  ///     outside the range `0..<lhs.bitWidth`, it is masked to produce a
  ///     value within that range.
  @_transparent
  public static func &<<(lhs: Int16, rhs: Int16) -> Int16 {
    var lhs = lhs
    lhs &<<= rhs
    return lhs
  }

  /// Returns the quotient of dividing the first value by the second.
  ///
  /// For integer types, any remainder of the division is discarded.
  ///
  ///     let x = 21 / 5
  ///     // x == 4
  ///
  /// - Parameters:
  ///   - lhs: The value to divide.
  ///   - rhs: The value to divide `lhs` by. `rhs` must not be zero.
  @_transparent
  public static func /(lhs: Int16, rhs: Int16) -> Int16 {
    var lhs = lhs
    lhs /= rhs
    return lhs
  }

  /// Returns the remainder of dividing the first value by the second.
  ///
  /// The result of the remainder operator (`%`) has the same sign as `lhs` and
  /// has a magnitude less than `rhs.magnitude`.
  ///
  ///     let x = 22 % 5
  ///     // x == 2
  ///     let y = 22 % -5
  ///     // y == 2
  ///     let z = -22 % -5
  ///     // z == -2
  ///
  /// For any two integers `a` and `b`, their quotient `q`, and their remainder
  /// `r`, `a == b * q + r`.
  ///
  /// - Parameters:
  ///   - lhs: The value to divide.
  ///   - rhs: The value to divide `lhs` by. `rhs` must not be zero.
  @_transparent
  public static func %(lhs: Int16, rhs: Int16) -> Int16 {
    var lhs = lhs
    lhs %= rhs
    return lhs
  }

  /// Adds two values and produces their sum.
  ///
  /// The addition operator (`+`) calculates the sum of its two arguments. For
  /// example:
  ///
  ///     1 + 2                   // 3
  ///     -10 + 15                // 5
  ///     -15 + -5                // -20
  ///     21.5 + 3.25             // 24.75
  ///
  /// You cannot use `+` with arguments of different types. To add values of
  /// different types, convert one of the values to the other value's type.
  ///
  ///     let x: Int8 = 21
  ///     let y: Int = 1000000
  ///     Int(x) + y              // 1000021
  ///
  /// The sum of the two arguments must be representable in the arguments'
  /// type. In the following example, the result of `21 + 120` is greater than
  /// the maximum representable `Int8` value:
  ///
  ///     x + 120                 // Overflow error
  ///
  /// - Note: Overflow checking is not performed in `-Ounchecked` builds.
  ///
  /// If you want to opt out of overflow checking and wrap the result in case
  /// of any overflow, use the overflow addition operator (`&+`).
  ///
  ///     x &+ 120                // -115
  ///
  /// - Parameters:
  ///   - lhs: The first value to add.
  ///   - rhs: The second value to add.
  @_transparent
  public static func +(lhs: Int16, rhs: Int16) -> Int16 {
    var lhs = lhs
    lhs += rhs
    return lhs
  }

  /// Subtracts one value from another and produces their difference.
  ///
  /// The subtraction operator (`-`) calculates the difference of its two
  /// arguments. For example:
  ///
  ///     8 - 3                   // 5
  ///     -10 - 5                 // -15
  ///     100 - -5                // 105
  ///     10.5 - 100.0            // -89.5
  ///
  /// You cannot use `-` with arguments of different types. To subtract values
  /// of different types, convert one of the values to the other value's type.
  ///
  ///     let x: UInt8 = 21
  ///     let y: UInt = 1000000
  ///     y - UInt(x)             // 999979
  ///
  /// The difference of the two arguments must be representable in the
  /// arguments' type. In the following example, the result of `21 - 50` is
  /// less than zero, the minimum representable `UInt8` value:
  ///
  ///     x - 50                  // Overflow error
  ///
  /// - Note: Overflow checking is not performed in `-Ounchecked` builds.
  ///
  /// If you want to opt out of overflow checking and wrap the result in case
  /// of any overflow, use the overflow subtraction operator (`&-`).
  ///
  ///     x &- 50                // 227
  ///
  /// - Parameters:
  ///   - lhs: A numeric value.
  ///   - rhs: The value to subtract from `lhs`.
  @_transparent
  public static func -(lhs: Int16, rhs: Int16) -> Int16 {
    var lhs = lhs
    lhs -= rhs
    return lhs
  }

  /// Multiplies two values and produces their product.
  ///
  /// The multiplication operator (`*`) calculates the product of its two
  /// arguments. For example:
  ///
  ///     2 * 3                   // 6
  ///     100 * 21                // 2100
  ///     -10 * 15                // -150
  ///     3.5 * 2.25              // 7.875
  ///
  /// You cannot use `*` with arguments of different types. To multiply values
  /// of different types, convert one of the values to the other value's type.
  ///
  ///     let x: Int8 = 21
  ///     let y: Int = 1000000
  ///     Int(x) * y              // 21000000
  ///
  /// The product of the two arguments must be representable in the arguments'
  /// type. In the following example, the result of `21 * 21` is greater than
  /// the maximum representable `Int8` value:
  ///
  ///     x * 21                  // Overflow error
  ///
  /// - Note: Overflow checking is not performed in `-Ounchecked` builds.
  ///
  /// If you want to opt out of overflow checking and wrap the result in case
  /// of any overflow, use the overflow multiplication operator (`&*`).
  ///
  ///     x &* 21                // -115
  ///
  /// - Parameters:
  ///   - lhs: The first value to multiply.
  ///   - rhs: The second value to multiply.
  @_transparent
  public static func *(lhs: Int16, rhs: Int16) -> Int16 {
    var lhs = lhs
    lhs *= rhs
    return lhs
  }

  /// Multiplies two values and produces their product.
  ///
  /// The multiplication operator (`*`) calculates the product of its two
  /// arguments. For example:
  ///
  ///     2 * 3                   // 6
  ///     100 * 21                // 2100
  ///     -10 * 15                // -150
  ///     3.5 * 2.25              // 7.875
  ///
  /// You cannot use `*` with arguments of different types. To multiply values
  /// of different types, convert one of the values to the other value's type.
  ///
  ///     let x: Int8 = 21
  ///     let y: Int = 1000000
  ///     Int(x) * y              // 21000000
  ///
  /// The product of the two arguments must be representable in the arguments'
  /// type. In the following example, the result of `21 * 21` is greater than
  /// the maximum representable `Int8` value:
  ///
  ///     x * 21                  // Overflow error
  ///
  /// - Note: Overflow checking is not performed in `-Ounchecked` builds.
  ///
  /// If you want to opt out of overflow checking and wrap the result in case
  /// of any overflow, use the overflow multiplication operator (`&*`).
  ///
  ///     x &* 21                // -115
  ///
  /// - Parameters:
  ///   - lhs: The first value to multiply.
  ///   - rhs: The second value to multiply.
  @available(swift, obsoleted: 4)
  @_semantics("optimize.sil.specialize.generic.partial.never")
  @_transparent
  public static func >>(
    lhs: Int16, rhs: Int16
  ) -> Int16 {
    var lhs = lhs
    _nonMaskingRightShiftGeneric(&lhs, rhs)
    return lhs
  }

  /// Multiplies two values and stores the result in the left-hand-side
  /// variable.
  ///
  /// The product of the two arguments must be representable in the arguments'
  /// type. In the following example, the result of `21 * 21` is greater than
  /// the maximum representable `Int8` value:
  ///
  ///     var x: Int8 = 21
  ///     x * 21
  ///     // Overflow error
  ///
  /// - Note: Overflow checking is not performed in `-Ounchecked` builds.
  ///
  /// - Parameters:
  ///   - lhs: The first value to multiply.
  ///   - rhs: The second value to multiply.
  @available(swift, obsoleted: 4)
  @_semantics("optimize.sil.specialize.generic.partial.never")
  @_transparent
  public static func >>=(
    lhs: inout Int16, rhs: Int16
  ) {
    _nonMaskingRightShiftGeneric(&lhs, rhs)
  }

  /// Multiplies two values and produces their product.
  ///
  /// The multiplication operator (`*`) calculates the product of its two
  /// arguments. For example:
  ///
  ///     2 * 3                   // 6
  ///     100 * 21                // 2100
  ///     -10 * 15                // -150
  ///     3.5 * 2.25              // 7.875
  ///
  /// You cannot use `*` with arguments of different types. To multiply values
  /// of different types, convert one of the values to the other value's type.
  ///
  ///     let x: Int8 = 21
  ///     let y: Int = 1000000
  ///     Int(x) * y              // 21000000
  ///
  /// The product of the two arguments must be representable in the arguments'
  /// type. In the following example, the result of `21 * 21` is greater than
  /// the maximum representable `Int8` value:
  ///
  ///     x * 21                  // Overflow error
  ///
  /// - Note: Overflow checking is not performed in `-Ounchecked` builds.
  ///
  /// If you want to opt out of overflow checking and wrap the result in case
  /// of any overflow, use the overflow multiplication operator (`&*`).
  ///
  ///     x &* 21                // -115
  ///
  /// - Parameters:
  ///   - lhs: The first value to multiply.
  ///   - rhs: The second value to multiply.
  @available(swift, obsoleted: 4)
  @_semantics("optimize.sil.specialize.generic.partial.never")
  @_transparent
  public static func <<(
    lhs: Int16, rhs: Int16
  ) -> Int16 {
    var lhs = lhs
    _nonMaskingLeftShiftGeneric(&lhs, rhs)
    return lhs
  }

  /// Multiplies two values and stores the result in the left-hand-side
  /// variable.
  ///
  /// The product of the two arguments must be representable in the arguments'
  /// type. In the following example, the result of `21 * 21` is greater than
  /// the maximum representable `Int8` value:
  ///
  ///     var x: Int8 = 21
  ///     x * 21
  ///     // Overflow error
  ///
  /// - Note: Overflow checking is not performed in `-Ounchecked` builds.
  ///
  /// - Parameters:
  ///   - lhs: The first value to multiply.
  ///   - rhs: The second value to multiply.
  @available(swift, obsoleted: 4)
  @_semantics("optimize.sil.specialize.generic.partial.never")
  @_transparent
  public static func <<=(
    lhs: inout Int16, rhs: Int16
  ) {
    _nonMaskingLeftShiftGeneric(&lhs, rhs)
  }

  @_transparent
  public static func <= (lhs: Int16, rhs: Int16) -> Bool {
    return !(rhs < lhs)
  }

  @_transparent
  public static func >= (lhs: Int16, rhs: Int16) -> Bool {
    return !(lhs < rhs)
  }

  @_transparent
  public static func > (lhs: Int16, rhs: Int16) -> Bool {
    return rhs < lhs
  }
}

// TODO: Consider removing the underscore.
/// Returns the argument and specifies that the value is not negative.
/// It has only an effect if the argument is a load or call.
@_transparent
public func _assumeNonNegative(_ x: Int16) -> Int16 {
  _internalInvariant(x >= (0 as Int16))
  return Int16(Builtin.assumeNonNegative_Int16(x._value))
}
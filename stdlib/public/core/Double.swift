//===--- Double.swift -----------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

/// A double-precision (64-bit), floating-point value type.
@frozen
public struct Double {
  public // @testable
  var _value: Builtin.FPIEEE64

  @_transparent
  public init() {
    let zero: Int64 = 0
    self._value = Builtin.sitofp_Int64_FPIEEE64(zero._value)
  }

  @_transparent
  public // @testable
  init(_ _value: Builtin.FPIEEE64) {
    self._value = _value
  }
}


extension Double: CustomStringConvertible {
  /// A textual representation of the value.
  ///
  /// For any finite value, this property provides a string that can be
  /// converted back to an instance of `Double` without rounding errors.  That
  /// is, if `x` is an instance of `Double`, then `Double(x.description) ==
  /// x` is always true.  For any NaN value, the property's value is "nan", and
  /// for positive and negative infinity its value is "inf" and "-inf".
  public var description: String {
    if isNaN {
      return "nan"
    } else {
      return debugDescription
    }
  }
}


extension Double: CustomDebugStringConvertible {
  /// A textual representation of the value, suitable for debugging.
  ///
  /// This property has the same value as the `description` property, except
  /// that NaN values are printed in an extended format.
  public var debugDescription: String {
    var buffer = _InlineArray<32, UTF8.CodeUnit>(repeating: 0x30)
    var span = buffer.mutableSpan
    let textRange = _Float64ToASCII(value: self, buffer: &span)
    let ascii = unsafe buffer.span.extracting(unchecked: textRange)
    return ascii.withUnsafeBufferPointer {
      unsafe String._fromASCII($0)
    }
  }
}


extension Double: TextOutputStreamable {
  public func write<Target>(to target: inout Target) where Target: TextOutputStream {
    if isNaN {
      // Match `description`.
      target.write("nan")
      return
    }
    var buffer = _InlineArray<32, UTF8.CodeUnit>(repeating: 0x30)
    var span = buffer.mutableSpan
    let textRange = _Float64ToASCII(value: self, buffer: &span)
    let ascii = unsafe buffer.span.extracting(unchecked: textRange)
    return ascii.withUnsafeBufferPointer {
      unsafe target._writeASCII($0)
    }
  }
}


extension Double: BinaryFloatingPoint {

  // Floating-point types are always symmetric, so Magnitude is Self.
  public typealias Magnitude = Double

  public typealias Exponent = Int

  public typealias RawSignificand = UInt64

  @inlinable
  public static var exponentBitCount: Int {
    return 11
  }

  @inlinable
  public static var significandBitCount: Int {
    return 52
  }

  //  Implementation details.
  @inlinable // FIXME(inline-always) was usableFromInline
  internal static var _infinityExponent: UInt {
    @inline(__always) get { return 1 &<< UInt(exponentBitCount) - 1 }
  }

  @inlinable // FIXME(inline-always) was usableFromInline
  internal static var _exponentBias: UInt {
    @inline(__always) get { return _infinityExponent &>> 1 }
  }

  @inlinable // FIXME(inline-always) was usableFromInline
  internal static var _significandMask: UInt64 {
    @inline(__always) get {
      return 1 &<< UInt64(significandBitCount) - 1
    }
  }

  @inlinable // FIXME(inline-always) was usableFromInline
  internal static var _quietNaNMask: UInt64 {
    @inline(__always) get {
      return 1 &<< UInt64(significandBitCount - 1)
    }
  }

  //  Conversions to/from integer encoding.  These are not part of the
  //  BinaryFloatingPoint prototype because there's no guarantee that an
  //  integer type of the same size actually exists (e.g. Float80).
  //
  //  If we want them in a protocol at some future point, that protocol should
  //  be "InterchangeFloatingPoint" or "PortableFloatingPoint" or similar, and
  //  apply to IEEE 754 "interchange types".
  /// The bit pattern of the value's encoding.
  ///
  /// The bit pattern matches the binary interchange format defined by the
  /// [IEEE 754 specification][spec].
  ///
  /// [spec]: http://ieeexplore.ieee.org/servlet/opac?punumber=4610933
  @inlinable
  public var bitPattern: UInt64 {
    return UInt64(Builtin.bitcast_FPIEEE64_Int64(_value))
  }

  /// Creates a new value with the given bit pattern.
  ///
  /// The value passed as `bitPattern` is interpreted in the binary interchange
  /// format defined by the [IEEE 754 specification][spec].
  ///
  /// [spec]: http://ieeexplore.ieee.org/servlet/opac?punumber=4610933
  ///
  /// - Parameter bitPattern: The integer encoding of a `Double` instance.
  @inlinable
  public init(bitPattern: UInt64) {
    self.init(Builtin.bitcast_Int64_FPIEEE64(bitPattern._value))
  }

  @inlinable
  public var sign: FloatingPointSign {
    let shift = Double.significandBitCount + Double.exponentBitCount
    return FloatingPointSign(
      rawValue: Int(bitPattern &>> UInt64(shift))
    )!
  }

  @available(*, unavailable, renamed: "sign")
  public var isSignMinus: Bool { Builtin.unreachable() }

  @inlinable
  public var exponentBitPattern: UInt {
    return UInt(bitPattern &>> UInt64(Double.significandBitCount)) &
      Double._infinityExponent
  }

  @inlinable
  public var significandBitPattern: UInt64 {
    return UInt64(bitPattern) & Double._significandMask
  }

  @inlinable
  public init(
    sign: FloatingPointSign,
    exponentBitPattern: UInt,
    significandBitPattern: UInt64
  ) {
    let signShift = Double.significandBitCount + Double.exponentBitCount
    let sign = UInt64(sign == .minus ? 1 : 0)
    let exponent = UInt64(
      exponentBitPattern & Double._infinityExponent
    )
    let significand = UInt64(
      significandBitPattern & Double._significandMask
    )
    self.init(bitPattern:
      sign &<< UInt64(signShift) |
      exponent &<< UInt64(Double.significandBitCount) |
      significand
    )
  }

  @inlinable
  public var isCanonical: Bool {
    // All Float and Double encodings are canonical in IEEE 754.
    //
    // On platforms that do not support subnormals, we treat them as
    // non-canonical encodings of zero.
    if Self.leastNonzeroMagnitude == Self.leastNormalMagnitude {
      if exponentBitPattern == 0 && significandBitPattern != 0 {
        return false
      }
    }
    return true
  }

  @inlinable
  public static var infinity: Double {
    return Double(bitPattern: 0x7ff0000000000000)
  }

  @inlinable
  public static var nan: Double {
    return Double(bitPattern: 0x7ff8000000000000)
  }

  @inlinable
  public static var signalingNaN: Double {
    return Double(nan: 0, signaling: true)
  }

  @available(*, unavailable, renamed: "nan")
  public static var quietNaN: Double { Builtin.unreachable() }

  @inlinable
  public static var greatestFiniteMagnitude: Double {
    return 0x1.fffffffffffffp1023
  }

  @inlinable
  public static var pi: Double {
    return 0x1.921fb54442d18p1
  }

  @inlinable
  public var ulp: Double {
    guard _fastPath(isFinite) else { return .nan }
    if _fastPath(isNormal) {
      let bitPattern_ = bitPattern & Double.infinity.bitPattern
      return Double(bitPattern: bitPattern_) * 0x1p-52
    }
    // On arm, flush subnormal values to 0.
    return .leastNormalMagnitude * 0x1p-52
  }

  @inlinable
  public static var leastNormalMagnitude: Double {
    return 0x1.0p-1022
  }

  @inlinable
  public static var leastNonzeroMagnitude: Double {
#if arch(arm)
    // On 32b arm, the default FPCR has subnormals flushed to zero.
    return leastNormalMagnitude
#else
    return leastNormalMagnitude * ulpOfOne
#endif
  }

  /// The unit in the last place of 1.0.
  ///
  /// The positive difference between 1.0 and the next greater representable
  /// number. The `ulpOfOne` constant corresponds to the C macros
  /// `FLT_EPSILON`, `DBL_EPSILON`, and others with a similar purpose.
  @inlinable
  public static var ulpOfOne: Double {
    return 0x1.0p-52
  }

  @inlinable
  @_semantics("optimize.sil.inline.constant.arguments")
  public var exponent: Int {
    if !isFinite { return .max }
    if isZero { return .min }
    let provisional = Int(exponentBitPattern) - Int(Double._exponentBias)
    if isNormal { return provisional }
    let shift =
      Double.significandBitCount - significandBitPattern._binaryLogarithm()
    return provisional + 1 - shift
  }

  @inlinable
  public var significand: Double {
    if isNaN { return self }
    if isNormal {
      return Double(sign: .plus,
        exponentBitPattern: Double._exponentBias,
        significandBitPattern: significandBitPattern)
    }
    if isSubnormal {
      let shift =
        Double.significandBitCount - significandBitPattern._binaryLogarithm()
      return Double(
        sign: .plus,
        exponentBitPattern: Double._exponentBias,
        significandBitPattern: significandBitPattern &<< shift
      )
    }
    // zero or infinity.
    return Double(
      sign: .plus,
      exponentBitPattern: exponentBitPattern,
      significandBitPattern: 0
    )
  }

  @inlinable
  public init(sign: FloatingPointSign, exponent: Int, significand: Double) {
    var result = significand
    if sign == .minus { result = -result }
    if significand.isFinite && !significand.isZero {
      var clamped = exponent
      let leastNormalExponent = 1 - Int(Double._exponentBias)
      let greatestFiniteExponent = Int(Double._exponentBias)
      if clamped < leastNormalExponent {
        clamped = max(clamped, 3*leastNormalExponent)
        while clamped < leastNormalExponent {
          result  *= Double.leastNormalMagnitude
          clamped -= leastNormalExponent
        }
      }
      else if clamped > greatestFiniteExponent {
        clamped = min(clamped, 3*greatestFiniteExponent)
        let step = Double(sign: .plus,
          exponentBitPattern: Double._infinityExponent - 1,
          significandBitPattern: 0)
        while clamped > greatestFiniteExponent {
          result  *= step
          clamped -= greatestFiniteExponent
        }
      }
      let scale = Double(
        sign: .plus,
        exponentBitPattern: UInt(Int(Double._exponentBias) + clamped),
        significandBitPattern: 0
      )
      result = result * scale
    }
    self = result
  }
  
  /// Creates a NaN ("not a number") value with the specified payload.
  ///
  /// NaN values compare not equal to every value, including themselves. Most
  /// operations with a NaN operand produce a NaN result. Don't use the
  /// equal-to operator (`==`) to test whether a value is NaN. Instead, use
  /// the value's `isNaN` property.
  ///
  ///     let x = Double(nan: 0, signaling: false)
  ///     print(x == .nan)
  ///     // Prints "false"
  ///     print(x.isNaN)
  ///     // Prints "true"
  ///
  /// - Parameters:
  ///   - payload: The payload to use for the new NaN value.
  ///   - signaling: Pass `true` to create a signaling NaN or `false` to create
  ///     a quiet NaN.
  @inlinable
  public init(nan payload: RawSignificand, signaling: Bool) {
    // We use significandBitCount - 2 bits for NaN payload.
    _precondition(payload < (Double._quietNaNMask &>> 1),
      "NaN payload is not encodable.")
    var significand = payload
    significand |= Double._quietNaNMask &>> (signaling ? 1 : 0)
    self.init(
      sign: .plus,
      exponentBitPattern: Double._infinityExponent,
      significandBitPattern: significand
    )
  }

  @inlinable
  public var nextUp: Double {
    // Silence signaling NaNs, map -0 to +0.
    let x = self + 0
#if arch(arm)
    // On arm, treat subnormal values as zero.
    if _slowPath(x == 0) { return .leastNonzeroMagnitude }
    if _slowPath(x == -.leastNonzeroMagnitude) { return -0.0 }
#endif
    if _fastPath(x < .infinity) {
      let increment = Int64(bitPattern: x.bitPattern) &>> 63 | 1
      let bitPattern_ = x.bitPattern &+ UInt64(bitPattern: increment)
      return Double(bitPattern: bitPattern_)
    }
    return x
  }

  //  For core standard library floating-point types, LLVM can lower copysign
  //  for us; this gets somewhat better codegen than the generic implementation,
  //  but more importantly allows it to participate in other optimizations
  //  at the LLVM level.
  @_transparent
  public init(signOf sign: Double, magnitudeOf mag: Double) {
    _value = Builtin.int_copysign_FPIEEE64(mag._value, sign._value)
  }

  @_transparent
  public mutating func round(_ rule: FloatingPointRoundingRule) {
    switch rule {
    case .toNearestOrAwayFromZero:
      _value = Builtin.int_round_FPIEEE64(_value)
    case .toNearestOrEven:
      _value = Builtin.int_rint_FPIEEE64(_value)
    case .towardZero:
      _value = Builtin.int_trunc_FPIEEE64(_value)
    case .awayFromZero:
      if sign == .minus {
        _value = Builtin.int_floor_FPIEEE64(_value)
      }
      else {
        _value = Builtin.int_ceil_FPIEEE64(_value)
      }
    case .up:
      _value = Builtin.int_ceil_FPIEEE64(_value)
    case .down:
      _value = Builtin.int_floor_FPIEEE64(_value)
    #if !$Embedded
    @unknown default:
      self._roundSlowPath(rule)
    #endif
    }
  }
  
  // Slow path for new cases that might have been inlined into an old
  // ABI-stable version of round(_:) called from a newer version. If this is
  // the case, this non-inlinable function will call into the _newer_ version
  // which _will_ support this rounding rule.
  @usableFromInline
  internal mutating func _roundSlowPath(_ rule: FloatingPointRoundingRule) {
    self.round(rule)
  }

  @_transparent
  public mutating func negate() {
    _value = Builtin.fneg_FPIEEE64(self._value)
  }

  @_transparent
  public static func +=(lhs: inout Double, rhs: Double) {
    lhs._value = Builtin.fadd_FPIEEE64(lhs._value, rhs._value)
  }

  @_transparent
  public static func -=(lhs: inout Double, rhs: Double) {
    lhs._value = Builtin.fsub_FPIEEE64(lhs._value, rhs._value)
  }

  @_transparent
  public static func *=(lhs: inout Double, rhs: Double) {
    lhs._value = Builtin.fmul_FPIEEE64(lhs._value, rhs._value)
  }

  @_transparent
  public static func /=(lhs: inout Double, rhs: Double) {
    lhs._value = Builtin.fdiv_FPIEEE64(lhs._value, rhs._value)
  }

  @inlinable // FIXME(inline-always)
  @inline(__always)
  public mutating func formRemainder(dividingBy other: Double) {
    self = _stdlib_remainder(self, other)
  }

  @inlinable // FIXME(inline-always)
  @inline(__always)
  public mutating func formTruncatingRemainder(dividingBy other: Double) {
    _value = Builtin.frem_FPIEEE64(self._value, other._value)
  }

  @_transparent
  public mutating func formSquareRoot( ) {
    _value = Builtin.int_sqrt_FPIEEE64(_value)
  }

  @_transparent
  public mutating func addProduct(_ lhs: Double, _ rhs: Double) {
    _value = Builtin.int_fma_FPIEEE64(lhs._value, rhs._value, _value)
  }

  @_transparent
  public func isEqual(to other: Double) -> Bool {
    return Bool(Builtin.fcmp_oeq_FPIEEE64(self._value, other._value))
  }

  @_transparent
  public func isLess(than other: Double) -> Bool {
    return Bool(Builtin.fcmp_olt_FPIEEE64(self._value, other._value))
  }

  @_transparent
  public func isLessThanOrEqualTo(_ other: Double) -> Bool {
    return Bool(Builtin.fcmp_ole_FPIEEE64(self._value, other._value))
  }

  @inlinable // FIXME(inline-always)
  public var isNormal: Bool {
    @inline(__always)
    get {
      return exponentBitPattern > 0 && isFinite
    }
  }

  @inlinable // FIXME(inline-always)
  public var isFinite: Bool {
    @inline(__always)
    get {
      return exponentBitPattern < Double._infinityExponent
    }
  }

  @inlinable // FIXME(inline-always)
  public var isZero: Bool {
    @inline(__always)
    get {
      return exponentBitPattern == 0 && significandBitPattern == 0
    }
  }

  @inlinable // FIXME(inline-always)
  public var isSubnormal:  Bool {
    @inline(__always)
    get {
      return exponentBitPattern == 0 && significandBitPattern != 0
    }
  }

  @inlinable // FIXME(inline-always)
  public var isInfinite:  Bool {
    @inline(__always)
    get {
      return !isFinite && significandBitPattern == 0
    }
  }

  @inlinable // FIXME(inline-always)
  public var isNaN:  Bool {
    @inline(__always)
    get {
      return !isFinite && significandBitPattern != 0
    }
  }

  @inlinable // FIXME(inline-always)
  public var isSignalingNaN: Bool {
    @inline(__always)
    get {
      return isNaN && (significandBitPattern & Double._quietNaNMask) == 0
    }
  }

  @inlinable
  public var binade: Double {
    guard _fastPath(isFinite) else { return .nan }
#if !arch(arm)
    if _slowPath(isSubnormal) {
      let bitPattern_ =
        (self * 0x1p52).bitPattern
          & (-Double.infinity).bitPattern
      return Double(bitPattern: bitPattern_) * 0x1p-52
    }
#endif
    return Double(bitPattern: bitPattern & (-Double.infinity).bitPattern)
  }

  @inlinable
  @_semantics("optimize.sil.inline.constant.arguments")
  public var significandWidth: Int {
    let trailingZeroBits = significandBitPattern.trailingZeroBitCount
    if isNormal {
      guard significandBitPattern != 0 else { return 0 }
      return Double.significandBitCount &- trailingZeroBits
    }
    if isSubnormal {
      let leadingZeroBits = significandBitPattern.leadingZeroBitCount
      return UInt64.bitWidth &- (trailingZeroBits &+ leadingZeroBits &+ 1)
    }
    return -1
  }

  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(floatLiteral value: Double) {
    self = value
  }
}


extension Double: _ExpressibleByBuiltinIntegerLiteral, ExpressibleByIntegerLiteral {
  @_transparent
  public
  init(_builtinIntegerLiteral value: Builtin.IntLiteral){
    self = Double(Builtin.itofp_with_overflow_IntLiteral_FPIEEE64(value))
  }

  @_transparent
  public init(integerLiteral value: Int64) {
    self = Double(Builtin.sitofp_Int64_FPIEEE64(value._value))
  }
}

#if !(os(Windows) || os(Android) || ($Embedded && !os(Linux) && !os(anyAppleOS))) && (arch(i386) || arch(x86_64))


extension Double: _ExpressibleByBuiltinFloatLiteral {
  @_transparent
  public
  init(_builtinFloatLiteral value: Builtin.FPIEEE80) {
    self = Double(Builtin.fptrunc_FPIEEE80_FPIEEE64(value))
  }
}

#else


extension Double: _ExpressibleByBuiltinFloatLiteral {
  @_transparent
  public
  init(_builtinFloatLiteral value: Builtin.FPIEEE64) {
    self = Double(value)
  }
}

#endif


extension Double: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    var v = self
    if isZero {
      // To satisfy the axiom that equality implies hash equality, we need to
      // finesse the hash value of -0.0 to match +0.0.
      v = 0
    }
    hasher.combine(v.bitPattern)
  }

  @inlinable
  public func _rawHashValue(seed: Int) -> Int {
    // To satisfy the axiom that equality implies hash equality, we need to
    // finesse the hash value of -0.0 to match +0.0.
    let v = isZero ? 0 : self
    return Hasher._hash(seed: seed, v.bitPattern)
  }
}

@_unavailableInEmbedded
extension Double: _HasCustomAnyHashableRepresentation {
  // Not @inlinable
  public func _toCustomAnyHashable() -> AnyHashable? {
    return AnyHashable(_box: _DoubleAnyHashableBox(self))
  }
}


extension Double {
  @inlinable // FIXME(inline-always)
  public var magnitude: Double {
    @inline(__always)
    get {
      return Double(Builtin.int_fabs_FPIEEE64(_value))
    }
  }
}


extension Double {
  @_transparent
  public static prefix func - (x: Double) -> Double {
    return Double(Builtin.fneg_FPIEEE64(x._value))
  }
}


extension Double: Sendable { }

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

// Construction from other concrete types.

extension Double {

  // We "shouldn't" need this, but the typechecker barfs on an expression
  // in the test suite without it.
  // If replaced with @inline(__always) the init no longer gets
  // inlined in -Onone and this breaks the abi_v7k test in a subtle way.
  @_transparent
  public init(_ v: Int) {
    _value = Builtin.sitofp_Int64_FPIEEE64(v._value)
  }

  // Fast-path for conversion when the source is representable as int,
  // falling back on the generic _convert operation otherwise.
  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init<Source: BinaryInteger>(_ value: Source) {
    if value.bitWidth <= 64 {
      if Source.isSigned {
        let asInt = Int64(truncatingIfNeeded: value)
        _value = Builtin.sitofp_Int64_FPIEEE64(asInt._value)
      } else {
        let asUInt = UInt64(truncatingIfNeeded: value)
        _value = Builtin.uitofp_Int64_FPIEEE64(asUInt._value)
      }
    } else {
      // TODO: we can do much better than the generic _convert here for Float
      // and Double by pulling out the high-order 32/64b of the integer, ORing
      // in a sticky bit, and then using the builtin.
      self = Double._convert(from: value).value
    }
  }

  // Fast-path for conversion when the source is representable as int,
  // falling back on the generic _convert operation otherwise.
  @export(implementation) @inline(never)
  public init?<Source: BinaryInteger>(exactly value: Source) {
    if value.bitWidth <= 64 {
      // If the source is small enough to fit in a word, we can use the LLVM
      // conversion intrinsic, then check if we can round-trip back to the
      // the original value; if so, the conversion was exact. We need to be
      // careful, however, to make sure that the first conversion does not
      // round to a value that is out of the defined range of the second
      // conversion. E.g. Float(Int.max) rounds to Int.max + 1, and converting
      // that back to Int will trap. For Float, Double, and Float80, this is
      // only an issue for the upper bound (because the lower bound of [U]Int
      // is either zero or a power of two, both of which are exactly
      // representable). For Float16, we also need to check for overflow to
      // -.infinity.
      if Source.isSigned {
        let extended = Int(truncatingIfNeeded: value)
        _value = Builtin.sitofp_Int64_FPIEEE64(extended._value)
        guard self < 0x1.0p63 && Int(self) == extended else {
          return nil
        }
      } else {
        let extended = UInt(truncatingIfNeeded: value)
        _value = Builtin.uitofp_Int64_FPIEEE64(extended._value)
        guard self < 0x1.0p64 && UInt(self) == extended else {
          return nil
        }
      }
    } else {
      // TODO: we can do much better than the generic _convert here for Float
      // and Double by pulling out the high-order 32/64b of the integer, ORing
      // in a sticky bit, and then using the builtin.
      let (value_, exact) = Self._convert(from: value)
      guard exact else { return nil }
      self = value_
    }
  }
  

#if !((os(macOS) || targetEnvironment(macCatalyst)) && arch(x86_64))

  /// Creates a new instance that approximates the given value.
  ///
  /// The value of `other` is rounded to a representable value, if necessary.
  /// A NaN passed as `other` results in another NaN, with a signaling NaN
  /// value converted to quiet NaN.
  ///
  ///     let x: Float16 = 21.25
  ///     let y = Double(x)
  ///     // y == 21.25
  ///
  ///     let z = Double(Float16.nan)
  ///     // z.isNaN == true
  ///
  /// - Parameter other: The value to use for the new instance.
  @available(SwiftStdlib 5.3, *)
  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(_ other: Float16) {
    _value = Builtin.fpext_FPIEEE16_FPIEEE64(other._value)
  }

  /// Creates a new instance initialized to the given value, if it can be
  /// represented without rounding.
  ///
  /// If `other` can't be represented as an instance of `Double` without
  /// rounding, the result of this initializer is `nil`. In particular,
  /// passing NaN as `other` always results in `nil`.
  ///
  ///     let x: Float16 = 21.25
  ///     let y = Double(exactly: x)
  ///     // y == Optional.some(21.25)
  ///
  ///     let z = Double(exactly: Float16.nan)
  ///     // z == nil
  ///
  /// - Parameter other: The value to use for the new instance.
  @available(SwiftStdlib 5.3, *)
  @inlinable
  @inline(__always)
  public init?(exactly other: Float16) {
    self.init(other)
    // Converting the infinity value is considered value preserving.
    // In other cases, check that we can round-trip and get the same value.
    // NaN always fails.
    if Float16(self) != other {
      return nil
    }
  }

#endif


  /// Creates a new instance that approximates the given value.
  ///
  /// The value of `other` is rounded to a representable value, if necessary.
  /// A NaN passed as `other` results in another NaN, with a signaling NaN
  /// value converted to quiet NaN.
  ///
  ///     let x: Float = 21.25
  ///     let y = Double(x)
  ///     // y == 21.25
  ///
  ///     let z = Double(Float.nan)
  ///     // z.isNaN == true
  ///
  /// - Parameter other: The value to use for the new instance.
  
  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(_ other: Float) {
    _value = Builtin.fpext_FPIEEE32_FPIEEE64(other._value)
  }

  /// Creates a new instance initialized to the given value, if it can be
  /// represented without rounding.
  ///
  /// If `other` can't be represented as an instance of `Double` without
  /// rounding, the result of this initializer is `nil`. In particular,
  /// passing NaN as `other` always results in `nil`.
  ///
  ///     let x: Float = 21.25
  ///     let y = Double(exactly: x)
  ///     // y == Optional.some(21.25)
  ///
  ///     let z = Double(exactly: Float.nan)
  ///     // z == nil
  ///
  /// - Parameter other: The value to use for the new instance.
  
  @inlinable
  @inline(__always)
  public init?(exactly other: Float) {
    self.init(other)
    // Converting the infinity value is considered value preserving.
    // In other cases, check that we can round-trip and get the same value.
    // NaN always fails.
    if Float(self) != other {
      return nil
    }
  }



  /// Creates a new instance initialized to the given value.
  ///
  /// The value of `other` is represented exactly by the new instance. A NaN
  /// passed as `other` results in another NaN, with a signaling NaN value
  /// converted to quiet NaN.
  ///
  ///     let x: Double = 21.25
  ///     let y = Double(x)
  ///     // y == 21.25
  ///
  ///     let z = Double(Double.nan)
  ///     // z.isNaN == true
  ///
  /// - Parameter other: The value to use for the new instance.
  
  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(_ other: Double) {
    _value = other._value
  }

  /// Creates a new instance initialized to the given value, if it can be
  /// represented without rounding.
  ///
  /// If `other` can't be represented as an instance of `Double` without
  /// rounding, the result of this initializer is `nil`. In particular,
  /// passing NaN as `other` always results in `nil`.
  ///
  ///     let x: Double = 21.25
  ///     let y = Double(exactly: x)
  ///     // y == Optional.some(21.25)
  ///
  ///     let z = Double(exactly: Double.nan)
  ///     // z == nil
  ///
  /// - Parameter other: The value to use for the new instance.
  
  @inlinable
  @inline(__always)
  public init?(exactly other: Double) {
    self.init(other)
    // Converting the infinity value is considered value preserving.
    // In other cases, check that we can round-trip and get the same value.
    // NaN always fails.
    if Double(self) != other {
      return nil
    }
  }


#if !(os(Windows) || os(Android) || ($Embedded && !os(Linux) && !os(anyAppleOS))) && (arch(i386) || arch(x86_64))

  /// Creates a new instance that approximates the given value.
  ///
  /// The value of `other` is rounded to a representable value, if necessary.
  /// A NaN passed as `other` results in another NaN, with a signaling NaN
  /// value converted to quiet NaN.
  ///
  ///     let x: Float80 = 21.25
  ///     let y = Double(x)
  ///     // y == 21.25
  ///
  ///     let z = Double(Float80.nan)
  ///     // z.isNaN == true
  ///
  /// - Parameter other: The value to use for the new instance.
  
  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(_ other: Float80) {
    _value = Builtin.fptrunc_FPIEEE80_FPIEEE64(other._value)
  }

  /// Creates a new instance initialized to the given value, if it can be
  /// represented without rounding.
  ///
  /// If `other` can't be represented as an instance of `Double` without
  /// rounding, the result of this initializer is `nil`. In particular,
  /// passing NaN as `other` always results in `nil`.
  ///
  ///     let x: Float80 = 21.25
  ///     let y = Double(exactly: x)
  ///     // y == Optional.some(21.25)
  ///
  ///     let z = Double(exactly: Float80.nan)
  ///     // z == nil
  ///
  /// - Parameter other: The value to use for the new instance.
  
  @inlinable
  @inline(__always)
  public init?(exactly other: Float80) {
    self.init(other)
    // Converting the infinity value is considered value preserving.
    // In other cases, check that we can round-trip and get the same value.
    // NaN always fails.
    if Float80(self) != other {
      return nil
    }
  }

#endif
}

//===----------------------------------------------------------------------===//
// Standard Operator Table
//===----------------------------------------------------------------------===//

//  TODO: These should not be necessary, since they're already provided by
//  <T: FloatingPoint>, but in practice they are currently needed to
//  disambiguate overloads.  We should find a way to remove them, either by
//  tweaking the overload resolution rules, or by removing the other
//  definitions in the standard lib, or both.


extension Double {
  @_transparent
  public static func + (lhs: Double, rhs: Double) -> Double {
    var lhs = lhs
    lhs += rhs
    return lhs
  }

  @_transparent
  public static func - (lhs: Double, rhs: Double) -> Double {
    var lhs = lhs
    lhs -= rhs
    return lhs
  }

  @_transparent
  public static func * (lhs: Double, rhs: Double) -> Double {
    var lhs = lhs
    lhs *= rhs
    return lhs
  }

  @_transparent
  public static func / (lhs: Double, rhs: Double) -> Double {
    var lhs = lhs
    lhs /= rhs
    return lhs
  }
}

//===----------------------------------------------------------------------===//
// Strideable Conformance
//===----------------------------------------------------------------------===//


extension Double: Strideable {
  @_transparent
  public func distance(to other: Double) -> Double {
    return other - self
  }

  @_transparent
  public func advanced(by amount: Double) -> Double {
    return self + amount
  }
}

//===----------------------------------------------------------------------===//
// AnyHashable
//===----------------------------------------------------------------------===//

@_unavailableInEmbedded
internal struct _DoubleAnyHashableBox: _AnyHashableBox {
  internal typealias Base = Double

  internal let _value: Base

  internal init(_ value: Base) {
    self._value = value
  }

  internal var _canonicalBox: _AnyHashableBox {
    // Float and Double are bridged with NSNumber, so we have to follow
    // NSNumber's rules for equality.  I.e., we need to make sure equal
    // numerical values end up in identical boxes after canonicalization, so
    // that _isEqual will consider them equal and they're hashed the same way.
    //
    // Note that these AnyHashable boxes don't currently feed discriminator bits
    // to the hasher, so we allow repeatable collisions. E.g., -1 will always
    // collide with UInt64.max.
    if _value < 0 {
      if let i = Int64(exactly: _value) {
        return _IntegerAnyHashableBox(i)
      }
    } else {
      if let i = UInt64(exactly: _value) {
        return _IntegerAnyHashableBox(i)
      }
    }
    if let d = Double(exactly: _value) {
      return _DoubleAnyHashableBox(d)
    }
    // If a value can't be represented by a Double, keep it in its original
    // representation so that it won't compare equal to approximations. (So that
    // we don't round off Float80 values.)
    return self
  }

  internal func _isEqual(to box: _AnyHashableBox) -> Bool? {
    _internalInvariant(Int64(exactly: _value) == nil, "self isn't canonical")
    _internalInvariant(UInt64(exactly: _value) == nil, "self isn't canonical")
    if let box = box as? _DoubleAnyHashableBox {
      return _value == box._value
    }
    return nil
  }

  internal var _hashValue: Int {
    return _rawHashValue(_seed: 0)
  }

  internal func _hash(into hasher: inout Hasher) {
    _internalInvariant(Int64(exactly: _value) == nil, "self isn't canonical")
    _internalInvariant(UInt64(exactly: _value) == nil, "self isn't canonical")
    hasher.combine(_value)
  }

  internal func _rawHashValue(_seed: Int) -> Int {
    var hasher = Hasher(_seed: _seed)
    _hash(into: &hasher)
    return hasher.finalize()
  }

  internal var _base: Any {
    return _value
  }

  internal func _unbox<T: Hashable>() -> T? {
    return _value as? T
  }

  internal func _downCastConditional<T>(
    into result: UnsafeMutablePointer<T>
  ) -> Bool {
    guard let value = _value as? T else { return false }
    unsafe result.initialize(to: value)
    return true
  }
}


extension Double: ConvertibleToBytes {}

extension Double: ConvertibleFromBytes {}


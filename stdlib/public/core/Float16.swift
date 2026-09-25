//===--- Float16.swift ----------------------------------------*- swift -*-===//
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



#if !((os(macOS) || targetEnvironment(macCatalyst)) && arch(x86_64))

/// A half-precision (16-bit), floating-point value type.
///
/// On macOS, `Float16` is only available when targeting Apple silicon.
/// On other supported platforms, `Float16` is available for all
/// architectures. If the specified target supports 16-bit floating point
/// arithmetic directly, those instructions will be used; otherwise Float16
/// arithmetic will be emulated by the swift compiler and runtime.
@available(SwiftStdlib 5.3, *)
@frozen
public struct Float16 {
  public // @testable
  var _value: Builtin.FPIEEE16

  @_transparent
  public init() {
    self._value = Builtin.zeroInitializer()
  }

  @_transparent
  public // @testable
  init(_ _value: Builtin.FPIEEE16) {
    self._value = _value
  }
}

@available(SwiftStdlib 5.3, *)
extension Float16: CustomStringConvertible {
  /// A textual representation of the value.
  ///
  /// For any finite value, this property provides a string that can be
  /// converted back to an instance of `Float16` without rounding errors.  That
  /// is, if `x` is an instance of `Float16`, then `Float16(x.description) ==
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

@available(SwiftStdlib 5.3, *)
extension Float16: CustomDebugStringConvertible {
  /// A textual representation of the value, suitable for debugging.
  ///
  /// This property has the same value as the `description` property, except
  /// that NaN values are printed in an extended format.
  public var debugDescription: String {
    var buffer = _InlineArray<32, UTF8.CodeUnit>(repeating: 0x30)
    var span = buffer.mutableSpan
    let textRange = _Float16ToASCII(value: self, buffer: &span)
    let ascii = unsafe buffer.span.extracting(unchecked: textRange)
    return ascii.withUnsafeBufferPointer {
      unsafe String._fromASCII($0)
    }
  }
}

@available(SwiftStdlib 5.3, *)
extension Float16: TextOutputStreamable {
  public func write<Target>(to target: inout Target) where Target: TextOutputStream {
    if isNaN {
      // Match `description`.
      target.write("nan")
      return
    }
    var buffer = _InlineArray<32, UTF8.CodeUnit>(repeating: 0x30)
    var span = buffer.mutableSpan
    let textRange = _Float16ToASCII(value: self, buffer: &span)
    let ascii = unsafe buffer.span.extracting(unchecked: textRange)
    return ascii.withUnsafeBufferPointer {
      unsafe target._writeASCII($0)
    }
  }
}

@available(SwiftStdlib 5.3, *)
extension Float16: BinaryFloatingPoint {

  // Floating-point types are always symmetric, so Magnitude is Self.
  public typealias Magnitude = Float16

  public typealias Exponent = Int

  public typealias RawSignificand = UInt16

  @inlinable
  public static var exponentBitCount: Int {
    return 5
  }

  @inlinable
  public static var significandBitCount: Int {
    return 10
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
  internal static var _significandMask: UInt16 {
    @inline(__always) get {
      return 1 &<< UInt16(significandBitCount) - 1
    }
  }

  @inlinable // FIXME(inline-always) was usableFromInline
  internal static var _quietNaNMask: UInt16 {
    @inline(__always) get {
      return 1 &<< UInt16(significandBitCount - 1)
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
  public var bitPattern: UInt16 {
    return UInt16(Builtin.bitcast_FPIEEE16_Int16(_value))
  }

  /// Creates a new value with the given bit pattern.
  ///
  /// The value passed as `bitPattern` is interpreted in the binary interchange
  /// format defined by the [IEEE 754 specification][spec].
  ///
  /// [spec]: http://ieeexplore.ieee.org/servlet/opac?punumber=4610933
  ///
  /// - Parameter bitPattern: The integer encoding of a `Float16` instance.
  @inlinable
  public init(bitPattern: UInt16) {
    self.init(Builtin.bitcast_Int16_FPIEEE16(bitPattern._value))
  }

  @inlinable
  public var sign: FloatingPointSign {
    let shift = Float16.significandBitCount + Float16.exponentBitCount
    return FloatingPointSign(
      rawValue: Int(bitPattern &>> UInt16(shift))
    )!
  }

  @available(*, unavailable, renamed: "sign")
  public var isSignMinus: Bool { Builtin.unreachable() }

  @inlinable
  public var exponentBitPattern: UInt {
    return UInt(bitPattern &>> UInt16(Float16.significandBitCount)) &
      Float16._infinityExponent
  }

  @inlinable
  public var significandBitPattern: UInt16 {
    return UInt16(bitPattern) & Float16._significandMask
  }

  @inlinable
  public init(
    sign: FloatingPointSign,
    exponentBitPattern: UInt,
    significandBitPattern: UInt16
  ) {
    let signShift = Float16.significandBitCount + Float16.exponentBitCount
    let sign = UInt16(sign == .minus ? 1 : 0)
    let exponent = UInt16(
      exponentBitPattern & Float16._infinityExponent
    )
    let significand = UInt16(
      significandBitPattern & Float16._significandMask
    )
    self.init(bitPattern:
      sign &<< UInt16(signShift) |
      exponent &<< UInt16(Float16.significandBitCount) |
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
  public static var infinity: Float16 {
    return Float16(
      sign: .plus,
      exponentBitPattern: _infinityExponent,
      significandBitPattern: 0
    )
  }

  @inlinable
  public static var nan: Float16 {
    return Float16(nan: 0, signaling: false)
  }

  @inlinable
  public static var signalingNaN: Float16 {
    return Float16(nan: 0, signaling: true)
  }

  @available(*, unavailable, renamed: "nan")
  public static var quietNaN: Float16 { Builtin.unreachable() }

  @inlinable
  public static var greatestFiniteMagnitude: Float16 {
    return Float16(
      sign: .plus,
      exponentBitPattern: _infinityExponent - 1,
      significandBitPattern: _significandMask
    )
  }

  @inlinable
  public static var pi: Float16 {
    return 0x1.92p1
  }

  @inlinable
  public var ulp: Float16 {
    guard _fastPath(isFinite) else { return .nan }
    if _fastPath(isNormal) {
      let bitPattern_ = bitPattern & Float16.infinity.bitPattern
      return Float16(bitPattern: bitPattern_) * 0x1p-10
    }
    // On arm, flush subnormal values to 0.
    return .leastNormalMagnitude * 0x1p-10
  }

  @inlinable
  public static var leastNormalMagnitude: Float16 {
    return 0x1.0p-14
  }

  @inlinable
  public static var leastNonzeroMagnitude: Float16 {
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
  public static var ulpOfOne: Float16 {
    return 0x1.0p-10
  }

  @inlinable
  @_semantics("optimize.sil.inline.constant.arguments")
  public var exponent: Int {
    if !isFinite { return .max }
    if isZero { return .min }
    let provisional = Int(exponentBitPattern) - Int(Float16._exponentBias)
    if isNormal { return provisional }
    let shift =
      Float16.significandBitCount - significandBitPattern._binaryLogarithm()
    return provisional + 1 - shift
  }

  @inlinable
  public var significand: Float16 {
    if isNaN { return self }
    if isNormal {
      return Float16(sign: .plus,
        exponentBitPattern: Float16._exponentBias,
        significandBitPattern: significandBitPattern)
    }
    if isSubnormal {
      let shift =
        Float16.significandBitCount - significandBitPattern._binaryLogarithm()
      return Float16(
        sign: .plus,
        exponentBitPattern: Float16._exponentBias,
        significandBitPattern: significandBitPattern &<< shift
      )
    }
    // zero or infinity.
    return Float16(
      sign: .plus,
      exponentBitPattern: exponentBitPattern,
      significandBitPattern: 0
    )
  }

  @inlinable
  public init(sign: FloatingPointSign, exponent: Int, significand: Float16) {
    var result = significand
    if sign == .minus { result = -result }
    if significand.isFinite && !significand.isZero {
      var clamped = exponent
      let leastNormalExponent = 1 - Int(Float16._exponentBias)
      let greatestFiniteExponent = Int(Float16._exponentBias)
      if clamped < leastNormalExponent {
        clamped = max(clamped, 3*leastNormalExponent)
        while clamped < leastNormalExponent {
          result  *= Float16.leastNormalMagnitude
          clamped -= leastNormalExponent
        }
      }
      else if clamped > greatestFiniteExponent {
        clamped = min(clamped, 3*greatestFiniteExponent)
        let step = Float16(sign: .plus,
          exponentBitPattern: Float16._infinityExponent - 1,
          significandBitPattern: 0)
        while clamped > greatestFiniteExponent {
          result  *= step
          clamped -= greatestFiniteExponent
        }
      }
      let scale = Float16(
        sign: .plus,
        exponentBitPattern: UInt(Int(Float16._exponentBias) + clamped),
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
  ///     let x = Float16(nan: 0, signaling: false)
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
    _precondition(payload < (Float16._quietNaNMask &>> 1),
      "NaN payload is not encodable.")
    var significand = payload
    significand |= Float16._quietNaNMask &>> (signaling ? 1 : 0)
    self.init(
      sign: .plus,
      exponentBitPattern: Float16._infinityExponent,
      significandBitPattern: significand
    )
  }

  @inlinable
  public var nextUp: Float16 {
    // Silence signaling NaNs, map -0 to +0.
    let x = self + 0
#if arch(arm)
    // On arm, treat subnormal values as zero.
    if _slowPath(x == 0) { return .leastNonzeroMagnitude }
    if _slowPath(x == -.leastNonzeroMagnitude) { return -0.0 }
#endif
    if _fastPath(x < .infinity) {
      let increment = Int16(bitPattern: x.bitPattern) &>> 15 | 1
      let bitPattern_ = x.bitPattern &+ UInt16(bitPattern: increment)
      return Float16(bitPattern: bitPattern_)
    }
    return x
  }

  //  For core standard library floating-point types, LLVM can lower copysign
  //  for us; this gets somewhat better codegen than the generic implementation,
  //  but more importantly allows it to participate in other optimizations
  //  at the LLVM level.
  @_transparent
  public init(signOf sign: Float16, magnitudeOf mag: Float16) {
    _value = Builtin.int_copysign_FPIEEE16(mag._value, sign._value)
  }

  @_transparent
  public mutating func round(_ rule: FloatingPointRoundingRule) {
    switch rule {
    case .toNearestOrAwayFromZero:
      _value = Builtin.int_round_FPIEEE16(_value)
    case .toNearestOrEven:
      _value = Builtin.int_rint_FPIEEE16(_value)
    case .towardZero:
      _value = Builtin.int_trunc_FPIEEE16(_value)
    case .awayFromZero:
      if sign == .minus {
        _value = Builtin.int_floor_FPIEEE16(_value)
      }
      else {
        _value = Builtin.int_ceil_FPIEEE16(_value)
      }
    case .up:
      _value = Builtin.int_ceil_FPIEEE16(_value)
    case .down:
      _value = Builtin.int_floor_FPIEEE16(_value)
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
    _value = Builtin.fneg_FPIEEE16(self._value)
  }

  @_transparent
  public static func +=(lhs: inout Float16, rhs: Float16) {
    lhs._value = Builtin.fadd_FPIEEE16(lhs._value, rhs._value)
  }

  @_transparent
  public static func -=(lhs: inout Float16, rhs: Float16) {
    lhs._value = Builtin.fsub_FPIEEE16(lhs._value, rhs._value)
  }

  @_transparent
  public static func *=(lhs: inout Float16, rhs: Float16) {
    lhs._value = Builtin.fmul_FPIEEE16(lhs._value, rhs._value)
  }

  @_transparent
  public static func /=(lhs: inout Float16, rhs: Float16) {
    lhs._value = Builtin.fdiv_FPIEEE16(lhs._value, rhs._value)
  }

  @inlinable // FIXME(inline-always)
  @inline(__always)
  public mutating func formRemainder(dividingBy other: Float16) {
    self = Float16(_stdlib_remainderf(Float(self), Float(other)))
  }

  @inlinable // FIXME(inline-always)
  @inline(__always)
  public mutating func formTruncatingRemainder(dividingBy other: Float16) {
    _value = Builtin.frem_FPIEEE16(self._value, other._value)
  }

  @_transparent
  public mutating func formSquareRoot( ) {
    _value = Builtin.int_sqrt_FPIEEE16(_value)
  }

  @_transparent
  public mutating func addProduct(_ lhs: Float16, _ rhs: Float16) {
    _value = Builtin.int_fma_FPIEEE16(lhs._value, rhs._value, _value)
  }

  @_transparent
  public func isEqual(to other: Float16) -> Bool {
    return Bool(Builtin.fcmp_oeq_FPIEEE16(self._value, other._value))
  }

  @_transparent
  public func isLess(than other: Float16) -> Bool {
    return Bool(Builtin.fcmp_olt_FPIEEE16(self._value, other._value))
  }

  @_transparent
  public func isLessThanOrEqualTo(_ other: Float16) -> Bool {
    return Bool(Builtin.fcmp_ole_FPIEEE16(self._value, other._value))
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
      return exponentBitPattern < Float16._infinityExponent
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
      return isNaN && (significandBitPattern & Float16._quietNaNMask) == 0
    }
  }

  @inlinable
  public var binade: Float16 {
    guard _fastPath(isFinite) else { return .nan }
#if !arch(arm)
    if _slowPath(isSubnormal) {
      let bitPattern_ =
        (self * 0x1p10).bitPattern
          & (-Float16.infinity).bitPattern
      return Float16(bitPattern: bitPattern_) * 0x1p-10
    }
#endif
    return Float16(bitPattern: bitPattern & (-Float16.infinity).bitPattern)
  }

  @inlinable
  @_semantics("optimize.sil.inline.constant.arguments")
  public var significandWidth: Int {
    let trailingZeroBits = significandBitPattern.trailingZeroBitCount
    if isNormal {
      guard significandBitPattern != 0 else { return 0 }
      return Float16.significandBitCount &- trailingZeroBits
    }
    if isSubnormal {
      let leadingZeroBits = significandBitPattern.leadingZeroBitCount
      return UInt16.bitWidth &- (trailingZeroBits &+ leadingZeroBits &+ 1)
    }
    return -1
  }

  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(floatLiteral value: Float16) {
    self = value
  }
}

@available(SwiftStdlib 5.3, *)
extension Float16: _ExpressibleByBuiltinIntegerLiteral, ExpressibleByIntegerLiteral {
  @_transparent
  public
  init(_builtinIntegerLiteral value: Builtin.IntLiteral){
    self = Float16(Builtin.itofp_with_overflow_IntLiteral_FPIEEE16(value))
  }

  @_transparent
  public init(integerLiteral value: Int64) {
    self = Float16(Builtin.sitofp_Int64_FPIEEE16(value._value))
  }
}

#if !(os(Windows) || os(Android) || ($Embedded && !os(Linux) && !os(anyAppleOS))) && (arch(i386) || arch(x86_64))

@available(SwiftStdlib 5.3, *)
extension Float16: _ExpressibleByBuiltinFloatLiteral {
  @_transparent
  public
  init(_builtinFloatLiteral value: Builtin.FPIEEE80) {
    // workaround missing __truncxfhf
    self = Float16(Float80(value))
  }
}

#else

@available(SwiftStdlib 5.3, *)
extension Float16: _ExpressibleByBuiltinFloatLiteral {
  @_transparent
  public
  init(_builtinFloatLiteral value: Builtin.FPIEEE64) {
    // FIXME: This can result in double rounding errors (https://github.com/apple/swift/issues/49672).
    self = Float16(Builtin.fptrunc_FPIEEE64_FPIEEE16(value))
  }
}

#endif

@available(SwiftStdlib 5.3, *)
extension Float16: Hashable {
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
    return Hasher._hash(seed: seed, bytes: UInt64(v.bitPattern), count: 2)
  }
}


@available(SwiftStdlib 5.3, *)
extension Float16 {
  @inlinable // FIXME(inline-always)
  public var magnitude: Float16 {
    @inline(__always)
    get {
      return Float16(Builtin.int_fabs_FPIEEE16(_value))
    }
  }
}

@available(SwiftStdlib 5.3, *)
extension Float16 {
  @_transparent
  public static prefix func -(x: Float16) -> Float16 {
    return Float16(Builtin.fneg_FPIEEE16(x._value))
  }
}

@available(SwiftStdlib 5.3, *)
extension Float16: Sendable { }

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

// Construction from other concrete types.
@available(SwiftStdlib 5.3, *)
extension Float16 {
  @_transparent
  public init(_ v: Int) {
#if _pointerBitWidth(_64)
    _value = Builtin.sitofp_Int64_FPIEEE16(v._value)
#elseif _pointerBitWidth(_32)
    _value = Builtin.sitofp_Int32_FPIEEE16(v._value)
#else
    _value = Builtin.sitofp_Int16_FPIEEE16(v._value)
#endif
  }

  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init<Source: BinaryInteger>(_ value: Source) {
    if value.bitWidth <= 64 {
      if Source.isSigned {
        let asInt = Int64(truncatingIfNeeded: value)
        _value = Builtin.sitofp_Int64_FPIEEE16(asInt._value)
      } else {
        let asUInt = UInt64(truncatingIfNeeded: value)
        _value = Builtin.uitofp_Int64_FPIEEE16(asUInt._value)
      }
    } else {
      // TODO: we can do much better than the generic _convert here for Float
      // and Double by pulling out the high-order 32/64b of the integer, ORing
      // in a sticky bit, and then using the builtin.
      self = Float16._convert(from: value).value
    }
  }

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
        let extended = Int64(truncatingIfNeeded: value)
        _value = Builtin.sitofp_Int64_FPIEEE16(extended._value)
        guard self.isFinite && Int(self) == extended else {
          return nil
        }
      } else {
        let extended = UInt64(truncatingIfNeeded: value)
        _value = Builtin.uitofp_Int64_FPIEEE16(extended._value)
        guard self.isFinite && UInt(self) == extended else {
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

  /// Creates a new instance initialized to the given value.
  ///
  /// The value of `other` is represented exactly by the new instance. A NaN
  /// passed as `other` results in another NaN, with a signaling NaN value
  /// converted to quiet NaN.
  ///
  ///     let x: Float16 = 21.25
  ///     let y = Float16(x)
  ///     // y == 21.25
  ///
  ///     let z = Float16(Float16.nan)
  ///     // z.isNaN == true
  ///
  /// - Parameter other: The value to use for the new instance.
  @available(SwiftStdlib 5.3, *)
  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(_ other: Float16) {
    _value = other._value
  }

  /// Creates a new instance initialized to the given value, if it can be
  /// represented without rounding.
  ///
  /// If `other` can't be represented as an instance of `Float16` without
  /// rounding, the result of this initializer is `nil`. In particular,
  /// passing NaN as `other` always results in `nil`.
  ///
  ///     let x: Float16 = 21.25
  ///     let y = Float16(exactly: x)
  ///     // y == Optional.some(21.25)
  ///
  ///     let z = Float16(exactly: Float16.nan)
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
  ///     let y = Float16(x)
  ///     // y == 21.25
  ///
  ///     let z = Float16(Float.nan)
  ///     // z.isNaN == true
  ///
  /// - Parameter other: The value to use for the new instance.
  
  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(_ other: Float) {
    _value = Builtin.fptrunc_FPIEEE32_FPIEEE16(other._value)
  }

  /// Creates a new instance initialized to the given value, if it can be
  /// represented without rounding.
  ///
  /// If `other` can't be represented as an instance of `Float16` without
  /// rounding, the result of this initializer is `nil`. In particular,
  /// passing NaN as `other` always results in `nil`.
  ///
  ///     let x: Float = 21.25
  ///     let y = Float16(exactly: x)
  ///     // y == Optional.some(21.25)
  ///
  ///     let z = Float16(exactly: Float.nan)
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



  /// Creates a new instance that approximates the given value.
  ///
  /// The value of `other` is rounded to a representable value, if necessary.
  /// A NaN passed as `other` results in another NaN, with a signaling NaN
  /// value converted to quiet NaN.
  ///
  ///     let x: Double = 21.25
  ///     let y = Float16(x)
  ///     // y == 21.25
  ///
  ///     let z = Float16(Double.nan)
  ///     // z.isNaN == true
  ///
  /// - Parameter other: The value to use for the new instance.
  
  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(_ other: Double) {
    _value = Builtin.fptrunc_FPIEEE64_FPIEEE16(other._value)
  }

  /// Creates a new instance initialized to the given value, if it can be
  /// represented without rounding.
  ///
  /// If `other` can't be represented as an instance of `Float16` without
  /// rounding, the result of this initializer is `nil`. In particular,
  /// passing NaN as `other` always results in `nil`.
  ///
  ///     let x: Double = 21.25
  ///     let y = Float16(exactly: x)
  ///     // y == Optional.some(21.25)
  ///
  ///     let z = Float16(exactly: Double.nan)
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
  ///     let y = Float16(x)
  ///     // y == 21.25
  ///
  ///     let z = Float16(Float80.nan)
  ///     // z.isNaN == true
  ///
  /// - Parameter other: The value to use for the new instance.
  
  public init(_ other: Float80) {
    // If we use the Builtin for this operation, LLVM emits a call to
    // __truncxfhf, which would be a compiler-rt intrinsic but doesn't exist.
    // Workaround this by doing conversion in two stages, with the first stage
    // rounding to odd to avoid double-rounding.
    var flt = Float(other)
    if flt.isFinite {
      if Float80(flt).magnitude > other.magnitude {
        flt = Float(bitPattern: flt.bitPattern &- (~flt.bitPattern & 1))
      }
      else if Float80(flt).magnitude < other.magnitude {
        flt = Float(bitPattern: flt.bitPattern | 1)
      }
    }
    self = Float16(flt)
  }

  /// Creates a new instance initialized to the given value, if it can be
  /// represented without rounding.
  ///
  /// If `other` can't be represented as an instance of `Float16` without
  /// rounding, the result of this initializer is `nil`. In particular,
  /// passing NaN as `other` always results in `nil`.
  ///
  ///     let x: Float80 = 21.25
  ///     let y = Float16(exactly: x)
  ///     // y == Optional.some(21.25)
  ///
  ///     let z = Float16(exactly: Float80.nan)
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

@available(SwiftStdlib 5.3, *)
extension Float16 {
  @_transparent
  public static func +(lhs: Float16, rhs: Float16) -> Float16 {
    var lhs = lhs
    lhs += rhs
    return lhs
  }

  @_transparent
  public static func -(lhs: Float16, rhs: Float16) -> Float16 {
    var lhs = lhs
    lhs -= rhs
    return lhs
  }

  @_transparent
  public static func *(lhs: Float16, rhs: Float16) -> Float16 {
    var lhs = lhs
    lhs *= rhs
    return lhs
  }

  @_transparent
  public static func /(lhs: Float16, rhs: Float16) -> Float16 {
    var lhs = lhs
    lhs /= rhs
    return lhs
  }
}

//===----------------------------------------------------------------------===//
// Strideable Conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.3, *)
extension Float16: Strideable {
  @_transparent
  public func distance(to other: Float16) -> Float16 {
    return other - self
  }

  @_transparent
  public func advanced(by amount: Float16) -> Float16 {
    return self + amount
  }
}

//===----------------------------------------------------------------------===//
// AnyHashable
//===----------------------------------------------------------------------===//


@available(SwiftStdlib 5.3, *)
extension Float16: ConvertibleToBytes {}
@available(SwiftStdlib 5.3, *)
extension Float16: ConvertibleFromBytes {}

//===----------------------------------------------------------------------===//
// Deprecated operators
//===----------------------------------------------------------------------===//

#else

/// A half-precision (16-bit), floating-point value type.
///
/// On macOS, `Float16` is only available when targeting Apple silicon.
/// On other supported platforms, `Float16` is available for all
/// architectures. If the specified target supports 16-bit floating point
/// arithmetic directly, those instructions will be used; otherwise Float16
/// arithmetic will be emulated by the swift compiler and runtime.
@frozen
@available(SwiftStdlib 5.3, *)
@available(macOS, unavailable)
@available(macCatalyst, unavailable)
public struct Float16 {
  /// Creates a value initialized to zero.
  @_transparent
  public init() {
    fatalError("Float16 is not available")
  }
}

// This is a workaround for a compiler bug that omits the macOS 11 availability
// from the implicit conformance emitted into the generated .swiftinterface
// file. See https://github.com/apple/swift/pull/36669 for details.
// FIXME: rdar://76092800
@available(SwiftStdlib 5.3, *)
@available(macOS, unavailable)
@available(macCatalyst, unavailable)
extension Float16: Sendable { }

#endif

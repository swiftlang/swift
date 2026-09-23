//===--- Float80.swift ----------------------------------------*- swift -*-===//
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

#if !(os(Windows) || os(Android) || ($Embedded && !os(Linux) && !os(anyAppleOS))) && (arch(i386) || arch(x86_64))

/// An extended-precision, floating-point value type.
///
/// `Float80` is available on x86 if the target system's `long double` C type
/// is 80-bit, and unavailable otherwise.
@frozen
public struct Float80 {
  public // @testable
  var _value: Builtin.FPIEEE80

  @_transparent
  public init() {
    let zero: Int64 = 0
    self._value = Builtin.sitofp_Int64_FPIEEE80(zero._value)
  }

  @_transparent
  public // @testable
  init(_ _value: Builtin.FPIEEE80) {
    self._value = _value
  }
}


extension Float80: CustomStringConvertible {
  /// A textual representation of the value.
  ///
  /// For any finite value, this property provides a string that can be
  /// converted back to an instance of `Float80` without rounding errors.  That
  /// is, if `x` is an instance of `Float80`, then `Float80(x.description) ==
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


extension Float80: CustomDebugStringConvertible {
  /// A textual representation of the value, suitable for debugging.
  ///
  /// This property has the same value as the `description` property, except
  /// that NaN values are printed in an extended format.
  public var debugDescription: String {
    var buffer = _InlineArray<64, UTF8.CodeUnit>(repeating: 0x30)
    var span = buffer.mutableSpan
    let textRange = _Float80ToASCII(value: self, buffer: &span)
    let ascii = unsafe buffer.span.extracting(unchecked: textRange)
    return ascii.withUnsafeBufferPointer {
      unsafe String._fromASCII($0)
    }
  }
}


extension Float80: TextOutputStreamable {
  public func write<Target>(to target: inout Target) where Target: TextOutputStream {
    if isNaN {
      // Match `description`.
      target.write("nan")
      return
    }
    var buffer = _InlineArray<64, UTF8.CodeUnit>(repeating: 0x30)
    var span = buffer.mutableSpan
    let textRange = _Float80ToASCII(value: self, buffer: &span)
    let ascii = unsafe buffer.span.extracting(unchecked: textRange)
    return ascii.withUnsafeBufferPointer {
      unsafe target._writeASCII($0)
    }
  }
}


extension Float80: BinaryFloatingPoint {

  // Floating-point types are always symmetric, so Magnitude is Self.
  public typealias Magnitude = Float80

  public typealias Exponent = Int

  public typealias RawSignificand = UInt64

  @inlinable
  public static var exponentBitCount: Int {
    return 15
  }

  // `Float80.significandBitCount` is 63, even though 64 bits are used to
  // store the significand in the memory representation of a `Float80`
  // instance. Unlike other floating-point types, the `Float80` type
  // explicitly stores the leading integral significand bit; we abstract
  // that away to present the same user-facing interface as the floating-
  // point types.
  @inlinable
  public static var significandBitCount: Int {
    return 63
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

  // Internal implementation details of x86 Float80
  @frozen
  @usableFromInline
  internal struct _Representation {
    @usableFromInline
    internal var _storage: (UInt64, UInt16, /* pad */ UInt16, UInt16, UInt16)

    @usableFromInline
    @_transparent
    internal init(explicitSignificand: UInt64, signAndExponent: UInt16) {
      _storage = (explicitSignificand, signAndExponent, 0, 0, 0)
    }

    @usableFromInline
    @_transparent
    internal var explicitSignificand: UInt64 { return _storage.0 }

    @usableFromInline
    @_transparent
    internal var signAndExponent: UInt16 { return _storage.1 }

    @usableFromInline
    @_transparent
    internal var sign: FloatingPointSign {
      return FloatingPointSign(rawValue: Int(signAndExponent &>> 15))!
    }

    @usableFromInline
    @_transparent
    internal var exponentBitPattern: UInt {
      return UInt(signAndExponent) & 0x7fff
    }
  }

  @inlinable
  internal var _representation: _Representation {
    return unsafe unsafeBitCast(self, to: _Representation.self)
  }

  @inlinable
  public var sign: FloatingPointSign {
    return _representation.sign
  }

  @inlinable
  internal static var _explicitBitMask: UInt64 {
    @inline(__always) get { return 1 &<< 63 }
  }

  @inlinable
  public var exponentBitPattern: UInt {
    let provisional = _representation.exponentBitPattern
    if provisional == 0 {
      if _representation.explicitSignificand >= Float80._explicitBitMask {
        //  Pseudo-denormals have an exponent of 0 but the leading bit of the
        //  significand field is set.  These are noncanonical encodings of the
        //  same significand with an exponent of 1.
        return 1
      }
      //  Exponent is zero, leading bit of significand is clear, so this is
      //  a canonical zero or subnormal number.
      return 0
    }
    if _representation.explicitSignificand < Float80._explicitBitMask {
      //  If the exponent is not-zero but the leading bit of the significand
      //  is clear, then we have an invalid operand (unnormal, pseudo-inf, or
      //  pseudo-NaN).  All of these are noncanonical encodings of NaN.
      return Float80._infinityExponent
    }
    //  We have a canonical number, so the provisional exponent is correct.
    return provisional
  }

  @inlinable
  public var significandBitPattern: UInt64 {
    if _representation.exponentBitPattern > 0 &&
      _representation.explicitSignificand < Float80._explicitBitMask {
        //  If the exponent is nonzero and the leading bit of the significand
        //  is clear, then we have an invalid operand (unnormal, pseudo-inf, or
        //  pseudo-NaN).  All of these are noncanonical encodings of qNaN.
        return _representation.explicitSignificand | Float80._quietNaNMask
    }
    //  Otherwise we always get the "right" significand by simply clearing the
    //  integral bit.
    return _representation.explicitSignificand & Float80._significandMask
  }

  @inlinable
  public init(sign: FloatingPointSign,
              exponentBitPattern: UInt,
              significandBitPattern: UInt64) {
    let signBit = UInt16(sign == .minus ? 0x8000 : 0)
    let exponent = UInt16(exponentBitPattern)
    var significand = significandBitPattern
    if exponent != 0 { significand |= Float80._explicitBitMask }
    let rep = _Representation(
      explicitSignificand: significand, signAndExponent: signBit|exponent)
    self = unsafe unsafeBitCast(rep, to: Float80.self)
  }

  @inlinable
  public var isCanonical: Bool {
    if exponentBitPattern == 0 {
      // If exponent field is zero, canonical numbers have the explicit
      // significand bit clear.
      return _representation.explicitSignificand < Float80._explicitBitMask
    }
    // If exponent is nonzero, canonical values have the explicit significand
    // bit set.
    return _representation.explicitSignificand >= Float80._explicitBitMask
  }

  @inlinable
  public static var infinity: Float80 {
    let rep = _Representation(
      explicitSignificand: Float80._explicitBitMask,
      signAndExponent: 0x7fff
    )
    return unsafe unsafeBitCast(rep, to: Float80.self)
  }

  @inlinable
  public static var nan: Float80 {
    let rep = _Representation(
      explicitSignificand: Float80._explicitBitMask | Float80._quietNaNMask,
      signAndExponent: 0x7fff
    )
    return unsafe unsafeBitCast(rep, to: Float80.self)
  }

  @inlinable
  public static var signalingNaN: Float80 {
    return Float80(nan: 0, signaling: true)
  }

  @available(*, unavailable, renamed: "nan")
  public static var quietNaN: Float80 { Builtin.unreachable() }

  @inlinable
  public static var greatestFiniteMagnitude: Float80 {
    return 0x1.fffffffffffffffep16383
  }

  @inlinable
  public static var pi: Float80 {
    return 0x1.921fb54442d1846ap1
  }

  @inlinable
  public var ulp: Float80 {
    guard _fastPath(isFinite) else { return .nan }
    if exponentBitPattern > UInt(Float80.significandBitCount) {
      // self is large enough that self.ulp is normal, so we just compute its
      // exponent and construct it with a significand of zero.
      let ulpExponent =
        exponentBitPattern - UInt(Float80.significandBitCount)
      return Float80(
        sign: .plus,
        exponentBitPattern: ulpExponent,
        significandBitPattern: 0
      )
    }
    if exponentBitPattern >= 1 {
      // self is normal but ulp is subnormal.
      let ulpShift = UInt64(exponentBitPattern - 1)
      return Float80(
        sign: .plus,
        exponentBitPattern: 0,
        significandBitPattern: 1 &<< ulpShift
      )
    }
    return Float80(
      sign: .plus,
      exponentBitPattern: 0,
      significandBitPattern: 1
    )
  }

  @inlinable
  public static var leastNormalMagnitude: Float80 {
    return 0x1.0p-16382
  }

  @inlinable
  public static var leastNonzeroMagnitude: Float80 {
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
  public static var ulpOfOne: Float80 {
    return 0x1.0p-63
  }

  @inlinable
  @_semantics("optimize.sil.inline.constant.arguments")
  public var exponent: Int {
    if !isFinite { return .max }
    if isZero { return .min }
    let provisional = Int(exponentBitPattern) - Int(Float80._exponentBias)
    if isNormal { return provisional }
    let shift =
      Float80.significandBitCount - significandBitPattern._binaryLogarithm()
    return provisional + 1 - shift
  }

  @inlinable
  public var significand: Float80 {
    if isNaN { return self }
    if isNormal {
      return Float80(sign: .plus,
        exponentBitPattern: Float80._exponentBias,
        significandBitPattern: significandBitPattern)
    }
    if isSubnormal {
      let shift =
        Float80.significandBitCount - significandBitPattern._binaryLogarithm()
      return Float80(
        sign: .plus,
        exponentBitPattern: Float80._exponentBias,
        significandBitPattern: significandBitPattern &<< shift
      )
    }
    // zero or infinity.
    return Float80(
      sign: .plus,
      exponentBitPattern: exponentBitPattern,
      significandBitPattern: 0
    )
  }

  @inlinable
  public init(sign: FloatingPointSign, exponent: Int, significand: Float80) {
    var result = significand
    if sign == .minus { result = -result }
    if significand.isFinite && !significand.isZero {
      var clamped = exponent
      let leastNormalExponent = 1 - Int(Float80._exponentBias)
      let greatestFiniteExponent = Int(Float80._exponentBias)
      if clamped < leastNormalExponent {
        clamped = max(clamped, 3*leastNormalExponent)
        while clamped < leastNormalExponent {
          result  *= Float80.leastNormalMagnitude
          clamped -= leastNormalExponent
        }
      }
      else if clamped > greatestFiniteExponent {
        clamped = min(clamped, 3*greatestFiniteExponent)
        let step = Float80(sign: .plus,
          exponentBitPattern: Float80._infinityExponent - 1,
          significandBitPattern: 0)
        while clamped > greatestFiniteExponent {
          result  *= step
          clamped -= greatestFiniteExponent
        }
      }
      let scale = Float80(
        sign: .plus,
        exponentBitPattern: UInt(Int(Float80._exponentBias) + clamped),
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
  ///     let x = Float80(nan: 0, signaling: false)
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
    _precondition(payload < (Float80._quietNaNMask &>> 1),
      "NaN payload is not encodable.")
    var significand = payload
    significand |= Float80._quietNaNMask &>> (signaling ? 1 : 0)
    self.init(
      sign: .plus,
      exponentBitPattern: Float80._infinityExponent,
      significandBitPattern: significand
    )
  }

  @inlinable
  public var nextUp: Float80 {
    if isNaN { /* Silence signaling NaNs. */ return self + 0 }
    if sign == .minus {
      if significandBitPattern == 0 {
        if exponentBitPattern == 0 {
          return .leastNonzeroMagnitude
        }
        return Float80(sign: .minus,
          exponentBitPattern: exponentBitPattern - 1,
          significandBitPattern: Float80._significandMask)
      }
      return Float80(sign: .minus,
        exponentBitPattern: exponentBitPattern,
        significandBitPattern: significandBitPattern - 1)
    }
    if isInfinite { return self }
    if significandBitPattern == Float80._significandMask {
      return Float80(sign: .plus,
        exponentBitPattern: exponentBitPattern + 1,
        significandBitPattern: 0)
    }
    return Float80(sign: .plus,
      exponentBitPattern: exponentBitPattern,
      significandBitPattern: significandBitPattern + 1)
  }

  //  For core standard library floating-point types, LLVM can lower copysign
  //  for us; this gets somewhat better codegen than the generic implementation,
  //  but more importantly allows it to participate in other optimizations
  //  at the LLVM level.
  @_transparent
  public init(signOf sign: Float80, magnitudeOf mag: Float80) {
    _value = Builtin.int_copysign_FPIEEE80(mag._value, sign._value)
  }

  @_transparent
  public mutating func round(_ rule: FloatingPointRoundingRule) {
    switch rule {
    case .toNearestOrAwayFromZero:
      _value = Builtin.int_round_FPIEEE80(_value)
    case .toNearestOrEven:
      _value = Builtin.int_rint_FPIEEE80(_value)
    case .towardZero:
      _value = Builtin.int_trunc_FPIEEE80(_value)
    case .awayFromZero:
      if sign == .minus {
        _value = Builtin.int_floor_FPIEEE80(_value)
      }
      else {
        _value = Builtin.int_ceil_FPIEEE80(_value)
      }
    case .up:
      _value = Builtin.int_ceil_FPIEEE80(_value)
    case .down:
      _value = Builtin.int_floor_FPIEEE80(_value)
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
    _value = Builtin.fneg_FPIEEE80(self._value)
  }

  @_transparent
  public static func +=(lhs: inout Float80, rhs: Float80) {
    lhs._value = Builtin.fadd_FPIEEE80(lhs._value, rhs._value)
  }

  @_transparent
  public static func -=(lhs: inout Float80, rhs: Float80) {
    lhs._value = Builtin.fsub_FPIEEE80(lhs._value, rhs._value)
  }

  @_transparent
  public static func *=(lhs: inout Float80, rhs: Float80) {
    lhs._value = Builtin.fmul_FPIEEE80(lhs._value, rhs._value)
  }

  @_transparent
  public static func /=(lhs: inout Float80, rhs: Float80) {
    lhs._value = Builtin.fdiv_FPIEEE80(lhs._value, rhs._value)
  }

  @inlinable // FIXME(inline-always)
  @inline(__always)
  public mutating func formRemainder(dividingBy other: Float80) {
    self = _stdlib_remainderl(self, other)
  }

  @inlinable // FIXME(inline-always)
  @inline(__always)
  public mutating func formTruncatingRemainder(dividingBy other: Float80) {
    _value = Builtin.frem_FPIEEE80(self._value, other._value)
  }

  @_transparent
  public mutating func formSquareRoot( ) {
    _value = Builtin.int_sqrt_FPIEEE80(_value)
  }

  @_transparent
  public mutating func addProduct(_ lhs: Float80, _ rhs: Float80) {
    _value = Builtin.int_fma_FPIEEE80(lhs._value, rhs._value, _value)
  }

  @_transparent
  public func isEqual(to other: Float80) -> Bool {
    return Bool(Builtin.fcmp_oeq_FPIEEE80(self._value, other._value))
  }

  @_transparent
  public func isLess(than other: Float80) -> Bool {
    return Bool(Builtin.fcmp_olt_FPIEEE80(self._value, other._value))
  }

  @_transparent
  public func isLessThanOrEqualTo(_ other: Float80) -> Bool {
    return Bool(Builtin.fcmp_ole_FPIEEE80(self._value, other._value))
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
      return exponentBitPattern < Float80._infinityExponent
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
      return isNaN && (significandBitPattern & Float80._quietNaNMask) == 0
    }
  }

  @inlinable
  public var binade: Float80 {
    guard _fastPath(isFinite) else { return .nan }
    if exponentBitPattern != 0 {
      return Float80(sign: sign, exponentBitPattern: exponentBitPattern,
        significandBitPattern: 0)
    }
    if significandBitPattern == 0 { return self }
    // For subnormals, we isolate the leading significand bit.
    let index = significandBitPattern._binaryLogarithm()
    return Float80(sign: sign, exponentBitPattern: 0,
      significandBitPattern: 1 &<< index)
  }

  @inlinable
  @_semantics("optimize.sil.inline.constant.arguments")
  public var significandWidth: Int {
    let trailingZeroBits = significandBitPattern.trailingZeroBitCount
    if isNormal {
      guard significandBitPattern != 0 else { return 0 }
      return Float80.significandBitCount &- trailingZeroBits
    }
    if isSubnormal {
      let leadingZeroBits = significandBitPattern.leadingZeroBitCount
      return UInt64.bitWidth &- (trailingZeroBits &+ leadingZeroBits &+ 1)
    }
    return -1
  }

  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(floatLiteral value: Float80) {
    self = value
  }
}


extension Float80: _ExpressibleByBuiltinIntegerLiteral, ExpressibleByIntegerLiteral {
  @_transparent
  public
  init(_builtinIntegerLiteral value: Builtin.IntLiteral){
    self = Float80(Builtin.itofp_with_overflow_IntLiteral_FPIEEE80(value))
  }

  @_transparent
  public init(integerLiteral value: Int64) {
    self = Float80(Builtin.sitofp_Int64_FPIEEE80(value._value))
  }
}



extension Float80: _ExpressibleByBuiltinFloatLiteral {
  @_transparent
  public
  init(_builtinFloatLiteral value: Builtin.FPIEEE80) {
    self = Float80(value)
  }
}



extension Float80: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    var v = self
    if isZero {
      // To satisfy the axiom that equality implies hash equality, we need to
      // finesse the hash value of -0.0 to match +0.0.
      v = 0
    }
    hasher.combine(v._representation.signAndExponent)
    hasher.combine(v.significandBitPattern)
  }

  @inlinable
  public func _rawHashValue(seed: Int) -> Int {
    // To satisfy the axiom that equality implies hash equality, we need to
    // finesse the hash value of -0.0 to match +0.0.
    let v = isZero ? 0 : self
    var hasher = Hasher(_seed: seed)
    hasher.combine(v._representation.signAndExponent)
    hasher.combine(v.significandBitPattern)
    return hasher._finalize()
  }
}

@_unavailableInEmbedded
extension Float80: _HasCustomAnyHashableRepresentation {
  // Not @inlinable
  public func _toCustomAnyHashable() -> AnyHashable? {
    return AnyHashable(_box: _Float80AnyHashableBox(self))
  }
}


extension Float80 {
  @inlinable // FIXME(inline-always)
  public var magnitude: Float80 {
    @inline(__always)
    get {
      return Float80(Builtin.int_fabs_FPIEEE80(_value))
    }
  }
}


extension Float80 {
  @_transparent
  public static prefix func - (x: Float80) -> Float80 {
    return Float80(Builtin.fneg_FPIEEE80(x._value))
  }
}


extension Float80: Sendable { }

//===----------------------------------------------------------------------===//
// Explicit conversions between types.
//===----------------------------------------------------------------------===//

// Construction from other concrete types.

extension Float80 {

  // We "shouldn't" need this, but the typechecker barfs on an expression
  // in the test suite without it.
  // If replaced with @inline(__always) the init no longer gets
  // inlined in -Onone and this breaks the abi_v7k test in a subtle way.
  @_transparent
  public init(_ v: Int) {
    _value = Builtin.sitofp_Int64_FPIEEE80(v._value)
  }

  // Fast-path for conversion when the source is representable as int,
  // falling back on the generic _convert operation otherwise.
  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init<Source: BinaryInteger>(_ value: Source) {
    if value.bitWidth <= 64 {
      if Source.isSigned {
        let asInt = Int64(truncatingIfNeeded: value)
        _value = Builtin.sitofp_Int64_FPIEEE80(asInt._value)
      } else {
        let asUInt = UInt64(truncatingIfNeeded: value)
        _value = Builtin.uitofp_Int64_FPIEEE80(asUInt._value)
      }
    } else {
      // TODO: we can do much better than the generic _convert here for Float
      // and Double by pulling out the high-order 32/64b of the integer, ORing
      // in a sticky bit, and then using the builtin.
      self = Float80._convert(from: value).value
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
        _value = Builtin.sitofp_Int64_FPIEEE80(extended._value)
        guard self < 0x1.0p63 && Int(self) == extended else {
          return nil
        }
      } else {
        let extended = UInt(truncatingIfNeeded: value)
        _value = Builtin.uitofp_Int64_FPIEEE80(extended._value)
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
  ///     let y = Float80(x)
  ///     // y == 21.25
  ///
  ///     let z = Float80(Float16.nan)
  ///     // z.isNaN == true
  ///
  /// - Parameter other: The value to use for the new instance.
  @available(SwiftStdlib 5.3, *)
  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(_ other: Float16) {
    _value = Builtin.fpext_FPIEEE16_FPIEEE80(other._value)
  }

  /// Creates a new instance initialized to the given value, if it can be
  /// represented without rounding.
  ///
  /// If `other` can't be represented as an instance of `Float80` without
  /// rounding, the result of this initializer is `nil`. In particular,
  /// passing NaN as `other` always results in `nil`.
  ///
  ///     let x: Float16 = 21.25
  ///     let y = Float80(exactly: x)
  ///     // y == Optional.some(21.25)
  ///
  ///     let z = Float80(exactly: Float16.nan)
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
  ///     let y = Float80(x)
  ///     // y == 21.25
  ///
  ///     let z = Float80(Float.nan)
  ///     // z.isNaN == true
  ///
  /// - Parameter other: The value to use for the new instance.
  
  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(_ other: Float) {
    _value = Builtin.fpext_FPIEEE32_FPIEEE80(other._value)
  }

  /// Creates a new instance initialized to the given value, if it can be
  /// represented without rounding.
  ///
  /// If `other` can't be represented as an instance of `Float80` without
  /// rounding, the result of this initializer is `nil`. In particular,
  /// passing NaN as `other` always results in `nil`.
  ///
  ///     let x: Float = 21.25
  ///     let y = Float80(exactly: x)
  ///     // y == Optional.some(21.25)
  ///
  ///     let z = Float80(exactly: Float.nan)
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
  ///     let y = Float80(x)
  ///     // y == 21.25
  ///
  ///     let z = Float80(Double.nan)
  ///     // z.isNaN == true
  ///
  /// - Parameter other: The value to use for the new instance.
  
  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(_ other: Double) {
    _value = Builtin.fpext_FPIEEE64_FPIEEE80(other._value)
  }

  /// Creates a new instance initialized to the given value, if it can be
  /// represented without rounding.
  ///
  /// If `other` can't be represented as an instance of `Float80` without
  /// rounding, the result of this initializer is `nil`. In particular,
  /// passing NaN as `other` always results in `nil`.
  ///
  ///     let x: Double = 21.25
  ///     let y = Float80(exactly: x)
  ///     // y == Optional.some(21.25)
  ///
  ///     let z = Float80(exactly: Double.nan)
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

  /// Creates a new instance initialized to the given value.
  ///
  /// The value of `other` is represented exactly by the new instance. A NaN
  /// passed as `other` results in another NaN, with a signaling NaN value
  /// converted to quiet NaN.
  ///
  ///     let x: Float80 = 21.25
  ///     let y = Float80(x)
  ///     // y == 21.25
  ///
  ///     let z = Float80(Float80.nan)
  ///     // z.isNaN == true
  ///
  /// - Parameter other: The value to use for the new instance.
  
  @inlinable // FIXME(inline-always)
  @inline(__always)
  public init(_ other: Float80) {
    _value = other._value
  }

  /// Creates a new instance initialized to the given value, if it can be
  /// represented without rounding.
  ///
  /// If `other` can't be represented as an instance of `Float80` without
  /// rounding, the result of this initializer is `nil`. In particular,
  /// passing NaN as `other` always results in `nil`.
  ///
  ///     let x: Float80 = 21.25
  ///     let y = Float80(exactly: x)
  ///     // y == Optional.some(21.25)
  ///
  ///     let z = Float80(exactly: Float80.nan)
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


extension Float80 {
  @_transparent
  public static func + (lhs: Float80, rhs: Float80) -> Float80 {
    var lhs = lhs
    lhs += rhs
    return lhs
  }

  @_transparent
  public static func - (lhs: Float80, rhs: Float80) -> Float80 {
    var lhs = lhs
    lhs -= rhs
    return lhs
  }

  @_transparent
  public static func * (lhs: Float80, rhs: Float80) -> Float80 {
    var lhs = lhs
    lhs *= rhs
    return lhs
  }

  @_transparent
  public static func / (lhs: Float80, rhs: Float80) -> Float80 {
    var lhs = lhs
    lhs /= rhs
    return lhs
  }
}

//===----------------------------------------------------------------------===//
// Strideable Conformance
//===----------------------------------------------------------------------===//


extension Float80: Strideable {
  @_transparent
  public func distance(to other: Float80) -> Float80 {
    return other - self
  }

  @_transparent
  public func advanced(by amount: Float80) -> Float80 {
    return self + amount
  }
}

//===----------------------------------------------------------------------===//
// AnyHashable
//===----------------------------------------------------------------------===//

@_unavailableInEmbedded
internal struct _Float80AnyHashableBox: _AnyHashableBox {
  internal typealias Base = Float80

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
    if let box = box as? _Float80AnyHashableBox {
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

@available(*, unavailable, message: "Float80 cannot conform to ConvertibleToBytes")

extension Float80: ConvertibleToBytes {}

extension Float80: ConvertibleFromBytes {}

//===----------------------------------------------------------------------===//
// Deprecated operators
//===----------------------------------------------------------------------===//

#else

/// An extended-precision, floating-point value type.
///
/// `Float80` is available on x86 if the target system's `long double` C type
/// is 80-bit, and unavailable otherwise.
@frozen
@available(*, unavailable, message: "Float80 is not available on target platform.")
public struct Float80 {
  /// Creates a value initialized to zero.
  @_transparent
  public init() {
    fatalError("Float80 is not available")
  }
}


#endif

@_transparent
@available(*, unavailable,
  message: "For floating point numbers use truncatingRemainder instead")
public func % <T: BinaryFloatingPoint>(lhs: T, rhs: T) -> T {
  fatalError("% is not available.")
}

@_transparent
@available(*, unavailable,
  message: "For floating point numbers use formTruncatingRemainder instead")
public func %= <T: BinaryFloatingPoint> (lhs: inout T, rhs: T) {
  fatalError("%= is not available.")
}

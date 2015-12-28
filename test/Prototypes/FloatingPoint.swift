// RUN: rm -rf %t && mkdir -p %t
// RUN: %S/../../utils/line-directive %s -- %target-build-swift -parse-stdlib %s -o %t/a.out
// RUN: %S/../../utils/line-directive %s -- %target-run %t/a.out
// REQUIRES: executable_test
import Swift

//  TODO: These should probably subsumed into UnsignedIntegerType or
//  another integer protocol.  Dave has already done some work here.
public protocol FloatingPointRepresentationType : UnsignedIntegerType {
  var leadingZeros: UInt { get }
  func <<(left: Self, right: Self) -> Self
  func >>(left: Self, right: Self) -> Self
  init(_ value: UInt)
}

extension UInt64 : FloatingPointRepresentationType {
  public var leadingZeros: UInt {
    return UInt(_countLeadingZeros(Int64(bitPattern: self)))
  }
}
extension UInt32 : FloatingPointRepresentationType {
  public var leadingZeros: UInt {
    return UInt64(self).leadingZeros - 32
  }
}

//  Ewwww? <rdar://problem/20060017>
#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
  import Darwin
#elseif os(Linux)
  import Glibc
#endif

public protocol FloatingPointType : Comparable, SignedNumberType,
  IntegerLiteralConvertible,
FloatLiteralConvertible {
  
  /// An unsigned integer type large enough to hold the significand field.
  typealias SignificandBits: FloatingPointRepresentationType
  
  /// Positive infinity.
  ///
  /// Compares greater than all finite numbers.
  static var infinity: Self { get }
  
  /// Quiet NaN.
  ///
  /// Compares not equal to every value, including itself.  Most operations
  /// with a `NaN` operand will produce a `NaN` result.
  static var NaN: Self { get }
  
  /// NaN with specified `payload`.
  ///
  /// Compares not equal to every value, including itself.  Most operations
  /// with a `NaN` operand will produce a `NaN` result.
  static func NaN(payload bits: SignificandBits, signaling: Bool) -> Self
  
  /// The greatest finite value.
  ///
  /// Compares greater than or equal to all finite numbers, but less than
  /// infinity.
  static var greatestFiniteMagnitude: Self { get }
  
  // Note -- rationale for "ulp" instead of "epsilon":
  // We do not use that name because it is ambiguous at best and misleading
  // at worst:
  //
  // - Historically several definitions of "machine epsilon" have commonly
  //   been used, which differ by up to a factor of two or so.  By contrast
  //   "ulp" is a term with a specific unambiguous definition.
  //
  // - Some languages have used "epsilon" to refer to wildly different values,
  //   such as `leastMagnitude`.
  //
  // - Inexperienced users often believe that "epsilon" should be used as a
  //   tolerance for floating-point comparisons, because of the name.  It is
  //   nearly always the wrong value to use for this purpose.
  /// The unit in the last place of 1.0.
  ///
  /// This is the weight of the least significant bit of the significand of 1.0,
  /// or the positive difference between 1.0 and the next greater representable
  /// number.
  ///
  /// This value (or a similar value) is often called "epsilon", "machine
  /// epsilon", or "macheps" in other languages.
  static var ulp: Self { get }
  
  /// The least positive normal value.
  ///
  /// Compares less than or equal to all positive normal numbers.  There may
  /// be smaller positive numbers, but they are "subnormal", meaning that
  /// they are represented with less precision than normal numbers.
  static var leastNormalMagnitude: Self { get }
  
  /// The least positive value.
  ///
  /// Compares less than or equal to all positive numbers, but greater than
  /// zero.  If the target supports subnormal values, this is smaller than
  /// `leastNormalMagnitude`; otherwise (as on armv7), they are equal.
  static var leastMagnitude: Self { get }
  
  /// The `signbit`.  True for negative numbers, false for positive.
  ///
  /// This is simply the high-order bit in the encoding of `self`, regardless
  /// of the encoded value.  This *is not* the same thing as `self < 0`.
  /// In particular:
  ///
  /// - If `x` is `-0.0`, then `x.signbit` is `true`, but `x < 0` is `false`.
  /// - If `x` is `NaN`, then `x.signbit` could be either `true` or `false`,
  ///   (the signbit of `NaN` is unspecified) but `x < 0` is `false`.
  ///
  /// Implements the IEEE-754 `isSignMinus` operation.
  var signbit: Bool { get }
  
  /// The mathematical `exponent`.
  ///
  /// If `x` is a normal floating-point number, then `exponent` is simply the
  /// raw encoded exponent interpreted as a signed integer with the exponent
  /// bias removed.
  ///
  /// For subnormal numbers, `exponent` is computed as though the exponent
  /// range of `Self` were unbounded.  In particular, `x.exponent` will
  /// be smaller than the minimum normal exponent that can be encoded.
  ///
  /// Other edge cases:
  ///
  /// - If `x` is zero, then `x.exponent` is `Int.min`.
  /// - If `x` is +/-infinity or NaN, then `x.exponent` is `Int.max`
  ///
  /// Implements the IEEE-754 `logB` operation.
  var exponent: Int { get }
  
  /// The mathematical `significand` (sometimes erroneously called the "mantissa").
  ///
  /// `significand` is computed as though the exponent range of `Self` were
  /// unbounded; if `x` is a finite non-zero number, then `x.significand` is
  /// in the range `[1,2)`.
  ///
  /// For other values of `x`, `x.significand` is defined as follows:
  ///
  /// - If `x` is zero, then `x.significand` is 0.0.
  /// - If `x` is infinity, then `x.significand` is 1.0.
  /// - If `x` is NaN, then `x.significand` is NaN.
  ///
  /// For all floating-point `x`, if we define y by:
  ///
  ///    let y = Self(signbit: x.signbit, exponent: x.exponent,
  ///                 significand: x.significand)
  ///
  /// then `y` is equivalent to `x`, meaning that `y` is `x` canonicalized.
  /// For types that do not have non-canonical encodings, this implies that
  /// `y` has the same encoding as `x`.  Note that this is a stronger
  /// statement than `x == y`, as it implies that both the sign of zero and
  /// the payload of NaN are preserved.
  var significand: Self { get }
  
  /// Combines a signbit, exponent, and significand to produce a floating-point
  /// datum.
  ///
  /// In common usage, `significand` will generally be a number in the range
  /// `[1,2)`, but this is not required; the initializer supports any valid
  /// floating-point datum.  The result is:
  ///
  ///    `(-1)^signbit * significand * 2^exponent`
  ///
  /// (where ^ denotes the mathematical operation of exponentiation) computed
  /// as if by a single correctly-rounded floating-point operation.  If this
  /// value is outside the representable range of the type, overflow or
  /// underflow will occur, and zero, a subnormal value, or infinity will be
  /// returned, as with any basic operation.  Other edge cases:
  ///
  /// - If `significand` is zero or infinite, the result is zero or infinite,
  ///   regardless of the value of `exponent`.
  /// - If `significand` is NaN, the result is NaN.
  ///
  /// Note that for any floating-point datum `x` the result of
  ///
  ///   `Self(signbit: x.signbit,
  ///         exponent: x.exponent,
  ///         significand: x.significand)`
  ///
  /// is "the same" as `x` (if `x` is NaN, then this result is also `NaN`, but
  /// it might be a different NaN).
  ///
  /// Because of these properties, this initializer also implements the
  /// IEEE-754 `scaleB` operation.
  init(signbit: Bool, exponent: Int, significand: Self)
  
  /// The unit in the last place of `self`.
  ///
  /// This is the value of the least significant bit in the significand of
  /// `self`.  For most numbers `x`, this is the difference between `x` and
  /// the next greater (in magnitude) representable number.  There are some
  /// edge cases to be aware of:
  ///
  /// - `greatestFiniteMagnitude.ulp` is a finite number, even though
  ///   the next greater representable value is `infinity`.
  /// - `x.ulp` is `NaN` if `x` is not a finite number.
  /// - If `x` is very small in magnitude, then `x.ulp` may be a subnormal
  ///   number.  On targets that do not support subnormals, `x.ulp` may be
  ///   flushed to zero.
  var ulp: Self { get }
  
  //  TODO: IEEE-754 requires the following operations for every FP type.
  //  They need bindings (names) for Swift.  Some of them map to existing
  //  C library functions, so the default choice would be to use the C
  //  names, but we should consider if other names would be more appropriate
  //  for Swift.
  //
  //  For now I have simply used the IEEE-754 names to track them.
  //
  //    The C bindings for these operations are:
  //    roundToIntegralTiesToEven      roundeven (n1778)
  //    roundToIntegralTiesAway        round (c99)
  //    roundToIntegralTowardZero      trunc (c99)
  //    roundToIntegralTowardPositive  ceil  (c90)
  //    roundToIntegralTowardNegative  floor (c90)
  //
  //  Also TBD: should these only be available as free functions?
  /// Rounds `self` to nearest integral value, with halfway cases rounded
  /// to the even integer.
  func roundToIntegralTiesToEven() -> Self
  
  /// Rounds `self` to the nearest integral value, with halfway cases rounded
  /// away from zero.
  func roundToIntegralTiesToAway() -> Self
  
  /// Rounds `self` to an integral value towards zero.
  func roundToIntegralTowardZero() -> Self
  
  /// Rounds `self` to an integral value toward positive infinity.
  func roundToIntegralTowardPositive() -> Self
  
  /// Rounds `self` to an integral value toward negative infinity.
  func roundToIntegralTowardNegative() -> Self
  
  //  TODO: roundToIntegralExact requires a notion of flags and of
  //  rounding modes, which require language design.
  
  //  TODO: should nextUp and nextDown be computed properties or funcs?
  //  For me, these sit right on the edge in terms of what makes sense.
  /// The least `Self` that compares greater than `self`.
  ///
  /// - If `x` is `-infinity`, then `x.nextUp` is `-greatestMagnitude`.
  /// - If `x` is `-leastMagnitude`, then `x.nextUp` is `-0.0`.
  /// - If `x` is zero, then `x.nextUp` is `leastMagnitude`.
  /// - If `x` is `greatestMagnitude`, then `x.nextUp` is `infinity`.
  /// - If `x` is `infinity` or `NaN`, then `x.nextUp` is `x`.
  var nextUp: Self { get }
  
  /// The greatest `Self` that compares less than `self`.
  ///
  /// `x.nextDown` is equivalent to `-(-x).nextUp`
  var nextDown: Self { get }
  
  //  TODO: IEEE-754 defines the following semantics for remainder(x, y).
  //
  //  This operation differs from what is currently provided by the %
  //  operator, which implements fmod, not remainder.  The difference is
  //  that fmod is the remainder of truncating division (the sign matches
  //  that of x and the magnitude is in [0, y)), whereas remainder is what
  //  results from round-to-nearest division (it lies in [y/2, y/2]).
  //
  //  I would prefer that % implement the remainder operation, but this is
  //  a fairly significant change to the language that needs discussion.
  //  Both operations should be probably be available, anyway.
  /// Remainder of `x` divided by `y`.
  ///
  /// `remainder(x,y)` is defined for finite `x` and `y` by the mathematical
  /// relation `r = x - yn`, where `n` is the integer nearest to the exact
  /// number (*not* the floating-point value) `x/y`.  If `x/y` is exactly
  /// halfway between two integers, `n` is even.  `remainder` is always
  /// exact, and therefore is not affected by the rounding mode.
  ///
  /// If `remainder(x,y)` is zero, it has the same sign as `x`.  If `y` is
  /// infinite, then `remainder(x,y)` is `x`.
  static func remainder(x: Self, _ y: Self) -> Self
  
  //  TODO: The IEEE-754 "minNumber" and "maxNumber" operations should
  //  probably be provided by the min and max generic free functions, but
  //  there is some question of how best to do that.  As an initial
  //  binding, they are provided as static functions.
  //
  //  We will end up naming the static functions something else
  //  (if we keep them at all) to avoid confusion with Int.min, etc.
  /// The minimum of `x` and `y`.
  ///
  /// Returns `x` if `x < y`, `y` if `y < x`, and whichever of `x` or `y`
  /// is a number if the other is NaN.  The result is NaN only if both
  /// arguments are NaN.
  static func min(x: Self, _ y: Self) -> Self
  
  /// The maximum of `x` and `y`.
  ///
  /// Returns `x` if `x > y`, `y` if `y > x`, and whichever of `x` or `y`
  /// is a number if the other is NaN.  The result is NaN only if both
  /// arguments are NaN.
  static func max(x: Self, _ y: Self) -> Self
  
  //  Note: IEEE-754 calls these "minNumMag" and "maxNumMag".  C (n1778)
  //  uses "fminmag" and "fmaxmag".  Neither of these strike me as very
  //  good names.  I prefer minMagnitude and maxMagnitude, which are
  //  clear without being too wordy.
  /// Whichever of `x` or `y` has lesser magnitude.
  ///
  /// Returns `x` if `|x| < |y|`, `y` if `|y| < |x|`, and whichever of
  /// `x` or `y` is a number if the other is NaN.  The result is NaN
  /// only if both arguments are NaN.
  static func minMagnitude(left: Self, _ right: Self) -> Self
  
  /// Whichever of `x` or `y` has greater magnitude.
  ///
  /// Returns `x` if `|x| > |y|`, `y` if `|y| > |x|`, and whichever of
  /// `x` or `y` is a number if the other is NaN.  The result is NaN
  /// only if both arguments are NaN.
  static func maxMagnitude(left: Self, _ right: Self) -> Self
  
  func +(x: Self, y: Self) -> Self
  func -(x: Self, y: Self) -> Self
  func *(x: Self, y: Self) -> Self
  func /(x: Self, y: Self) -> Self
  
  //  Implementation details of formatOf operations.
  static func _addStickyRounding(x: Self, _ y: Self) -> Self
  static func _mulStickyRounding(x: Self, _ y: Self) -> Self
  static func _divStickyRounding(x: Self, _ y: Self) -> Self
  static func _sqrtStickyRounding(x: Self) -> Self
  static func _mulAddStickyRounding(x: Self, _ y: Self, _ z: Self) -> Self
  
  //  TODO: do we actually want to provide remainder as an operator?  It's
  //  definitely not obvious to me that we should, but we have until now.
  //  See further discussion with func remainder(x,y) above.
  func %(x: Self, y: Self) -> Self
  
  //  Conversions from all integer types.
  init(_ value: Int8)
  init(_ value: Int16)
  init(_ value: Int32)
  init(_ value: Int64)
  init(_ value: Int)
  init(_ value: UInt8)
  init(_ value: UInt16)
  init(_ value: UInt32)
  init(_ value: UInt64)
  init(_ value: UInt)
  init(_ value: SignificandBits)
  
  //  Conversions from all floating-point types.
  init(_ value: Float)
  init(_ value: Double)
#if arch(i386) || arch(x86_64)
  init(_ value: Float80)
#endif
  
  //  TODO: where do conversions to/from string live?  IEEE-754 requires
  //  conversions to/from decimal character and hexadecimal character
  //  sequences.
  
  /// Implements the IEEE-754 copy operation.
  prefix func +(value: Self) -> Self
  
  //  IEEE-754 negate operation is prefix `-`, provided by SignedNumberType.
  //  TODO: ensure that the optimizer is able to produce a simple xor for -.
  
  //  IEEE-754 abs operation is the free function abs( ), provided by
  //  SignedNumberType.  TODO: ensure that the optimizer is able to produce
  //  a simple and or bic for abs( ).
  
  //  TODO: should this be the free function copysign(x, y) instead, a la C?
  /// Returns datum with magnitude of `self` and sign of `from`.
  ///
  /// Implements the IEEE-754 copysign operation.
  func copysign(from: Self) -> Self
  
  //  TODO: "signaling/quiet" comparison predicates if/when we have a model for
  //  floating-point flags and exceptions in Swift.
  
  /// The floating point "class" of this datum.
  ///
  /// Implements the IEEE-754 `class` operation.
  var floatingPointClass: FloatingPointClassification { get }
  
  /// True if and only if `self` is zero.
  var isZero: Bool { get }
  
  /// True if and only if `self` is subnormal.
  ///
  /// A subnormal number does not use the full precision available to normal
  /// numbers of the same format.  Zero is not a subnormal number.
  var isSubnormal: Bool { get }
  
  /// True if and only if `self` is normal.
  ///
  /// A normal number uses the full precision available in the format.  Zero
  /// is not a normal number.
  var isNormal: Bool { get }
  
  /// True if and only if `self` is finite.
  ///
  /// If `x.isFinite` is `true`, then one of `x.isZero`, `x.isSubnormal`, or
  /// `x.isNormal` is also `true`, and `x.isInfinite` and `x.isNaN` are
  /// `false`.
  var isFinite: Bool { get }
  
  /// True if and only if `self` is infinite.
  ///
  /// Note that `isFinite` and `isInfinite` do not form a dichotomy, because
  /// they are not total.  If `x` is `NaN`, then both properties are `false`.
  var isInfinite: Bool { get }
  
  /// True if and only if `self` is NaN ("not a number").
  var isNaN: Bool { get }
  
  /// True if and only if `self` is a signaling NaN.
  var isSignaling: Bool { get }
  
  /// True if and only if `self` is canonical.
  ///
  /// Every floating-point datum of type Float or Double is canonical, but
  /// non-canonical values of type Float80 exist.  These are known as
  /// "pseudo-denormal", "unnormal", "pseudo-infinity", and "pseudo-nan".
  /// (https://en.wikipedia.org/wiki/Extended_precision#x86_Extended_Precision_Format)
  var isCanonical: Bool { get }
  
  /// A total order relation on all values of type Self (including NaN).
  func totalOrder(other: Self) -> Bool
  
  /// A total order relation that compares magnitudes.
  func totalOrderMagnitude(other: Self) -> Bool
  
  //  Note: this operation is *not* required by IEEE-754, but it is an oft-
  //  requested feature.  TBD: should +0 and -0 be equivalent?  Substitution
  //  property of equality says no.
  //
  //  More adventurous (probably crazy) thought: we *could* make this (or
  //  something like it) the default behavior of the == operator, and make
  //  IEEE-754 equality be the function.  IEEE-754 merely dictates that
  //  certain operations be available, not what their bindings in a
  //  language actually are.  Their are two problems with this:
  //
  //    - it violates the hell out of the principle of least surprise,
  //    given that programmers have been conditioned by years of using
  //    other languages.
  //
  //    - it would introduce a gratuitous minor inefficiency to most
  //    code on the hardware we have today, where IEEE-754 equality is
  //    generally a single test, and "equivalent" is not.
  //
  //  Still, I want to at least make note of the possibility.
  /// An equivalence relation on all values of type Self (including NaN).
  ///
  /// Unlike `==`, this relation is a formal equivalence relation.  In
  /// particular, it is reflexive.  All NaNs compare equal to each other
  /// under this relation.
  func equivalent(other: Self) -> Bool
}

//  Features of FloatingPointType that can be implemented without any
//  dependence on the internals of the type.
extension FloatingPointType {
  
  public static var NaN: Self { return NaN(payload: 0, signaling: false) }
  
  public static var ulp: Self { return Self(1).ulp }
  
  public func roundToIntegralTiesToEven() -> Self {
    fatalError("TODO once roundeven functions are provided in math.h")
  }
  
  public static func minMagnitude(left: Self, _ right: Self) -> Self {
    fatalError("TODO once fminmag functions are available in libm")
  }
  
  public static func maxMagnitude(left: Self, _ right: Self) -> Self {
    fatalError("TODO once fmaxmag functions are available in libm")
  }
  
  public static func _addStickyRounding(x: Self, _ y: Self) -> Self {
    fatalError("TODO: Unimplemented")
  }
  
  public static func _mulStickyRounding(x: Self, _ y: Self) -> Self {
    fatalError("TODO: Unimplemented")
  }
  
  public static func _divStickyRounding(x: Self, _ y: Self) -> Self {
    fatalError("TODO: Unimplemented")
  }
  
  public static func _sqrtStickyRounding(x: Self) -> Self {
    fatalError("TODO: Unimplemented")
  }
  
  public static func _mulAddStickyRounding(x: Self, _ y: Self, _ z: Self) -> Self {
    fatalError("TODO: Unimplemented")
  }
  
  public var floatingPointClass: FloatingPointClassification {
    if isSignaling { return .SignalingNaN }
    if isNaN { return .QuietNaN }
    if isInfinite { return signbit ? .NegativeInfinity : .PositiveInfinity }
    if isNormal { return signbit ? .NegativeNormal : .PositiveNormal }
    if isSubnormal { return signbit ? .NegativeSubnormal : .PositiveSubnormal }
    return signbit ? .NegativeZero : .PositiveZero
  }
  
  public func totalOrderMagnitude(other: Self) -> Bool {
    return abs(self).totalOrder(abs(other))
  }
  
  public func equivalent(other: Self) -> Bool {
    if self.isNaN && other.isNaN { return true }
    if self.isZero && other.isZero { return self.signbit == other.signbit }
    return self == other
  }
}

public protocol BinaryFloatingPointType: FloatingPointType {
  
  /// Values that parameterize the type:
  static var _exponentBitCount: UInt { get }
  static var _fractionalBitCount: UInt { get }
  
  /// The raw encoding of the exponent field of the floating-point datum.
  var exponentBitPattern: UInt { get }
  
  /// The raw encoding of the significand field of the floating-point datum.
  var significandBitPattern: SignificandBits { get }
  
  /// The least-magnitude member of the binade of `self`.
  ///
  /// If `x` is `+/-significand * 2^exponent`, then `x.binade` is
  /// `+/- 2^exponent`; i.e. the floating point number with the same sign
  /// and exponent, but a significand of 1.0.
  var binade: Self { get }
  
  /// Combines a signbit, exponent and significand bit patterns to produce a
  /// floating-point datum.  No error-checking is performed by this function;
  /// the bit patterns are simply concatenated to produce the floating-point
  /// encoding of the result.
  init(signbit: Bool,
    exponentBitPattern: UInt,
    significandBitPattern: SignificandBits)
  
  init<T: BinaryFloatingPointType>(_ other: T)
  
  //  TODO: IEEE-754 requires that the six basic operations (add, subtract,
  //  multiply, divide, square root, and FMA) be provided from all source
  //  formats to all destination formats, with a single rounding.  In order
  //  to satisfy this requirement (which I believe we should), we'll need
  //  something like the following.
  //
  //  fusedMultiplyAdd needs naming attention for Swift.  The C name
  //  fma(x,y,z) might be too terse for the Swift standard library, but
  //  fusedMultiplyAdd is awfully verbose.  mulAdd(x, y, z), perhaps?
  //  Or we could go full Obj-C style and do mul(_:, _:, add:), I suppose.
  //
  //  While `sqrt` and `fma` have traditionally been part of the
  //  math library in C-derived languages, they rightfully belong as part
  //  of the base FloatingPointType protocol in Swift, because they are
  //  IEEE-754 required operations, like + or *.
  /// The sum of `x` and `y`, correctly rounded to `Self`.
  static func add<X: BinaryFloatingPointType, Y: BinaryFloatingPointType>(x: X, _ y: Y) -> Self
  
  /// The difference of `x` and `y`, correctly rounded to `Self`.
  static func sub<X: BinaryFloatingPointType, Y: BinaryFloatingPointType>(x: X, _ y: Y) -> Self
  
  /// The product of `x` and `y`, correctly rounded to `Self`.
  static func mul<X: BinaryFloatingPointType, Y: BinaryFloatingPointType>(x: X, _ y: Y) -> Self
  
  /// The quotient of `x` and `y`, correctly rounded to `Self`.
  static func div<X: BinaryFloatingPointType, Y: BinaryFloatingPointType>(x: X, _ y: Y) -> Self
  
  /// The square root of `x`, correctly rounded to `Self`.
  static func sqrt<X: BinaryFloatingPointType>(x: X) -> Self
  
  /// (x*y) + z correctly rounded to `Self`.
  static func mulAdd<X: BinaryFloatingPointType, Y: BinaryFloatingPointType, Z: BinaryFloatingPointType>(x: X, _ y: Y, _ z: Z) -> Self
}

extension BinaryFloatingPointType {
  
  static var _exponentBias: UInt {
    return Self._infinityExponent >> 1
  }
  
  static var _infinityExponent: UInt {
    return 1 << _exponentBitCount - 1
  }
  
  static var _integralBitMask: SignificandBits {
    return 1 << SignificandBits(UIntMax(_fractionalBitCount))
  }
  
  static var _fractionalBitMask: SignificandBits {
    return _integralBitMask - 1
  }
  
  static var _quietBitMask: SignificandBits {
    return _integralBitMask >> 1
  }
  
  static var _payloadBitMask: SignificandBits {
    return _quietBitMask - 1
  }
  
  public static var infinity: Self {
    return Self(signbit: false,
      exponentBitPattern:_infinityExponent,
      significandBitPattern: 0)
  }
  
  public static func NaN(payload bits: SignificandBits, signaling: Bool) -> Self {
    var significand = bits & _payloadBitMask
    if signaling {
      // Ensure at least one bit is set in payload, otherwise we will get
      // an infinity instead of NaN.
      if significand == 0 {
        significand = _quietBitMask >> 1
      }
    } else {
      significand = significand | _quietBitMask
    }
    return Self(signbit: false,
      exponentBitPattern: _infinityExponent,
      significandBitPattern: significand)
  }
  
  public static var greatestFiniteMagnitude: Self {
    return Self(signbit: false,
      exponentBitPattern: _infinityExponent - 1,
      significandBitPattern: _fractionalBitMask)
  }
  
  public static var leastNormalMagnitude: Self {
    return Self(signbit: false, exponentBitPattern: 1, significandBitPattern: 0)
  }
  
  public static var leastMagnitude: Self {
#if arch(arm)
    return .leastNormalMagnitude
#else
    return Self(signbit: false, exponentBitPattern: 0, significandBitPattern: 1)
#endif
  }
  
  public var exponent: Int {
    if !isFinite { return .max }
    let provisional = Int(exponentBitPattern) - Int(Self._exponentBias)
    if isNormal { return provisional }
    if isZero { return .min }
    let shift = significandBitPattern.leadingZeros - Self._fractionalBitMask.leadingZeros
    return provisional - Int(shift)
  }
  
  public var significand: Self {
    if isNaN { return self }
    if isNormal {
      return Self(Self._integralBitMask | significandBitPattern) * Self.ulp
    }
    if isZero { return 0 }
    let shift = 1 + significandBitPattern.leadingZeros - Self._fractionalBitMask.leadingZeros
    return Self(significandBitPattern << SignificandBits(shift)) * Self.ulp
  }
  
  public init(signbit: Bool, exponent: Int, significand: Self) {
    var result = significand
    if signbit { result = -result }
    if significand.isFinite && !significand.isZero {
      var clamped = exponent
      if clamped < Self.leastNormalMagnitude.exponent {
        clamped = max(clamped, 3*Self.leastNormalMagnitude.exponent)
        while clamped < Self.leastNormalMagnitude.exponent {
          result = result * Self.leastNormalMagnitude
          clamped += Self.leastNormalMagnitude.exponent
        }
      }
      else if clamped > Self.greatestFiniteMagnitude.exponent {
        clamped = min(clamped, 3*Self.greatestFiniteMagnitude.exponent)
        while clamped > Self.greatestFiniteMagnitude.exponent {
          result = result * Self.greatestFiniteMagnitude.binade
          clamped -= Self.greatestFiniteMagnitude.exponent
        }
      }
      let scale = Self(signbit: false,
        exponentBitPattern: UInt(Int(Self._exponentBias) + clamped),
        significandBitPattern: Self._integralBitMask)
      result = result * scale
    }
    self = result
  }
  
  public var ulp: Self {
    if !isFinite { return .NaN }
    if exponentBitPattern > Self._fractionalBitCount {
      //  ulp is normal, so we directly manifest its exponent and use a
      //  significand of 1.
      let ulpExponent = exponentBitPattern - Self._fractionalBitCount
      return Self(signbit: false, exponentBitPattern: ulpExponent, significandBitPattern: 0)
    }
    if exponentBitPattern >= 1 {
      //  self is normal, but ulp is subnormal; we need to compute a shift
      //  to apply to the significand.
      let ulpShift = SignificandBits(exponentBitPattern - 1)
      return Self(signbit: false, exponentBitPattern: 0, significandBitPattern: 1 << ulpShift)
    }
    return Self(signbit:false, exponentBitPattern:0, significandBitPattern:1)
  }
  
  public var nextUp: Self {
    if isNaN { return self }
    if signbit {
      if significandBitPattern == 0 {
        if exponentBitPattern == 0 { return Self.leastMagnitude }
        return Self(signbit: true,
          exponentBitPattern: exponentBitPattern - 1,
          significandBitPattern: Self._fractionalBitMask)
      }
      return Self(signbit: true,
        exponentBitPattern: exponentBitPattern,
        significandBitPattern: significandBitPattern - 1)
    }
    if isInfinite { return self }
    if significandBitPattern == Self._fractionalBitMask {
      return Self(signbit: false,
        exponentBitPattern: exponentBitPattern + 1,
        significandBitPattern: 0)
    }
    return Self(signbit: false,
      exponentBitPattern: exponentBitPattern,
      significandBitPattern: significandBitPattern + 1)
  }
  
  public var nextDown: Self { return -(-self).nextUp }
  
  public var binade: Self {
    if !isFinite { return .NaN }
    if isNormal {
      return Self(signbit: false, exponentBitPattern: exponentBitPattern,
        significandBitPattern: Self._integralBitMask)
    }
    if isZero { return 0 }
    let shift = significandBitPattern.leadingZeros - Self._integralBitMask.leadingZeros
    let significand = Self._integralBitMask >> SignificandBits(shift)
    return Self(signbit: false, exponentBitPattern: 0,
      significandBitPattern: significand)
  }
  
  public static func add<X: BinaryFloatingPointType, Y: BinaryFloatingPointType>(x: X, _ y: Y) -> Self {
    if X._fractionalBitCount < Y._fractionalBitCount { return add(y, x) }
    if X._fractionalBitCount <= Self._fractionalBitCount { return Self(x) + Self(y) }
    return Self(X._addStickyRounding(x, X(y)))
  }
  
  public static func sub<X: BinaryFloatingPointType, Y: BinaryFloatingPointType>(x: X, _ y: Y) -> Self {
    return Self.add(x, -y)
  }
  
  public static func mul<X: BinaryFloatingPointType, Y: BinaryFloatingPointType>(x: X, _ y: Y) -> Self {
    if X._fractionalBitCount < Y._fractionalBitCount { return mul(y, x) }
    if X._fractionalBitCount <= Self._fractionalBitCount { return Self(x) * Self(y) }
    return Self(X._mulStickyRounding(x, X(y)))
  }
  
  public static func div<X: BinaryFloatingPointType, Y: BinaryFloatingPointType>(x: X, _ y: Y) -> Self {
    if X._fractionalBitCount <= Self._fractionalBitCount &&
      Y._fractionalBitCount <= Self._fractionalBitCount { return Self(x) / Self(y) }
    if X._fractionalBitCount < Y._fractionalBitCount { return Self(Y._divStickyRounding(Y(x), y)) }
    return Self(X._divStickyRounding(x, X(y)))
  }
  
  public static func sqrt<X: BinaryFloatingPointType>(x: X) -> Self {
    if X._fractionalBitCount <= Self._fractionalBitCount { return sqrt(Self(x)) }
    return Self(X._sqrtStickyRounding(x))
  }
  
  public static func mulAdd<X: BinaryFloatingPointType, Y: BinaryFloatingPointType, Z: BinaryFloatingPointType>(x: X, _ y: Y, _ z: Z) -> Self {
    if X._fractionalBitCount < Y._fractionalBitCount { return mulAdd(y, x, z) }
    if X._fractionalBitCount <= Self._fractionalBitCount &&
      Z._fractionalBitCount <= Self._fractionalBitCount { return mulAdd(Self(x), Self(y), Self(z)) }
    if X._fractionalBitCount < Z._fractionalBitCount { return Self(Z._mulAddStickyRounding(Z(x), Z(y), z)) }
    return Self(X._mulAddStickyRounding(x, X(y), X(z)))
  }
  
  public var absoluteValue: Self {
    return Self(signbit: false, exponentBitPattern: exponentBitPattern,
      significandBitPattern: significandBitPattern)
  }
  
  public func copysign(from: Self) -> Self {
    return Self(signbit: from.signbit, exponentBitPattern: exponentBitPattern,
      significandBitPattern: significandBitPattern)
  }
  
  public var isZero: Bool {
    return exponentBitPattern == 0 && significandBitPattern == 0
  }
  
  public var isSubnormal: Bool {
    return exponentBitPattern == 0 && significandBitPattern != 0
  }
  
  public var isNormal: Bool {
    return exponentBitPattern > 0 && isFinite
  }
  
  public var isFinite: Bool {
    return exponentBitPattern < Self._infinityExponent
  }
  
  public var isInfinite: Bool {
    return exponentBitPattern == Self._infinityExponent &&
      significandBitPattern == Self._integralBitMask
  }
  
  public var isNaN: Bool {
    return exponentBitPattern == Self._infinityExponent && !isInfinite
  }
  
  public var isSignaling: Bool {
    return isNaN && (significandBitPattern & Self._quietBitMask == 0)
  }
  
  public var isCanonical: Bool { return true }
  
  public func totalOrder(other: Self) -> Bool {
    //  Every negative-signed value (even NaN) is less than every positive-
    //  signed value, so if the signs do not match, we simply return the
    //  signbit of self.
    if signbit != other.signbit { return signbit }
    //  Signbits match; look at exponents.
    if exponentBitPattern > other.exponentBitPattern { return signbit }
    if exponentBitPattern < other.exponentBitPattern { return !signbit }
    //  Signs and exponents match, look at significands.
    if significandBitPattern > other.significandBitPattern { return signbit }
    if significandBitPattern < other.significandBitPattern { return !signbit }
    return true
  }
}

public protocol FloatingPointInterchangeType: FloatingPointType {
  
  /// An unsigned integer type used to represent floating-point encodings.
  typealias BitPattern: FloatingPointRepresentationType
  
  /// Interpret `encoding` as a little-endian encoding of `Self`.
  init(littleEndian encoding: BitPattern)
  
  /// Interpret `encoding` as a big-endian encoding of `Self`.
  init(bigEndian encoding: BitPattern)
  
  /// Get the little-endian encoding of `self` as an integer.
  var littleEndian: BitPattern { get }
  
  /// Get the big-endian encoding of `self` as an integer.
  var bigEndian: BitPattern { get }
}

extension FloatingPointInterchangeType {
  public init(littleEndian encoding: BitPattern) {
#if arch(i386) || arch(x86_64) || arch(arm) || arch(arm64)
    self = unsafeBitCast(encoding, Self.self)
#else
    _UnsupportedArchitectureError()
#endif
  }
  public init(bigEndian encoding: BitPattern) {
    fatalError("TODO: with low-level generic integer type support for bswap.")
  }
  public var littleEndian: BitPattern {
#if arch(i386) || arch(x86_64) || arch(arm) || arch(arm64)
    return unsafeBitCast(self, BitPattern.self)
#else
    _UnsupportedArchitectureError()
#endif
  }
  public var bigEndian: BitPattern {
    fatalError("TODO: with low-level generic integer type support for bswap.")
  }
}

extension Float : BinaryFloatingPointType, FloatingPointInterchangeType {
  var _representation: UInt32 { return unsafeBitCast(self, UInt32.self) }
  public typealias SignificandBits = UInt32
  public typealias BitPattern = UInt32
  public static var _fractionalBitCount: UInt { return 23 }
  public static var _exponentBitCount: UInt { return 8 }
  public var signbit: Bool { return _representation >> 31 == 1 }
  public var exponentBitPattern: UInt { return UInt(_representation >> 23) & 0xff }
  public var significandBitPattern: SignificandBits { return _representation & Float._fractionalBitMask }
  public init(signbit: Bool, exponentBitPattern: UInt, significandBitPattern: SignificandBits) {
    let sign = SignificandBits(signbit ? 1 : 0) << 31
    let exponent = SignificandBits(exponentBitPattern) << 23
    let _representation = sign | exponent | significandBitPattern
    self = unsafeBitCast(_representation, Float.self)
  }
  public init<T: BinaryFloatingPointType>(_ other: T) {
    // rdar://16980851 #if does not work with 'switch'
#if arch(i386) || arch(x86_64)
    switch other {
    case let f as Float:
      self = f
    case let d as Double:
      self = Float(d)
    case let ld as Float80:
      self = Float(ld)
    default:
      fatalError()
    }
#else
    switch other {
    case let f as Float:
      self = f
    case let d as Double:
      self = Float(d)
    default:
      fatalError()
    }
#endif
  }
  public func roundToIntegralTiesToAway() -> Float { return roundf(self) }
  public func roundToIntegralTowardZero() -> Float { return truncf(self) }
  public func roundToIntegralTowardPositive() -> Float { return ceilf(self) }
  public func roundToIntegralTowardNegative() -> Float { return floorf(self) }
  public static func remainder(left: Float, _ right: Float) -> Float { return remainderf(left, right) }
  public static func min(left: Float, _ right: Float) -> Float { return fminf(left, right) }
  public static func max(left: Float, _ right: Float) -> Float { return fmaxf(left, right) }
  public static func sqrt(x: Float) -> Float { return sqrtf(x) }
}

extension Double : BinaryFloatingPointType {
  var _representation: UInt64 { return unsafeBitCast(self, UInt64.self) }
  public typealias SignificandBits = UInt64
  public static var _fractionalBitCount: UInt { return 52 }
  public static var _exponentBitCount: UInt { return 11 }
  public var signbit: Bool { return _representation >> 63 == 1 }
  public var exponentBitPattern: UInt { return UInt(_representation >> 52) & 0x7ff }
  public var significandBitPattern: SignificandBits { return _representation & Double._fractionalBitMask }
  public init(signbit: Bool, exponentBitPattern: UInt, significandBitPattern: SignificandBits) {
    let sign = SignificandBits(signbit ? 1 : 0) << 63
    let exponent = SignificandBits(exponentBitPattern) << 52
    let _representation = sign | exponent | significandBitPattern
    self = unsafeBitCast(_representation, Double.self)
  }
  public init<T: BinaryFloatingPointType>(_ other: T) {
    // rdar://16980851 #if does not work with 'switch' cases
#if arch(i386) || arch(x86_64)
    switch other {
    case let f as Float:
      self = Double(f)
    case let d as Double:
      self = d
    case let ld as Float80:
      self = Double(ld)
    default:
      fatalError()
    }
#else
    switch other {
    case let f as Float:
      self = Double(f)
    case let d as Double:
      self = d
    default:
      fatalError()
    }
#endif
  }
  public func roundToIntegralTiesToAway() -> Double { return round(self) }
  public func roundToIntegralTowardZero() -> Double { return trunc(self) }
  public func roundToIntegralTowardPositive() -> Double { return ceil(self) }
  public func roundToIntegralTowardNegative() -> Double { return floor(self) }
  public static func remainder(left: Double, _ right: Double) -> Double { return remainder(left, right) }
  public static func min(left: Double, _ right: Double) -> Double { return fmin(left, right) }
  public static func max(left: Double, _ right: Double) -> Double { return fmax(left, right) }
}

#if arch(i386) || arch(x86_64)
extension Float80 : BinaryFloatingPointType {
  
  // Internal implementation details
  struct _Float80Representation {
    var explicitSignificand: UInt64
    var signAndExponent: UInt16
    var _padding: (UInt16, UInt16, UInt16) = (0, 0, 0)
    var signbit: Bool { return signAndExponent >> 15 == 1 }
    var exponentBitPattern: UInt { return UInt(signAndExponent) & 0x7fff }
    init(explicitSignificand: UInt64, signAndExponent: UInt16) {
      self.explicitSignificand = explicitSignificand
      self.signAndExponent = signAndExponent
    }
  }
  
  var _representation: _Float80Representation {
    return unsafeBitCast(self, _Float80Representation.self)
  }
  
  //  Public requirements
  public typealias SignificandBits = UInt64
  
  public static var _fractionalBitCount: UInt { return 63 }
  
  public static var _exponentBitCount: UInt { return 15 }
  
  public var signbit: Bool { return _representation.signbit }
  
  public var exponentBitPattern: UInt {
    if _representation.exponentBitPattern == 0 {
      if _representation.explicitSignificand >= Float80._integralBitMask {
        //  Pseudo-denormals have an exponent of 0 but the leading bit of the
        //  significand field is set.  These are non-canonical encodings of the
        //  same significand with an exponent of 1.
        return 1
      }
      //  Exponent is zero, leading bit of significand is clear, so this is
      //  just a canonical zero or subnormal number.
      return 0
    }
    if _representation.explicitSignificand < Float80._integralBitMask {
      //  If the exponent is not-zero and the leading bit of the significand
      //  is clear, then we have an invalid operand (unnormal, pseudo-inf, or
      //  pseudo-nan).  All of these are treated as NaN by the hardware, so
      //  we pretend that the exponent is that of a NaN.
      return Float80._infinityExponent
    }
    return _representation.exponentBitPattern
  }
  
  public var significandBitPattern: UInt64 {
    if _representation.exponentBitPattern > 0 &&
      _representation.explicitSignificand < Float80._integralBitMask {
        //  If the exponent is non-zero and the leading bit of the significand
        //  is clear, then we have an invalid operand (unnormal, pseudo-inf, or
        //  pseudo-nan).  All of these are treated as NaN by the hardware, so
        //  we make sure that bit of the significand field is set.
        return _representation.explicitSignificand | Float80._quietBitMask
    }
    //  Otherwise we always get the "right" significand by simply clearing the
    //  integral bit.
    return _representation.explicitSignificand & Float80._fractionalBitMask
  }
  
  public init(signbit: Bool, exponentBitPattern: UInt, significandBitPattern: UInt64) {
    let sign = UInt16(signbit ? 0x8000 : 0)
    let exponent = UInt16(exponentBitPattern)
    var significand = significandBitPattern
    if exponent != 0 { significand |= Float80._integralBitMask }
    let representation =  _Float80Representation(explicitSignificand: significand, signAndExponent: sign|exponent)
    self = unsafeBitCast(representation, Float80.self)
  }
  
  public init<T: BinaryFloatingPointType>(_ other: T) {
    switch other {
    case let f as Float:
      self = Float80(f)
    case let d as Double:
      self = Float80(d)
    case let ld as Float80:
      self = ld
    default:
      fatalError()
    }
  }
  
  public func roundToIntegralTiesToAway() -> Float80 { fatalError("TODO: nead roundl( )") }
  public func roundToIntegralTowardZero() -> Float80 { fatalError("TODO: nead truncl( )") }
  public func roundToIntegralTowardPositive() -> Float80 { fatalError("TODO: nead ceill( )") }
  public func roundToIntegralTowardNegative() -> Float80 { fatalError("TODO: nead floorl( )") }
  public static func remainder(left: Float80, _ right: Float80) -> Float80 { fatalError("TODO: nead remainderl( )") }
  public static func min(left: Float80, _ right: Float80) -> Float80 { fatalError("TODO: nead fminl( )") }
  public static func max(left: Float80, _ right: Float80) -> Float80 { fatalError("TODO: nead fmaxl( )") }
  
  public var isCanonical: Bool {
    if exponentBitPattern == 0 {
      return _representation.explicitSignificand < Float80._integralBitMask
    }
    return _representation.explicitSignificand >= Float80._integralBitMask
  }
}
#endif

//  Unchanged from existing FloatingPoint protocol support.
/// The set of IEEE-754 floating-point "classes".  Every floating-point datum
/// belongs to exactly one of these classes.
public enum FloatingPointClassification {
  case SignalingNaN
  case QuietNaN
  case NegativeInfinity
  case NegativeNormal
  case NegativeSubnormal
  case NegativeZero
  case PositiveZero
  case PositiveSubnormal
  case PositiveNormal
  case PositiveInfinity
}

extension FloatingPointClassification : Equatable {}

public func ==(lhs: FloatingPointClassification, rhs: FloatingPointClassification) -> Bool {
  switch (lhs, rhs) {
  case (.SignalingNaN, .SignalingNaN),
  (.QuietNaN, .QuietNaN),
  (.NegativeInfinity, .NegativeInfinity),
  (.NegativeNormal, .NegativeNormal),
  (.NegativeSubnormal, .NegativeSubnormal),
  (.NegativeZero, .NegativeZero),
  (.PositiveZero, .PositiveZero),
  (.PositiveSubnormal, .PositiveSubnormal),
  (.PositiveNormal, .PositiveNormal),
  (.PositiveInfinity, .PositiveInfinity):
    return true
  default:
    return false
  }
}

//===--- tests ------------------------------------------------------------===//
import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

var tests = TestSuite("Floating Point")

tests.test("Parts") {
  expectEqual(Int.max, (-Float.infinity).exponent)
  expectEqual(127, (-Float.greatestFiniteMagnitude).exponent)
  expectEqual(1, Float(-2).exponent)
  expectEqual(0, Float(-1).exponent)
  expectEqual(-1, Float(-0.5).exponent)
  expectEqual(-23, (-Float.ulp).exponent)
  expectEqual(-125, (-2*Float.leastNormalMagnitude).exponent)
  expectEqual(-126, Float.leastNormalMagnitude.exponent)
#if !arch(arm)
  expectEqual(-127, (-0.5*Float.leastNormalMagnitude).exponent)
  expectEqual(-149, (-Float.leastMagnitude).exponent)
#endif
  expectEqual(Int.min, Float(-0).exponent)
  expectEqual(Int.min, Float(0).exponent)
#if !arch(arm)
  expectEqual(-149, Float.leastMagnitude.exponent)
  expectEqual(-127, (Float.leastNormalMagnitude/2).exponent)
#endif
  expectEqual(-126, Float.leastNormalMagnitude.exponent)
  expectEqual(-125, (2*Float.leastNormalMagnitude).exponent)
  expectEqual(-23, Float.ulp.exponent)
  expectEqual(-1, Float(0.5).exponent)
  expectEqual(0, Float(1).exponent)
  expectEqual(1, Float(2).exponent)
  expectEqual(127, Float.greatestFiniteMagnitude.exponent)
  expectEqual(Int.max, Float.infinity.exponent)
  expectEqual(Int.max, Float.NaN.exponent)
  
  expectEqual(Int.max, (-Double.infinity).exponent)
  expectEqual(1023, (-Double.greatestFiniteMagnitude).exponent)
  expectEqual(1, Double(-2).exponent)
  expectEqual(0, Double(-1).exponent)
  expectEqual(-1, Double(-0.5).exponent)
  expectEqual(-52, (-Double.ulp).exponent)
  expectEqual(-1021, (-2*Double.leastNormalMagnitude).exponent)
  expectEqual(-1022, Double.leastNormalMagnitude.exponent)
#if !arch(arm)
  expectEqual(-1023, (-0.5*Double.leastNormalMagnitude).exponent)
  expectEqual(-1074, (-Double.leastMagnitude).exponent)
#endif
  expectEqual(Int.min, Double(-0).exponent)
  expectEqual(Int.min, Double(0).exponent)
#if !arch(arm)
  expectEqual(-1074, Double.leastMagnitude.exponent)
  expectEqual(-1023, (Double.leastNormalMagnitude/2).exponent)
#endif
  expectEqual(-1022, Double.leastNormalMagnitude.exponent)
  expectEqual(-1021, (2*Double.leastNormalMagnitude).exponent)
  expectEqual(-52, Double.ulp.exponent)
  expectEqual(-1, Double(0.5).exponent)
  expectEqual(0, Double(1).exponent)
  expectEqual(1, Double(2).exponent)
  expectEqual(1023, Double.greatestFiniteMagnitude.exponent)
  expectEqual(Int.max, Double.infinity.exponent)
  expectEqual(Int.max, Double.NaN.exponent)
  
#if arch(i386) || arch(x86_64)
  expectEqual(Int.max, (-Float80.infinity).exponent)
  expectEqual(16383, (-Float80.greatestFiniteMagnitude).exponent)
  expectEqual(1, Float80(-2).exponent)
  expectEqual(0, Float80(-1).exponent)
  expectEqual(-1, Float80(-0.5).exponent)
  expectEqual(-63, (-Float80.ulp).exponent)
  expectEqual(-16381, (-2*Float80.leastNormalMagnitude).exponent)
  expectEqual(-16382, Float80.leastNormalMagnitude.exponent)
  expectEqual(-16383, (-0.5*Float80.leastNormalMagnitude).exponent)
  expectEqual(-16445, (-Float80.leastMagnitude).exponent)
  expectEqual(Int.min, Float80(-0).exponent)
  expectEqual(Int.min, Float80(0).exponent)
  expectEqual(-16445, Float80.leastMagnitude.exponent)
  expectEqual(-16383, (Float80.leastNormalMagnitude/2).exponent)
  expectEqual(-16382, Float80.leastNormalMagnitude.exponent)
  expectEqual(-16381, (2*Float80.leastNormalMagnitude).exponent)
  expectEqual(-63, Float80.ulp.exponent)
  expectEqual(-1, Float80(0.5).exponent)
  expectEqual(0, Float80(1).exponent)
  expectEqual(1, Float80(2).exponent)
  expectEqual(16383, Float80.greatestFiniteMagnitude.exponent)
  expectEqual(Int.max, Float80.infinity.exponent)
  expectEqual(Int.max, Float80.NaN.exponent)
#endif
}

runAllTests()

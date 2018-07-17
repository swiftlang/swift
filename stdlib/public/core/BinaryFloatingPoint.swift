/// A radix-2 (binary) floating-point type.
///
/// The `BinaryFloatingPoint` protocol extends the `FloatingPoint` protocol
/// with operations specific to floating-point binary types, as defined by the
/// [IEEE 754 specification][spec]. `BinaryFloatingPoint` is implemented in
/// the standard library by `Float`, `Double`, and `Float80` where available.
///
/// [spec]: http://ieeexplore.ieee.org/servlet/opac?punumber=4610933
public protocol BinaryFloatingPoint: FloatingPoint, ExpressibleByFloatLiteral {
  
  /// A type that represents the encoded significand of a value.
  associatedtype RawSignificand: UnsignedInteger
  
  /// A type that represents the encoded exponent of a value.
  associatedtype RawExponent: UnsignedInteger
  
  /// Creates a new instance from the specified sign and bit patterns.
  ///
  /// The values passed as `exponentBitPattern` and `significandBitPattern` are
  /// interpreted in the binary interchange format defined by the [IEEE 754
  /// specification][spec].
  ///
  /// [spec]: http://ieeexplore.ieee.org/servlet/opac?punumber=4610933
  ///
  /// - Parameters:
  ///   - sign: The sign of the new value.
  ///   - exponentBitPattern: The bit pattern to use for the exponent field of
  ///     the new value.
  ///   - significandBitPattern: The bit pattern to use for the significand
  ///     field of the new value.
  init(sign: FloatingPointSign,
       exponentBitPattern: RawExponent,
       significandBitPattern: RawSignificand)
  
  /// Creates a new instance from the given value, rounded to the closest
  /// possible representation.
  ///
  /// - Parameter value: A floating-point value to be converted.
  init(_ value: Float)
  
  /// Creates a new instance from the given value, rounded to the closest
  /// possible representation.
  ///
  /// - Parameter value: A floating-point value to be converted.
  init(_ value: Double)
  
  #if !os(Windows) && (arch(i386) || arch(x86_64))
  /// Creates a new instance from the given value, rounded to the closest
  /// possible representation.
  ///
  /// - Parameter value: A floating-point value to be converted.
  init(_ value: Float80)
  #endif
  
  /// Creates a new instance from the given value, rounded to the closest
  /// possible representation.
  ///
  /// If two representable values are equally close, the result is the value
  /// with more trailing zeros in its significand bit pattern.
  ///
  /// - Parameter value: A floating-point value to be converted.
  init<Source : BinaryFloatingPoint>(_ value: Source)
  
  /// Creates a new instance from the given value, if it can be represented
  /// exactly.
  ///
  /// If the given floating-point value cannot be represented exactly, the
  /// result is `nil`. A value that is NaN ("not a number") cannot be
  /// represented exactly if its payload cannot be encoded exactly.
  ///
  /// - Parameter value: A floating-point value to be converted.
  init?<Source : BinaryFloatingPoint>(exactly value: Source)
  
  /// The number of bits used to represent the type's exponent.
  ///
  /// A binary floating-point type's `exponentBitCount` imposes a limit on the
  /// range of the exponent for normal, finite values. The *exponent bias* of
  /// a type `F` can be calculated as the following, where `**` is
  /// exponentiation:
  ///
  ///     let bias = 2 ** (F.exponentBitCount - 1) - 1
  ///
  /// The least normal exponent for values of the type `F` is `1 - bias`, and
  /// the largest finite exponent is `bias`. An all-zeros exponent is reserved
  /// for subnormals and zeros, and an all-ones exponent is reserved for
  /// infinity and NaN.
  ///
  /// For example, the `Float` type has an `exponentBitCount` of 8, which gives
  /// an exponent bias of `127` by the calculation above.
  ///
  ///     let bias = 2 ** (Float.exponentBitCount - 1) - 1
  ///     // bias == 127
  ///     print(Float.greatestFiniteMagnitude.exponent)
  ///     // Prints "127"
  ///     print(Float.leastNormalMagnitude.exponent)
  ///     // Prints "-126"
  static var exponentBitCount: Int { get }
  
  /// The available number of fractional significand bits.
  ///
  /// For fixed-width floating-point types, this is the actual number of
  /// fractional significand bits.
  ///
  /// For extensible floating-point types, `significandBitCount` should be the
  /// maximum allowed significand width (without counting any leading integral
  /// bit of the significand). If there is no upper limit, then
  /// `significandBitCount` should be `Int.max`.
  ///
  /// Note that `Float80.significandBitCount` is 63, even though 64 bits are
  /// used to store the significand in the memory representation of a
  /// `Float80` (unlike other floating-point types, `Float80` explicitly
  /// stores the leading integral significand bit, but the
  /// `BinaryFloatingPoint` APIs provide an abstraction so that users don't
  /// need to be aware of this detail).
  static var significandBitCount: Int { get }
  
  /// The raw encoding of the value's exponent field.
  ///
  /// This value is unadjusted by the type's exponent bias.
  var exponentBitPattern: RawExponent { get }
  
  /// The raw encoding of the value's significand field.
  ///
  /// The `significandBitPattern` property does not include the leading
  /// integral bit of the significand, even for types like `Float80` that
  /// store it explicitly.
  var significandBitPattern: RawSignificand { get }
  
  /// The floating-point value with the same sign and exponent as this value,
  /// but with a significand of 1.0.
  ///
  /// A *binade* is a set of binary floating-point values that all have the
  /// same sign and exponent. The `binade` property is a member of the same
  /// binade as this value, but with a unit significand.
  ///
  /// In this example, `x` has a value of `21.5`, which is stored as
  /// `1.34375 * 2**4`, where `**` is exponentiation. Therefore, `x.binade` is
  /// equal to `1.0 * 2**4`, or `16.0`.
  ///
  ///     let x = 21.5
  ///     // x.significand == 1.34375
  ///     // x.exponent == 4
  ///
  ///     let y = x.binade
  ///     // y == 16.0
  ///     // y.significand == 1.0
  ///     // y.exponent == 4
  var binade: Self { get }
  
  /// The number of bits required to represent the value's significand.
  ///
  /// If this value is a finite nonzero number, `significandWidth` is the
  /// number of fractional bits required to represent the value of
  /// `significand`; otherwise, `significandWidth` is -1. The value of
  /// `significandWidth` is always -1 or between zero and
  /// `significandBitCount`. For example:
  ///
  /// - For any representable power of two, `significandWidth` is zero, because
  ///   `significand` is `1.0`.
  /// - If `x` is 10, `x.significand` is `1.01` in binary, so
  ///   `x.significandWidth` is 2.
  /// - If `x` is Float.pi, `x.significand` is `1.10010010000111111011011` in
  ///   binary, and `x.significandWidth` is 23.
  var significandWidth: Int { get }
}

extension BinaryFloatingPoint {
  
  /// The radix, or base of exponentiation, for this floating-point type.
  ///
  /// All binary floating-point types have a radix of 2. The magnitude of a
  /// floating-point value `x` of type `F` can be calculated by using the
  /// following formula, where `**` is exponentiation:
  ///
  ///     let magnitude = x.significand * F.radix ** x.exponent
  public static var radix: Int { return 2 }
  
  /// Creates a new floating-point value using the sign of one value and the
  /// magnitude of another.
  ///
  /// The following example uses this initializer to create a new `Double`
  /// instance with the sign of `a` and the magnitude of `b`:
  ///
  ///     let a = -21.5
  ///     let b = 305.15
  ///     let c = Double(signOf: a, magnitudeOf: b)
  ///     print(c)
  ///     // Prints "-305.15"
  ///
  /// This initializer implements the IEEE 754 `copysign` operation.
  ///
  /// - Parameters:
  ///   - signOf: A value from which to use the sign. The result of the
  ///     initializer has the same sign as `signOf`.
  ///   - magnitudeOf: A value from which to use the magnitude. The result of
  ///     the initializer has the same magnitude as `magnitudeOf`.
  @inlinable // FIXME(sil-serialize-all)
  public init(signOf: Self, magnitudeOf: Self) {
    self.init(sign: signOf.sign,
              exponentBitPattern: magnitudeOf.exponentBitPattern,
              significandBitPattern: magnitudeOf.significandBitPattern)
  }
  
  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  static func _convert<Source : BinaryInteger>(
    from source: Source
    ) -> (value: Self, exact: Bool) {
    guard _fastPath(source != 0) else { return (0, true) }
    
    let magnitude = source.magnitude
    
    let exponent = Int(magnitude._binaryLogarithm())
    let exemplar = Self.greatestFiniteMagnitude
    guard _fastPath(exponent <= exemplar.exponent) else {
      return Source.isSigned && source < 0
        ? (-.infinity, false)
        : (.infinity, false)
    }
    let exponentBitPattern =
      (1 as Self).exponentBitPattern /* (i.e., bias) */
        + Self.RawExponent(exponent)
    
    let maxSignificandWidth = exemplar.significandWidth
    let shift = maxSignificandWidth &- exponent
    let significandBitPattern = shift >= 0
      ? Self.RawSignificand(magnitude) << shift & exemplar.significandBitPattern
      : Self.RawSignificand(
        magnitude >> -shift & Source.Magnitude(exemplar.significandBitPattern))
    
    let value = Self(
      sign: Source.isSigned && source < 0 ? .minus : .plus,
      exponentBitPattern: exponentBitPattern,
      significandBitPattern: significandBitPattern)
    
    if exponent &- magnitude.trailingZeroBitCount <= maxSignificandWidth {
      return (value, true)
    }
    // We promise to round to the closest representation, and if two
    // representable values are equally close, the value with more trailing
    // zeros in its significand bit pattern. Therefore, we must take a look at
    // the bits that we've just truncated.
    let ulp = (1 as Source.Magnitude) << -shift
    let truncatedBits = magnitude & (ulp - 1)
    if truncatedBits < ulp / 2 {
      return (value, false)
    }
    let rounded = Source.isSigned && source < 0 ? value.nextDown : value.nextUp
    guard _fastPath(
      truncatedBits != ulp / 2 ||
        exponentBitPattern.trailingZeroBitCount <
        rounded.exponentBitPattern.trailingZeroBitCount) else {
          return (value, false)
    }
    return (rounded, false)
  }
  
  /// Creates a new value, rounded to the closest possible representation.
  ///
  /// If two representable values are equally close, the result is the value
  /// with more trailing zeros in its significand bit pattern.
  ///
  /// - Parameter value: The integer to convert to a floating-point value.
  @inlinable // FIXME(sil-serialize-all)
  public init<Source : BinaryInteger>(_ value: Source) {
    self = Self._convert(from: value).value
  }
  
  /// Creates a new value, if the given integer can be represented exactly.
  ///
  /// If the given integer cannot be represented exactly, the result is `nil`.
  ///
  /// - Parameter value: The integer to convert to a floating-point value.
  @inlinable // FIXME(sil-serialize-all)
  public init?<Source : BinaryInteger>(exactly value: Source) {
    let (value_, exact) = Self._convert(from: value)
    guard exact else { return nil }
    self = value_
  }
  
  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  static func _convert<Source : BinaryFloatingPoint>(
    from source: Source
    ) -> (value: Self, exact: Bool) {
    guard _fastPath(!source.isZero) else {
      return (source.sign == .minus ? -0.0 : 0, true)
    }
    
    guard _fastPath(source.isFinite) else {
      if source.isInfinite {
        return (source.sign == .minus ? -.infinity : .infinity, true)
      }
      // IEEE 754 requires that any NaN payload be propagated, if possible.
      let payload_ =
        source.significandBitPattern &
          ~(Source.nan.significandBitPattern |
            Source.signalingNaN.significandBitPattern)
      let mask =
        Self.greatestFiniteMagnitude.significandBitPattern &
          ~(Self.nan.significandBitPattern |
            Self.signalingNaN.significandBitPattern)
      let payload = Self.RawSignificand(truncatingIfNeeded: payload_) & mask
      // Although .signalingNaN.exponentBitPattern == .nan.exponentBitPattern,
      // we do not *need* to rely on this relation, and therefore we do not.
      let value = source.isSignalingNaN
        ? Self(
          sign: source.sign,
          exponentBitPattern: Self.signalingNaN.exponentBitPattern,
          significandBitPattern: payload |
            Self.signalingNaN.significandBitPattern)
        : Self(
          sign: source.sign,
          exponentBitPattern: Self.nan.exponentBitPattern,
          significandBitPattern: payload | Self.nan.significandBitPattern)
      // We define exactness by equality after roundtripping; since NaN is never
      // equal to itself, it can never be converted exactly.
      return (value, false)
    }
    
    let exponent = source.exponent
    var exemplar = Self.leastNormalMagnitude
    let exponentBitPattern: Self.RawExponent
    let leadingBitIndex: Int
    let shift: Int
    let significandBitPattern: Self.RawSignificand
    
    if exponent < exemplar.exponent {
      // The floating-point result is either zero or subnormal.
      exemplar = Self.leastNonzeroMagnitude
      let minExponent = exemplar.exponent
      if exponent + 1 < minExponent {
        return (source.sign == .minus ? -0.0 : 0, false)
      }
      if _slowPath(exponent + 1 == minExponent) {
        // Although the most significant bit (MSB) of a subnormal source
        // significand is explicit, Swift BinaryFloatingPoint APIs actually
        // omit any explicit MSB from the count represented in
        // significandWidth. For instance:
        //
        //   Double.leastNonzeroMagnitude.significandWidth == 0
        //
        // Therefore, we do not need to adjust our work here for a subnormal
        // source.
        return source.significandWidth == 0
          ? (source.sign == .minus ? -0.0 : 0, false)
          : (source.sign == .minus ? -exemplar : exemplar, false)
      }
      
      exponentBitPattern = 0 as Self.RawExponent
      leadingBitIndex = Int(Self.Exponent(exponent) - minExponent)
      shift =
        leadingBitIndex &-
        (source.significandWidth &+
          source.significandBitPattern.trailingZeroBitCount)
      let leadingBit = source.isNormal
        ? (1 as Self.RawSignificand) << leadingBitIndex
        : 0
      significandBitPattern = leadingBit | (shift >= 0
        ? Self.RawSignificand(source.significandBitPattern) << shift
        : Self.RawSignificand(source.significandBitPattern >> -shift))
    } else {
      // The floating-point result is either normal or infinite.
      exemplar = Self.greatestFiniteMagnitude
      if exponent > exemplar.exponent {
        return (source.sign == .minus ? -.infinity : .infinity, false)
      }
      
      exponentBitPattern = exponent < 0
        ? (1 as Self).exponentBitPattern - Self.RawExponent(-exponent)
        : (1 as Self).exponentBitPattern + Self.RawExponent(exponent)
      leadingBitIndex = exemplar.significandWidth
      shift =
        leadingBitIndex &-
        (source.significandWidth &+
          source.significandBitPattern.trailingZeroBitCount)
      let sourceLeadingBit = source.isSubnormal
        ? (1 as Source.RawSignificand) <<
          (source.significandWidth &+
            source.significandBitPattern.trailingZeroBitCount)
        : 0
      significandBitPattern = shift >= 0
        ? Self.RawSignificand(
          sourceLeadingBit ^ source.significandBitPattern) << shift
        : Self.RawSignificand(
          (sourceLeadingBit ^ source.significandBitPattern) >> -shift)
    }
    
    let value = Self(
      sign: source.sign,
      exponentBitPattern: exponentBitPattern,
      significandBitPattern: significandBitPattern)
    
    if source.significandWidth <= leadingBitIndex {
      return (value, true)
    }
    // We promise to round to the closest representation, and if two
    // representable values are equally close, the value with more trailing
    // zeros in its significand bit pattern. Therefore, we must take a look at
    // the bits that we've just truncated.
    let ulp = (1 as Source.RawSignificand) << -shift
    let truncatedBits = source.significandBitPattern & (ulp - 1)
    if truncatedBits < ulp / 2 {
      return (value, false)
    }
    let rounded = source.sign == .minus ? value.nextDown : value.nextUp
    guard _fastPath(
      truncatedBits != ulp / 2 ||
        exponentBitPattern.trailingZeroBitCount <
        rounded.exponentBitPattern.trailingZeroBitCount) else {
          return (value, false)
    }
    return (rounded, false)
  }
  
  /// Creates a new instance from the given value, rounded to the closest
  /// possible representation.
  ///
  /// If two representable values are equally close, the result is the value
  /// with more trailing zeros in its significand bit pattern.
  ///
  /// - Parameter value: A floating-point value to be converted.
  @inlinable // FIXME(sil-serialize-all)
  public init<Source : BinaryFloatingPoint>(_ value: Source) {
    self = Self._convert(from: value).value
  }
  
  /// Creates a new instance from the given value, if it can be represented
  /// exactly.
  ///
  /// If the given floating-point value cannot be represented exactly, the
  /// result is `nil`.
  ///
  /// - Parameter value: A floating-point value to be converted.
  @inlinable // FIXME(sil-serialize-all)
  public init?<Source : BinaryFloatingPoint>(exactly value: Source) {
    let (value_, exact) = Self._convert(from: value)
    guard exact else { return nil }
    self = value_
  }
  
  /// Returns a Boolean value indicating whether this instance should precede
  /// or tie positions with the given value in an ascending sort.
  ///
  /// This relation is a refinement of the less-than-or-equal-to operator
  /// (`<=`) that provides a total order on all values of the type, including
  /// signed zeros and NaNs.
  ///
  /// The following example uses `isTotallyOrdered(belowOrEqualTo:)` to sort an
  /// array of floating-point values, including some that are NaN:
  ///
  ///     var numbers = [2.5, 21.25, 3.0, .nan, -9.5]
  ///     numbers.sort { !$1.isTotallyOrdered(belowOrEqualTo: $0) }
  ///     // numbers == [-9.5, 2.5, 3.0, 21.25, NaN]
  ///
  /// The `isTotallyOrdered(belowOrEqualTo:)` method implements the total order
  /// relation as defined by the [IEEE 754 specification][spec].
  ///
  /// [spec]: http://ieeexplore.ieee.org/servlet/opac?punumber=4610933
  ///
  /// - Parameter other: A floating-point value to compare to this value.
  /// - Returns: `true` if this value is ordered below or the same as `other`
  ///   in a total ordering of the floating-point type; otherwise, `false`.
  @inlinable // FIXME(sil-serialize-all)
  public func isTotallyOrdered(belowOrEqualTo other: Self) -> Bool {
    // Quick return when possible.
    if self < other { return true }
    if other > self { return false }
    // Self and other are either equal or unordered.
    // Every negative-signed value (even NaN) is less than every positive-
    // signed value, so if the signs do not match, we simply return the
    // sign bit of self.
    if sign != other.sign { return sign == .minus }
    // Sign bits match; look at exponents.
    if exponentBitPattern > other.exponentBitPattern { return sign == .minus }
    if exponentBitPattern < other.exponentBitPattern { return sign == .plus }
    // Signs and exponents match, look at significands.
    if significandBitPattern > other.significandBitPattern {
      return sign == .minus
    }
    if significandBitPattern < other.significandBitPattern {
      return sign == .plus
    }
    //  Sign, exponent, and significand all match.
    return true
  }
}

extension BinaryFloatingPoint where Self.RawSignificand : FixedWidthInteger {
  
  /// Returns a random value within the specified range, using the given
  /// generator as a source for randomness.
  ///
  /// Use this method to generate a floating-point value within a specific
  /// range when you are using a custom random number generator. This example
  /// creates three new values in the range `10.0..<20.0`.
  ///
  ///     for _ in 1...3 {
  ///         print(Double.random(in: 10.0..<20.0, using: &myGenerator))
  ///     }
  ///     // Prints "18.1900709259179"
  ///     // Prints "14.2286325689993"
  ///     // Prints "13.1485686260762"
  ///
  /// The `random(in:using:)` static method chooses a random value from a
  /// continuous uniform distribution in `range`, and then converts that value
  /// to the nearest representable value in this type. Depending on the size and
  /// span of `range`, some concrete values may be represented more frequently
  /// than others.
  ///
  /// - Parameters:
  ///   - range: The range in which to create a random value.
  ///     `range` must be non-empty and finite.
  ///   - generator: The random number generator to use when creating the
  ///     new random value.
  /// - Returns: A random value within the bounds of `range`.
  @inlinable
  public static func random<T: RandomNumberGenerator>(
    in range: Range<Self>,
    using generator: inout T
  ) -> Self {
    _precondition(
      !range.isEmpty,
      "Can't get random value with an empty range"
    )
    let delta = range.upperBound - range.lowerBound
    //  TODO: this still isn't quite right, because the computation of delta
    //  can overflow (e.g. if .upperBound = .maximumFiniteMagnitude and
    //  .lowerBound = -.upperBound); this should be re-written with an
    //  algorithm that handles that case correctly, but this precondition
    //  is an acceptable short-term fix.
    _precondition(
      delta.isFinite,
      "There is no uniform distribution on an infinite range"
    )
    let rand: Self.RawSignificand
    if Self.RawSignificand.bitWidth == Self.significandBitCount + 1 {
      rand = generator.next()
    } else {
      let significandCount = Self.significandBitCount + 1
      let maxSignificand: Self.RawSignificand = 1 << significandCount
      // Rather than use .next(upperBound:), which has to work with arbitrary
      // upper bounds, and therefore does extra work to avoid bias, we can take
      // a shortcut because we know that maxSignificand is a power of two.
      rand = generator.next() & (maxSignificand - 1)
    }
    let unitRandom = Self.init(rand) * (Self.ulpOfOne / 2)
    let randFloat = delta * unitRandom + range.lowerBound
    if randFloat == range.upperBound {
      return Self.random(in: range, using: &generator)
    }
    return randFloat
  }
  
  /// Returns a random value within the specified range.
  ///
  /// Use this method to generate a floating-point value within a specific
  /// range. This example creates three new values in the range
  /// `10.0..<20.0`.
  ///
  ///     for _ in 1...3 {
  ///         print(Double.random(in: 10.0..<20.0))
  ///     }
  ///     // Prints "18.1900709259179"
  ///     // Prints "14.2286325689993"
  ///     // Prints "13.1485686260762"
  ///
  /// The `random()` static method chooses a random value from a continuous
  /// uniform distribution in `range`, and then converts that value to the
  /// nearest representable value in this type. Depending on the size and span
  /// of `range`, some concrete values may be represented more frequently than
  /// others.
  ///
  /// This method uses the default random generator, `Random.default`. The call
  /// to `Double.random(in: 10.0..<20.0)` above is equivalent to calling
  /// `Double.random(in: 10.0..<20.0, using: &Random.default)`.
  ///
  /// - Parameter range: The range in which to create a random value.
  ///   `range` must be non-empty and finite.
  /// - Returns: A random value within the bounds of `range`.
  @inlinable
  public static func random(in range: Range<Self>) -> Self {
    return Self.random(in: range, using: &Random.default)
  }
  
  /// Returns a random value within the specified range, using the given
  /// generator as a source for randomness.
  ///
  /// Use this method to generate a floating-point value within a specific
  /// range when you are using a custom random number generator. This example
  /// creates three new values in the range `10.0...20.0`.
  ///
  ///     for _ in 1...3 {
  ///         print(Double.random(in: 10.0...20.0, using: &myGenerator))
  ///     }
  ///     // Prints "18.1900709259179"
  ///     // Prints "14.2286325689993"
  ///     // Prints "13.1485686260762"
  ///
  /// The `random(in:using:)` static method chooses a random value from a
  /// continuous uniform distribution in `range`, and then converts that value
  /// to the nearest representable value in this type. Depending on the size and
  /// span of `range`, some concrete values may be represented more frequently
  /// than others.
  ///
  /// - Parameters:
  ///   - range: The range in which to create a random value.
  ///     `range` must be finite.
  ///   - generator: The random number generator to use when creating the
  ///     new random value.
  /// - Returns: A random value within the bounds of `range`.
  @inlinable
  public static func random<T: RandomNumberGenerator>(
    in range: ClosedRange<Self>,
    using generator: inout T
  ) -> Self {
    let delta = range.upperBound - range.lowerBound
    //  TODO: this still isn't quite right, because the computation of delta
    //  can overflow (e.g. if .upperBound = .maximumFiniteMagnitude and
    //  .lowerBound = -.upperBound); this should be re-written with an
    //  algorithm that handles that case correctly, but this precondition
    //  is an acceptable short-term fix.
    _precondition(
      delta.isFinite,
      "There is no uniform distribution on an infinite range"
    )
    let rand: Self.RawSignificand
    if Self.RawSignificand.bitWidth == Self.significandBitCount + 1 {
      rand = generator.next()
      let tmp: UInt8 = generator.next() & 1
      if rand == Self.RawSignificand.max && tmp == 1 {
        return range.upperBound
      }
    } else {
      let significandCount = Self.significandBitCount + 1
      let maxSignificand: Self.RawSignificand = 1 << significandCount
      rand = generator.next(upperBound: maxSignificand + 1)
      if rand == maxSignificand {
        return range.upperBound
      }
    }
    let unitRandom = Self.init(rand) * (Self.ulpOfOne / 2)
    let randFloat = delta * unitRandom + range.lowerBound
    return randFloat
  }
  
  /// Returns a random value within the specified range.
  ///
  /// Use this method to generate a floating-point value within a specific
  /// range. This example creates three new values in the range
  /// `10.0...20.0`.
  ///
  ///     for _ in 1...3 {
  ///         print(Double.random(in: 10.0...20.0))
  ///     }
  ///     // Prints "18.1900709259179"
  ///     // Prints "14.2286325689993"
  ///     // Prints "13.1485686260762"
  ///
  /// The `random()` static method chooses a random value from a continuous
  /// uniform distribution in `range`, and then converts that value to the
  /// nearest representable value in this type. Depending on the size and span
  /// of `range`, some concrete values may be represented more frequently than
  /// others.
  ///
  /// This method uses the default random generator, `Random.default`. The call
  /// to `Double.random(in: 10.0...20.0)` above is equivalent to calling
  /// `Double.random(in: 10.0...20.0, using: &Random.default)`.
  ///
  /// - Parameter range: The range in which to create a random value.
  ///   `range` must be finite.
  /// - Returns: A random value within the bounds of `range`.
  @inlinable
  public static func random(in range: ClosedRange<Self>) -> Self {
    return Self.random(in: range, using: &Random.default)
  }
}

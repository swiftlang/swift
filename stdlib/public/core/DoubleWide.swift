//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A fixed-width integer that has twice the bit width of its base type.
///
/// You can use the `DoubleWidth` type to continue calculations with the result
/// of a full width arithmetic operation. Normally, when you perform a full
/// width operation, the result is a tuple of the high and low parts of the
/// result.
///
///     let a = 2241543570477705381
///     let b = 186319822866995413
///     let c = a.multipliedFullWidth(by: b)
///     // c == (high: 22640526660490081, low: 7959093232766896457)
///
/// The tuple `c` can't be used in any further comparisons or calculations. To
/// use this value, create a `DoubleWidth` instance from the result. You can
/// use the `DoubleWidth` instance in the same way that you would use any other
/// integer type.
///
///     let d = DoubleWidth(a.multipliedFullWidth(by: b))
///     // d == 417644001000058515200174966092417353
///
///     // Check the calculation:
///     print(d / DoubleWidth(a) == b)
///     // Prints "true"
///
///     if d > Int.max {
///         print("Too big to be an 'Int'!")
///     } else {
///         print("Small enough to fit in an 'Int'")
///     }
///     // Prints "Too big to be an 'Int'!"
///
/// The `DoubleWidth` type is not intended as a replacement for a variable-width
/// integer type. Nesting `DoubleWidth` instances, in particular, may result in
/// undesirable performance.
internal struct _DoubleWidth<Base: FixedWidthInteger> {
  internal typealias High = Base
  internal typealias Low = Base.Magnitude

  /// The high part of the value.
  internal var high: High

  /// The low part of the value.
  internal var low: Low

  /// Creates a new instance from the given tuple of high and low parts.
  ///
  /// - Parameter value: The tuple to use as the source of the new instance's
  ///   high and low parts.
  internal init(_ value: (high: High, low: Low)) {
    self.high = value.high
    self.low = value.low
  }

  // We expect users to invoke the  initializer above as demonstrated in
  // the documentation (that is, by passing in the result of a full width
  // operation).
  //
  // Internally, we'll need to create new instances by supplying high and low
  // parts directly; ((double parentheses)) greatly impair readability,
  // especially when nested:
  //
  //   DoubleWidth<DoubleWidth>((DoubleWidth((0, 0)), DoubleWidth((0, 0))))
  //
  // For that reason, we'll include an internal initializer that takes two
  // separate arguments.
  internal init(_ high: High, _ low: Low) {
    self.init((high, low))
  }

  internal init() {
    self.init(0, 0)
  }
}

extension _DoubleWidth: CustomStringConvertible {
  internal var description: String {
    return String(self, radix: 10)
  }
}

extension _DoubleWidth: CustomDebugStringConvertible {
  internal var debugDescription: String {
    return "(\(high), \(low))"
  }
}

extension _DoubleWidth: Equatable {
  internal static func == (_ lhs: _DoubleWidth, _ rhs: _DoubleWidth) -> Bool {
    return lhs.low == rhs.low && lhs.high == rhs.high
  }
}

extension _DoubleWidth: Comparable {
  internal static func < (_ lhs: _DoubleWidth, _ rhs: _DoubleWidth) -> Bool {
    if lhs.high < rhs.high {
      return true
    } else if lhs.high > rhs.high {
      return false
    } else {
      return lhs.low < rhs.low
    }
  }
}

extension _DoubleWidth: Hashable {
  internal func hash(into hasher: inout Hasher) {
    hasher.combine(low)
    hasher.combine(high)
  }
}

extension _DoubleWidth: AdditiveArithmetic {
  internal static func - (
    _ lhs: _DoubleWidth<Base>, _ rhs: _DoubleWidth<Base>
  ) -> _DoubleWidth<Base> {
    var lhs = lhs
    lhs -= rhs
    return lhs
  }

  internal static func -= (_ lhs: inout _DoubleWidth<Base>, _ rhs: _DoubleWidth<Base>) {
    let (result, overflow) = lhs.subtractingReportingOverflow(rhs)
    precondition(!overflow, "Overflow in -=")
    lhs = result
  }

  internal static func + (
    _ lhs: _DoubleWidth<Base>, _ rhs: _DoubleWidth<Base>
  ) -> _DoubleWidth<Base> {
    var lhs = lhs
    lhs += rhs
    return lhs
  }

  internal static func += (_ lhs: inout _DoubleWidth<Base>, _ rhs: _DoubleWidth<Base>) {
    let (result, overflow) = lhs.addingReportingOverflow(rhs)
    precondition(!overflow, "Overflow in +=")
    lhs = result
  }
}

extension _DoubleWidth : Numeric {
  internal typealias Magnitude = _DoubleWidth<Low>

  internal var magnitude: Magnitude {
    let result = Magnitude(Low(truncatingIfNeeded: high), low)
    if Base.isSigned && high < (0 as High) {
      return ~result &+ 1
    } else {
      return result
    }
  }

  internal init(_ magnitude: Magnitude) {
    self.init(High(magnitude.high), magnitude.low)
  }

  internal init<T : BinaryInteger>(_ source: T) {
    guard let result = _DoubleWidth<Base>(exactly: source) else {
      preconditionFailure("Value is outside the representable range")
    }
    self = result
  }

  internal init?<T : BinaryInteger>(exactly source: T) {
    // Can't represent a negative 'source' if Base is unsigned.
    guard _DoubleWidth.isSigned || source >= 0 else {
      return nil
    }

    // Is 'source' entirely representable in Low?
    if let low = Low(exactly: source.magnitude) {
      self.init(source < (0 as T) ? (~0, ~low &+ 1) : (0, low))
    } else {
      // At this point we know source.bitWidth > Base.bitWidth, or else we
      // would've taken the first branch.
      let lowInT = source & T(~0 as Low)
      let highInT = source >> Low.bitWidth

      let low = Low(lowInT)
      guard let high = High(exactly: highInT) else {
        return nil
      }
      self.init(high, low)
    }
  }

  internal static func * (
    _ lhs: _DoubleWidth<Base>, _ rhs: _DoubleWidth<Base>
  ) -> _DoubleWidth<Base> {
    var lhs = lhs
    lhs *= rhs
    return lhs
  }

  internal static func *= (_ lhs: inout _DoubleWidth<Base>, _ rhs: _DoubleWidth<Base>) {
    let (result, overflow) = lhs.multipliedReportingOverflow(by: rhs)
    precondition(!overflow, "Overflow in *=")
    lhs = result
  }
}


extension _DoubleWidth {
  internal struct Words {
    internal var high: High.Words
    internal var low: Low.Words

    internal init(_ value: _DoubleWidth<Base>) {
      // Multiples of word size only.
      guard Base.bitWidth == Base.Magnitude.bitWidth &&
            (UInt.bitWidth % Base.bitWidth == 0 ||
             Base.bitWidth % UInt.bitWidth == 0) else {
        fatalError("Access to words is not supported on this type")
      }
      self.init(high: value.high.words, low: value.low.words)
      assert(!low.isEmpty)
    }

    internal init(high: High.Words, low: Low.Words) {
      self.high = high
      self.low = low
    }
  }
}

extension _DoubleWidth.Words: RandomAccessCollection {
  internal typealias Index = Int

  internal var startIndex: Index {
    return 0
  }

  internal var endIndex: Index {
    return count
  }

  internal var count: Int {
    if Base.bitWidth < UInt.bitWidth {
      return 1
    }
    return low.count + high.count
  }

  internal subscript(_ i: Index) -> UInt {
    if Base.bitWidth < UInt.bitWidth {
      precondition(i == 0, "Invalid index")
      assert(2 * Base.bitWidth <= UInt.bitWidth)
      return low.first! | (high.first! &<< Base.bitWidth._lowWord)
    }
    if i < low.count {
      return low[i + low.startIndex]
    }

    return high[i - low.count + high.startIndex]
  }
}

extension _DoubleWidth: FixedWidthInteger {
  internal var words: Words {
    return Words(self)
  }

  internal static var isSigned: Bool {
    return Base.isSigned
  }

  internal static var max: _DoubleWidth {
    return self.init(High.max, Low.max)
  }

  internal static var min: _DoubleWidth {
    return self.init(High.min, Low.min)
  }

  internal static var bitWidth: Int {
    return High.bitWidth + Low.bitWidth
  }

  internal func addingReportingOverflow(
    _ rhs: _DoubleWidth<Base>
  ) -> (partialValue: _DoubleWidth<Base>, overflow: Bool) {
    let (low, lowOverflow) =
    low.addingReportingOverflow(rhs.low)
    let (high, highOverflow) =
    high.addingReportingOverflow(rhs.high)
    let result = (high + (lowOverflow ? 1 : 0), low)
    let overflow = highOverflow || high == Base.max && lowOverflow
    return (partialValue: _DoubleWidth(result), overflow: overflow)
  }

  internal func subtractingReportingOverflow(
    _ rhs: _DoubleWidth<Base>
  ) -> (partialValue: _DoubleWidth<Base>, overflow: Bool) {
    let (low, lowOverflow) =
    low.subtractingReportingOverflow(rhs.low)
    let (high, highOverflow) =
    high.subtractingReportingOverflow(rhs.high)
    let result = (high - (lowOverflow ? 1 : 0), low)
    let overflow = highOverflow || high == Base.min && lowOverflow
    return (partialValue: _DoubleWidth(result), overflow: overflow)
  }

  internal func multipliedReportingOverflow(
    by rhs: _DoubleWidth
  ) -> (partialValue: _DoubleWidth, overflow: Bool) {
    let (carry, product) = multipliedFullWidth(by: rhs)
    let result = _DoubleWidth(truncatingIfNeeded: product)

    let isNegative = _DoubleWidth.isSigned &&
                     (self < (0 as _DoubleWidth)) != (rhs < (0 as _DoubleWidth))
    let didCarry = isNegative ?
                   carry != ~(0 as _DoubleWidth) :
                   carry != (0 as _DoubleWidth)
    let hadPositiveOverflow = _DoubleWidth.isSigned &&
                              !isNegative && product.leadingZeroBitCount == 0

    return (result, didCarry || hadPositiveOverflow)
  }

  internal func quotientAndRemainder(
    dividingBy other: _DoubleWidth
  ) -> (quotient: _DoubleWidth, remainder: _DoubleWidth) {
    let (quotient, remainder) =
      Magnitude._divide(self.magnitude, by: other.magnitude)
    guard _DoubleWidth.isSigned else {
      return (_DoubleWidth(quotient), _DoubleWidth(remainder))
    }
    let isNegative = (self.high < (0 as High)) != (other.high < (0 as High))
    let quotient_ = isNegative ?
                    quotient == _DoubleWidth.min.magnitude ? _DoubleWidth.min :
                    0 - _DoubleWidth(quotient) : _DoubleWidth(quotient)
    let remainder_ = self.high < (0 as High) ?
                     0 - _DoubleWidth(remainder) :
                     _DoubleWidth(remainder)
    return (quotient_, remainder_)
  }

  internal func dividedReportingOverflow(
    by other: _DoubleWidth
  ) -> (partialValue: _DoubleWidth, overflow: Bool) {
    if other == (0 as _DoubleWidth) {
      return (self, true)
    }
    if _DoubleWidth.isSigned && other == -1 && self == .min {
      return (self, true)
    }
    return (quotientAndRemainder(dividingBy: other).quotient, false)
  }

  internal func remainderReportingOverflow(
    dividingBy other: _DoubleWidth
  ) -> (partialValue: _DoubleWidth, overflow: Bool) {
    if other == (0 as _DoubleWidth) {
      return (self, true)
    }
    if _DoubleWidth.isSigned && other == -1 && self == .min {
      return (0, true)
    }
    return (quotientAndRemainder(dividingBy: other).remainder, false)
  }

  internal func multipliedFullWidth(
    by other: _DoubleWidth
  ) -> (high: _DoubleWidth, low: _DoubleWidth.Magnitude) {
    let isNegative = _DoubleWidth.isSigned &&
                   (self < (0 as _DoubleWidth)) != (other < (0 as _DoubleWidth))

    func mul(_ x: Low, _ y: Low) -> (partial: Low, carry: Low) {
      let (high, low) = x.multipliedFullWidth(by: y)
      return (low, high)
    }

    func sum(_ x: Low, _ y: Low, _ z: Low) -> (partial: Low, carry: Low) {
      let (sum1, overflow1) = x.addingReportingOverflow(y)
      let (sum2, overflow2) = sum1.addingReportingOverflow(z)
      let carry: Low = (overflow1 ? 1 : 0) + (overflow2 ? 1 : 0)
      return (sum2, carry)
    }

    let lhs = self.magnitude
    let rhs = other.magnitude

    let a = mul(rhs.low, lhs.low)
    let b = mul(rhs.low, lhs.high)
    let c = mul(rhs.high, lhs.low)
    let d = mul(rhs.high, lhs.high)

    let mid1 = sum(a.carry, b.partial, c.partial)
    let mid2 = sum(b.carry, c.carry, d.partial)

    let low = _DoubleWidth<Low>(mid1.partial, a.partial)
    let high = _DoubleWidth(High(mid2.carry + d.carry),
                            mid1.carry + mid2.partial)

    if isNegative {
      let (lowComplement, overflow) = (~low).addingReportingOverflow(1)
      return (~high + (overflow ? 1 : 0 as _DoubleWidth), lowComplement)
    } else {
      return (high, low)
    }
  }

  internal func dividingFullWidth(
    _ dividend: (high: _DoubleWidth, low: _DoubleWidth.Magnitude)
  ) -> (quotient: _DoubleWidth, remainder: _DoubleWidth) {
    let other = _DoubleWidth<_DoubleWidth>(dividend)
    let (quotient, remainder) =
      Magnitude._divide(other.magnitude, by: self.magnitude)
    guard _DoubleWidth.isSigned else {
      return (_DoubleWidth(quotient), _DoubleWidth(remainder))
    }
    let isNegative =
      (self.high < (0 as High)) != (other.high.high < (0 as High))
    let quotient_ = isNegative ?
                      quotient == _DoubleWidth.min.magnitude ?
                        _DoubleWidth.min :
                        0 - _DoubleWidth(quotient) :
                      _DoubleWidth(quotient)
    let remainder_ = other.high.high < (0 as High) ?
                      0 - _DoubleWidth(remainder) :
                      _DoubleWidth(remainder)
    return (quotient_, remainder_)
  }

  internal static prefix func ~(x: _DoubleWidth) -> _DoubleWidth {
    _DoubleWidth(~x.high, ~x.low)
  }

  internal static func &= (_ lhs: inout _DoubleWidth, _ rhs: _DoubleWidth) {
    lhs.low &= rhs.low
    lhs.high &= rhs.high
  }

  internal static func |= (_ lhs: inout _DoubleWidth, _ rhs: _DoubleWidth) {
    lhs.low |= rhs.low
    lhs.high |= rhs.high
  }

  internal static func ^= (_ lhs: inout _DoubleWidth, _ rhs: _DoubleWidth) {
    lhs.low ^= rhs.low
    lhs.high ^= rhs.high
  }

  internal static func <<= (_ lhs: inout _DoubleWidth, _ rhs: _DoubleWidth) {
    if _DoubleWidth.isSigned && rhs < (0 as _DoubleWidth) {
      lhs >>= 0 - rhs
      return
    }

    // Shift is larger than this type's bit width.
    if rhs.high != (0 as High) ||
        rhs.low >= _DoubleWidth.bitWidth
    {
      lhs = 0
      return
    }

    lhs &<<= rhs
  }

  internal static func >>= (_ lhs: inout _DoubleWidth, _ rhs: _DoubleWidth) {
    if _DoubleWidth.isSigned && rhs < (0 as _DoubleWidth) {
      lhs <<= 0 - rhs
      return
    }

    // Shift is larger than this type's bit width.
    if rhs.high != (0 as High) || rhs.low >= _DoubleWidth.bitWidth {
      lhs = lhs < (0 as _DoubleWidth) ? ~0 : 0
      return
    }

    lhs &>>= rhs
  }

  /// Returns this value "masked" by its bit width.
  ///
  /// "Masking" notionally involves repeatedly incrementing or decrementing this
  /// value by `self.bitWidth` until the result is contained in the range
  /// `0..<self.bitWidth`.
  internal func _masked() -> _DoubleWidth {
    // FIXME(integers): test types with bit widths that aren't powers of 2
    if _DoubleWidth.bitWidth.nonzeroBitCount == 1 {
      return self & _DoubleWidth(_DoubleWidth.bitWidth &- 1)
    }
    if _DoubleWidth.isSigned && self.high < (0 as High) {
      return self % _DoubleWidth(_DoubleWidth.bitWidth) + self
    }
    return self % _DoubleWidth(_DoubleWidth.bitWidth)
  }

  internal static func &<<= (_ lhs: inout _DoubleWidth, _ rhs: _DoubleWidth) {
    let rhs = rhs._masked()

    guard rhs.low < Base.bitWidth else {
      lhs.high = High(
        truncatingIfNeeded: lhs.low &<<
        (rhs.low &- Low(Base.bitWidth)))
      lhs.low = 0
      return
    }

    guard rhs.low != (0 as Low) else { return }
    lhs.high &<<= High(rhs.low)
    lhs.high |= High(truncatingIfNeeded: lhs.low &>>
                (Low(Base.bitWidth) &- rhs.low))
    lhs.low &<<= rhs.low
  }

  internal static func &>>= (_ lhs: inout _DoubleWidth, _ rhs: _DoubleWidth) {
    let rhs = rhs._masked()

    guard rhs.low < Base.bitWidth else {
      lhs.low = Low(truncatingIfNeeded: lhs.high &>>
                High(rhs.low &- Low(Base.bitWidth)))
      lhs.high = lhs.high < (0 as High) ? ~0 : 0
      return
    }

    guard rhs.low != (0 as Low) else {
      return
    }
    lhs.low &>>= rhs.low
    lhs.low |= Low(truncatingIfNeeded: lhs.high &<<
               High(Low(Base.bitWidth) &- rhs.low))
    lhs.high &>>= High(rhs.low)
  }

  internal static func / (
    _ lhs: _DoubleWidth<Base>, _ rhs: _DoubleWidth<Base>
  ) -> _DoubleWidth<Base> {
    var lhs = lhs
    lhs /= rhs
    return lhs
  }

  internal static func /= (_ lhs: inout _DoubleWidth<Base>, _ rhs: _DoubleWidth<Base>) {
    let (result, overflow) = lhs.dividedReportingOverflow(by: rhs)
    precondition(!overflow, "Overflow in /=")
    lhs = result
  }

  internal static func % (
    _ lhs: _DoubleWidth<Base>, _ rhs: _DoubleWidth<Base>
  ) -> _DoubleWidth<Base> {
    var lhs = lhs
    lhs %= rhs
    return lhs
  }

  internal static func %= (_ lhs: inout _DoubleWidth<Base>, _ rhs: _DoubleWidth<Base>) {
    let (result, overflow) = lhs.remainderReportingOverflow(dividingBy: rhs)
    precondition(!overflow, "Overflow in %=")
    lhs = result
  }

  internal init(_truncatingBits bits: UInt) {
    low = Low(_truncatingBits: bits)
    high = High(_truncatingBits: bits >> UInt(Low.bitWidth))
  }

  internal init(integerLiteral x: Int64) {
    self.init(x)
  }

  internal var leadingZeroBitCount: Int {
    return high == (0 as High) ?
            High.bitWidth + low.leadingZeroBitCount :
            high.leadingZeroBitCount
  }

  internal var trailingZeroBitCount: Int {
    return low == (0 as Low) ?
                    Low.bitWidth + high.trailingZeroBitCount :
                    low.trailingZeroBitCount
  }

  internal var nonzeroBitCount: Int {
    return high.nonzeroBitCount + low.nonzeroBitCount
  }

  internal var byteSwapped: _DoubleWidth {
    return _DoubleWidth(High(truncatingIfNeeded: low.byteSwapped),
                        Low(truncatingIfNeeded: high.byteSwapped))
  }
}

extension _DoubleWidth : UnsignedInteger where Base : UnsignedInteger {
  /// Returns the quotient and remainder after dividing a triple-width magnitude
  /// `lhs` by a double-width magnitude `rhs`.
  ///
  /// This operation is conceptually that described by Burnikel and Ziegler
  /// (1998).
  internal static func _divide(
    _ lhs: (high: Low, mid: Low, low: Low), by rhs: Magnitude
  ) -> (quotient: Low, remainder: Magnitude) {
    // The following invariants are guaranteed to hold by dividingFullWidth or
    // quotientAndRemainder before this method is invoked:
    assert(lhs.high != (0 as Low))
    assert(rhs.leadingZeroBitCount == 0)
    assert(Magnitude(lhs.high, lhs.mid) < rhs)

    // Estimate the quotient.
    var quotient = lhs.high == rhs.high ?
                     Low.max :
                     rhs.high.dividingFullWidth((lhs.high, lhs.mid)).quotient
    // Compute quotient * rhs.
    // TODO: This could be performed more efficiently.
    var product = _DoubleWidth<Magnitude>(0,
      Magnitude(quotient.multipliedFullWidth(by: rhs.low)))
    let (x, y) = quotient.multipliedFullWidth(by: rhs.high)
    product += _DoubleWidth<Magnitude>(Magnitude(0, x), Magnitude(y, 0))
    // Compute the remainder after decrementing quotient as necessary.
    var remainder = _DoubleWidth<Magnitude>(Magnitude(0, lhs.high),
                                            Magnitude(lhs.mid, lhs.low))
    while remainder < product {
      quotient = quotient &- 1
      remainder += _DoubleWidth<Magnitude>(0, rhs)
    }
    remainder -= product

    return (quotient, remainder.low)
  }

  /// Returns the quotient and remainder after dividing a quadruple-width
  /// magnitude `lhs` by a double-width magnitude `rhs`.
  @usableFromInline
  internal static func _divide(
    _ lhs: _DoubleWidth<Magnitude>, by rhs: Magnitude
  ) -> (quotient: Magnitude, remainder: Magnitude) {
    guard _fastPath(rhs > (0 as Magnitude)) else {
      fatalError("Division by zero")
    }
    guard _fastPath(rhs >= lhs.high) else {
      fatalError("Division results in an overflow")
    }

    if lhs.high == (0 as Magnitude) {
      return lhs.low.quotientAndRemainder(dividingBy: rhs)
    }

    if rhs.high == (0 as Low) {
      let a = lhs.high.high % rhs.low
      let b = a == (0 as Low) ?
                     lhs.high.low % rhs.low :
                     rhs.low.dividingFullWidth((a, lhs.high.low)).remainder
      let (x, c) = b == (0 as Low) ?
                        lhs.low.high.quotientAndRemainder(dividingBy: rhs.low) :
                        rhs.low.dividingFullWidth((b, lhs.low.high))
      let (y, d) = c == (0 as Low) ?
                        lhs.low.low.quotientAndRemainder(dividingBy: rhs.low) :
                        rhs.low.dividingFullWidth((c, lhs.low.low))
      return (Magnitude(x, y), Magnitude(0, d))
    }

    // Left shift both rhs and lhs, then divide and right shift the remainder.
    let shift = rhs.leadingZeroBitCount
    let rhs = rhs &<< shift
    let lhs = lhs &<< shift
    if lhs.high.high == (0 as Low) &&
       Magnitude(lhs.high.low, lhs.low.high) < rhs {
      let (quotient, remainder) =
        Magnitude._divide((lhs.high.low, lhs.low.high, lhs.low.low), by: rhs)
      return (Magnitude(0, quotient), remainder &>> shift)
    }
    let (x, a) =
      Magnitude._divide((lhs.high.high, lhs.high.low, lhs.low.high), by: rhs)
    let (y, b) = Magnitude._divide((a.high, a.low, lhs.low.low), by: rhs)
    return (Magnitude(x, y), b &>> shift)
  }

  /// Returns the quotient and remainder after dividing a double-width
  /// magnitude `lhs` by a double-width magnitude `rhs`.
  internal static func _divide(
    _ lhs: Magnitude, by rhs: Magnitude
  ) -> (quotient: Magnitude, remainder: Magnitude) {
    guard _fastPath(rhs > (0 as Magnitude)) else {
      fatalError("Division by zero")
    }
    guard rhs < lhs else {
      if _fastPath(rhs > lhs) { return (0, lhs) }
      return (1, 0)
    }

    if lhs.high == (0 as Low) {
      let (quotient, remainder) =
        lhs.low.quotientAndRemainder(dividingBy: rhs.low)
      return (Magnitude(quotient), Magnitude(remainder))
    }

    if rhs.high == (0 as Low) {
      let (x, a) = lhs.high.quotientAndRemainder(dividingBy: rhs.low)
      let (y, b) = a == (0 as Low) ?
                     lhs.low.quotientAndRemainder(dividingBy: rhs.low) :
                     rhs.low.dividingFullWidth((a, lhs.low))
      return (Magnitude(x, y), Magnitude(0, b))
    }

    // Left shift both rhs and lhs, then divide and right shift the remainder.
    let shift = rhs.leadingZeroBitCount
    let rhs = rhs &<< shift
    let high = (lhs &>> (Magnitude.bitWidth &- shift)).low
    let lhs = lhs &<< shift
    let (quotient, remainder) =
      Magnitude._divide((high, lhs.high, lhs.low), by: rhs)
    return (Magnitude(0, quotient), remainder &>> shift)
  }
}

extension _DoubleWidth: SignedNumeric, SignedInteger
  where Base: SignedInteger { }

extension _DoubleWidth: Sendable
  where Base: Sendable, Base.Magnitude: Sendable { }

extension _DoubleWidth: Codable
  where Base: Codable, Base.Magnitude: Codable { }

//===--- BigInt.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// XFAIL: linux
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import Darwin

extension FixedWidthInteger {
  /// Returns the high and low parts of a potentially overflowing addition.
  static func doubleWidthAdd(_ lhs: Self, _ rhs: Self) ->
    (high: Self, low: Self) {
    let sum = lhs.addingWithOverflow(rhs)
    return (sum.overflow == .overflow ? 1 : 0, sum.partialValue)
  }

  /// Returns the high and low parts of two seqeuential potentially overflowing
  /// additions.
  static func doubleWidthAdd(_ x: Self, _ y: Self, _ z: Self) ->
    (high: Self, low: Self) {
    let xy = x.addingWithOverflow(y)
    let xyz = xy.partialValue.addingWithOverflow(z)
    let high: Self = (xy.overflow == .overflow ? 1 : 0) +
      (xyz.overflow == .overflow ? 1 : 0)
    return (high, xyz.partialValue)
  }

  /// Returns a tuple containing the value that would be borrowed from a higher
  /// place and the partial difference of this value and `rhs`.
  func subtractingWithBorrow(_ rhs: Self) ->
    (borrow: Self, partialValue: Self) {
    let difference = subtractingWithOverflow(rhs)
    return (difference.overflow == .overflow ? 1 : 0, difference.partialValue)
  }

  /// Returns a tuple containing the value that would be borrowed from a higher
  /// place and the partial value of `x` and `y` subtracted from this value.
  func subtractingWithBorrow(_ x: Self, _ y: Self) ->
    (borrow: Self, partialValue: Self) {
    let firstDifference = subtractingWithOverflow(x)
    let secondDifference = firstDifference.partialValue.subtractingWithOverflow(y)
    let borrow: Self = (firstDifference.overflow == .overflow ? 1 : 0) +
      (secondDifference.overflow == .overflow ? 1 : 0)
    return (borrow, secondDifference.partialValue)
  }
}

/// A dynamically-sized signed integer.
public struct BigInt : SignedInteger, CustomStringConvertible,
  CustomDebugStringConvertible {

  /// The type that represents each word. Must be an unsigned
  /// `FixedWidthInteger`.
  typealias Word = UInt

  /// The binary representation of the value, with the least significant word
  /// at index `0`.
  ///
  /// - `_data` has no trailing zero elements
  /// - If `self == 0`, then `isNegative == false` and `_data == []`
  internal var _data: [Word] = []

  /// A Boolean value indicating whether this instance is negative.
  public private(set) var isNegative = false

  /// A Boolean value indicating whether this instance is equal to zero.
  public var isZero: Bool {
    return _data.count == 0
  }

  // MARK: Numeric initializers

  /// Creates a new instance equal to zero.
  public init() { }

  /// Creates a new instance using `_data` as the data collection.
  init<C: Collection>(_ _data: C) where C.Iterator.Element == Word {
    self._data = Array(_data)
    _standardize()
  }

  public init(integerLiteral value: Int) {
    self.init(value)
  }

  public init<T : BinaryInteger>(_ source: T) {
    var source = source
    if source < 0 as T {
      self.isNegative = true
      source *= -1
    }
    let wordRatio = UInt.bitWidth / Word.bitWidth
    _sanityCheck(wordRatio != 0)
    for i in 0..<source.countRepresentedWords {
      var sourceWord = source.word(at: i)
      for _ in 0..<wordRatio {
        _data.append(Word(extendingOrTruncating: sourceWord))
        sourceWord >>= Word.bitWidth
      }
    }
    _standardize()
  }

  public init?<T : BinaryInteger>(exactly source: T) {
    self.init(source)
  }

  public init<T : BinaryInteger>(extendingOrTruncating source: T) {
    self.init(source)
  }

  public init<T : BinaryInteger>(clamping source: T) {
    self.init(source)
  }

  public init<T : FloatingPoint>(_ source: T) {
    fatalError("Not implemented")
  }

  public init?<T : FloatingPoint>(exactly source: T) {
    fatalError("Not implemented")
  }

  /// Returns a randomly-generated word.
  static func _randomWord() -> Word {
    // This handles up to a 64-bit word
    if Word.bitWidth > UInt32.bitWidth {
      return Word(arc4random()) << 32 | Word(arc4random())
    } else {
      return Word(extendingOrTruncating: arc4random())
    }
  }

  /// Creates a new instance whose magnitude has `randomBits` bits of random
  /// data. The sign of the new value is randomly selected.
  public init(randomBits: Int) {
    let (words, extraBits) =
      randomBits.quotientAndRemainder(dividingBy: Word.bitWidth)

    // Get the bits for any full words.
    self._data = (0..<words).map({ _ in BigInt._randomWord() })

    // Get another random number - the highest bit will determine the sign,
    // while the lower `Word.bitWidth - 1` bits are available for any leftover
    // bits in `randomBits`.
    let word = BigInt._randomWord()
    if extraBits != 0 {
      let mask = ~((~0 as Word) << Word(extraBits))
      _data.append(word & mask)
    }
    isNegative = word >> (Word.bitWidth - 1) == 0

    _standardize()
  }

  // MARK: Private methods

  /// Standardizes this instance after mutation, removing trailing zeros
  /// and making sure zero is nonnegative. Calling this method satisfies the
  /// two invariants.
  mutating func _standardize(source: String = #function) {
    defer { _checkInvariants(source: source + " >> _standardize()") }
    while _data.last == 0 {
      _data.removeLast()
    }
    // Zero is never negative.
    isNegative = isNegative && _data.count != 0
  }

  /// Checks and asserts on invariants -- all invariants must be satisfied
  /// at the end of every mutating method.
  ///
  /// - `_data` has no trailing zero elements
  /// - If `self == 0`, then `isNegative == false`
  func _checkInvariants(source: String = #function) {
    if _data.count == 0 {
      assert(isNegative == false, "\(source): isNegative with zero length _data")
    }
    assert(_data.last != 0, "\(source): extra zeroes on _data")
  }

  // MARK: - Word-based arithmetic

  mutating func _unsignedAdd(_ rhs: Word) {
    defer { _checkInvariants() }

    // Quick return if `rhs == 0`
    guard rhs != 0 else { return }

    // Quick return if `self == 0`
    if isZero {
      _data.append(rhs)
      return
    }

    // Add `rhs` to the first word, catching any carry.
    var carry: Word
    (carry, _data[0]) = Word.doubleWidthAdd(_data[0], rhs)

    // Handle any additional carries
    for i in 1..<_data.count {
      // No more action needed if there's nothing to carry
      if carry == 0 { break }
      (carry, _data[i]) = Word.doubleWidthAdd(_data[i], carry)
    }

    // If there's any carry left, add it now
    if carry != 0 {
      _data.append(1)
    }
  }

  /// Subtracts `rhs` from this instance, ignoring the sign.
  ///
  /// - Precondition: `rhs <= self.magnitude`
  mutating func _unsignedSubtract(_ rhs: Word) {
    _precondition(_data.count > 1 || _data[0] > rhs)

    // Quick return if `rhs == 0`
    guard rhs != 0 else { return }

    // If `isZero == true`, then `rhs` must also be zero.
    _precondition(!isZero)

    var carry: Word
    (carry, _data[0]) = _data[0].subtractingWithBorrow(rhs)

    for i in 1..<_data.count {
      // No more action needed if there's nothing to carry
      if carry == 0 { break }
      (carry, _data[i]) = _data[i].subtractingWithBorrow(carry)
    }
    _sanityCheck(carry == 0)

    _standardize()
  }

  /// Adds `rhs` to this instance.
  mutating func add(_ rhs: Word) {
    if isNegative {
      // If _data only contains one word and `rhs` is greater, swap them,
      // make self positive and continue with unsigned subtraction.
      var rhs = rhs
      if _data.count == 1 && _data[0] < rhs {
        swap(&rhs, &_data[0])
        isNegative = false
      }
      _unsignedSubtract(rhs)
    } else {   // positive or zero
      _unsignedAdd(rhs)
    }
  }

  /// Subtracts `rhs` from this instance.
  mutating func subtract(_ rhs: Word) {
    guard rhs != 0 else { return }

    if isNegative {
      _unsignedAdd(rhs)
    } else if isZero {
      isNegative = true
      _data.append(rhs)
    } else {
      var rhs = rhs
      if _data.count == 1 && _data[0] < rhs {
        swap(&rhs, &_data[0])
        isNegative = true
      }
      _unsignedSubtract(rhs)
    }
  }

  /// Multiplies this instance by `rhs`.
  mutating func multiply(by rhs: Word) {
    // If either `self` or `rhs` is zero, the result is zero.
    guard !isZero && rhs != 0 else {
      self = 0
      return
    }

    // If `rhs` is a power of two, can just left shift `self`.
    let rhsLSB = rhs.trailingZeros
    if rhs >> rhsLSB == 1 {
      self <<= rhsLSB
      return
    }

    var carry: Word = 0
    for i in 0..<_data.count {
      let product = Word.doubleWidthMultiply(_data[i], rhs)
      (carry, _data[i]) = Word.doubleWidthAdd(product.low, carry)
      carry = carry &+ product.high
    }

    // Add the leftover carry
    if carry != 0 {
      _data.append(carry)
    }
    _standardize()
  }

  /// Divides this instance by `rhs`, returning the remainder.
  @discardableResult
  mutating func divide(by rhs: Word) -> Word {
    _precondition(rhs != 0, "divide by zero")

    // No-op if `rhs == 1` or `self == 0`.
    if rhs == 1 || isZero {
      return 0
    }

    // If `rhs` is a power of two, can just right shift `self`.
    let rhsLSB = rhs.trailingZeros
    if rhs >> rhsLSB == 1 {
      defer { self >>= rhsLSB }
      return _data[0] & ~(~0 << rhsLSB)
    }

    var carry: Word = 0
    for i in (0..<_data.count).reversed() {
      let lhs = (high: carry, low: _data[i])
      (_data[i], carry) = Word.doubleWidthDivide(lhs, rhs)
    }

    _standardize()
    return carry
  }

  // MARK: Arithmetic

  public var magnitude: BigInt {
    var result = self
    result.isNegative = false
    return result
  }

  /// Adds `rhs` to this instance, ignoring the sign.
  mutating func _unsignedAdd(_ rhs: BigInt) {
    defer { _checkInvariants() }

    let commonCount = Swift.min(_data.count, rhs._data.count)
    let maxCount = Swift.max(_data.count, rhs._data.count)
    _data.reserveCapacity(maxCount)

    // Add the words up to the common count, carrying any overflows
    var carry: Word = 0
    for i in 0..<commonCount {
      (carry, _data[i]) = Word.doubleWidthAdd(_data[i], rhs._data[i], carry)
    }

    // If there are leftover words in `self`, just need to handle any carries
    if _data.count > rhs._data.count {
      for i in commonCount..<maxCount {
        // No more action needed if there's nothing to carry
        if carry == 0 { break }
        (carry, _data[i]) = Word.doubleWidthAdd(_data[i], carry)
      }

      // If there are leftover words in `rhs`, need to copy to `self` with carries
    } else if _data.count < rhs._data.count {
      for i in commonCount..<maxCount {
        // Append remaining words if nothing to carry
        if carry == 0 {
          _data.append(contentsOf: rhs._data.suffix(from: i))
          break
        }
        let sum: Word
        (carry, sum) = Word.doubleWidthAdd(rhs._data[i], carry)
        _data.append(sum)
      }
    }

    // If there's any carry left, add it now
    if carry != 0 {
      _data.append(1)
    }
  }

  /// Subtracts `rhs` from this instance, ignoring the sign.
  ///
  /// - Precondition: `rhs.magnitude <= self.magnitude` (unchecked)
  /// - Precondition: `rhs._data.count <= self._data.count`
  mutating func _unsignedSubtract(_ rhs: BigInt) {
    _precondition(rhs._data.count <= _data.count)

    var carry: Word = 0
    for i in 0..<rhs._data.count {
      (carry, _data[i]) = _data[i].subtractingWithBorrow(rhs._data[i], carry)
    }

    for i in rhs._data.count..<_data.count {
      // No more action needed if there's nothing to carry
      if carry == 0 { break }
      (carry, _data[i]) = _data[i].subtractingWithBorrow(carry)
    }
    _sanityCheck(carry == 0)

    _standardize()
  }

  public static func +=(lhs: inout BigInt, rhs: BigInt) {
    defer { lhs._checkInvariants() }
    if lhs.isNegative == rhs.isNegative {
      lhs._unsignedAdd(rhs)
    } else {
      lhs -= -rhs
    }
  }

  public static func -=(lhs: inout BigInt, rhs: BigInt) {
    defer { lhs._checkInvariants() }

    // Subtracting something of the opposite sign just adds magnitude.
    guard lhs.isNegative == rhs.isNegative else {
      lhs._unsignedAdd(rhs)
      return
    }

    // Comare `lhs` and `rhs` so we can use `_unsignedSubtract` to subtract
    // the smaller magnitude from the larger magnitude.
    switch lhs._compareMagnitude(to: rhs) {
    case .equal:
      self = 0
    case .greaterThan:
      lhs._unsignedSubtract(rhs)
    case .lessThan:
      // x - y == -y + x == -(y - x)
      var result = rhs
      result._unsignedSubtract(lhs)
      result.isNegative = !lhs.isNegative
      lhs = result
    }
  }

  public static func *=(lhs: inout BigInt, rhs: BigInt) {
    // If either `lhs` or `rhs` is zero, the result is zero.
    guard !lhs.isZero && !rhs.isZero else {
      self = 0
      return
    }

    var newData: [Word] = Array(repeating: 0, count: lhs._data.count + rhs._data.count)
    let (a, b) = _data.count > rhs._data.count
      ? (lhs._data, rhs._data)
      : (rhs._data, lhs._data)
    _sanityCheck(a.count >= b.count)

    var carry: Word = 0
    for ai in 0..<a.count {
      carry = 0
      for bi in 0..<b.count {
        // Each iteration needs to perform this operation:
        //
        //     newData[ai + bi] += (a[ai] * b[bi]) + carry
        //
        // However, `a[ai] * b[bi]` produces a double-width result, and both
        // additions can overflow to a higher word. The following two lines
        // capture the low word of the multiplication and additions in
        // `newData[ai + bi]` and any addition overflow in `carry`.
        let product = Word.doubleWidthMultiply(a[ai], b[bi])
        (carry, newData[ai + bi]) = Word.doubleWidthAdd(
          newData[ai + bi], product.low, carry)

        // Now we combine the high word of the multiplication with any addition
        // overflow. It is safe to add `product.high` and `carry` here without
        // checking for overflow, because if `product.high == .max - 1`, then
        // `carry <= 1`. Otherwise, `carry <= 2`.
        //
        // Worst-case (aka 9 + 9*9 + 9):
        //
        //       newData         a[ai]        b[bi]         carry
        //      0b11111111 + (0b11111111 * 0b11111111) + 0b11111111
        //      0b11111111 + (0b11111110_____00000001) + 0b11111111
        //                   (0b11111111_____00000000) + 0b11111111
        //                   (0b11111111_____11111111)
        //
        // Second-worse case:
        //
        //      0b11111111 + (0b11111111 * 0b11111110) + 0b11111111
        //      0b11111111 + (0b11111101_____00000010) + 0b11111111
        //                   (0b11111110_____00000001) + 0b11111111
        //                   (0b11111111_____00000000)
        _sanityCheck(product.high.addingWithOverflow(carry).overflow == .none)
        carry = product.high &+ carry
      }

      // Leftover `carry` is inserted in new highest word.
      _sanityCheck(newData[ai + b.count] == 0)
      newData[ai + b.count] = carry
    }

    lhs._data = newData
    lhs.isNegative = lhs.isNegative != rhs.isNegative
    lhs._standardize()
  }

  /// Divides this instance by `rhs`, returning the remainder.
  @discardableResult
  mutating func _internalDivide(by rhs: BigInt) -> BigInt {
    _precondition(!rhs.isZero, "Divided by zero")
    defer { _checkInvariants() }

    // Handle quick cases that don't require division:
    // If `self < rhs`, the result is zero, remainder = self
    // If `self == rhs`, the result is 1 or -1, remainder = 0
    switch _compareMagnitude(to: rhs) {
    case .lessThan:
      defer { self = 0 }
      return self
    case .equal:
      self = isNegative != rhs.isNegative ? -1 : 1
      return 0
    default:
      break
    }

    let n = countBits - rhs.countBits

    var quotient: BigInt = 0
    var tempSelf = self.magnitude
    var tempRHS = rhs.magnitude << n
    var tempQuotient: BigInt = 1 << n

    for _ in (0...n).reversed() {
      if tempRHS._compare(to: tempSelf) != .greaterThan {
        tempSelf.subtract(tempRHS)
        quotient.add(tempQuotient)
      }
      tempRHS >>= 1
      tempQuotient >>= 1
    }

    // `tempSelf` is the remainder - match sign of original `self`
    tempSelf.isNegative = self.isNegative
    tempSelf._standardize()

    // `quotient` is the quotient
    quotient.isNegative = isNegative != rhs.isNegative
    self = quotient
    _standardize()

    return tempSelf
  }

  public static func /=(lhs: inout BigInt, rhs: BigInt) {
    defer { _checkInvariants() }
    _internalDivide(by: rhs)
  }

  // MARK: BinaryInteger

  public func word(at n: Int) -> UInt {
    let ratio = UInt.bitWidth / Word.bitWidth
    _sanityCheck(ratio != 0)

    // Early return if `n` is larger than `_data`.
    guard n * ratio < _data.count else {
      return isNegative ? ~0 : 0
    }

    // Subtract one from magnitude if negative to fake two's complement.
    // Seems unnecessary to make copy `_data` here...
    var x = self
    if isNegative {
      x.add(1 as Word)
    }

    let start = n * ratio
    let end = Swift.min(x._data.count, (n + 1) * ratio)
    let wordSlice = x._data[start..<end]
    var result: UInt = 0
    for v in wordSlice.reversed() {
      result <<= Word.bitWidth
      result |= UInt(extendingOrTruncating: v)
    }

    return isNegative ? ~result : result
  }

  /// The number of bits used for storage of this value. Always a multiple of
  /// `Word.bitWidth`.
  public var bitWidth: Int {
    return _data.count * Word.bitWidth
  }

  /// The number of bits actually used in the binary representation of this
  /// value's magnitude. Always less than or equal to `bitWidth`. Does not
  /// include any sign.
  public var countBits: Int {
    return isZero ? 0 : bitWidth - _data.last!.leadingZeros
  }

  /// The number of sequential zeros in the least-significant position of this
  /// value's binary representation.
  ///
  /// The numbers 1 and zero have zero trailing zeros.
  public var trailingZeros: Int {
    guard !isZero else {
      return 0
    }

    let i = _data.index(where: { $0 != 0 })!
    _sanityCheck(_data[i] != 0)
    return i * Word.bitWidth + _data[i].trailingZeros
  }

  public var minimumSignedRepresentationBitWidth: Int {
    return countBits + 1
  }

  public mutating func formRemainder(dividingBy rhs: BigInt) {
    defer { _checkInvariants() }
    self = _internalDivide(by: rhs)
  }

  public func quotientAndRemainder(dividingBy rhs: BigInt) -> (BigInt, BigInt) {
    var x = self
    let r = x._internalDivide(by: rhs)
    return (x, r)
  }

  // MARK: SignedArithmetic

  public mutating func negate() {
    defer { _checkInvariants() }
    guard _data.count > 0 else { return }
    isNegative = !isNegative
  }

  // MARK: Strideable

  public func distance(to other: BigInt) -> BigInt {
    return other.subtracting(self)
  }

  public func advanced(by n: BigInt) -> BigInt {
    return self.adding(n)
  }

  // MARK: Other arithmetic

  /// Returns the greatest common divisor for this value and `other`.
  public func greatestCommonDivisor(with other: BigInt) -> BigInt {
    // Quick return if either is zero
    if other.isZero {
      return magnitude
    }
    if isZero {
      return other.magnitude
    }

    var (x, y) = (self.magnitude, other.magnitude)
    let (xLSB, yLSB) = (x.trailingZeros, y.trailingZeros)

    // Remove any common factor of two
    let commonPower = Swift.min(xLSB, yLSB)
    x >>= commonPower
    y >>= commonPower

    // Remove any remaining factor of two
    if xLSB != commonPower {
      x >>= xLSB - commonPower
    }
    if yLSB != commonPower {
      y >>= yLSB - commonPower
    }

    while !x.isZero {
      // Swap values to ensure that `x >= y`.
      if x._compareMagnitude(to: y) == .lessThan {
        swap(&x, &y)
      }

      // Subtract smaller and remove any factors of two
      x._unsignedSubtract(y)
      x >>= x.trailingZeros
    }

    // Add original common factor of two back into result
    y <<= commonPower
    return y
  }

  /// Returns the lowest common multiple for this value and `other`.
  public func lowestCommonMultiple(with other: BigInt) -> BigInt {
    let gcd = greatestCommonDivisor(with: other)
    if _compareMagnitude(to: other) == .lessThan {
      return ((self / gcd) * other).magnitude
    } else {
      return ((other / gcd) * self).magnitude
    }
  }

  // MARK: - String methods

  /// Creates a new instance from the given string.
  ///
  /// - Parameters:
  ///   - source: The string to parse for the new instance's value. If a
  ///     character in `source` is not in the range `0...9` or `a...z`, case
  ///     insensitive, or is not less than `radix`, the result is `nil`.
  ///   - radix: The radix to use when parsing `source`. `radix` must be in the
  ///     range `2...36`. The default is `10`.
  public init?(_ source: String, radix: Int = 10) {
    assert(2...36 ~= radix, "radix must be in range 2...36")
    let radix = Word(radix)

    func valueForCodeUnit(_ unit: UTF16.CodeUnit) -> Word? {
      switch unit {
      // "0"..."9"
      case 48...57: return Word(unit - 48)
      // "a"..."z"
      case 97...122: return Word(unit - 87)
      // "A"..."Z"
      case 65...90: return Word(unit - 55)
      // invalid character
      default: return nil
      }
    }

    var source = source

    // Check for a single prefixing hyphen
    let negative = source.hasPrefix("-")
    if negative {
      source = String(source.characters.dropFirst())
    }

    // Loop through characters, multiplying
    for v in source.utf16.map(valueForCodeUnit) {
      // Character must be valid and less than radix
      guard let v = v else { return nil }
      guard v < radix else { return nil }

      self.multiply(by: radix)
      self.add(v)
    }

    self.isNegative = negative
  }

  /// Returns a string representation of this instance.
  ///
  /// - Parameters:
  ///   - radix: The radix to use when converting this instance to a string.
  ///     The value passed as `radix` must be in the range `2...36`. The
  ///     default is `10`.
  ///   - lowercase: Whether to use lowercase letters to represent digits
  ///     greater than 10. The default is `true`.
  public func toString(radix: Int = 10, lowercase: Bool = true) -> String {
    assert(2...36 ~= radix, "radix must be in range 2...36")

    let digitsStart = Word(("0" as UnicodeScalar).value)
    let lettersStart = Word(((lowercase ? "a" : "A") as UnicodeScalar).value - 10)
    func toLetter(_ x: Word) -> UnicodeScalar {
      return x < 10
        ? UnicodeScalar(UInt32(digitsStart + x))!
        : UnicodeScalar(UInt32(lettersStart + x))!
    }

    let radix = Word(radix)
    var result: [UnicodeScalar] = []

    var x = self
    while !x.isZero {
      result.append(toLetter(x.divide(by: radix)))
    }

    let sign = isNegative ? "-" : ""
    let rest = result.count == 0
      ? "0"
      : String(result.reversed())
    return sign + rest
  }

  public var description: String {
    return decimalString
  }

  public var debugDescription: String {
    return "BigInt(\(hexString), words: \(_data.count))"
  }

  /// A string representation of this instance's value in base 2.
  public var binaryString: String {
    return toString(radix: 2)
  }

  /// A string representation of this instance's value in base 10.
  public var decimalString: String {
    return toString(radix: 10)
  }

  /// A string representation of this instance's value in base 16.
  public var hexString: String {
    return toString(radix: 16, lowercase: false)
  }

  /// A string representation of this instance's value in base 36.
  public var compactString: String {
    return toString(radix: 36, lowercase: false)
  }

  // MARK: Comparable

  enum _ComparisonResult {
    case lessThan, equal, greaterThan
  }

  /// Returns whether this instance is less than, greather than, or equal to
  /// the given value.
  func _compare(to rhs: BigInt) -> _ComparisonResult {
    // Negative values are less than positive values
    guard isNegative == rhs.isNegative else {
      return isNegative ? .lessThan : .greaterThan
    }

    guard _data.count == rhs._data.count else {
      return isNegative
        // A negative value with fewer bits is greater
        ? _data.count < rhs._data.count ? .greaterThan : .lessThan
        // A positive value with fewer bits is less
        : _data.count < rhs._data.count ? .lessThan : .greaterThan
    }

    // Equal signs and number of words: compare from most significant word
    for i in (0..<_data.count).reversed() {
      if _data[i] < rhs._data[i] {
        return isNegative ? .greaterThan : .lessThan
      }
      if _data[i] > rhs._data[i] {
        return isNegative ? .lessThan : .greaterThan
      }
    }

    // Everything is equal
    return .equal
  }

  /// Returns whether the magnitude of this instance is less than, greather
  /// than, or equal to the magnitude of the given value.
  func _compareMagnitude(to rhs: BigInt) -> _ComparisonResult {
    guard _data.count == rhs._data.count else {
      return _data.count < rhs._data.count ? .lessThan : .greaterThan
    }

    // Equal number of words: compare from most significant word
    for i in (0..<_data.count).reversed() {
      if _data[i] < rhs._data[i] { return .lessThan }
      if _data[i] > rhs._data[i] { return .greaterThan }
    }
    return .equal
  }

  public static func ==(lhs: BigInt, rhs: BigInt) -> Bool {
    return lhs._compare(to: rhs) == .equal
  }

  public static func < (lhs: BigInt, rhs: BigInt) -> Bool {
    return lhs._compare(to: rhs) == .lessThan
  }

  // MARK: Hashable

  public var hashValue: Int {
#if arch(i386) || arch(arm)
    let p: UInt = 16777619
    let h: UInt = (2166136261 &* p) ^ (isNegative ? 1 : 0)
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
    let p: UInt = 1099511628211
    let h: UInt = (14695981039346656037 &* p) ^ (isNegative ? 1 : 0)
#else
    fatalError("Unimplemented")
#endif
    return Int(bitPattern: _data.reduce(h, { ($0 &* p) ^ UInt($1) }))
  }

  // MARK: Bit shifting operators

  mutating func _shiftLeft(byWords words: Int) {
    guard words > 0 else { return }
    _data.insert(contentsOf: repeatElement(0, count: words), at: 0)
  }

  mutating func _shiftRight(byWords words: Int) {
    guard words > 0 else { return }
    _data.removeFirst(Swift.min(_data.count, words))
  }

  public static func <<=(lhs: inout BigInt, rhs: Int) {
    defer { lhs._checkInvariants() }
    guard rhs != 0 else { return }
    guard rhs > 0 else {
      lhs >>= -rhs
      return
    }

    // We can add `rhs / bits` extra words full of zero at the low end.
    let extraWords = rhs / Word.bitWidth
    lhs._data.reserveCapacity(lhs._data.count + extraWords + 1)
    lhs._shiftLeft(byWords: extraWords)

    // Each existing word will need to be shifted left by `rhs % bits`.
    // For each pair of words, we'll use the high `offset` bits of the
    // lower word and the low `Word.bitWidth - offset` bits of the higher
    // word. If we start out with `_data == [0b01101110, 0b00110110]` and
    // `rhs == 3`, these are the steps:
    //
    //             (  2  )  (  1  )  (  0  )
    //
    //     pre 1: _0000000 _0110110 _1101110
    //                ^^^^  ###
    //    post 1: _0000011 _0110110 _1101110
    //             ^^^^###
    //     pre 2: _0000011 _0110110 _1101110
    //                         ^^^^  ###
    //    post 2: _0000011 _0110110 _1101110
    //                      ^^^^###
    //     pre 3: _0000011 _0110110 _1101110
    //                                  ^^^^
    //    post 4: _0000011 _0110110 _1110000
    //                               ^^^^
    let highOffset = rhs % Word.bitWidth
    let lowOffset = Word.bitWidth - highOffset

    // If there's no offset, we're finished, as `rhs` was a multiple of
    // `BigInt.bits`. No need to even standardize.
    guard highOffset != 0 else { return }

    // Add new word at the end, then shift everything left by `offset` bits.
    lhs._data.append(0)
    for i in ((extraWords + 1)..<lhs._data.count).reversed() {
      lhs._data[i] = lhs._data[i] << highOffset
        | lhs._data[i - 1] >> lowOffset
    }
    // Finally, shift the lowest word and standardize the result.
    lhs._data[extraWords] = lhs._data[extraWords] << highOffset

    lhs._standardize()
  }

  public static func >>=(lhs: inout BigInt, rhs: Int) {
    defer { lhs._checkInvariants() }
    guard rhs != 0 else { return }
    guard rhs > 0 else {
      lhs <<= -rhs
      return
    }

    // We can remove `rhs / bits` full words at the low end.
    // If that removes the entirety of `_data`, we're done.
    let wordsToRemove = rhs / Word.bitWidth
    lhs._shiftRight(byWords: wordsToRemove)
    guard lhs._data.count != 0 else {
      lhs._standardize()
      return
    }

    // Each existing word will need to be shifted right by `rhs % bits`.
    // For each pair of words, we'll use the low `offset` bits of the
    // higher word and the high `BigInt.Word.bitWidth - offset` bits of the lower
    // word. If we start out with `_data == [0b01101110, 0b00110110, 0b00001111]`
    // and `rhs == 3`, these are the steps:
    //
    //             (  2  )  (  1  )  (  0  )
    //     pre 1: _0001111 _0110110 _1101110
    //                          ^^^  ####
    //    post 1: _0001111 _0110110 _1101101
    //                               ^^^####
    //     pre 2: _0001111 _0110110 _1101110
    //                 ^^^  ####
    //    post 2: _0001111 _1110110 _1101101
    //                      ^^^####
    //     pre 3: _0001111 _0110110 _1101110
    //             ####
    //    post 3: _0000001 _1110110 _1101101
    //                ####
    let lowOffset = rhs % Word.bitWidth
    let highOffset = Word.bitWidth - lowOffset

    // If there's no offset, we're finished, as `rhs` was a multiple of
    // `BigInt.bits`. No need to even standardize.
    guard lowOffset != 0 else {
      lhs._standardize()
      return
    }

    // Shift everything right by `offset` bits.
    for i in 0..<(lhs._data.count - 1) {
      lhs._data[i] = lhs._data[i] >> lowOffset |
        lhs._data[i + 1] << highOffset
    }
    // Finally, shift the highest word and standardize the result.
    lhs._data[lhs._data.count - 1] >>= lowOffset
    lhs._standardize()
  }

  public static func <<(lhs: BigInt, rhs: Int) -> BigInt {
    var lhs = lhs
    lhs <<= rhs
    return lhs
  }

  public static func >>(lhs: BigInt, rhs: Int) -> BigInt {
    var lhs = lhs
    lhs >>= rhs
    return lhs
  }
}

// MARK: - Tests

var DivisionTests = TestSuite("DoubleWidthDivide")

DivisionTests.test("Unsigned division") {
  let result1 = UInt8.doubleWidthDivide((4, 0), 8)
  expectEqual(result1.quotient, 128)
  expectEqual(result1.remainder, 0)

  let result2 = UInt64.doubleWidthDivide((18446744073709551614, 1001), UInt64.max)
  expectEqual(result2.quotient, UInt64.max)
  expectEqual(result2.remainder, 1000)
}

DivisionTests.test("Signed division") {
  let result1 = Int8.doubleWidthDivide((4, 0), 8)
  // Not a good result: 0b00000100_00000000 >> 3 == 0b10000000 == -128
  // Trap on overflow instead?
  expectEqual(result1.quotient, -128)
  expectEqual(result1.remainder, 0)

  // Not a good result: Int8 has room to store -64
  expectCrashLater()
  _ = Int8.doubleWidthDivide((-4, 0), 16)
}

DivisionTests.test("Divide by zero") {
  let x = (0, 0 as Int.Magnitude)
  let y = 0

  expectCrashLater()
  _ = Int.doubleWidthDivide(x, y)
}

DivisionTests.test("Overflow") {
  expectCrashLater()
  _ = UInt8.doubleWidthDivide((4, 0), 4)
}

var BigIntTests = TestSuite("BigInt")

func testBinaryInit<T: BinaryInteger>(_ x: T) -> BigInt {
  return BigInt(x)
}

BigIntTests.test("Initialization") {
  let x = testBinaryInit(1_000_000 as Int)
  expectEqual(x._data[0], 1_000_000)

  let y = testBinaryInit(1_000 as UInt16)
  expectEqual(y._data[0], 1_000)

  let z = testBinaryInit(-1_000_000 as Int)
  expectEqual(z._data[0], 1_000_000)
  expectTrue(z.isNegative)

  let z6 = testBinaryInit(z * z * z * z * z * z)
  expectEqual(z6._data, [12919594847110692864, 54210108624275221])
  expectFalse(z6.isNegative)
}

BigIntTests.test("Identity/Fixed point") {
  let x = BigInt(repeatElement(BigInt.Word.max, count: 20))
  let y = x.negated()

  expectEqual(x / x, 1)
  expectEqual(x / y, -1)
  expectEqual(y / x, -1)
  expectEqual(y / y, 1)
  expectEqual(x % x, 0)
  expectEqual(x % y, 0)
  expectEqual(y % x, 0)
  expectEqual(y % y, 0)

  expectEqual(x * 1, x)
  expectEqual(y * 1, y)
  expectEqual(x * -1, y)
  expectEqual(y * -1, x)
  expectEqual(-x, y)
  expectEqual(-y, x)

  expectEqual(x + 0, x)
  expectEqual(y + 0, y)
  expectEqual(x - 0, x)
  expectEqual(y - 0, y)

  expectEqual(x - x, 0)
  expectEqual(y - y, 0)
}

BigIntTests.test("Max arithmetic") {
  let x = BigInt(repeatElement(BigInt.Word.max, count: 50))
  let y = BigInt(repeatElement(BigInt.Word.max, count: 35))
  let (q, r) = x.quotientAndRemainder(dividingBy: y)

  expectEqual(q * y + r, x)
  expectEqual(q * y, x - r)
}

BigIntTests.test("Zero arithmetic") {
  let zero: BigInt = 0
  expectTrue(zero.isZero)
  expectFalse(zero.isNegative)

  let x: BigInt = 1
  expectTrue((x - x).isZero)
  expectFalse((x - x).isNegative)

  let y: BigInt = -1
  expectTrue(y.isNegative)
  expectTrue((y - y).isZero)
  expectFalse((y - y).isNegative)

  expectEqual(x * zero, zero)
  expectCrashLater()
  _ = x / zero
}

BigIntTests.test("Conformances") {
  // Comparable
  let x = BigInt(Int.max)
  let y = x * x * x * x * x
  expectLT(y, y + 1)
  expectGT(y, y - 1)
  expectGT(y, 0)

  let z = y.negated()
  expectLT(z, z + 1)
  expectGT(z, z - 1)
  expectLT(z, 0)

  expectEqual(z.negated(), y)
  expectEqual(y + z, 0)

  // Hashable
  expectNotEqual(x.hashValue, y.hashValue)
  expectNotEqual(y.hashValue, z.hashValue)

  let set = Set([x, y, z])
  expectTrue(set.contains(x))
  expectTrue(set.contains(y))
  expectTrue(set.contains(z))
  expectFalse(set.contains(x.negated()))
}

BigIntTests.test("BinaryInteger interop") {
  let x: BigInt = 100
  let xComp = UInt8(x)
  expectTrue(x == xComp)
  expectTrue(x < xComp + 1)

  let y: BigInt = -100
  let yComp = Int8(y)
  expectTrue(y == yComp)
  expectTrue(y < yComp + 1)

  let z = BigInt(Int.min)
  let zComp = Int(extendingOrTruncating: z)
  expectTrue(z == zComp)
  expectTrue(z < zComp + 1)

  let w = BigInt(UInt.max)
  let wComp = UInt(extendingOrTruncating: w)
  expectTrue(w == wComp)
  expectTrue(w > wComp - 1)
}

BigIntTests.test("Huge") {
  let x = BigInt(randomBits: 1_000_000)
  expectGT(x, x - 1)
  let y = x.negated()
  expectGT(y, y - 1)
}

BigIntTests.test("Randomized arithmetic").forEach(in: Array(1...20)) { _ in
  // Get bit lengths for six BigInts
  let bitLengths = Array(sequence(first: 0, next: { _ in Int(arc4random_uniform(1000) + 1) }).dropFirst().prefix(6))

  // Test x == (x / y) * x + (x % y)
  let (x, y) = (
    BigInt(randomBits: bitLengths[0]), BigInt(randomBits: bitLengths[1]))
  if !y.isZero {
    let (q, r) = x.quotientAndRemainder(dividingBy: y)
    expectEqual(q * y + r, x)
    expectEqual(q * y, x - r)
  }

  // Test (x0 + y0)(x1 + y1) == x0x1 + x0y1 + y0x1 + y0y1
  let (x0, y0, x1, y1) = (
    BigInt(randomBits: bitLengths[2]), BigInt(randomBits: bitLengths[3]),
    BigInt(randomBits: bitLengths[4]), BigInt(randomBits: bitLengths[5]))
  let r1 = (x0 + y0) * (x1 + y1)
  let r2 = (x0 * x1) + (x0 * y1) + (y0 * x1) + (y0 * y1)
  expectEqual(r1, r2)
}

runAllTests()

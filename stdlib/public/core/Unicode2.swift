//===--- Unicode2.swift ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// The result of parsing input from a collection.
///
/// - Parameter T: the result of a successful parse
/// - Parameter Index: the index type of the collection being transcoded.
public enum ParseResult<T, Index> { // FIXME: RENAME THIS!
/// Indicates valid input was recognized.
///
/// `resumptionPoint` is the end of the parsed region
case valid(T, resumptionPoint: Index)

/// Indicates invalid input was recognized.
///
/// `resumptionPoint` is the next position at which to continue parsing after
/// the invalid input is repaired.
case error(resumptionPoint: Index)

/// Indicates that there was no more input to consume.
case emptyInput

  /// If any input was consumed, the point from which to continue parsing.
  var resumptionPoint: Index? {
    switch self {
    case .valid(_,let r): return r
    case .error(let r): return r
    case .emptyInput: return nil
    }
  }
}

// FIXME: closure-taking methods should rethrow.

public protocol AnyUnicodeEncoding {
  // FIXME: a single scalar might not be the most efficient buffer to use here.
  // SIMD instructions can be used to decode UTF-8 much more efficiently, which
  // could result in processing up to 16 code units at a time.
  /// The maximum number of code units in an encoded unicoded scalar value
  static var maxLengthOfEncodedScalar: UInt { get }
  
  // FIXME: do we even want these single-scalar parse methods on the thing
  // that's going to be used with type erasure?  You pretty much want to be
  // doing bulk-stuff behind the type erasure boundary, so things like
  // decodeForward/decodeBackward may make ore sense.
  
  /// Parse a single unicode scalar forward from `input`.
  ///
  /// - Parameter knownCount: a number of code units known to exist in `input`.
  ///   **Note:** passing a known compile-time constant is strongly advised,
  ///   even if it's zero.
  static func _parse1Forward<C: Collection>(
    _ input: C, knownCount: Int /* = 0 */
  ) -> ParseResult<UInt32, C.Index>
  where C.Iterator.Element == UInt32

  /// Parse a single unicode scalar in reverse from `input`.
  ///
  /// - Parameter knownCount: a number of code units known to exist in `input`.
  ///   **Note:** passing a known compile-time constant is strongly advised,
  ///   even if it's zero.
  static func _parse1Reverse<C: BidirectionalCollection>(
    _ input: C, knownCount: Int/* = 0 */
  ) -> ParseResult<UInt32, C.Index>
  where C.Iterator.Element == UInt32

  /// Decode a whole collection efficiently, writing results into `output`.
  ///
  /// - Returns: a pair consisting of:
  ///   0. the suffix of input starting with the first decoding error if
  ///      `stopOnError` is true, and the empty suffix otherwise.
  ///   1. The number of errors that were detected.  If `stopOnError` is true
  ///      this value will never exceed 1.
  ///
  /// - Note: using this function may be faster than repeatedly using `parse`
  ///   directly, because it avoids intra-scalar checks for end of sequence.
  @discardableResult
  static func decodeForward<C: Collection>(
    _ input: C,
    repairingIllFormedSequences makeRepairs: Bool /* = true */,
    into output: (UInt32)->Void
  ) -> (remainder: C.SubSequence, errorCount: Int)
  where C.Iterator.Element == UInt32
  
  /// Decode a whole collection efficiently in reverse, writing results into
  /// `output`.
  ///
  /// - Returns: a pair consisting of:
  ///   0. the suffix of input starting with the first decoding error if
  ///      `stopOnError` is true, and the empty suffix otherwise.
  ///   1. The number of errors that were detected.  If `stopOnError` is true
  ///      this value will never exceed 1.
  ///
  /// - Note: using this function may be faster than repeatedly using `parse`
  ///   directly, because it avoids intra-scalar checks for end of sequence.
  @discardableResult
  static func decodeReverse<C: BidirectionalCollection>(
    _ input: C,
    repairingIllFormedSequences makeRepairs: Bool /* = true */,
    into output: (UInt32)->Void
  ) -> (remainder: C.SubSequence, errorCount: Int)
  where C.Iterator.Element == UInt32
}

/// An encoding for text with UnicodeScalar as a common currency type
public protocol UnicodeEncoding : AnyUnicodeEncoding {
  // FIXME: a single scalar might not be the most efficient buffer to use here.
  // SIMD instructions can be used to decode UTF-8 much more efficiently, which
  // could result in processing up to 16 code units at a time.
  /// The maximum number of code units in an encoded unicoded scalar value
  static var maxLengthOfEncodedScalar: UInt { get }
  
  /// A type that can represent a single UnicodeScalar as it is encoded in this
  /// encoding.
  associatedtype EncodedScalar : EncodedScalarProtocol
  // where Iterator.Element : UnsignedInteger

  /// Produces a scalar of this encoding if possible; returns `nil` otherwise.
  static func encode<Scalar: EncodedScalarProtocol>(
    _:Scalar) -> Self.EncodedScalar?
  
  /// Parse a single unicode scalar forward from `input`.
  ///
  /// - Parameter knownCount: a number of code units known to exist in `input`.
  ///   **Note:** passing a known compile-time constant is strongly advised,
  ///   even if it's zero.
  static func parse1Forward<C: Collection>(
    _ input: C, knownCount: Int /* = 0 */
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == EncodedScalar.Iterator.Element

  /// Parse a single unicode scalar in reverse from `input`.
  ///
  /// - Parameter knownCount: a number of code units known to exist in `input`.
  ///   **Note:** passing a known compile-time constant is strongly advised,
  ///   even if it's zero.
  static func parse1Reverse<C: BidirectionalCollection>(
    _ input: C, knownCount: Int/* = 0 */
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == EncodedScalar.Iterator.Element,
  // FIXME: drop these constraints once we have the compiler features.
  C.SubSequence.Index == C.Index,
  C.SubSequence.Iterator.Element == C.Iterator.Element
}

// Overloads that work around the fact that you can't put default arguments in
// protocol requirements
extension UnicodeEncoding {
  // FIXME: this becomes ambiguous with UnicodeCodec.CodeUnit
  public typealias CodeUnit = EncodedScalar.Iterator.Element
  
  /// Parse a single unicode scalar forward from `input`.
  public static func parse1Forward<C: Collection>(
    _ input: C
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == EncodedScalar.Iterator.Element {
    return parse1Forward(input, knownCount: 0)
  }

  /// Parse a single unicode scalar in reverse from `input`.
  public static func parse1Reverse<C: BidirectionalCollection>(
    _ input: C
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == EncodedScalar.Iterator.Element,
  // FIXME: drop these constraints once we have the compiler features.
  C.SubSequence.Index == C.Index,
  C.SubSequence.Iterator.Element == C.Iterator.Element {
    return parse1Reverse(input, knownCount: 0)
  }
}

/// AnyUnicodeEncoding conformance in terms of more strictly-typed
/// stuff.
extension UnicodeEncoding
where EncodedScalar.Iterator.Element : UnsignedInteger {
  
  public static func _parse1Forward<C: Collection>(
    _ input: C, knownCount: Int = 0
  ) -> ParseResult<UInt32, C.Index>
  where C.Iterator.Element == UInt32 {
    switch parse1Forward(
      input.lazy.map { numericCast($0) as CodeUnit }, knownCount: knownCount) {
    case .valid(let s,let r):
      return .valid(s.utf32.first!, resumptionPoint: r)
    case .error(let r):
      return .error(resumptionPoint: r)
    case .emptyInput:
      return .emptyInput
    }
  }

  public static func _parse1Reverse<C: BidirectionalCollection>(
    _ input: C, knownCount: Int = 0
  ) -> ParseResult<UInt32, C.Index>
  where C.Iterator.Element == UInt32 {
    switch parse1Reverse(
      input.lazy.map { numericCast($0) as CodeUnit },
      knownCount: knownCount) {
    case .valid(let s,let r):
      return .valid(s.utf32.last!, resumptionPoint: r)
    case .error(let r):
      return .error(resumptionPoint: r)
    case .emptyInput:
      return .emptyInput
    }
  }

  @discardableResult
  public static func decodeForward<C: Collection>(
    _ input: C,
    repairingIllFormedSequences makeRepairs: Bool = true,
    into output: (UInt32)->Void
  ) -> (remainder: C.SubSequence, errorCount: Int)
  where C.Iterator.Element == UInt32 {
    let (remainder, errorCount) = parseForward(
      input.lazy.map { numericCast($0) as CodeUnit },
      repairingIllFormedSequences: makeRepairs) {
      output($0.utf32.first!)
    }
    return (
      remainder: input[remainder.startIndex..<remainder.endIndex],
      errorCount: errorCount)
  }
  
  @discardableResult
  public static func decodeReverse<C: BidirectionalCollection>(
    _ input: C,
    repairingIllFormedSequences makeRepairs: Bool = true,
    into output: (UInt32)->Void
  ) -> (remainder: C.SubSequence, errorCount: Int)
  where C.Iterator.Element == UInt32 {
    let (remainder, errorCount) = parseReverse(
      input.lazy.map { numericCast($0) as CodeUnit },
      repairingIllFormedSequences: makeRepairs) {
      output($0.utf32.first!)
    }
    return (
      remainder: input[remainder.startIndex..<remainder.endIndex],
      errorCount: errorCount)
  }
}

/// Parsing multiple unicode scalar values
extension UnicodeEncoding {
  /// Returns the number of code units required for the given code unit
  /// sequence when transcoded to UTF-16, and a Boolean value indicating
  /// whether the sequence was found to contain only ASCII characters.
  ///
  /// The following example finds the length of the UTF-16 encoding of the
  /// string `"Fermata 𝄐"`, starting with its UTF-8 representation.
  ///
  ///     let fermata = "Fermata 𝄐"
  ///     let bytes = fermata.utf8
  ///     print(Array(bytes))
  ///     // Prints "[70, 101, 114, 109, 97, 116, 97, 32, 240, 157, 132, 144]"
  ///
  ///     let result = transcodedLength(of: bytes.makeIterator(),
  ///                                   decodedAs: UTF8.self,
  ///                                   repairingIllFormedSequences: false)
  ///     print(result)
  ///     // Prints "Optional((10, false))"
  ///
  /// - Parameters:
  ///   - input: An iterator of code units to be translated, encoded as
  ///     `sourceEncoding`. If `repairingIllFormedSequences` is `true`, the
  ///     entire iterator will be exhausted. Otherwise, iteration will stop if
  ///     an ill-formed sequence is detected.
  ///   - sourceEncoding: The Unicode encoding of `input`.
  ///   - repairingIllFormedSequences: Pass `true` to measure the length of
  ///     `input` even when `input` contains ill-formed sequences. Each
  ///     ill-formed sequence is replaced with a Unicode replacement character
  ///     (`"\u{FFFD}"`) and is measured as such. Pass `false` to immediately
  ///     stop measuring `input` when an ill-formed sequence is encountered.
  /// - Returns: A tuple containing the number of UTF-16 code units required to
  ///   encode `input` and a Boolean value that indicates whether the `input`
  ///   contained only ASCII characters. If `repairingIllFormedSequences` is
  ///   `false` and an ill-formed sequence is detected, this method returns
  ///   `nil`.
  public static func transcodedLength<Input, Encoding>(
    of input: Input,
    decodedAs sourceEncoding: Encoding.Type,
    repairingIllFormedSequences makeRepairs: Bool
  ) -> (count: Int, isASCII: Bool)?
  where
    Input : IteratorProtocol,
    Encoding : UnicodeEncoding,
    Encoding.EncodedScalar.Iterator.Element == Input.Element {

    var isASCII = true
    var count = 0
    let buffer = Array(IteratorSequence(input))
    let (_, errorCount) = Encoding.parseForward(
      buffer,
      repairingIllFormedSequences: makeRepairs
    ) {
      s in 
      let u32 = s.utf32.first!
      isASCII = isASCII && (u32 <= 0x7f)
      if let encoded = EncodedScalar(UnicodeScalar(u32)!) {
        count += numericCast(encoded.count)
      }
    }

    if errorCount != 0 && !makeRepairs { return nil }
    return (count, isASCII)
  }
  
  /// Parse a whole collection efficiently, writing results into `output`.
  ///
  /// - Returns: a pair consisting of:
  ///   0. the suffix of input starting with the first decoding error if
  ///      `stopOnError` is true, and the empty suffix otherwise.
  ///   1. The number of errors that were detected.  If `stopOnError` is true
  ///      this value will never exceed 1.
  ///
  /// - Note: using this function may be faster than repeatedly using `parse`
  ///   directly, because it avoids intra-scalar checks for end of sequence.
  @discardableResult
  public static func parseForward<C: Collection>(
    _ input: C,
    repairingIllFormedSequences makeRepairs: Bool = true,
    into output: (EncodedScalar)->Void
  ) -> (remainder: C.SubSequence, errorCount: Int)
  where C.SubSequence : Collection, C.SubSequence.SubSequence == C.SubSequence,
    C.SubSequence.Iterator.Element == EncodedScalar.Iterator.Element {
    var remainder = input[input.startIndex..<input.endIndex]
    var errorCount = 0
    
    func eat(
      _ o: ParseResult<EncodedScalar, C.SubSequence.Index>) -> EncodedScalar? {
      if case .valid(let scalar, let resumptionPoint) = o {
        remainder = remainder.suffix(from: resumptionPoint)
        return scalar
      }
      else if case .error(let resumptionPoint) = o {
        errorCount += 1
        if !makeRepairs { return nil }
        remainder = remainder.suffix(from: resumptionPoint)
        return EncodedScalar(UnicodeScalar(0xFFFD as UInt32)!)
      }
      return nil
    }

    let chunkSize = maxLengthOfEncodedScalar
    
    // Repeatedly handle as many as possible without checking for end-of-input.
    while remainder.count >= numericCast(chunkSize) {
      // This loop could be unrolled, obviously
      for _ in 0 ..< numericCast(remainder.count) / chunkSize {
        guard let o = eat(
          parse1Forward(remainder, knownCount: Int(chunkSize))
        ) else {
          return (remainder, errorCount)
        }
        output(o)
      }
    }

    // Handle whatever is left
    while true {
      guard let o = eat(parse1Forward(remainder)) else {
        return (remainder, errorCount)
      }
      output(o)
    }
  }

  /// Parse a whole collection efficiently in reverse, writing results into
  /// `output`.
  ///
  /// - Returns: a pair consisting of:
  ///   0. the suffix of input starting with the first decoding error if
  ///      `stopOnError` is true, and the empty suffix otherwise.
  ///   1. The number of errors that were detected.  If `stopOnError` is true
  ///      this value will never exceed 1.
  ///
  /// - Note: using this function may be faster than repeatedly using `parse`
  ///   directly, because it avoids intra-scalar checks for end of sequence.
  @discardableResult
  public static func parseReverse<C: BidirectionalCollection>(
    _ input: C,
    repairingIllFormedSequences makeRepairs: Bool = true,
    into output: (EncodedScalar)->Void
  ) -> (remainder: C.SubSequence, errorCount: Int)
  where C.SubSequence : BidirectionalCollection,
        C.SubSequence.SubSequence == C.SubSequence,
        C.SubSequence.Iterator.Element == EncodedScalar.Iterator.Element {
    var remainder = input[input.startIndex..<input.endIndex]
    var errorCount = 0
    
    func eat(
      _ o: ParseResult<EncodedScalar, C.SubSequence.Index>) -> EncodedScalar? {
      if case .valid(let scalar, let resumptionPoint) = o {
        remainder = remainder.prefix(upTo: resumptionPoint)
        return scalar
      }
      else if case .error(let resumptionPoint) = o {
        errorCount += 1
        if !makeRepairs { return nil }
        remainder = remainder.prefix(upTo: resumptionPoint)
        return EncodedScalar(UnicodeScalar(0xFFFD as UInt32)!)
      }
      return nil
    }

    let chunkSize = maxLengthOfEncodedScalar
    
    // Repeatedly handle as many as possible without checking for end-of-input.
    while remainder.count >= numericCast(chunkSize) {
      // This loop could be unrolled, obviously
      for _ in 0 ..< numericCast(remainder.count) / chunkSize {
        guard let o = eat(
          parse1Reverse(remainder, knownCount: Int(chunkSize))
        ) else {
          return (remainder, errorCount)
        }
        output(o)
      }
    }

    // Handle whatever is left
    while true {
      guard let o = eat(parse1Reverse(remainder)) else {
        return (remainder, errorCount)
      }
      output(o)
    }
  }
  // FIXME: without inducing a speed penalty, can we collapse the logic for
  // parseReverse and parseForward by using a reverse collection view and
  // changing the logic for the low level parsing routine to use forward
  // traversal?  Run the experiment.
}

// UTF-8 format
// ############
//
// Bits | Scalar Range       | Byte 0    | Byte 1    | Byte 2    | Byte 3   
// =====+====================+===========+===========+===========+==========
//   7  |  U+0000...U+007F   | 0_xxxxxxx |           |           |          
// -----+--------------------+-----------+-----------+-----------+----------
//  11  |  U+0080...U+07FF   | 110_XXXXx | 10_xxxxxx |           |          
// -----+--------------------+-----------+-----------+-----------+----------
//  16  |  U+0800...U+FFFF   | 1110_XXXX | 10_Xxxxxx | 10_xxxxxx |          
// -----+--------------------+-----------+-----------+-----------+----------
//  21  | U+10000...U+10FFFF | 11110_YYY | 10_YYxxxx | 10_xxxxxx | 10_xxxxxx
// -----+--------------------+-----------+-----------+-----------+----------
//
// The YYYs in a 4-byte sequence must be in the range 00001...10000
// In rows where capital Xs appear, at least one of those bits must be set.
//
// 3-byte sequences must rule out U+D800...U+DFFF, i.e.
//
//    1110_1101 10_100000 ... 1110_1101 10_111111

public enum UTF8 : UnicodeEncoding {

  @inline(__always)
  static internal func _isASCII(_ x: UInt8) -> Bool {
    return _fastPath(Int8(bitPattern: x) >= 0)
  }
  
  public typealias CodeUnit = UInt8

  public static var maxLengthOfEncodedScalar: UInt { return 4 }
  
  /// Returns `true` iff [`c0`, `c1`] is a prefix of a valid 3-byte sequence
  static internal func isValid3BytePrefix(_ c0: CodeUnit, _ c1: CodeUnit) -> Bool {
    let joint = UInt16(c0) << 8 | UInt16(c1)
    return 0b1110_0000__10_100000...0b1110_1101__10_011111 ~= joint
        || 0b1110_1110__10_000000...0b1110_1111__10_111111 ~= joint
  }

  /// Returns true iff [`c0`, `c1`] is a prefix of a valid 4-byte sequence
  static internal func isValid4BytePrefix(_ c0: CodeUnit, _ c1: CodeUnit) -> Bool {
    let joint = UInt16(c0) << 8 | UInt16(c1)
    return 0b11110_000__10_010000...0b11110_100__10_001111 ~= joint
  }

  /// Returns true iff [`c0`, `c1`] is a prefix of a valid sequence
  static internal func isValidPrefix(_ c0: CodeUnit, _ c1: CodeUnit) -> Bool {
    return isValid3BytePrefix(c0, c1) || isValid4BytePrefix(c0, c1)
  }

  /// Given a valid leading byte of a multibyte sequence, strip the leading 1
  /// bits.
  ///
  /// - Note: Given any other byte, the result is unspecified.
  static internal func maskLeadByte(_ x: UInt8) -> UInt8 {
    return x & (0b11111 >> (x >> 5 & 1))
  }
  
  public static func encode<T: EncodedScalarProtocol>(
    _ other:T) -> UTF8.EncodedScalar? {
    return other.utf8
  }

  public static func parse1Forward<C: Collection>(
    _ input: C, knownCount knownCount_: Int = 0
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == UTF8.CodeUnit {

    var knownCount = knownCount_
    
    // See
    // https://gist.github.com/dabrahams/1880044370a192ae51c263a93f25a4c5#gistcomment-1931947
    // for an explanation of this state machine.
    if _slowPath(knownCount <= 0 && input.isEmpty) { return .emptyInput }
    
    var i = input.startIndex 
    let end = input.endIndex
    
    // Returns `true` iff there is no more input
    @inline(__always) func inputConsumed() -> Bool {
      return _slowPath(knownCount <= 0 && i == end)
    }
    
    let u0 = input[i]
    var j = input.index(after: i)
    
    if _isASCII(u0) {
      return .valid(EncodedScalar(u0), resumptionPoint: j)
    }
    i = j // even if there are errors, we eat 1 byte
    knownCount -= 1

    // Begin accumulating result
    var r = UInt32(u0)
    var shift: UInt32 = 0
    
    // Marks one more token recognized and gets the next lookahead token iff it
    // falls within pattern.  Returns `false` otherwise.
    @inline (__always)
    func nextContinuation(_ pattern: ClosedRange<UInt8>) -> Bool {
      i = j
      if inputConsumed() { return false } // no more tokens
      let u = input[j]
      if _fastPath(pattern ~= u) {
        shift += 8
        r |= UInt32(u) << shift
        j = input.index(after: j)
        knownCount -= 1
        return true
      }
      return false
    }
    
    @inline(__always)
    func state7() -> ParseResult<EncodedScalar, C.Index> {
      return nextContinuation(0x80...0xbf)
      ? .valid(EncodedScalar(_bits: r), resumptionPoint: j)
      : .error(resumptionPoint: i)
    }
    
    @inline(__always)
    func state3() -> ParseResult<EncodedScalar, C.Index> {
      return nextContinuation(0x80...0xbf)
        ? state7() : .error(resumptionPoint: i)
    }
    
    // Two-byte case
    if _fastPath(0xc2...0xdf ~= u0) {
      return state7()
    }

    // Three-byte cases
    else if _fastPath(u0 == 0xe0) {
      if nextContinuation(0xa0...0xbf) { return state7() }
    }
    else if _fastPath(u0 == 0xed) { // state 2
      if nextContinuation(0x80...0x9f) { return state7() }
    }
    else if _fastPath(0xe1...0xef ~= u0) { return state3() }
    
    // Four-byte cases
    else if _fastPath(0xf1...0xf3 ~= u0) { // state 5
      if nextContinuation(0x80...0xbf) { return state3() }
    }
    else if u0 == 0xf0 {
      if nextContinuation(0x90...0xbf) { return state3() }
    }
    else if u0 == 0xf4 {
      if nextContinuation(0x80...0x8f) { return state3() }
    }
    
    return .error(resumptionPoint: i)
  }

  public static func  parse1Reverse<C: BidirectionalCollection>(
    _ input: C, knownCount knownCount_: Int = 0
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == UTF8.CodeUnit,
  // FIXME: drop these constraints once we have the compiler features.
  C.SubSequence.Index == C.Index,
  C.SubSequence.Iterator.Element == C.Iterator.Element
  {
    // See
    // https://gist.github.com/dabrahams/1880044370a192ae51c263a93f25a4c5#gistcomment-1931947
    // for an explanation of this state machine.

    var knownCount = knownCount_
    if _slowPath(knownCount <= 0 && input.isEmpty) { return .emptyInput }

    var i = input.index(before: input.endIndex)
    var j = i
    let j0 = j
    var u = input[j]
    if _isASCII(u) {
      return .valid(EncodedScalar(u), resumptionPoint: j)
    }
    knownCount -= 1
    
    let start = input.startIndex
    var r = UInt32(u)

    // Mark one more token recognized and get the next lookahead token iff it
    // satisfies the predicate
    @inline(__always)
    func consumeContinuation(_ pattern: ClosedRange<UInt8>) -> Bool {
      guard _fastPath(pattern ~= u) else { return false }
      i = j
      guard _fastPath(knownCount > 0 || j != start) else { return false }
      r <<= 8
      r |= UInt32(u)
      j = input.index(before: j)
      u = input[j]
      knownCount -= 1
      return true
    }

    @inline(__always)
    func accept(_ pat: ClosedRange<UInt8>) -> ParseResult<EncodedScalar, C.Index>? {
      if _fastPath(pat.contains(u)) {
        r <<= 8
        r |= UInt32(u)
        return .valid(EncodedScalar(_bits: r), resumptionPoint: j)
      }
      return nil
    }
    
    @inline(__always)
    func state4_5() -> ParseResult<EncodedScalar, C.Index> {
      return accept(0xf0...0xf3) ?? .error(resumptionPoint: j0)
    }
    
    @inline(__always)
    func state5_6() -> ParseResult<EncodedScalar, C.Index> {
      return accept(0xf1...0xf4) ?? .error(resumptionPoint: j0)
    }

    let u0 = u
    if consumeContinuation(0x80...0xbf) {          // state 7
      if let x = accept(0xc2...0xdf)                     { return x }

      let u1 = u
      if consumeContinuation(0x80...0x9f) {                         // state 2/3
        if let x = accept(0xe1...0xef)                              { return x }
        if consumeContinuation(0x90...0xbf)                { return state4_5() }
        if consumeContinuation(0x80...0x8f)                { return state5_6() }
        if isValid4BytePrefix(u, u1)       { return .error(resumptionPoint: j) }
      }
      else if consumeContinuation(0xa0...0xbf) {                    // state 1/3
        if let x = accept(0xe0...0xec)                              { return x }
        if let x = accept(0xee...0xef)                              { return x }
        if consumeContinuation(0x90...0xbf)                { return state4_5() }
        if consumeContinuation(0x80...0x8f)                { return state5_6() }
        if isValid4BytePrefix(u, u1)       { return .error(resumptionPoint: j) }
      }
      else if isValidPrefix(u, u0)         { return .error(resumptionPoint: j) }
    }
    return .error(resumptionPoint: j0)
  }
  // FIXME: without inducing a speed penalty, can we collapse the logic for
  // parseReverse and parseForward by using a reverse collection view and
  // changing the logic for the low level parsing routine to use forward
  // traversal?  Run the experiment.

  //===--- Swift 3 compatibility ------------------------------------------===//
case _swift3Buffer(bits: UInt32, bitCount: UInt32)
}

extension UTF8 {
  /// Given a valid lead byte, return the expected length of the whole encoded
  /// Unicode scalar value.
  static internal func _encodedLength(leadByte: UInt8) -> UInt8 {
    let table: UInt64 = 0x4322000011111111
    let shift = UInt64((leadByte >> 4) << 2)
    return UInt8((table >> shift) & 0xf)
  }
  
  public struct EncodedScalar : RandomAccessCollection {
    internal let _bits: UInt32
    public var startIndex: UInt8 { return 0 }
    
    public var endIndex: UInt8 {
      let lowByte = UInt8(truncatingBitPattern: _bits)
      return UTF8._encodedLength(leadByte: lowByte)
    }
    
    internal init(_ _0: CodeUnit) {
      _bits = UInt32(_0)
      _sanityCheck(count == 1)
    }
    
    internal init(_ _0: CodeUnit, _ _1: CodeUnit) {
      _bits = UInt32(_1) << 8 | UInt32(_0)
      _sanityCheck(count == 2)
    }
    
    internal init(_ _0: CodeUnit, _ _1: CodeUnit, _ _2: CodeUnit) {
      _bits = (UInt32(_2) << 8 | UInt32(_1)) << 8 | UInt32(_0)
      _sanityCheck(count == 3)
    }
    
    internal init(_ _0: CodeUnit, _ _1: CodeUnit, _ _2: CodeUnit, _ _3: CodeUnit) {
      _bits = ((UInt32(_3) << 8 | UInt32(_2)) << 8 | UInt32(_1)) << 8
        | UInt32(_0)
      _sanityCheck(count == 4)
    }
    
    internal init(_bits: UInt32) {
      self._bits = _bits
    }
    
    public typealias Index = UInt8
    public subscript(i: Index) -> UInt8 {
      return UInt8(
        truncatingBitPattern: _bits >> (UInt32(i & (32 - 1)) << 3))
    }
  }
}

public enum ValidUTF8 : UnicodeEncoding {
  public typealias EncodedScalar = UTF8.EncodedScalar
  
  public static var maxLengthOfEncodedScalar: UInt {
    return UTF8.maxLengthOfEncodedScalar
  }

  public static func encode<Scalar: EncodedScalarProtocol>(
    _ other: Scalar
  ) -> ValidUTF8.EncodedScalar? {
    return other.utf8
  }

  public static func parse1Forward<C: Collection>(
    _ input: C,
    knownCount: Int = 0
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == EncodedScalar.Iterator.Element {
    if _slowPath(knownCount <= 0 && input.isEmpty) { return .emptyInput }

    var i = input.startIndex 

    func next() -> UInt8 {
      defer { i = input.index(after: i) }
      return input[i]
    }
    
    let u0 = next()
    if UTF8._isASCII(u0) {
      return .valid(EncodedScalar(u0), resumptionPoint: i)
    }
    let u1 = next()
    if _fastPath(u0 <= 0b110_11111) {
      return .valid(EncodedScalar(u0, u1), resumptionPoint: i)
    }
    let u2 = next()
    if _fastPath(u0 <= 0b1110_1111) {
      return .valid(EncodedScalar(u0, u1, u2), resumptionPoint: i)
    }
    let u3 = next()
    return .valid(EncodedScalar(u0, u1, u2, u3), resumptionPoint: i)
  }

  public static func parse1Reverse<C: BidirectionalCollection>(
    _ input: C, knownCount: Int = 0
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == EncodedScalar.Iterator.Element,
  // FIXME: drop these constraints once we have the compiler features.
  C.SubSequence.Index == C.Index,
  C.SubSequence.Iterator.Element == C.Iterator.Element {
    if _slowPath(knownCount <= 0 && input.isEmpty) { return .emptyInput }

    var i = input.endIndex

    func next() -> UInt8 {
      i = input.index(before: i)
      return input[i]
    }
    
    let u0 = next()
    if UTF8._isASCII(u0) {
      return .valid(EncodedScalar(u0), resumptionPoint: i)
    }
    let u1 = next()
    if _fastPath(u1 > 0b10_111111) {
      return .valid(EncodedScalar(u1, u0), resumptionPoint: i)
    }
    let u2 = next()
    if _fastPath(u2 > 0b10_111111) {
      return .valid(EncodedScalar(u2, u1, u0), resumptionPoint: i)
    }
    let u3 = next()
    return .valid(EncodedScalar(u3, u2, u1, u0), resumptionPoint: i)
  }
}

// UTF8 <-> UTF16
// ==============
//
//  U+0000...U+007F                                  0_xxxxxxx
//                                          00000000_0_xxxxxxx
//  U+0080...U+07FF                        110_wwwww 10_xxxxxx
//                                         00000_www_ww_xxxxxx
//  U+0800...U+FFFF:             1110_wwww 10_xxxxxx 10_yyyyyy
//                                         wwww_xxxx_xx_yyyyyy
// U+10000...U+10FFFF: 11110_www 10_xxxxxX 10_yyyyyy 10_zzzzzz
//                     wwww_xxxx_xx_yyyyyy

// UTF16
// =====
//
// U+0000...U+FFFF        xxxxxxxx_xxxxxxxx
// U+10000...U+10FFFF     xxxxxxxx_xxxxxxxx


public protocol EncodedScalarProtocol : RandomAccessCollection {
  init?(_ scalarValue: UnicodeScalar)
  var utf8: UTF8.EncodedScalar { get }
  var utf16: UTF16.EncodedScalar { get }
  var utf32: UTF32.EncodedScalar { get }
}

public extension UnicodeScalar {
  public init<E: EncodedScalarProtocol>(_ e: E) {
    self = UnicodeScalar(_unchecked: e.utf32[0])
  }
}

extension UTF8.EncodedScalar : EncodedScalarProtocol {
  public init(_ scalarValue: UnicodeScalar) {
    self = UTF32.EncodedScalar(scalarValue).utf8
  }  
  public var utf8: UTF8.EncodedScalar { return self }
  public var utf16: UTF16.EncodedScalar {
    return utf32.utf16
  }
  public var utf32: UTF32.EncodedScalar {
    if _fastPath(_bits <= 0x7f) {
      return UTF32.EncodedScalar(_bits: _bits)
    }
    var r = UInt32(UTF8.maskLeadByte(UInt8(truncatingBitPattern: _bits)))
    for b in self[1..<endIndex] {
      r <<= 6
      r |= UInt32(b & ((1 << 6) - 1))
    }
    return UTF32.EncodedScalar(_bits: r)
  }
}

public enum UTF16 : UnicodeEncoding {
  public typealias CodeUnit = UInt16
  
  public static var maxLengthOfEncodedScalar: UInt { return 2 }
  
  /// Returns the decoded scalar value of a valid surrogate pair
  internal static func decodeValid(_ unit0: CodeUnit, _ unit1: CodeUnit) -> UInt32 {
    // [1101 10xx xxxx xxxx] [1101 11xx xxxx xxxx]
    return 0x10000 + ((UInt32(unit0 & 0x03ff) << 10) | UInt32(unit1 & 0x03ff))
  }
  
  public static func encode<T: EncodedScalarProtocol>(
    _ other:T
  ) -> UTF16.EncodedScalar? {
    return other.utf16
  }

  public static func parse1Forward<C: Collection>(
    _ input: C, knownCount: Int = 0
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == CodeUnit {

    if _slowPath(knownCount <= 0 && input.isEmpty) {
      return .emptyInput
    }
    let i0 = input.startIndex
    let unit0 = input[i0]
    let end = input.endIndex
    let i1 = input.index(after: i0)
    
    // A well-formed pair of surrogates looks like this:
    //     high-surrogate        low-surrogate
    // [1101 10xx xxxx xxxx] [1101 11xx xxxx xxxx]

    // Common case first, non-surrogate -- just a sequence of 1 code unit.
    if _fastPath((unit0 >> 11) != 0b1101_1) {
      return .valid(EncodedScalar(unit0), resumptionPoint: i1)
    }

    // Ensure `unit0` is a high-surrogate and there's another byte which is a
    // low-surrogate
    if _fastPath(
      (unit0 >> 10) == 0b1101_10 && (knownCount > 1 || i1 != end)),
      let unit1 = Optional(input[i1]),
      _fastPath((unit1 >> 10) == 0b1101_11) {
      return .valid(
        EncodedScalar(unit0, unit1),
        resumptionPoint: input.index(after: i1)
      )
    }
    return .error(resumptionPoint: i1)
  }

  public static func parse1Reverse<C: BidirectionalCollection>(
    _ input: C, knownCount: Int = 0
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == CodeUnit,
  // FIXME: drop these constraints once we have the compiler features.
  C.SubSequence.Index == C.Index,
  C.SubSequence.Iterator.Element == C.Iterator.Element
  {
    if _slowPath(knownCount <= 0 && input.isEmpty) { return .emptyInput }

    let i1 = input.index(before: input.endIndex)
    let unit1 = input[i1]
    
    // A well-formed pair of surrogates looks like this:
    //     high-surrogate        low-surrogate
    // [1101 10xx xxxx xxxx] [1101 11xx xxxx xxxx]

    // Common case first, non-surrogate -- just a sequence of 1 code unit.
    if _fastPath((unit1 >> 11) != 0b1101_1) {
      return .valid(EncodedScalar(unit1), resumptionPoint: i1)
    }
    let start = input.startIndex

    // Ensure `unit1` is a low-surrogate and there's another byte which is a
    // high-surrogate
    if _fastPath(
      (unit1 >> 10) == 0b1101_11 && (knownCount > 1 || i1 != start)),
      let i0 = Optional(input.index(before: i1)),
      let unit0 = Optional(input[i0]),
      _fastPath((unit0 >> 10) == 0b1101_10) {
      return .valid(
        EncodedScalar(unit0, unit1),        
        resumptionPoint: i0
      )
    }
    return .error(resumptionPoint: i1)
  }
  
  //===--- Swift 3 compatibility ------------------------------------------===//
case _swift3Buffer(bits: UInt32, bitCount: UInt32)
}

extension UTF16 {
  public struct EncodedScalar : RandomAccessCollection {
    internal let _bits: UInt32
    public var startIndex: UInt8 { return 0 }
    
    public var endIndex: UInt8 {
      return UInt8(truncatingBitPattern: self[1] >> 15) + 1
    }
    
    internal init(_ _0: CodeUnit) {
      _bits = UInt32(_0)
    }
    
    internal init(_ _0: CodeUnit, _ _1: CodeUnit) {
      _bits = UInt32(_1) << 16 | UInt32(_0)
    }
    
    internal init(_bits: UInt32) {
      self._bits = _bits
    }
    
    public typealias Index = UInt8
    public subscript(i: Index) -> UInt16 {
      return UInt16(
        truncatingBitPattern: _bits >> UInt32((i & 1) << 4))
    }
  }
}

extension UTF16.EncodedScalar : EncodedScalarProtocol {
  public init(_ scalarValue: UnicodeScalar) {
    self = UTF32.EncodedScalar(scalarValue).utf16
  }
  public var utf8: UTF8.EncodedScalar {
    return utf32.utf8
  }
  public var utf16: UTF16.EncodedScalar {
    return self
  }
  public var utf32: UTF32.EncodedScalar {
    if _fastPath(_bits >> 16 == 0) {
      return UTF32.EncodedScalar(_bits)
    }
    return UTF32.EncodedScalar(UTF16.decodeValid(self[0], self[1]))
  }
}

public enum ValidUTF16 : UnicodeEncoding {
  public typealias CodeUnit = UTF16.CodeUnit
  public typealias EncodedScalar = UTF16.EncodedScalar
  
  public static var maxLengthOfEncodedScalar: UInt {
    return UTF16.maxLengthOfEncodedScalar
  }
  
  public static func encode<Scalar : EncodedScalarProtocol>(
    _ other: Scalar
  ) -> ValidUTF16.EncodedScalar? {
    return other.utf16
  }

  public static func parse1Forward<C: Collection>(
    _ input: C, knownCount: Int = 0
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == CodeUnit {

    if _slowPath(knownCount <= 0 && input.isEmpty) {
      return .emptyInput
    }
    let i0 = input.startIndex
    let unit0 = input[i0]
    let i1 = input.index(after: i0)
    
    // A well-formed pair of surrogates looks like this:
    //     high-surrogate        low-surrogate
    // [1101 10xx xxxx xxxx] [1101 11xx xxxx xxxx]

    // Common case first, non-surrogate -- just a sequence of 1 code unit.
    if _fastPath((unit0 >> 11) != 0b1101_1) {
      return .valid(EncodedScalar(unit0), resumptionPoint: i1)
    }

    let unit1 = input[i1]
    return .valid(EncodedScalar(unit0, unit1),
      resumptionPoint: input.index(after: i1))
  }

  public static func parse1Reverse<C: BidirectionalCollection>(
    _ input: C, knownCount: Int = 0
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == CodeUnit,
  // FIXME: drop these constraints once we have the compiler features.
  C.SubSequence.Index == C.Index,
  C.SubSequence.Iterator.Element == C.Iterator.Element
  {
    if _slowPath(knownCount <= 0 && input.isEmpty) { return .emptyInput }

    let i1 = input.index(before: input.endIndex)
    let unit1 = input[i1]
    
    // A well-formed pair of surrogates looks like this:
    //     high-surrogate        low-surrogate
    // [1101 10xx xxxx xxxx] [1101 11xx xxxx xxxx]

    // Common case first, non-surrogate -- just a sequence of 1 code unit.
    if _fastPath((unit1 >> 11) != 0b1101_1) {
      return .valid(EncodedScalar(unit1), resumptionPoint: i1)
    }

    let i0 = input.index(before: i1)
    let unit0 = input[i0]
    return .valid(EncodedScalar(unit0, unit1), resumptionPoint: i0)
  }
}

public enum UTF32 : UnicodeEncoding {
  public typealias CodeUnit = UInt32
  
  public static var maxLengthOfEncodedScalar: UInt { return 1 }

  internal static func _isValid(_ u: CodeUnit) -> Bool {
    return u <= 0xD7FF || 0xE000...0x10FFFF ~= u
  }
  
  public static func encode<Scalar : EncodedScalarProtocol>(
    _ other: Scalar
  ) -> UTF32.EncodedScalar? {
    return other.utf32
  }

  public static func parse1Forward<C: Collection>(
    _ input: C, knownCount: Int = 0
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == CodeUnit {
    if _slowPath(knownCount <= 0 && input.isEmpty) {
      return .emptyInput
    }
    let i0 = input.startIndex
    let unit0 = input[i0]
    let i1 = input.index(after: i0)
    
    return _isValid(unit0) ? .valid(EncodedScalar(unit0), resumptionPoint: i1)
      : .error(resumptionPoint: i1)
  }

  public static func parse1Reverse<C: BidirectionalCollection>(
    _ input: C, knownCount: Int = 0
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == CodeUnit,
  // FIXME: drop these constraints once we have the compiler features.
  C.SubSequence.Index == C.Index,
  C.SubSequence.Iterator.Element == C.Iterator.Element
  {
    if _slowPath(knownCount <= 0 && input.isEmpty) { return .emptyInput }

    let i0 = input.index(before: input.endIndex)
    let unit0 = input[i0]
    
    return _isValid(unit0) ? .valid(EncodedScalar(unit0), resumptionPoint: i0)
      : .error(resumptionPoint: i0)
  }
  //===--- Swift 3 compatibility ------------------------------------------===//
  case _swift3
}

extension UTF32 {
  public struct EncodedScalar : RandomAccessCollection {
    public typealias Index = UInt8
    public var startIndex: UInt8 { return 0 }
    public var endIndex: UInt8 { return 1 }
    internal let _bits: UInt32

    internal init(_ _0: CodeUnit) {
      _bits = _0
    }
    internal init(_bits: UInt32) {
      self._bits = _bits
    }
    public subscript(i: Index) -> UInt32 {
      return _bits
    }
  }
}

extension UTF32.EncodedScalar : EncodedScalarProtocol {
  public init(_ scalarValue: UnicodeScalar) {
    _bits = scalarValue.value
  }
  
  public var utf8: UTF8.EncodedScalar {
    if _fastPath(_bits <= 0x7f) { return UTF8.EncodedScalar(_bits: _bits) }
    if _fastPath(_bits <= 0x7ff) {
      return UTF8.EncodedScalar(
        0b110_00000 | UInt8(truncatingBitPattern: _bits >> 6),
        0b10_000000 | UInt8(truncatingBitPattern: _bits) & 0b00_111111
      )
    }
    if _fastPath(_bits >> 16 == 0) {
      return UTF8.EncodedScalar(
        0b1110_0000 | UInt8(truncatingBitPattern: _bits >> 12),
        0b10_000000 | UInt8(truncatingBitPattern: _bits >> 6) & 0b00_111111,
        0b10_000000 | UInt8(truncatingBitPattern: _bits) & 0b00_111111
      )
    }
    return UTF8.EncodedScalar(
      0b11110_000 | UInt8(truncatingBitPattern: _bits >> 18),
      0b10_000000 | UInt8(truncatingBitPattern: _bits >> 12) & 0b00_111111,
      0b10_000000 | UInt8(truncatingBitPattern: _bits >> 6) & 0b00_111111,
      0b10_000000 | UInt8(truncatingBitPattern: _bits) & 0b00_111111
    )
  }
  public var utf16: UTF16.EncodedScalar {
    if _fastPath(_bits >> 16 == 0) {
      return UTF16.EncodedScalar(_bits: _bits)
    }
    let hl = _bits - 0x10000
    return UTF16.EncodedScalar(
      UInt16(truncatingBitPattern: hl >> 10 + 0xD800),
      UInt16(truncatingBitPattern: hl & (1 << 10 - 1) + 0xDC00)
    )
  }
  public var utf32: UTF32.EncodedScalar {
    return self
  }
}

public enum Latin1 : UnicodeEncoding {
  public static var maxLengthOfEncodedScalar: UInt { return 1 }
  
  /// A type that can represent a single UnicodeScalar as it is encoded in this
  /// encoding.
  public struct EncodedScalar : EncodedScalarProtocol {
    public init?(_ value: UnicodeScalar) {
      guard value.value <= 0xff else { return nil }
      self.value = UInt8(value.value)
    }
    init(_ value: UInt8) {
      self.value = value
    }
    public var startIndex : UInt8 {
      return 0
    }
    public var endIndex : UInt8 {
      return 1
    }
    public subscript(i: UInt8) -> UInt8 {
      return value
    }
    let value: UInt8
    
    public var utf8 : UTF8.EncodedScalar { return UTF8.EncodedScalar(value) }
    public var utf16 : UTF16.EncodedScalar { return UTF16.EncodedScalar(UInt16(value)) }
    public var utf32 : UTF32.EncodedScalar { return UTF32.EncodedScalar(UInt32(value)) }
  }
  
  public static func encode<Scalar: EncodedScalarProtocol>(
    _ s: Scalar
  ) -> EncodedScalar? {
    let u32 = s.utf32.first!
    return u32 <= 0xff ? EncodedScalar(UInt8(u32)) : nil
  }
  
  /// Parse a single unicode scalar forward from `input`.
  public static func parse1Forward<C: Collection>(
    _ input: C, knownCount: Int /* = 0 */
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == EncodedScalar.Iterator.Element {
    return input.isEmpty ? .emptyInput : .valid(
      EncodedScalar(input.first!),
      resumptionPoint: input.index(after: input.startIndex))
  }

  public static func parse1Reverse<C: BidirectionalCollection>(
    _ input: C, knownCount: Int/* = 0 */
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == EncodedScalar.Iterator.Element,
  // FIXME: drop these constraints once we have the compiler features.
  C.SubSequence.Index == C.Index,
  C.SubSequence.Iterator.Element == C.Iterator.Element {
    return input.isEmpty ? .emptyInput : .valid(
      EncodedScalar(input.last!),
      resumptionPoint: input.index(before: input.endIndex))
  }
}

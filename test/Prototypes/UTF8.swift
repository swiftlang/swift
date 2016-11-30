//===--- UTF8.swift - UTF8 Transcoding Schemes ----------------------------===//
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
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

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

/// The result of transcoding from a collection using a “pull-style” API.
///
/// - Associated value fields:
///  - `output` is the result of error-free transcoding
///  - `resumptionPoint` indicates a position in the input from which to continue
///    transcoding.
///
/// - Parameter T: the result of a successful parse
/// - Parameter Index: the index type of the collection being transcoded.
enum ParseResult<T, Index> {
case valid(T, resumptionPoint: Index)
case error(resumptionPoint: Index)
case emptyInput
  var resumptionPoint: Index? {
    switch self {
    case .valid(_,let r): return r
    case .error(let r): return r
    case .emptyInput: return nil
    }
  }
  var valid : T? {
    if case .valid(let r,_) = self { return r }
    return nil
  }
}

extension UTF8 {
  enum Classification : UInt8 {
    @inline(__always)
    init(_ u: CodeUnit) {
      let lz = _leadingZeros((~u)._value)
      self = Classification(rawValue: UInt8(lz)).unsafelyUnwrapped
    }
  case
    // Name          Pattern    Notes
    ascii,        // 0xxxxxxx   A 1-code-unit Unicode scalar
    continuation, // 10xxxxxx   Sequences of 1...3 of these follow start2...4
    start2,       // 110xxxxY   Start of a 2 byte scalar, where xxxx != 0
    start3,       // 1110xxxx   Start of a 3 byte scalar
    start4,       // 11110xxx   Start of a 4 byte scalar
    invalid1,     // 111110xx
    invalid2,     // 1111110x
    invalid3,     // 11111110
    invalid4      // 11111111
  }

  // The minimum start of a multibyte code unit
  static let minMultibyteStarter: CodeUnit = 0b110_00010

  /// Returns `true` iff [`c0`, `c1`] is a prefix of a valid 3-byte sequence
  static func isValid3BytePrefix(_ c0: CodeUnit, _ c1: CodeUnit) -> Bool {
    let joint = UInt16(c0) << 8 | UInt16(c1)
    return 0b1110_0000__10_100000...0b1110_1101__10_011111 ~= joint
        || 0b1110_1110__10_000000...0b1110_1111__10_111111 ~= joint
  }

  /// Returns true iff [`c0`, `c1`] is a prefix of a valid 4-byte sequence
  static func isValid4BytePrefix(_ c0: CodeUnit, _ c1: CodeUnit) -> Bool {
    let joint = UInt16(c0) << 8 | UInt16(c1)
    return 0b11110_000__10_010000...0b11110_100__10_001111 ~= joint
  }

  /// Returns true iff [`c0`, `c1`] is a prefix of a valid sequence
  static func isValidPrefix(_ c0: CodeUnit, _ c1: CodeUnit) -> Bool {
    return isValid3BytePrefix(c0, c1) || isValid4BytePrefix(c0, c1)
  }

  // FIXME: Benchmark parse1ForwardOpenCoded vs parse1Forward and decide which
  // implementation strategy to keep.

  /// Parses one scalar forward from `input`.
  ///
  /// - Parameters:
  ///
  ///   - input: the collection from which UTF-8 code units will be read
  ///
  ///   - knownValid: true if and only if the input is known to contain valid
  ///     utf-8.  If so, we can skip error checks.  Note: passing a compile-time
  ///     constant `false` may be better than passing a computed value.
  ///
  ///   - knownCountExceeds3: true if and only if the input is known be at least
  ///     4 elements long.  If so, we can skip end checks.  Note: pass a
  ///     compile-time constant here or you will just slow the algorithm down!
  static func parse1ForwardOpenCoded<C: Collection>(
    _ input: C,
    knownValid: Bool = false,
    knownCountExceeds3: Bool = false
  ) -> ParseResult<UInt32, C.Index>
  where C.Iterator.Element == UTF8.CodeUnit {
    
    var i = input.startIndex, j = i
    let end = input.endIndex

    // Helper "macro"
    @inline(__always)
    func atEnd() -> Bool {
      return _slowPath(!knownCountExceeds3 && i == end)
    }
    
    if atEnd() { return .emptyInput }
    let c0 = input[i]
    i = input.index(after: i)
    
    // Quickly detect ASCII
    if _fastPath(Int8(bitPattern: c0) >= 0) {
      return .valid(UInt32(c0), resumptionPoint: i)
    }

    //===------------------------------------------------------------------===//
    // Optimize for valid UTF-8 by assuming we got a start byte and counting
    // continuations.
    //===------------------------------------------------------------------===//
    
    //===--- Helper "macros" ----------------------------------------------===//
    @inline(__always)
    func valid(_ test: @autoclosure ()->Bool) -> Bool {
      return _fastPath(knownValid || test())
    }

    /// Returns the expected continuation at `i`, advancing `i`.  Return `nil`
    /// if there's no continuation here.
    @inline(__always)
    func nextContinuation() -> CodeUnit? {
      guard !atEnd() else { return nil }
      let r = input[i]
      guard valid(Classification(r) == .continuation) else { return nil }
      i = input.index(after: i)
      return r
    }
    //===------------------------------------------------------------------===//
    
    let class0 = Classification(c0)
    
    Validity: repeat { // labeled loop as "poor man's goto"
      guard valid(c0 >= minMultibyteStarter)             else { break Validity }
      guard let c1 = nextContinuation()                  else { break Validity }
      var r = UInt32(c0 & ((1 << (7 - class0.rawValue)) - 1))
      r = r << 6 | UInt32(c1 & 0b00_111111)
      
      if _fastPath(class0 == .start2) {
        return .valid(r, resumptionPoint: i)
      }

      guard let c2 = nextContinuation()                  else { break Validity }
      r = r << 6 | UInt32(c2 & 0b00_111111)
     
      if _fastPath(class0 == .start3) {
        guard valid(isValid3BytePrefix(c0, c1))          else { break Validity }
        return .valid(r, resumptionPoint: i)
      }
      
      guard let c3 = nextContinuation()                  else { break Validity }
      guard valid(isValid4BytePrefix(c0, c1))            else { break Validity }
      r = r << 6 | UInt32(c3 & 0b00_111111)
      return .valid(r, resumptionPoint: i)
    }
    while false

    // Handle the error cases.
    let i1 = input.index(after: input.startIndex)
    if _fastPath(i1 != end) {
      let c1 = input[i1]
      switch class0 {
      case .start3:
        if Classification(c1) == .continuation && isValid3BytePrefix(c0, c1) {
          return .error(resumptionPoint: input.index(after: i1))
        }
      case .start4:
        if isValid4BytePrefix(c0, c1) {
          return .error(resumptionPoint: i)
        }
      default: break
      }
    }
    return .error(resumptionPoint: i1)
  }

  static func leading1s(_ x:UInt8) -> UInt8 {
    return UInt8(_leadingZeros((~x)._value))
  }

  /// Given a valid leading byte of a multibyte sequence, strip the leading 1
  /// bits.
  ///
  /// - Note: Given any other byte, the result is unspecified.
  static func maskLeadByte(_ x: UInt8) -> UInt8 {
    return x & (0b11111 >> (x >> 5 & 1))
  }
  
  /// Parses one scalar forward from `input`.
  ///
  ///   - Parameter knownCountExceeds3: true if and only if the input is known
  ///   be at least 4 elements long.  If so, we can skip end checks.  Note: pass
  ///   a compile-time constant here or you will just slow the algorithm down!
  static func parse1Forward<C: Collection>(
    _ input: C, knownCountExceeds3: Bool = false
  ) -> ParseResult<UInt32, C.Index>
  where C.Iterator.Element == UTF8.CodeUnit {

    // See
    // https://gist.github.com/dabrahams/1880044370a192ae51c263a93f25a4c5#gistcomment-1931947
    // for an explanation of this state machine.
    if input.isEmpty { return .emptyInput }

    var i = input.startIndex 
    let end = input.endIndex
    @inline(__always) func inputConsumed() -> Bool {
      return _slowPath(!knownCountExceeds3 && i == end)
    }
    
    let u0 = input[i]
    var j = input.index(after: i)
    
    if _fastPath(Int8(bitPattern: u0) >= 0) {
      return .valid(UInt32(u0), resumptionPoint: j)
    }
    i = j // even if there are errors, we eat 1 byte

    // Begin accumulating result
    var r = UInt32(maskLeadByte(u0))

    
    // Mark one more token recognized and get the next lookahead token iff it
    // falls within pattern
    @inline (__always)
    func nextContinuation(_ pattern: ClosedRange<UInt8>) -> Bool {
      i = j
      if inputConsumed() { return false } // no more tokens
      let u = input[j]
      if _fastPath(pattern ~= u) {
        r = r << 6 | UInt32(u & 0b00_111111)
        j = input.index(after: j)
        return true
      }
      return false
    }
    
    @inline(__always)
    func state7() -> ParseResult<UInt32, C.Index> {
      return nextContinuation(0x80...0xbf)
        ? .valid(r, resumptionPoint: j) : .error(resumptionPoint: i)
    }
    
    @inline(__always)
    func state3() -> ParseResult<UInt32, C.Index> {
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

  /// Parses one scalar in reverse from `input`.
  /// 
  ///   - Parameter knownCountExceeds3: true if and only if the input is known
  ///   be at least 4 elements long.  If so, we can skip end checks.  Note: pass
  ///   a compile-time constant here or you will just slow the algorithm down!
  static func parse1Reverse<C: BidirectionalCollection>(
    _ input: C, knownCountExceeds3: Bool = false
  ) -> ParseResult<UInt32, C.Index>
  where C.Iterator.Element == UTF8.CodeUnit,
  // FIXME: drop these constraints once we have the compiler features.
  C.SubSequence.Index == C.Index,
  C.SubSequence.Iterator.Element == C.Iterator.Element
  {
    // See
    // https://gist.github.com/dabrahams/1880044370a192ae51c263a93f25a4c5#gistcomment-1931947
    // for an explanation of this state machine.

    if _slowPath(!knownCountExceeds3 && input.isEmpty) { return .emptyInput }

    var i = input.index(before: input.endIndex)
    var j = i
    let j0 = j
    var u = input[j]
    if _fastPath(Int8(bitPattern: u) >= 0) {
      return .valid(UInt32(u), resumptionPoint: j)
    }
    
    let start = input.startIndex
    var r: UInt32 = 0
    var shift: UInt32 = 0

    // Mark one more token recognized and get the next lookahead token iff it
    // satisfies the predicate
    @inline(__always)
    func consumeContinuation(_ pattern: ClosedRange<UInt8>) -> Bool {
      guard _fastPath(pattern ~= u) else { return false }
      i = j
      guard _fastPath(knownCountExceeds3 || j != start) else { return false }
      r |= UInt32(u & 0b00_111111) << shift
      shift += 6
      j = input.index(before: j)
      u = input[j]
      return true
    }

    @inline(__always)
    func accept(_ pat: ClosedRange<UInt8>) -> ParseResult<UInt32, C.Index>? {
      if _fastPath(pat.contains(u)) {
        r |= UInt32(maskLeadByte(u)) << shift
        return .valid(r, resumptionPoint: j)
      }
      return nil
    }
    
    @inline(__always)
    func state4_5() -> ParseResult<UInt32, C.Index> {
      return accept(0xf0...0xf3) ?? .error(resumptionPoint: j0)
    }
    
    @inline(__always)
    func state5_6() -> ParseResult<UInt32, C.Index> {
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

  /// Parse a whole collection efficiently, using `parse` to read each unicode
  /// scalar value, writing results into `output`.
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
  static func parseForward<C: Collection>(
    _ input: C,
    using parse: (C.SubSequence, Bool)->ParseResult<UInt32, C.SubSequence.Index>,
    stoppingOnError stopOnError: Bool = false,
    into output: (ParseResult<UInt32, C.SubSequence.Index>)->Void
  ) -> (remainder: C.SubSequence, errorCount: Int)
  where C.SubSequence : Collection, C.SubSequence.SubSequence == C.SubSequence,
    C.SubSequence.Iterator.Element == CodeUnit {
    var remainder = input[input.startIndex..<input.endIndex]
    var errorCount = 0
    
    func eat(_ o: ParseResult<UInt32, C.SubSequence.Index>)
      -> ParseResult<UInt32, C.SubSequence.Index>? {
      switch o {
      case .emptyInput:
        // Should we make this case unreachable for the first loop below, or is
        // the compiler smart enough to avoid introducing overhead?
        return nil 
      case .valid(_, _): break
      case .error(_): errorCount += 1
        if stopOnError { return nil }
      }
      remainder = remainder.suffix(from: o.resumptionPoint!)
      return o
    }

    // Repeatedly eat the first 25% without checking for end-of-input.
    while remainder.count >= 4 {
      // This loop could be unrolled, obviously
      for _ in 0 ..< numericCast(remainder.count) >> 2 {
        guard let o = eat(parse(remainder, true)) else {
          return (remainder, errorCount)
        }
        output(o)
      }
    }

    // Handle whatever is left
    while true {
      guard let o = eat(parse(remainder, false)) else {
        return (remainder, errorCount)
      }
      output(o)
    }
  }

  /// Parse a whole collection efficiently in reverse, using `parse`
  /// to read each unicode scalar value, writing results into
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
  static func parseReverse<C: BidirectionalCollection>(
    _ input: C,
    using parse: (C.SubSequence, Bool)->ParseResult<UInt32, C.SubSequence.Index>,
    stoppingOnError stopOnError: Bool = false,
    into output: (ParseResult<UInt32, C.SubSequence.Index>)->Void
  ) -> (remainder: C.SubSequence, errorCount: Int)
  where C.SubSequence : BidirectionalCollection,
        C.SubSequence.SubSequence == C.SubSequence,
        C.SubSequence.Iterator.Element == CodeUnit {
    var remainder = input[input.startIndex..<input.endIndex]
    var errorCount = 0
    
    func eat(_ o: ParseResult<UInt32, C.SubSequence.Index>)
      -> ParseResult<UInt32, C.SubSequence.Index>? {
      switch o {
      case .emptyInput:
        // Should we make this case unreachable for the first loop below, or is
        // the compiler smart enough to avoid introducing overhead?
        return nil 
      case .valid(_, _): break
      case .error(_): errorCount += 1
        if stopOnError { return nil }
      }
      remainder = remainder.prefix(upTo: o.resumptionPoint!)
      return o
    }

    // Repeatedly eat the last 25% without checking for end-of-input.
    while remainder.count >= 4 {
      // This loop could be unrolled, obviously
      for _ in 0 ..< numericCast(remainder.count) >> 2 {
        guard let o = eat(parse(remainder, true)) else {
          return (remainder, errorCount)
        }
        output(o)
      }
    }

    // Handle whatever is left
    while true {
      guard let o = eat(parse(remainder, false)) else {
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

//===----------------------------------------------------------------------===//
//===--- Semantic Testing -------------------------------------------------===//
//===----------------------------------------------------------------------===//
import SwiftPrivate
import StdlibUnittest
import StdlibUnicodeUnittest
import StdlibCollectionUnittest

protocol DefaultConstructible {
  init()
}

typealias UTF8ParseFunction = (
  _ input: ArraySlice<UTF8.CodeUnit>,
  _ knownCountExceeds3: Bool) -> ParseResult<UInt32, Int>

/// A compatibility codec designed to act like UTF8, so it can be used in tests
/// without rewriting them.
struct TestUTF8 : UnicodeCodec {
  static var parse: UTF8ParseFunction = {
    _,_ in fatalError("set parse function") }
  
  /// A type that can hold code unit values for this encoding.
  typealias CodeUnit = UTF8.CodeUnit

  // Doesn't have to be efficient; we're just testing here
  var results: ArraySlice<UnicodeDecodingResult> = []

  /// Creates an instance of the codec.
  init() {}

  mutating func decode<I : IteratorProtocol>(
    _ input: inout I
  ) -> UnicodeDecodingResult where I.Element == CodeUnit {

    if results.isEmpty {
      // Fortunately none of our tests check how much of the iterator gets
      // consumed.
      var buffer: [CodeUnit] = []
      while let x = input.next() { buffer.append(x) }
      
      UTF8.parseForward(buffer, using: TestUTF8.parse) {
        results.append(
          $0.valid.map { .scalarValue(UnicodeScalar($0)!) } ?? .error
        )
      }
    }
    return results.popFirst() ?? .emptyInput
  }

  /// Encodes a Unicode scalar as a series of code units by calling the given
  /// closure on each code unit.
  static func encode(
    _ input: UnicodeScalar,
    into processCodeUnit: (CodeUnit) -> Void
  ) {
    fatalError("not implemented")
  }

  /// Searches for the first occurrence of a `CodeUnit` that is equal to 0.
  static func _nullCodeUnitOffset(in input: UnsafePointer<CodeUnit>) -> Int {
    fatalError("not implemented")
  }
}

//===----------------------------------------------------------------------===//
// Test code lifted from validation-test/stdlib/Unicode.swift.gyb:

func checkDecodeUTF<Codec : UnicodeCodec>(
    _ codec: Codec.Type, _ expectedHead: [UInt32],
    _ expectedRepairedTail: [UInt32], _ utfStr: [Codec.CodeUnit]
) -> AssertionResult {
  do {
    var decoded = [UInt32]()
    let output: (UInt32) -> Void = { decoded.append($0) }
    let iterator = utfStr.makeIterator()
    _ = transcode(
      iterator,
      from: codec,
      to: UTF32.self,
      stoppingOnError: true,
      into: output)
    if expectedHead != decoded {
      return assertionFailure()
          .withDescription("\nstoppingOnError: true\n")
          .withDescription("expectedHead: \(asHex(expectedHead))\n")
          .withDescription("actual:       \(asHex(decoded))")
    }
  }

  do {
    var expected = expectedHead
    expected += expectedRepairedTail

    var decoded = [UInt32]()
    let output: (UInt32) -> Void = { decoded.append($0) }
    let iterator = utfStr.makeIterator()
    _ = transcode(
      iterator,
      from: codec.self,
      to: UTF32.self,
      stoppingOnError: false,
      into: output)
    if expected != decoded {
      return assertionFailure()
          .withDescription("\nstoppingOnError: false\n")
          .withDescription("expected: \(asHex(expected))\n")
          .withDescription("actual:   \(asHex(decoded))")
    }
  }

  return assertionSuccess()
}

func addUTF8Suite(name: String, parser: @escaping UTF8ParseFunction) {

  func checkDecodeUTF8(
    _ expectedHead: [UInt32],
    _ expectedRepairedTail: [UInt32], _ utf8Str: [UInt8]
  ) -> AssertionResult {
    TestUTF8.parse = parser
    let ret = checkDecodeUTF(TestUTF8.self, expectedHead, expectedRepairedTail, utf8Str)

    var reverseResult: [UInt32] = []
    UTF8.parseReverse(utf8Str, using: UTF8.parse1Reverse) {
      reverseResult.append($0.valid ?? 0xFFFD)
    }
    let expected = expectedHead + expectedRepairedTail
    if !expected.elementsEqual(reverseResult.reversed()) {
      return assertionFailure().withDescription(ret.description)
      .withDescription("\nreverse decoding failed!\n")
      .withDescription("\nexpected: \(asHex(expected))\n")
      .withDescription("\nactual: \(asHex(reverseResult.reversed()))")
    }

    return ret
  }

  let suite = TestSuite(name)

  suite.test("SmokeTest").forEach(in: utfTests) {
    test in

    expectTrue(
      checkDecodeUTF8(test.utf32, [], test.utf8),
      stackTrace: test.loc.withCurrentLoc())
    return ()
  }

  suite.test("FirstPossibleSequence") {
    //
    // First possible sequence of a certain length
    //
    // U+0000 NULL
    expectTrue(checkDecodeUTF8([ 0x0000 ], [], [ 0x00 ]))

    // U+0080 PADDING CHARACTER
    expectTrue(checkDecodeUTF8([ 0x0080 ], [], [ 0xc2, 0x80 ]))

    // U+0800 SAMARITAN LETTER ALAF
    expectTrue(checkDecodeUTF8(
        [ 0x0800 ], [],
        [ 0xe0, 0xa0, 0x80 ]))

    // U+10000 LINEAR B SYLLABLE B008 A
    expectTrue(checkDecodeUTF8(
        [ 0x10000 ], [],
        [ 0xf0, 0x90, 0x80, 0x80 ]))
    
    // U+200000 (invalid)
    expectTrue(checkDecodeUTF8(
        [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf8, 0x88, 0x80, 0x80, 0x80 ]))

    // U+4000000 (invalid)
    expectTrue(checkDecodeUTF8(
        [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfc, 0x84, 0x80, 0x80, 0x80, 0x80 ]))
  }

  suite.test("LastPossibleSequence") {
    //
    // Last possible sequence of a certain length
    //
    // U+007F DELETE
    expectTrue(checkDecodeUTF8([ 0x007f ], [], [ 0x7f ]))

    // U+07FF (unassigned)
    expectTrue(checkDecodeUTF8([ 0x07ff ], [], [ 0xdf, 0xbf ]))

    // U+FFFF (noncharacter)
    expectTrue(checkDecodeUTF8(
        [ 0xffff ], [],
        [ 0xef, 0xbf, 0xbf ]))

    // U+1FFFFF (invalid)
    expectTrue(checkDecodeUTF8(
        [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf7, 0xbf, 0xbf, 0xbf ]))

    // U+3FFFFFF (invalid)
    expectTrue(checkDecodeUTF8(
        [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfb, 0xbf, 0xbf, 0xbf, 0xbf ]))

    // U+7FFFFFFF (invalid)
    expectTrue(checkDecodeUTF8(
        [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfd, 0xbf, 0xbf, 0xbf, 0xbf, 0xbf ]))
  }

  suite.test("CodeSpaceBoundaryConditions") {
    //
    // Other boundary conditions
    //
    // U+D7FF (unassigned)
    expectTrue(checkDecodeUTF8([ 0xd7ff ], [], [ 0xed, 0x9f, 0xbf ]))

    // U+E000 (private use)
    expectTrue(checkDecodeUTF8([ 0xe000 ], [], [ 0xee, 0x80, 0x80 ]))

    // U+FFFD REPLACEMENT CHARACTER
    expectTrue(checkDecodeUTF8([ 0xfffd ], [], [ 0xef, 0xbf, 0xbd ]))

    // U+10FFFF (noncharacter)
    expectTrue(checkDecodeUTF8([ 0x10ffff ], [], [ 0xf4, 0x8f, 0xbf, 0xbf ]))

    // U+110000 (invalid)
    expectTrue(checkDecodeUTF8(
        [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf4, 0x90, 0x80, 0x80 ]))
  }

  suite.test("UnexpectedContinuationBytes") {
    //
    // Unexpected continuation bytes
    //
    // A sequence of unexpected continuation bytes that don't follow a first
    // byte, every byte is a maximal subpart.
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0x80, 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xbf, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [], [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0x80, 0xbf, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0x80, 0xbf, 0x80, 0xbf ]))
    expectTrue(checkDecodeUTF8(
        [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0x80, 0xbf, 0x82, 0xbf, 0xaa ]))
    expectTrue(checkDecodeUTF8(
        [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xaa, 0xb0, 0xbb, 0xbf, 0xaa, 0xa0 ]))
    expectTrue(checkDecodeUTF8(
        [], [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xaa, 0xb0, 0xbb, 0xbf, 0xaa, 0xa0, 0x8f ]))

    // All continuation bytes (0x80--0xbf).
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
          0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
          0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
          0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
          0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
          0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
          0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
          0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf ]))
  }

  suite.test("LonelyStartBytes") {
    //
    // Lonely start bytes
    //
    // Start bytes of 2-byte sequences (0xc0--0xdf).
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
          0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
          0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
          0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf ]))

    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
          0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
          0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
          0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
          0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
          0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
          0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
          0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
        [ 0xc0, 0x20, 0xc1, 0x20, 0xc2, 0x20, 0xc3, 0x20,
          0xc4, 0x20, 0xc5, 0x20, 0xc6, 0x20, 0xc7, 0x20,
          0xc8, 0x20, 0xc9, 0x20, 0xca, 0x20, 0xcb, 0x20,
          0xcc, 0x20, 0xcd, 0x20, 0xce, 0x20, 0xcf, 0x20,
          0xd0, 0x20, 0xd1, 0x20, 0xd2, 0x20, 0xd3, 0x20,
          0xd4, 0x20, 0xd5, 0x20, 0xd6, 0x20, 0xd7, 0x20,
          0xd8, 0x20, 0xd9, 0x20, 0xda, 0x20, 0xdb, 0x20,
          0xdc, 0x20, 0xdd, 0x20, 0xde, 0x20, 0xdf, 0x20 ]))

    // Start bytes of 3-byte sequences (0xe0--0xef).
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
          0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef ]))

    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
          0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
          0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
          0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
        [ 0xe0, 0x20, 0xe1, 0x20, 0xe2, 0x20, 0xe3, 0x20,
          0xe4, 0x20, 0xe5, 0x20, 0xe6, 0x20, 0xe7, 0x20,
          0xe8, 0x20, 0xe9, 0x20, 0xea, 0x20, 0xeb, 0x20,
          0xec, 0x20, 0xed, 0x20, 0xee, 0x20, 0xef, 0x20 ]))

    // Start bytes of 4-byte sequences (0xf0--0xf7).
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7 ]))

    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020,
          0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
        [ 0xf0, 0x20, 0xf1, 0x20, 0xf2, 0x20, 0xf3, 0x20,
          0xf4, 0x20, 0xf5, 0x20, 0xf6, 0x20, 0xf7, 0x20 ]))

    // Start bytes of 5-byte sequences (0xf8--0xfb).
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf8, 0xf9, 0xfa, 0xfb ]))

    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
        [ 0xf8, 0x20, 0xf9, 0x20, 0xfa, 0x20, 0xfb, 0x20 ]))

    // Start bytes of 6-byte sequences (0xfc--0xfd).
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0xfd ]))

    expectTrue(checkDecodeUTF8(
        [], [ 0xfffd, 0x0020, 0xfffd, 0x0020 ],
        [ 0xfc, 0x20, 0xfd, 0x20 ]))
  }

  suite.test("InvalidStartBytes") {
    //
    // Other bytes (0xc0--0xc1, 0xfe--0xff).
    //
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc0 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc1 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfe ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xff ]))

    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xc0, 0xc1, 0xfe, 0xff ]))

    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfe, 0xfe, 0xff, 0xff ]))

    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfe, 0x80, 0x80, 0x80, 0x80, 0x80 ]))

    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xff, 0x80, 0x80, 0x80, 0x80, 0x80 ]))

    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020, 0xfffd, 0x0020 ],
        [ 0xc0, 0x20, 0xc1, 0x20, 0xfe, 0x20, 0xff, 0x20 ]))
  }

  suite.test("MissingContinuationBytes") {
    // FIXME: This should be deleted
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xe0, 0x80 ]))
    //
    // Sequences with one continuation byte missing
    //
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc2 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xdf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xc2, 0x41 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xdf, 0x41 ]))

    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe0, 0xa0 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe0, 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xe0, 0xa0, 0x41 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xe0, 0xbf, 0x41 ]))

    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe1, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xec, 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xe1, 0x80, 0x41 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xec, 0xbf, 0x41 ]))

    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xed, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xed, 0x9f ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xed, 0x80, 0x41 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xed, 0x9f, 0x41 ]))

    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xee, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xef, 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xee, 0x80, 0x41 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xef, 0xbf, 0x41 ]))

    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0x90, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0xbf, 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf0, 0x90, 0x80, 0x41 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf0, 0xbf, 0xbf, 0x41 ]))

    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf1, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf3, 0xbf, 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf1, 0x80, 0x80, 0x41 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf3, 0xbf, 0xbf, 0x41 ]))

    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x8f, 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf4, 0x80, 0x80, 0x41 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0x0041 ], [ 0xf4, 0x8f, 0xbf, 0x41 ]))

    // Overlong sequences with one trailing byte missing.
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc0 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xc1 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xe0, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xe0, 0x9f ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf0, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf0, 0x8f, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf8, 0x80, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfc, 0x80, 0x80, 0x80, 0x80 ]))

    // Sequences that represent surrogates with one trailing byte missing.
    // High-surrogates
    
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xa0 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xac ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xaf ]))
    // Low-surrogates
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xb0 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xb4 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xed, 0xbf ]))
    
    // Ill-formed 4-byte sequences.
    // 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
    // U+1100xx (invalid)
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf4, 0x90, 0x80 ]))
    // U+13FBxx (invalid)
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf4, 0xbf, 0xbf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf5, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf6, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf7, 0x80, 0x80 ]))
    // U+1FFBxx (invalid)
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf7, 0xbf, 0xbf ]))

    // Ill-formed 5-byte sequences.
    // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
    // U+2000xx (invalid)
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf8, 0x88, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf8, 0xbf, 0xbf, 0xbf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf9, 0x80, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfa, 0x80, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfb, 0x80, 0x80, 0x80 ]))
    // U+3FFFFxx (invalid)
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfb, 0xbf, 0xbf, 0xbf ]))

    // Ill-formed 6-byte sequences.
    // 1111110u 10uuuuuu 10uzzzzz 10zzzyyyy 10yyyyxx 10xxxxxx
    // U+40000xx (invalid)
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfc, 0x84, 0x80, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfc, 0xbf, 0xbf, 0xbf, 0xbf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfd, 0x80, 0x80, 0x80, 0x80 ]))
    // U+7FFFFFxx (invalid)
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfd, 0xbf, 0xbf, 0xbf, 0xbf ]))

    //
    // Sequences with two continuation bytes missing
    //
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0x90 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0, 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf1, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf3, 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4, 0x8f ]))

    // Overlong sequences with two trailing byte missing.
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xe0 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf0, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf0, 0x8f ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf8, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfc, 0x80, 0x80, 0x80 ]))

    // Sequences that represent surrogates with two trailing bytes missing.
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xed ]))

    // Ill-formed 4-byte sequences.
    // 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
    // U+110yxx (invalid)
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf4, 0x90 ]))
    // U+13Fyxx (invalid)
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf4, 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf5, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf6, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf7, 0x80 ]))
    // U+1FFyxx (invalid)
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf7, 0xbf ]))

    // Ill-formed 5-byte sequences.
    // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
    // U+200yxx (invalid)
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf8, 0x88, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf8, 0xbf, 0xbf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf9, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfa, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfb, 0x80, 0x80 ]))
    // U+3FFFyxx (invalid)
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfb, 0xbf, 0xbf ]))

    // Ill-formed 6-byte sequences.
    // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
    // U+4000yxx (invalid)
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfc, 0x84, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfc, 0xbf, 0xbf, 0xbf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfd, 0x80, 0x80, 0x80 ]))
    // U+7FFFFyxx (invalid)
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfd, 0xbf, 0xbf, 0xbf ]))

    //
    // Sequences with three continuation bytes missing
    //
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf1 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf2 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf3 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf4 ]))

    // Broken overlong sequences.
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf0 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf8, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfc, 0x80, 0x80 ]))

    // Ill-formed 4-byte sequences.
    // 11110zzz 10zzyyyy 10yyyyxx 10xxxxxx
    // U+14yyxx (invalid)
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf5 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf6 ]))
    // U+1Cyyxx (invalid)
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf7 ]))

    // Ill-formed 5-byte sequences.
    // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
    // U+20yyxx (invalid)
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf8, 0x88 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf8, 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xf9, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfa, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfb, 0x80 ]))
    // U+3FCyyxx (invalid)
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfb, 0xbf ]))

    // Ill-formed 6-byte sequences.
    // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
    // U+400yyxx (invalid)
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfc, 0x84, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfc, 0xbf, 0xbf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfd, 0x80, 0x80 ]))
    // U+7FFCyyxx (invalid)
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfd, 0xbf, 0xbf ]))

    //
    // Sequences with four continuation bytes missing
    //
    // Ill-formed 5-byte sequences.
    // 111110uu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
    // U+uzyyxx (invalid)
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf8 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf9 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfa ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfb ]))
    // U+3zyyxx (invalid)
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfb ]))

    // Broken overlong sequences.
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xf8 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0x80 ]))

    // Ill-formed 6-byte sequences.
    // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
    // U+uzzyyxx (invalid)
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0x84 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfc, 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfd, 0x80 ]))
    // U+7Fzzyyxx (invalid)
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xfd, 0xbf ]))

    //
    // Sequences with five continuation bytes missing
    //
    // Ill-formed 6-byte sequences.
    // 1111110u 10uuuuuu 10zzzzzz 10zzyyyy 10yyyyxx 10xxxxxx
    // U+uzzyyxx (invalid)
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfc ]))
    // U+uuzzyyxx (invalid)
    expectTrue(checkDecodeUTF8([], [ 0xfffd ], [ 0xfd ]))

    //
    // Consecutive sequences with trailing bytes missing
    //
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, /**/ 0xfffd, 0xfffd, /**/ 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, /**/ 0xfffd, /**/ 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd,
          0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xc0, /**/ 0xe0, 0x80, /**/ 0xf0, 0x80, 0x80,
          0xf8, 0x80, 0x80, 0x80,
          0xfc, 0x80, 0x80, 0x80, 0x80,
          0xdf, /**/ 0xef, 0xbf, /**/ 0xf7, 0xbf, 0xbf,
          0xfb, 0xbf, 0xbf, 0xbf,
          0xfd, 0xbf, 0xbf, 0xbf, 0xbf ]))
  }

  suite.test("OverlongSequences") {
    //
    // Overlong UTF-8 sequences
    //
    // U+002F SOLIDUS
    expectTrue(checkDecodeUTF8([ 0x002f ], [], [ 0x2f ]))

    // Overlong sequences of the above.
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc0, 0xaf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xe0, 0x80, 0xaf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf0, 0x80, 0x80, 0xaf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf8, 0x80, 0x80, 0x80, 0xaf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfc, 0x80, 0x80, 0x80, 0x80, 0xaf ]))

    // U+0000 NULL
    expectTrue(checkDecodeUTF8([ 0x0000 ], [], [ 0x00 ]))

    // Overlong sequences of the above.
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc0, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xe0, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf0, 0x80, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf8, 0x80, 0x80, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfc, 0x80, 0x80, 0x80, 0x80, 0x80 ]))

    // Other overlong and ill-formed sequences.
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc0, 0xbf ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc1, 0x80 ]))
    expectTrue(checkDecodeUTF8([], [ 0xfffd, 0xfffd ], [ 0xc1, 0xbf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xe0, 0x9f, 0xbf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xa0, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xbf, 0xbf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf0, 0x8f, 0x80, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf0, 0x8f, 0xbf, 0xbf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xf8, 0x87, 0xbf, 0xbf, 0xbf ]))
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xfc, 0x83, 0xbf, 0xbf, 0xbf, 0xbf ]))
  }

  suite.test("IsolatedSurrogates") {
    // Unicode 6.3.0:
    //
    //    D71.  High-surrogate code point: A Unicode code point in the range
    //    U+D800 to U+DBFF.
    //
    //    D73.  Low-surrogate code point: A Unicode code point in the range
    //    U+DC00 to U+DFFF.
    // Note: U+E0100 is <DB40 DD00> in UTF-16.
    // High-surrogates
    // U+D800
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xa0, 0x80 ]))
    expectTrue(checkDecodeUTF8(
        [ 0x0041 ],
        [ 0xfffd, 0xfffd, 0xfffd, 0x0041 ],
        [ 0x41, 0xed, 0xa0, 0x80, 0x41 ]))

    // U+DB40
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xac, 0xa0 ]))

    // U+DBFF
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xaf, 0xbf ]))

    // Low-surrogates
    // U+DC00
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xb0, 0x80 ]))

    // U+DD00
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xb4, 0x80 ]))

    // U+DFFF
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xbf, 0xbf ]))
  }

  suite.test("SurrogatePairs") {
    // Surrogate pairs
    // U+D800 U+DC00
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xa0, 0x80, 0xed, 0xb0, 0x80 ]))

    // U+D800 U+DD00
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xa0, 0x80, 0xed, 0xb4, 0x80 ]))

    // U+D800 U+DFFF
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xa0, 0x80, 0xed, 0xbf, 0xbf ]))

    // U+DB40 U+DC00
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xac, 0xa0, 0xed, 0xb0, 0x80 ]))

    // U+DB40 U+DD00
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xac, 0xa0, 0xed, 0xb4, 0x80 ]))

    // U+DB40 U+DFFF
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xac, 0xa0, 0xed, 0xbf, 0xbf ]))

    // U+DBFF U+DC00
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xaf, 0xbf, 0xed, 0xb0, 0x80 ]))

    // U+DBFF U+DD00
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xaf, 0xbf, 0xed, 0xb4, 0x80 ]))

    // U+DBFF U+DFFF
    expectTrue(checkDecodeUTF8(
        [],
        [ 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd, 0xfffd ],
        [ 0xed, 0xaf, 0xbf, 0xed, 0xbf, 0xbf ]))
  }

  suite.test("Noncharacters") {
    //
    // Noncharacters
    //
    // Unicode 6.3.0:
    //
    //    D14.  Noncharacter: A code point that is permanently reserved for
    //    internal use and that should never be interchanged. Noncharacters
    //    consist of the values U+nFFFE and U+nFFFF (where n is from 0 to 1016)
    //    and the values U+FDD0..U+FDEF.
    // U+FFFE
    expectTrue(checkDecodeUTF8([ 0xfffe ], [], [ 0xef, 0xbf, 0xbe ]))

    // U+FFFF
    expectTrue(checkDecodeUTF8([ 0xffff ], [], [ 0xef, 0xbf, 0xbf ]))

    // U+1FFFE
    expectTrue(checkDecodeUTF8([ 0x1fffe ], [], [ 0xf0, 0x9f, 0xbf, 0xbe ]))

    // U+1FFFF
    expectTrue(checkDecodeUTF8([ 0x1ffff ], [], [ 0xf0, 0x9f, 0xbf, 0xbf ]))

    // U+2FFFE
    expectTrue(checkDecodeUTF8([ 0x2fffe ], [], [ 0xf0, 0xaf, 0xbf, 0xbe ]))

    // U+2FFFF
    expectTrue(checkDecodeUTF8([ 0x2ffff ], [], [ 0xf0, 0xaf, 0xbf, 0xbf ]))

    // U+3FFFE
    expectTrue(checkDecodeUTF8([ 0x3fffe ], [], [ 0xf0, 0xbf, 0xbf, 0xbe ]))

    // U+3FFFF
    expectTrue(checkDecodeUTF8([ 0x3ffff ], [], [ 0xf0, 0xbf, 0xbf, 0xbf ]))

    // U+4FFFE
    expectTrue(checkDecodeUTF8([ 0x4fffe ], [], [ 0xf1, 0x8f, 0xbf, 0xbe ]))

    // U+4FFFF
    expectTrue(checkDecodeUTF8([ 0x4ffff ], [], [ 0xf1, 0x8f, 0xbf, 0xbf ]))

    // U+5FFFE
    expectTrue(checkDecodeUTF8([ 0x5fffe ], [], [ 0xf1, 0x9f, 0xbf, 0xbe ]))

    // U+5FFFF
    expectTrue(checkDecodeUTF8([ 0x5ffff ], [], [ 0xf1, 0x9f, 0xbf, 0xbf ]))

    // U+6FFFE
    expectTrue(checkDecodeUTF8([ 0x6fffe ], [], [ 0xf1, 0xaf, 0xbf, 0xbe ]))

    // U+6FFFF
    expectTrue(checkDecodeUTF8([ 0x6ffff ], [], [ 0xf1, 0xaf, 0xbf, 0xbf ]))

    // U+7FFFE
    expectTrue(checkDecodeUTF8([ 0x7fffe ], [], [ 0xf1, 0xbf, 0xbf, 0xbe ]))

    // U+7FFFF
    expectTrue(checkDecodeUTF8([ 0x7ffff ], [], [ 0xf1, 0xbf, 0xbf, 0xbf ]))

    // U+8FFFE
    expectTrue(checkDecodeUTF8([ 0x8fffe ], [], [ 0xf2, 0x8f, 0xbf, 0xbe ]))

    // U+8FFFF
    expectTrue(checkDecodeUTF8([ 0x8ffff ], [], [ 0xf2, 0x8f, 0xbf, 0xbf ]))

    // U+9FFFE
    expectTrue(checkDecodeUTF8([ 0x9fffe ], [], [ 0xf2, 0x9f, 0xbf, 0xbe ]))

    // U+9FFFF
    expectTrue(checkDecodeUTF8([ 0x9ffff ], [], [ 0xf2, 0x9f, 0xbf, 0xbf ]))

    // U+AFFFE
    expectTrue(checkDecodeUTF8([ 0xafffe ], [], [ 0xf2, 0xaf, 0xbf, 0xbe ]))

    // U+AFFFF
    expectTrue(checkDecodeUTF8([ 0xaffff ], [], [ 0xf2, 0xaf, 0xbf, 0xbf ]))

    // U+BFFFE
    expectTrue(checkDecodeUTF8([ 0xbfffe ], [], [ 0xf2, 0xbf, 0xbf, 0xbe ]))

    // U+BFFFF
    expectTrue(checkDecodeUTF8([ 0xbffff ], [], [ 0xf2, 0xbf, 0xbf, 0xbf ]))

    // U+CFFFE
    expectTrue(checkDecodeUTF8([ 0xcfffe ], [], [ 0xf3, 0x8f, 0xbf, 0xbe ]))

    // U+CFFFF
    expectTrue(checkDecodeUTF8([ 0xcfffF ], [], [ 0xf3, 0x8f, 0xbf, 0xbf ]))

    // U+DFFFE
    expectTrue(checkDecodeUTF8([ 0xdfffe ], [], [ 0xf3, 0x9f, 0xbf, 0xbe ]))

    // U+DFFFF
    expectTrue(checkDecodeUTF8([ 0xdffff ], [], [ 0xf3, 0x9f, 0xbf, 0xbf ]))

    // U+EFFFE
    expectTrue(checkDecodeUTF8([ 0xefffe ], [], [ 0xf3, 0xaf, 0xbf, 0xbe ]))

    // U+EFFFF
    expectTrue(checkDecodeUTF8([ 0xeffff ], [], [ 0xf3, 0xaf, 0xbf, 0xbf ]))

    // U+FFFFE
    expectTrue(checkDecodeUTF8([ 0xffffe ], [], [ 0xf3, 0xbf, 0xbf, 0xbe ]))

    // U+FFFFF
    expectTrue(checkDecodeUTF8([ 0xfffff ], [], [ 0xf3, 0xbf, 0xbf, 0xbf ]))

    // U+10FFFE
    expectTrue(checkDecodeUTF8([ 0x10fffe ], [], [ 0xf4, 0x8f, 0xbf, 0xbe ]))

    // U+10FFFF
    expectTrue(checkDecodeUTF8([ 0x10ffff ], [], [ 0xf4, 0x8f, 0xbf, 0xbf ]))

    // U+FDD0
    expectTrue(checkDecodeUTF8([ 0xfdd0 ], [], [ 0xef, 0xb7, 0x90 ]))

    // U+FDD1
    expectTrue(checkDecodeUTF8([ 0xfdd1 ], [], [ 0xef, 0xb7, 0x91 ]))

    // U+FDD2
    expectTrue(checkDecodeUTF8([ 0xfdd2 ], [], [ 0xef, 0xb7, 0x92 ]))

    // U+FDD3
    expectTrue(checkDecodeUTF8([ 0xfdd3 ], [], [ 0xef, 0xb7, 0x93 ]))

    // U+FDD4
    expectTrue(checkDecodeUTF8([ 0xfdd4 ], [], [ 0xef, 0xb7, 0x94 ]))

    // U+FDD5
    expectTrue(checkDecodeUTF8([ 0xfdd5 ], [], [ 0xef, 0xb7, 0x95 ]))

    // U+FDD6
    expectTrue(checkDecodeUTF8([ 0xfdd6 ], [], [ 0xef, 0xb7, 0x96 ]))

    // U+FDD7
    expectTrue(checkDecodeUTF8([ 0xfdd7 ], [], [ 0xef, 0xb7, 0x97 ]))

    // U+FDD8
    expectTrue(checkDecodeUTF8([ 0xfdd8 ], [], [ 0xef, 0xb7, 0x98 ]))

    // U+FDD9
    expectTrue(checkDecodeUTF8([ 0xfdd9 ], [], [ 0xef, 0xb7, 0x99 ]))

    // U+FDDA
    expectTrue(checkDecodeUTF8([ 0xfdda ], [], [ 0xef, 0xb7, 0x9a ]))

    // U+FDDB
    expectTrue(checkDecodeUTF8([ 0xfddb ], [], [ 0xef, 0xb7, 0x9b ]))

    // U+FDDC
    expectTrue(checkDecodeUTF8([ 0xfddc ], [], [ 0xef, 0xb7, 0x9c ]))

    // U+FDDD
    expectTrue(checkDecodeUTF8([ 0xfddd ], [], [ 0xef, 0xb7, 0x9d ]))

    // U+FDDE
    expectTrue(checkDecodeUTF8([ 0xfdde ], [], [ 0xef, 0xb7, 0x9e ]))

    // U+FDDF
    expectTrue(checkDecodeUTF8([ 0xfddf ], [], [ 0xef, 0xb7, 0x9f ]))

    // U+FDE0
    expectTrue(checkDecodeUTF8([ 0xfde0 ], [], [ 0xef, 0xb7, 0xa0 ]))

    // U+FDE1
    expectTrue(checkDecodeUTF8([ 0xfde1 ], [], [ 0xef, 0xb7, 0xa1 ]))

    // U+FDE2
    expectTrue(checkDecodeUTF8([ 0xfde2 ], [], [ 0xef, 0xb7, 0xa2 ]))

    // U+FDE3
    expectTrue(checkDecodeUTF8([ 0xfde3 ], [], [ 0xef, 0xb7, 0xa3 ]))

    // U+FDE4
    expectTrue(checkDecodeUTF8([ 0xfde4 ], [], [ 0xef, 0xb7, 0xa4 ]))

    // U+FDE5
    expectTrue(checkDecodeUTF8([ 0xfde5 ], [], [ 0xef, 0xb7, 0xa5 ]))

    // U+FDE6
    expectTrue(checkDecodeUTF8([ 0xfde6 ], [], [ 0xef, 0xb7, 0xa6 ]))

    // U+FDE7
    expectTrue(checkDecodeUTF8([ 0xfde7 ], [], [ 0xef, 0xb7, 0xa7 ]))

    // U+FDE8
    expectTrue(checkDecodeUTF8([ 0xfde8 ], [], [ 0xef, 0xb7, 0xa8 ]))

    // U+FDE9
    expectTrue(checkDecodeUTF8([ 0xfde9 ], [], [ 0xef, 0xb7, 0xa9 ]))

    // U+FDEA
    expectTrue(checkDecodeUTF8([ 0xfdea ], [], [ 0xef, 0xb7, 0xaa ]))

    // U+FDEB
    expectTrue(checkDecodeUTF8([ 0xfdeb ], [], [ 0xef, 0xb7, 0xab ]))

    // U+FDEC
    expectTrue(checkDecodeUTF8([ 0xfdec ], [], [ 0xef, 0xb7, 0xac ]))

    // U+FDED
    expectTrue(checkDecodeUTF8([ 0xfded ], [], [ 0xef, 0xb7, 0xad ]))

    // U+FDEE
    expectTrue(checkDecodeUTF8([ 0xfdee ], [], [ 0xef, 0xb7, 0xae ]))

    // U+FDEF
    expectTrue(checkDecodeUTF8([ 0xfdef ], [], [ 0xef, 0xb7, 0xaf ]))

    // U+FDF0
    expectTrue(checkDecodeUTF8([ 0xfdf0 ], [], [ 0xef, 0xb7, 0xb0 ]))

    // U+FDF1
    expectTrue(checkDecodeUTF8([ 0xfdf1 ], [], [ 0xef, 0xb7, 0xb1 ]))

    // U+FDF2
    expectTrue(checkDecodeUTF8([ 0xfdf2 ], [], [ 0xef, 0xb7, 0xb2 ]))

    // U+FDF3
    expectTrue(checkDecodeUTF8([ 0xfdf3 ], [], [ 0xef, 0xb7, 0xb3 ]))

    // U+FDF4
    expectTrue(checkDecodeUTF8([ 0xfdf4 ], [], [ 0xef, 0xb7, 0xb4 ]))

    // U+FDF5
    expectTrue(checkDecodeUTF8([ 0xfdf5 ], [], [ 0xef, 0xb7, 0xb5 ]))

    // U+FDF6
    expectTrue(checkDecodeUTF8([ 0xfdf6 ], [], [ 0xef, 0xb7, 0xb6 ]))

    // U+FDF7
    expectTrue(checkDecodeUTF8([ 0xfdf7 ], [], [ 0xef, 0xb7, 0xb7 ]))

    // U+FDF8
    expectTrue(checkDecodeUTF8([ 0xfdf8 ], [], [ 0xef, 0xb7, 0xb8 ]))

    // U+FDF9
    expectTrue(checkDecodeUTF8([ 0xfdf9 ], [], [ 0xef, 0xb7, 0xb9 ]))

    // U+FDFA
    expectTrue(checkDecodeUTF8([ 0xfdfa ], [], [ 0xef, 0xb7, 0xba ]))

    // U+FDFB
    expectTrue(checkDecodeUTF8([ 0xfdfb ], [], [ 0xef, 0xb7, 0xbb ]))

    // U+FDFC
    expectTrue(checkDecodeUTF8([ 0xfdfc ], [], [ 0xef, 0xb7, 0xbc ]))

    // U+FDFD
    expectTrue(checkDecodeUTF8([ 0xfdfd ], [], [ 0xef, 0xb7, 0xbd ]))

    // U+FDFE
    expectTrue(checkDecodeUTF8([ 0xfdfe ], [], [ 0xef, 0xb7, 0xbe ]))

    // U+FDFF
    expectTrue(checkDecodeUTF8([ 0xfdff ], [], [ 0xef, 0xb7, 0xbf ]))
  }
}

addUTF8Suite(
  name: "OpenCodedUTF8Decoder",
  parser: { UTF8.parse1ForwardOpenCoded($0, knownCountExceeds3: $1) })

addUTF8Suite(
  name: "UnrolledStateMachineUTF8Decoder",
  parser: UTF8.parse1Forward)

runAllTests()


//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

// Conversions between different Unicode encodings.  Note that UTF-16 and
// UTF-32 decoding are *not* currently resilient to erroneous data.

/// The result of one Unicode decoding step.
///
/// Each `UnicodeDecodingResult` instance can represent a Unicode scalar value,
/// an indication that no more Unicode scalars are available, or an indication
/// of a decoding error.
///
/// - SeeAlso: `UnicodeCodec.decode(next:)`
@_fixed_layout
public enum UnicodeDecodingResult : Equatable {
  /// A decoded Unicode scalar value.
  case scalarValue(UnicodeScalar)

  /// An indication that no more Unicode scalars are available in the input.
  case emptyInput

  /// An indication of a decoding error.
  case error

  public static func == (
    lhs: UnicodeDecodingResult,
    rhs: UnicodeDecodingResult
  ) -> Bool {
    switch (lhs, rhs) {
    case (.scalarValue(let lhsScalar), .scalarValue(let rhsScalar)):
      return lhsScalar == rhsScalar
    case (.emptyInput, .emptyInput):
      return true
    case (.error, .error):
      return true
    default:
      return false
    }
  }
}

/// A Unicode encoding form that translates between Unicode scalar values and
/// form-specific code units.
///
/// The `UnicodeCodec` protocol declares methods that decode code unit
/// sequences into Unicode scalar values and encode Unicode scalar values
/// into code unit sequences. The standard library implements codecs for the
/// UTF-8, UTF-16, and UTF-32 encoding schemes as the `UTF8`, `UTF16`, and
/// `UTF32` types, respectively. Use the `UnicodeScalar` type to work with
/// decoded Unicode scalar values.
///
/// - SeeAlso: `UTF8`, `UTF16`, `UTF32`, `UnicodeScalar`
public protocol UnicodeCodec {

  /// A type that can hold code unit values for this encoding.
  associatedtype CodeUnit

  /// Creates an instance of the codec.
  init()

  /// Starts or continues decoding a code unit sequence into Unicode scalar
  /// values.
  ///
  /// To decode a code unit sequence completely, call this method repeatedly
  /// until it returns `UnicodeDecodingResult.emptyInput`. Checking that the
  /// iterator was exhausted is not sufficient, because the decoder can store
  /// buffered data from the input iterator.
  ///
  /// Because of buffering, it is impossible to find the corresponding position
  /// in the iterator for a given returned `UnicodeScalar` or an error.
  ///
  /// The following example decodes the UTF-8 encoded bytes of a string into an
  /// array of `UnicodeScalar` instances:
  ///
  ///     let str = "✨Unicode✨"
  ///     print(Array(str.utf8))
  ///     // Prints "[226, 156, 168, 85, 110, 105, 99, 111, 100, 101, 226, 156, 168]"
  ///
  ///     var bytesIterator = str.utf8.makeIterator()
  ///     var scalars: [UnicodeScalar] = []
  ///     var utf8Decoder = UTF8()
  ///     Decode: while true {
  ///         switch utf8Decoder.decode(&bytesIterator) {
  ///         case .scalarValue(let v): scalars.append(v)
  ///         case .emptyInput: break Decode
  ///         case .error:
  ///             print("Decoding error")
  ///             break Decode
  ///         }
  ///     }
  ///     print(scalars)
  ///     // Prints "["\u{2728}", "U", "n", "i", "c", "o", "d", "e", "\u{2728}"]"
  ///
  /// - Parameter input: An iterator of code units to be decoded. `input` must be
  ///   the same iterator instance in repeated calls to this method. Do not
  ///   advance the iterator or any copies of the iterator outside this
  ///   method.
  /// - Returns: A `UnicodeDecodingResult` instance, representing the next
  ///   Unicode scalar, an indication of an error, or an indication that the
  ///   UTF sequence has been fully decoded.
  mutating func decode<I : IteratorProtocol>(
    _ input: inout I
  ) -> UnicodeDecodingResult where I.Element == CodeUnit

  /// Encodes a Unicode scalar as a series of code units by calling the given
  /// closure on each code unit.
  ///
  /// For example, the musical fermata symbol ("𝄐") is a single Unicode scalar
  /// value (`\u{1D110}`) but requires four code units for its UTF-8
  /// representation. The following code uses the `UTF8` codec to encode a
  /// fermata in UTF-8:
  ///
  ///     var bytes: [UTF8.CodeUnit] = []
  ///     UTF8.encode("𝄐", into: { bytes.append($0) })
  ///     print(bytes)
  ///     // Prints "[240, 157, 132, 144]"
  ///
  /// - Parameters:
  ///   - input: The Unicode scalar value to encode.
  ///   - processCodeUnit: A closure that processes one code unit argument at a
  ///     time.
  static func encode(
    _ input: UnicodeScalar,
    into processCodeUnit: (CodeUnit) -> Void
  )

  /// Searches for the first occurrence of a `CodeUnit` that is equal to 0.
  ///
  /// Is an equivalent of `strlen` for C-strings.
  ///
  /// - Complexity: O(*n*)
  static func _nullCodeUnitOffset(in input: UnsafePointer<CodeUnit>) -> Int
}

/// A codec for translating between Unicode scalar values and UTF-8 code
/// units.
public struct UTF8 : UnicodeCodec {
  // See Unicode 8.0.0, Ch 3.9, UTF-8.
  // http://www.unicode.org/versions/Unicode8.0.0/ch03.pdf

  /// A type that can hold code unit values for this encoding.
  public typealias CodeUnit = UInt8

  /// Creates an instance of the UTF-8 codec.
  public init() {}

  /// Lookahead buffer used for UTF-8 decoding.  New bytes are inserted at MSB,
  /// and bytes are read at LSB.  Note that we need to use a buffer, because
  /// in case of invalid subsequences we sometimes don't know whether we should
  /// consume a certain byte before looking at it.
  internal var _decodeBuffer: UInt32 = 0

  /// The number of bits in `_decodeBuffer` that are current filled.
  internal var _bitsInBuffer: UInt8 = 0

  /// Starts or continues decoding a UTF-8 sequence.
  ///
  /// To decode a code unit sequence completely, call this method repeatedly
  /// until it returns `UnicodeDecodingResult.emptyInput`. Checking that the
  /// iterator was exhausted is not sufficient, because the decoder can store
  /// buffered data from the input iterator.
  ///
  /// Because of buffering, it is impossible to find the corresponding position
  /// in the iterator for a given returned `UnicodeScalar` or an error.
  ///
  /// The following example decodes the UTF-8 encoded bytes of a string into an
  /// array of `UnicodeScalar` instances. This is a demonstration only---if
  /// you need the Unicode scalar representation of a string, use its
  /// `unicodeScalars` view.
  ///
  ///     let str = "✨Unicode✨"
  ///     print(Array(str.utf8))
  ///     // Prints "[226, 156, 168, 85, 110, 105, 99, 111, 100, 101, 226, 156, 168]"
  ///
  ///     var bytesIterator = str.utf8.makeIterator()
  ///     var scalars: [UnicodeScalar] = []
  ///     var utf8Decoder = UTF8()
  ///     Decode: while true {
  ///         switch utf8Decoder.decode(&bytesIterator) {
  ///         case .scalarValue(let v): scalars.append(v)
  ///         case .emptyInput: break Decode
  ///         case .error:
  ///             print("Decoding error")
  ///             break Decode
  ///         }
  ///     }
  ///     print(scalars)
  ///     // Prints "["\u{2728}", "U", "n", "i", "c", "o", "d", "e", "\u{2728}"]"
  ///
  /// - Parameter input: An iterator of code units to be decoded. `input` must be
  ///   the same iterator instance in repeated calls to this method. Do not
  ///   advance the iterator or any copies of the iterator outside this
  ///   method.
  /// - Returns: A `UnicodeDecodingResult` instance, representing the next
  ///   Unicode scalar, an indication of an error, or an indication that the
  ///   UTF sequence has been fully decoded.
  public mutating func decode<I : IteratorProtocol>(
    _ input: inout I
  ) -> UnicodeDecodingResult where I.Element == CodeUnit {

    // Bufferless ASCII fastpath.
    if _fastPath(_bitsInBuffer == 0) {
      guard let codeUnit = input.next() else { return .emptyInput }
      // ASCII, return immediately.
      if codeUnit & 0x80 == 0 {
        return .scalarValue(UnicodeScalar(_unchecked: UInt32(codeUnit)))
      }
      // Non-ASCII, proceed to buffering mode.
      _decodeBuffer = UInt32(codeUnit)
      _bitsInBuffer = 8
    } else if _decodeBuffer & 0x80 == 0 {
      // ASCII in buffer.  We don't refill the buffer so we can return
      // to bufferless mode once we've exhausted it.
      let codeUnit = _decodeBuffer & 0xff
      _decodeBuffer >>= 8
      _bitsInBuffer = _bitsInBuffer &- 8
      return .scalarValue(UnicodeScalar(_unchecked: codeUnit))
    }
    // Buffering mode.
    // Fill buffer back to 4 bytes (or as many as are left in the iterator).
    _sanityCheck(_bitsInBuffer < 32)
    repeat {
      if let codeUnit = input.next() {
        // We know _bitsInBuffer < 32 so we use `& 0x1f` (31) to make the
        // compiler omit a bounds check branch for the bitshift.
        _decodeBuffer |= (UInt32(codeUnit) << UInt32(_bitsInBuffer & 0x1f))
        _bitsInBuffer = _bitsInBuffer &+ 8
      } else {
        if _bitsInBuffer == 0 { return .emptyInput }
        break // We still have some bytes left in our buffer.
      }
    } while _bitsInBuffer < 32

    // Decode one unicode scalar.
    // Note our empty bytes are always 0x00, which is required for this call.
    let (result, length) = UTF8._decodeOne(_decodeBuffer)

    // Consume the decoded bytes (or maximal subpart of ill-formed sequence).
    let bitsConsumed = 8 &* length
    _sanityCheck(1...4 ~= length && bitsConsumed <= _bitsInBuffer)
    // Swift doesn't allow shifts greater than or equal to the type width.
    // _decodeBuffer >>= UInt32(bitsConsumed) // >>= 32 crashes.
    // Mask with 0x3f (63) to let the compiler omit the '>= 64' bounds check.
    _decodeBuffer = UInt32(truncatingBitPattern:
      UInt64(_decodeBuffer) >> (UInt64(bitsConsumed) & 0x3f))
    _bitsInBuffer = _bitsInBuffer &- bitsConsumed

    guard _fastPath(result != nil) else { return .error }
    return .scalarValue(UnicodeScalar(_unchecked: result!))
  }

  /// Attempts to decode a single UTF-8 code unit sequence starting at the LSB
  /// of `buffer`.
  ///
  /// - Returns:
  ///   - result: The decoded code point if the code unit sequence is
  ///     well-formed; `nil` otherwise.
  ///   - length: The length of the code unit sequence in bytes if it is
  ///     well-formed; otherwise the *maximal subpart of the ill-formed
  ///     sequence* (Unicode 8.0.0, Ch 3.9, D93b), i.e. the number of leading
  ///     code units that were valid or 1 in case none were valid.  Unicode
  ///     recommends to skip these bytes and replace them by a single
  ///     replacement character (U+FFFD).
  ///
  /// - Requires: There is at least one used byte in `buffer`, and the unused
  ///   space in `buffer` is filled with some value not matching the UTF-8
  ///   continuation byte form (`0b10xxxxxx`).
  public // @testable
  static func _decodeOne(_ buffer: UInt32) -> (result: UInt32?, length: UInt8) {
    // Note the buffer is read least significant byte first: [ #3 #2 #1 #0 ].

    if buffer & 0x80 == 0 { // 1-byte sequence (ASCII), buffer: [ ... ... ... CU0 ].
      let value = buffer & 0xff
      return (value, 1)
    }

    // Determine sequence length using high 5 bits of 1st byte.  We use a
    // look-up table to branch less.  1-byte sequences are handled above.
    //
    //  case | pattern | description
    // ----------------------------
    //   00  |  110xx  | 2-byte sequence
    //   01  |  1110x  | 3-byte sequence
    //   10  |  11110  | 4-byte sequence
    //   11  |  other  | invalid
    //
    //                     11xxx      10xxx      01xxx      00xxx
    let lut0: UInt32 = 0b1011_0000__1111_1111__1111_1111__1111_1111
    let lut1: UInt32 = 0b1100_0000__1111_1111__1111_1111__1111_1111

    let index = (buffer >> 3) & 0x1f
    let bit0 = (lut0 >> index) & 1
    let bit1 = (lut1 >> index) & 1

    switch (bit1, bit0) {
    case (0, 0): // 2-byte sequence, buffer: [ ... ... CU1 CU0 ].
      // Require 10xx xxxx  110x xxxx.
      if _slowPath(buffer & 0xc0e0 != 0x80c0) { return (nil, 1) }
      // Disallow xxxx xxxx  xxx0 000x (<= 7 bits case).
      if _slowPath(buffer & 0x001e == 0x0000) { return (nil, 1) }
      // Extract data bits.
      let value = (buffer & 0x3f00) >> 8
                | (buffer & 0x001f) << 6
      return (value, 2)

    case (0, 1): // 3-byte sequence, buffer: [ ... CU2 CU1 CU0 ].
      // Disallow xxxx xxxx  xx0x xxxx  xxxx 0000 (<= 11 bits case).
      if _slowPath(buffer & 0x00200f == 0x000000) { return (nil, 1) }
      // Disallow xxxx xxxx  xx1x xxxx  xxxx 1101 (surrogate code points).
      if _slowPath(buffer & 0x00200f == 0x00200d) { return (nil, 1) }
      // Require 10xx xxxx  10xx xxxx  1110 xxxx.
      if _slowPath(buffer & 0xc0c0f0 != 0x8080e0) {
        if buffer & 0x00c000 != 0x008000 { return (nil, 1) }
        return (nil, 2) // All checks on CU0 & CU1 passed.
      }
      // Extract data bits.
      let value = (buffer & 0x3f0000) >> 16
                | (buffer & 0x003f00) >> 2
                | (buffer & 0x00000f) << 12
      return (value, 3)

    case (1, 0): // 4-byte sequence, buffer: [ CU3 CU2 CU1 CU0 ].
      // Disallow xxxx xxxx  xxxx xxxx  xx00 xxxx  xxxx x000 (<= 16 bits case).
      if _slowPath(buffer & 0x00003007 == 0x00000000) { return (nil, 1) }
      // If xxxx xxxx  xxxx xxxx  xxxx xxxx  xxxx x1xx.
      if buffer & 0x00000004 == 0x00000004 {
        // Require xxxx xxxx  xxxx xxxx  xx00 xxxx  xxxx xx00 (<= 0x10FFFF).
        if _slowPath(buffer & 0x00003003 != 0x00000000) { return (nil, 1) }
      }
      // Require 10xx xxxx  10xx xxxx  10xx xxxx  1111 0xxx.
      if _slowPath(buffer & 0xc0c0c0f8 != 0x808080f0) {
        if buffer & 0x0000c000 != 0x00008000 { return (nil, 1) }
        // All other checks on CU0, CU1 & CU2 passed.
        if buffer & 0x00c00000 != 0x00800000 { return (nil, 2) }
        return (nil, 3)
      }
      // Extract data bits.
      // FIXME(integers): remove extra type casts
      let value = (buffer & 0x3f000000) >> (24 as UInt32)
                | (buffer & 0x003f0000) >> (10 as UInt32)
                | (buffer & 0x00003f00) << (4 as UInt32)
                | (buffer & 0x00000007) << (18 as UInt32)
      return (value, 4)

    default: // Invalid sequence (CU0 invalid).
      return (nil, 1)
    }
  }

  /// Encodes a Unicode scalar as a series of code units by calling the given
  /// closure on each code unit.
  ///
  /// For example, the musical fermata symbol ("𝄐") is a single Unicode scalar
  /// value (`\u{1D110}`) but requires four code units for its UTF-8
  /// representation. The following code encodes a fermata in UTF-8:
  ///
  ///     var bytes: [UTF8.CodeUnit] = []
  ///     UTF8.encode("𝄐", into: { bytes.append($0) })
  ///     print(bytes)
  ///     // Prints "[240, 157, 132, 144]"
  ///
  /// - Parameters:
  ///   - input: The Unicode scalar value to encode.
  ///   - processCodeUnit: A closure that processes one code unit argument at a
  ///     time.
  public static func encode(
    _ input: UnicodeScalar,
    into processCodeUnit: (CodeUnit) -> Void
  ) {
    var c = UInt32(input)
    var buf3 = UInt8(c & 0xFF)

    if c >= UInt32(1<<7) {
      c >>= 6
      buf3 = (buf3 & 0x3F) | 0x80 // 10xxxxxx
      var buf2 = UInt8(c & 0xFF)
      if c < UInt32(1<<5) {
        buf2 |= 0xC0              // 110xxxxx
      }
      else {
        c >>= 6
        buf2 = (buf2 & 0x3F) | 0x80 // 10xxxxxx
        var buf1 = UInt8(c & 0xFF)
        if c < UInt32(1<<4) {
          buf1 |= 0xE0              // 1110xxxx
        }
        else {
          c >>= 6
          buf1 = (buf1 & 0x3F) | 0x80 // 10xxxxxx
          processCodeUnit(UInt8(c | 0xF0)) // 11110xxx
        }
        processCodeUnit(buf1)
      }
      processCodeUnit(buf2)
    }
    processCodeUnit(buf3)
  }

  /// Returns a Boolean value indicating whether the specified code unit is a
  /// UTF-8 continuation byte.
  ///
  /// Continuation bytes take the form `0b10xxxxxx`. For example, a lowercase
  /// "e" with an acute accent above it (`"é"`) uses 2 bytes for its UTF-8
  /// representation: `0b11000011` (195) and `0b10101001` (169). The second
  /// byte is a continuation byte.
  ///
  ///     let eAcute = "é"
  ///     for codePoint in eAcute.utf8 {
  ///         print(codePoint, UTF8.isContinuation(codePoint))
  ///     }
  ///     // Prints "195 false"
  ///     // Prints "169 true"
  ///
  /// - Parameter byte: A UTF-8 code unit.
  /// - Returns: `true` if `byte` is a continuation byte; otherwise, `false`.
  public static func isContinuation(_ byte: CodeUnit) -> Bool {
    return byte & 0b11_00__0000 == 0b10_00__0000
  }

  public static func _nullCodeUnitOffset(
    in input: UnsafePointer<CodeUnit>
  ) -> Int {
    return Int(_swift_stdlib_strlen_unsigned(input))
  }
  // Support parsing C strings as-if they are UTF8 strings.
  public static func _nullCodeUnitOffset(
    in input: UnsafePointer<CChar>
  ) -> Int {
    return Int(_swift_stdlib_strlen(input))
  }
}

/// A codec for translating between Unicode scalar values and UTF-16 code
/// units.
public struct UTF16 : UnicodeCodec {
  /// A type that can hold code unit values for this encoding.
  public typealias CodeUnit = UInt16

  /// Creates an instance of the UTF-16 codec.
  public init() {}

  /// A lookahead buffer for one UTF-16 code unit.
  internal var _decodeLookahead: UInt16?

  /// Starts or continues decoding a UTF-16 sequence.
  ///
  /// To decode a code unit sequence completely, call this method repeatedly
  /// until it returns `UnicodeDecodingResult.emptyInput`. Checking that the
  /// iterator was exhausted is not sufficient, because the decoder can store
  /// buffered data from the input iterator.
  ///
  /// Because of buffering, it is impossible to find the corresponding position
  /// in the iterator for a given returned `UnicodeScalar` or an error.
  ///
  /// The following example decodes the UTF-16 encoded bytes of a string into an
  /// array of `UnicodeScalar` instances. This is a demonstration only---if
  /// you need the Unicode scalar representation of a string, use its
  /// `unicodeScalars` view.
  ///
  ///     let str = "✨Unicode✨"
  ///     print(Array(str.utf16))
  ///     // Prints "[10024, 85, 110, 105, 99, 111, 100, 101, 10024]"
  ///
  ///     var codeUnitIterator = str.utf16.makeIterator()
  ///     var scalars: [UnicodeScalar] = []
  ///     var utf16Decoder = UTF16()
  ///     Decode: while true {
  ///         switch utf16Decoder.decode(&codeUnitIterator) {
  ///         case .scalarValue(let v): scalars.append(v)
  ///         case .emptyInput: break Decode
  ///         case .error:
  ///             print("Decoding error")
  ///             break Decode
  ///         }
  ///     }
  ///     print(scalars)
  ///     // Prints "["\u{2728}", "U", "n", "i", "c", "o", "d", "e", "\u{2728}"]"
  ///
  /// - Parameter input: An iterator of code units to be decoded. `input` must be
  ///   the same iterator instance in repeated calls to this method. Do not
  ///   advance the iterator or any copies of the iterator outside this
  ///   method.
  /// - Returns: A `UnicodeDecodingResult` instance, representing the next
  ///   Unicode scalar, an indication of an error, or an indication that the
  ///   UTF sequence has been fully decoded.
  public mutating func decode<I : IteratorProtocol>(
    _ input: inout I
  ) -> UnicodeDecodingResult where I.Element == CodeUnit {
    // Note: maximal subpart of ill-formed sequence for UTF-16 can only have
    // length 1.  Length 0 does not make sense.  Neither does length 2 -- in
    // that case the sequence is valid.

    let unit0: UInt16
    if _fastPath(_decodeLookahead == nil) {
      guard let next = input.next() else { return .emptyInput }
      unit0 = next
    } else { // Consume lookahead first.
      unit0 = _decodeLookahead!
      _decodeLookahead = nil
    }

    // A well-formed pair of surrogates looks like this:
    //     high-surrogate        low-surrogate
    // [1101 10xx xxxx xxxx] [1101 11xx xxxx xxxx]

    // Common case first, non-surrogate -- just a sequence of 1 code unit.
    if _fastPath((unit0 >> 11) != 0b1101_1) {
      return .scalarValue(UnicodeScalar(_unchecked: UInt32(unit0)))
    }

    // Ensure `unit0` is a high-surrogate.
    guard _fastPath((unit0 >> 10) == 0b1101_10) else { return .error }

    // We already have a high-surrogate, so there should be a next code unit.
    guard let unit1 = input.next() else { return .error }

    // `unit0` is a high-surrogate, so `unit1` should be a low-surrogate.
    guard _fastPath((unit1 >> 10) == 0b1101_11) else {
      // Invalid sequence, discard `unit0` and store `unit1` for the next call.
      _decodeLookahead = unit1
      return .error
    }

    // We have a well-formed surrogate pair, decode it.
    let result = 0x10000 + ((UInt32(unit0 & 0x03ff) << 10) | UInt32(unit1 & 0x03ff))
    return .scalarValue(UnicodeScalar(_unchecked: result))
  }

  /// Try to decode one Unicode scalar, and return the actual number of code
  /// units it spanned in the input.  This function may consume more code
  /// units than required for this scalar.
  @_versioned
  internal mutating func _decodeOne<I : IteratorProtocol>(
    _ input: inout I
  ) -> (UnicodeDecodingResult, Int) where I.Element == CodeUnit {
    let result = decode(&input)
    switch result {
    case .scalarValue(let us):
      return (result, UTF16.width(us))

    case .emptyInput:
      return (result, 0)

    case .error:
      return (result, 1)
    }
  }

  /// Encodes a Unicode scalar as a series of code units by calling the given
  /// closure on each code unit.
  ///
  /// For example, the musical fermata symbol ("𝄐") is a single Unicode scalar
  /// value (`\u{1D110}`) but requires two code units for its UTF-16
  /// representation. The following code encodes a fermata in UTF-16:
  ///
  ///     var codeUnits: [UTF16.CodeUnit] = []
  ///     UTF16.encode("𝄐", into: { codeUnits.append($0) })
  ///     print(codeUnits)
  ///     // Prints "[55348, 56592]"
  ///
  /// - Parameters:
  ///   - input: The Unicode scalar value to encode.
  ///   - processCodeUnit: A closure that processes one code unit argument at a
  ///     time.
  public static func encode(
    _ input: UnicodeScalar,
    into processCodeUnit: (CodeUnit) -> Void
  ) {
    let scalarValue: UInt32 = UInt32(input)

    if scalarValue <= UInt32(UInt16.max) {
      processCodeUnit(UInt16(scalarValue))
    }
    else {
      let lead_offset = UInt32(0xd800) - UInt32(0x10000 >> 10)
      processCodeUnit(UInt16(lead_offset + (scalarValue >> 10)))
      processCodeUnit(UInt16(0xdc00 + (scalarValue & 0x3ff)))
    }
  }
}

/// A codec for translating between Unicode scalar values and UTF-32 code
/// units.
public struct UTF32 : UnicodeCodec {
  /// A type that can hold code unit values for this encoding.
  public typealias CodeUnit = UInt32

  /// Creates an instance of the UTF-32 codec.
  public init() {}

  /// Starts or continues decoding a UTF-32 sequence.
  ///
  /// To decode a code unit sequence completely, call this method repeatedly
  /// until it returns `UnicodeDecodingResult.emptyInput`. Checking that the
  /// iterator was exhausted is not sufficient, because the decoder can store
  /// buffered data from the input iterator.
  ///
  /// Because of buffering, it is impossible to find the corresponding position
  /// in the iterator for a given returned `UnicodeScalar` or an error.
  ///
  /// The following example decodes the UTF-16 encoded bytes of a string
  /// into an array of `UnicodeScalar` instances. This is a demonstration
  /// only---if you need the Unicode scalar representation of a string, use
  /// its `unicodeScalars` view.
  ///
  ///     // UTF-32 representation of "✨Unicode✨"
  ///     let codeUnits: [UTF32.CodeUnit] =
  ///             [10024, 85, 110, 105, 99, 111, 100, 101, 10024]
  ///
  ///     var codeUnitIterator = codeUnits.makeIterator()
  ///     var scalars: [UnicodeScalar] = []
  ///     var utf32Decoder = UTF32()
  ///     Decode: while true {
  ///         switch utf32Decoder.decode(&codeUnitIterator) {
  ///         case .scalarValue(let v): scalars.append(v)
  ///         case .emptyInput: break Decode
  ///         case .error:
  ///             print("Decoding error")
  ///             break Decode
  ///         }
  ///     }
  ///     print(scalars)
  ///     // Prints "["\u{2728}", "U", "n", "i", "c", "o", "d", "e", "\u{2728}"]"
  ///
  /// - Parameter input: An iterator of code units to be decoded. `input` must be
  ///   the same iterator instance in repeated calls to this method. Do not
  ///   advance the iterator or any copies of the iterator outside this
  ///   method.
  /// - Returns: A `UnicodeDecodingResult` instance, representing the next
  ///   Unicode scalar, an indication of an error, or an indication that the
  ///   UTF sequence has been fully decoded.
  public mutating func decode<I : IteratorProtocol>(
    _ input: inout I
  ) -> UnicodeDecodingResult where I.Element == CodeUnit {
    return UTF32._decode(&input)
  }

  internal static func _decode<I : IteratorProtocol>(
    _ input: inout I
  ) -> UnicodeDecodingResult where I.Element == CodeUnit {
    guard let x = input.next() else { return .emptyInput }
    // Check code unit is valid: not surrogate-reserved and within range.
    guard _fastPath((x >> 11) != 0b1101_1 && x <= 0x10ffff)
      else { return .error }
    // x is a valid scalar.
    return .scalarValue(UnicodeScalar(_unchecked: x))
  }

  /// Encodes a Unicode scalar as a UTF-32 code unit by calling the given
  /// closure.
  ///
  /// For example, like every Unicode scalar, the musical fermata symbol ("𝄐")
  /// can be represented in UTF-32 as a single code unit. The following code
  /// encodes a fermata in UTF-32:
  ///
  ///     var codeUnit: UTF32.CodeUnit = 0
  ///     UTF32.encode("𝄐", into: { codeUnit = $0 })
  ///     print(codeUnit)
  ///     // Prints "119056"
  ///
  /// - Parameters:
  ///   - input: The Unicode scalar value to encode.
  ///   - processCodeUnit: A closure that processes one code unit argument at a
  ///     time.
  public static func encode(
    _ input: UnicodeScalar,
    into processCodeUnit: (CodeUnit) -> Void
  ) {
    processCodeUnit(UInt32(input))
  }
}

/// Translates the given input from one Unicode encoding to another by calling
/// the given closure.
///
/// The following example transcodes the UTF-8 representation of the string
/// `"Fermata 𝄐"` into UTF-32.
///
///     let fermata = "Fermata 𝄐"
///     let bytes = fermata.utf8
///     print(Array(bytes))
///     // Prints "[70, 101, 114, 109, 97, 116, 97, 32, 240, 157, 132, 144]"
///
///     var codeUnits: [UTF32.CodeUnit] = []
///     let sink = { codeUnits.append($0) }
///     transcode(bytes.makeIterator(), from: UTF8.self, to: UTF32.self,
///               stoppingOnError: false, into: sink)
///     print(codeUnits)
///     // Prints "[70, 101, 114, 109, 97, 116, 97, 32, 119056]"
///
/// The `sink` closure is called with each resulting UTF-32 code unit as the
/// function iterates over its input.
///
/// - Parameters:
///   - input: An iterator of code units to be translated, encoded as
///     `inputEncoding`. If `stopOnError` is `false`, the entire iterator will
///     be exhausted. Otherwise, iteration will stop if an encoding error is
///     detected.
///   - inputEncoding: The Unicode encoding of `input`.
///   - outputEncoding: The destination Unicode encoding.
///   - stopOnError: Pass `true` to stop translation when an encoding error is
///     detected in `input`. Otherwise, a Unicode replacement character
///     (`"\u{FFFD}"`) is inserted for each detected error.
///   - processCodeUnit: A closure that processes one `outputEncoding` code
///     unit at a time.
/// - Returns: `true` if the translation detected encoding errors in `input`;
///   otherwise, `false`.
public func transcode<Input, InputEncoding, OutputEncoding>(
  _ input: Input,
  from inputEncoding: InputEncoding.Type,
  to outputEncoding: OutputEncoding.Type,
  stoppingOnError stopOnError: Bool,
  into processCodeUnit: (OutputEncoding.CodeUnit) -> Void
) -> Bool
  where
  Input : IteratorProtocol,
  InputEncoding : UnicodeCodec,
  OutputEncoding : UnicodeCodec,
  InputEncoding.CodeUnit == Input.Element {
  var input = input

  // NB.  It is not possible to optimize this routine to a memcpy if
  // InputEncoding == OutputEncoding.  The reason is that memcpy will not
  // substitute U+FFFD replacement characters for ill-formed sequences.

  var inputDecoder = inputEncoding.init()
  var hadError = false
  loop:
  while true {
    switch inputDecoder.decode(&input) {
    case .scalarValue(let us):
      OutputEncoding.encode(us, into: processCodeUnit)
    case .emptyInput:
      break loop
    case .error:
      hadError = true
      if stopOnError {
        break loop
      }
      OutputEncoding.encode("\u{fffd}", into: processCodeUnit)
    }
  }
  return hadError
}

/// Transcode UTF-16 to UTF-8, replacing ill-formed sequences with U+FFFD.
///
/// Returns the index of the first unhandled code unit and the UTF-8 data
/// that was encoded.
internal func _transcodeSomeUTF16AsUTF8<Input>(
  _ input: Input, _ startIndex: Input.Index
) -> (Input.Index, _StringCore._UTF8Chunk)
  where
  Input : Collection,
  Input.Iterator.Element == UInt16 {

  typealias _UTF8Chunk = _StringCore._UTF8Chunk

  let endIndex = input.endIndex
  let utf8Max = MemoryLayout<_UTF8Chunk>.size
  var result: _UTF8Chunk = 0
  var utf8Count = 0
  var nextIndex = startIndex
  while nextIndex != input.endIndex && utf8Count != utf8Max {
    let u = UInt(input[nextIndex])
    let shift = _UTF8Chunk(utf8Count * 8)
    var utf16Length: Input.IndexDistance = 1

    if _fastPath(u <= 0x7f) {
      result |= _UTF8Chunk(u) << shift
      utf8Count += 1
    } else {
      var scalarUtf8Length: Int
      var r: UInt
      if _fastPath((u >> 11) != 0b1101_1) {
        // Neither high-surrogate, nor low-surrogate -- well-formed sequence
        // of 1 code unit, decoding is trivial.
        if u < 0x800 {
          r = 0b10__00_0000__110__0_0000
          r |= u >> 6
          r |= (u & 0b11_1111) << 8
          scalarUtf8Length = 2
        }
        else {
          r = 0b10__00_0000__10__00_0000__1110__0000
          r |= u >> 12
          r |= ((u >> 6) & 0b11_1111) << 8
          r |= (u        & 0b11_1111) << 16
          scalarUtf8Length = 3
        }
      } else {
        let unit0 = u
        if _slowPath((unit0 >> 10) == 0b1101_11) {
          // `unit0` is a low-surrogate.  We have an ill-formed sequence.
          // Replace it with U+FFFD.
          r = 0xbdbfef
          scalarUtf8Length = 3
        } else if _slowPath(input.index(nextIndex, offsetBy: 1) == endIndex) {
          // We have seen a high-surrogate and EOF, so we have an ill-formed
          // sequence.  Replace it with U+FFFD.
          r = 0xbdbfef
          scalarUtf8Length = 3
        } else {
          let unit1 = UInt(input[input.index(nextIndex, offsetBy: 1)])
          if _fastPath((unit1 >> 10) == 0b1101_11) {
            // `unit1` is a low-surrogate.  We have a well-formed surrogate
            // pair.
            let v = 0x10000 + (((unit0 & 0x03ff) << 10) | (unit1 & 0x03ff))

            r = 0b10__00_0000__10__00_0000__10__00_0000__1111_0__000
            r |= v >> 18
            r |= ((v >> 12) & 0b11_1111) << 8
            r |= ((v >> 6) & 0b11_1111) << 16
            r |= (v        & 0b11_1111) << 24
            scalarUtf8Length = 4
            utf16Length = 2
          } else {
            // Otherwise, we have an ill-formed sequence.  Replace it with
            // U+FFFD.
            r = 0xbdbfef
            scalarUtf8Length = 3
          }
        }
      }
      // Don't overrun the buffer
      if utf8Count + scalarUtf8Length > utf8Max {
        break
      }
      result |= numericCast(r) << shift
      utf8Count += scalarUtf8Length
    }
    nextIndex = input.index(nextIndex, offsetBy: utf16Length)
  }
  // FIXME: Annoying check, courtesy of <rdar://problem/16740169>
  if utf8Count < MemoryLayout.size(ofValue: result) {
    result |= ~0 << numericCast(utf8Count * 8)
  }
  return (nextIndex, result)
}

/// Instances of conforming types are used in internal `String`
/// representation.
public // @testable
protocol _StringElement {
  static func _toUTF16CodeUnit(_: Self) -> UTF16.CodeUnit

  static func _fromUTF16CodeUnit(_ utf16: UTF16.CodeUnit) -> Self
}

extension UTF16.CodeUnit : _StringElement {
  public // @testable
  static func _toUTF16CodeUnit(_ x: UTF16.CodeUnit) -> UTF16.CodeUnit {
    return x
  }
  public // @testable
  static func _fromUTF16CodeUnit(
    _ utf16: UTF16.CodeUnit
  ) -> UTF16.CodeUnit {
    return utf16
  }
}

extension UTF8.CodeUnit : _StringElement {
  public // @testable
  static func _toUTF16CodeUnit(_ x: UTF8.CodeUnit) -> UTF16.CodeUnit {
    _sanityCheck(x <= 0x7f, "should only be doing this with ASCII")
    return UTF16.CodeUnit(x)
  }
  public // @testable
  static func _fromUTF16CodeUnit(
    _ utf16: UTF16.CodeUnit
  ) -> UTF8.CodeUnit {
    _sanityCheck(utf16 <= 0x7f, "should only be doing this with ASCII")
    return UTF8.CodeUnit(utf16)
  }
}

extension UTF16 {
  /// Returns the number of code units required to encode the given Unicode
  /// scalar.
  ///
  /// Because a Unicode scalar value can require up to 21 bits to store its
  /// value, some Unicode scalars are represented in UTF-16 by a pair of
  /// 16-bit code units. The first and second code units of the pair,
  /// designated *leading* and *trailing* surrogates, make up a *surrogate
  /// pair*.
  ///
  ///     let anA: UnicodeScalar = "A"
  ///     print(anA.value)
  ///     // Prints "65"
  ///     print(UTF16.width(anA))
  ///     // Prints "1"
  ///
  ///     let anApple: UnicodeScalar = "🍎"
  ///     print(anApple.value)
  ///     // Prints "127822"
  ///     print(UTF16.width(anApple))
  ///     // Prints "2"
  ///
  /// - Parameter x: A Unicode scalar value.
  /// - Returns: The width of `x` when encoded in UTF-16, either `1` or `2`.
  public static func width(_ x: UnicodeScalar) -> Int {
    return x.value <= 0xFFFF ? 1 : 2
  }

  /// Returns the high-surrogate code unit of the surrogate pair representing
  /// the specified Unicode scalar.
  ///
  /// Because a Unicode scalar value can require up to 21 bits to store its
  /// value, some Unicode scalars are represented in UTF-16 by a pair of
  /// 16-bit code units. The first and second code units of the pair,
  /// designated *leading* and *trailing* surrogates, make up a *surrogate
  /// pair*.
  ///
  ///     let apple: UnicodeScalar = "🍎"
  ///     print(UTF16.leadSurrogate(apple)
  ///     // Prints "55356"
  ///
  /// - Parameter x: A Unicode scalar value. `x` must be represented by a
  ///   surrogate pair when encoded in UTF-16. To check whether `x` is
  ///   represented by a surrogate pair, use `UTF16.width(x) == 2`.
  /// - Returns: The leading surrogate code unit of `x` when encoded in UTF-16.
  ///
  /// - SeeAlso: `UTF16.width(_:)`, `UTF16.trailSurrogate(_:)`
  public static func leadSurrogate(_ x: UnicodeScalar) -> UTF16.CodeUnit {
    _precondition(width(x) == 2)
    return UTF16.CodeUnit((x.value - 0x1_0000) >> (10 as UInt32)) + 0xD800
  }

  /// Returns the low-surrogate code unit of the surrogate pair representing
  /// the specified Unicode scalar.
  ///
  /// Because a Unicode scalar value can require up to 21 bits to store its
  /// value, some Unicode scalars are represented in UTF-16 by a pair of
  /// 16-bit code units. The first and second code units of the pair,
  /// designated *leading* and *trailing* surrogates, make up a *surrogate
  /// pair*.
  ///
  ///     let apple: UnicodeScalar = "🍎"
  ///     print(UTF16.trailSurrogate(apple)
  ///     // Prints "57166"
  ///
  /// - Parameter x: A Unicode scalar value. `x` must be represented by a
  ///   surrogate pair when encoded in UTF-16. To check whether `x` is
  ///   represented by a surrogate pair, use `UTF16.width(x) == 2`.
  /// - Returns: The trailing surrogate code unit of `x` when encoded in UTF-16.
  ///
  /// - SeeAlso: `UTF16.width(_:)`, `UTF16.leadSurrogate(_:)`
  public static func trailSurrogate(_ x: UnicodeScalar) -> UTF16.CodeUnit {
    _precondition(width(x) == 2)
    return UTF16.CodeUnit(
      (x.value - 0x1_0000) & (((1 as UInt32) << 10) - 1)
    ) + 0xDC00
  }

  /// Returns a Boolean value indicating whether the specified code unit is a
  /// high-surrogate code unit.
  ///
  /// Here's an example of checking whether each code unit in a string's
  /// `utf16` view is a lead surrogate. The `apple` string contains a single
  /// emoji character made up of a surrogate pair when encoded in UTF-16.
  ///
  ///     let apple = "🍎"
  ///     for unit in apple.utf16 {
  ///         print(UTF16.isLeadSurrogate(unit))
  ///     }
  ///     // Prints "true"
  ///     // Prints "false"
  ///
  /// This method does not validate the encoding of a UTF-16 sequence beyond
  /// the specified code unit. Specifically, it does not validate that a
  /// low-surrogate code unit follows `x`.
  ///
  /// - Parameter x: A UTF-16 code unit.
  /// - Returns: `true` if `x` is a high-surrogate code unit; otherwise,
  ///   `false`.
  ///
  /// - SeeAlso: `UTF16.width(_:)`, `UTF16.leadSurrogate(_:)`
  public static func isLeadSurrogate(_ x: CodeUnit) -> Bool {
    return 0xD800...0xDBFF ~= x
  }

  /// Returns a Boolean value indicating whether the specified code unit is a
  /// low-surrogate code unit.
  ///
  /// Here's an example of checking whether each code unit in a string's
  /// `utf16` view is a trailing surrogate. The `apple` string contains a
  /// single emoji character made up of a surrogate pair when encoded in
  /// UTF-16.
  ///
  ///     let apple = "🍎"
  ///     for unit in apple.utf16 {
  ///         print(UTF16.isTrailSurrogate(unit))
  ///     }
  ///     // Prints "false"
  ///     // Prints "true"
  ///
  /// This method does not validate the encoding of a UTF-16 sequence beyond
  /// the specified code unit. Specifically, it does not validate that a
  /// high-surrogate code unit precedes `x`.
  ///
  /// - Parameter x: A UTF-16 code unit.
  /// - Returns: `true` if `x` is a low-surrogate code unit; otherwise,
  ///   `false`.
  ///
  /// - SeeAlso: `UTF16.width(_:)`, `UTF16.leadSurrogate(_:)`
  public static func isTrailSurrogate(_ x: CodeUnit) -> Bool {
    return 0xDC00...0xDFFF ~= x
  }

  public // @testable
  static func _copy<T : _StringElement, U : _StringElement>(
    source: UnsafeMutablePointer<T>,
    destination: UnsafeMutablePointer<U>,
    count: Int
  ) {
    if MemoryLayout<T>.stride == MemoryLayout<U>.stride {
      _memcpy(
        dest: UnsafeMutablePointer(destination),
        src: UnsafeMutablePointer(source),
        size: UInt(count) * UInt(MemoryLayout<U>.stride))
    }
    else {
      for i in 0..<count {
        let u16 = T._toUTF16CodeUnit((source + i).pointee)
        (destination + i).pointee = U._fromUTF16CodeUnit(u16)
      }
    }
  }

  /// Returns the number of UTF-16 code units required for the given code unit
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
    repairingIllFormedSequences: Bool
  ) -> (count: Int, isASCII: Bool)?
    where
    Input : IteratorProtocol,
    Encoding : UnicodeCodec,
    Encoding.CodeUnit == Input.Element {

    var input = input
    var count = 0
    var isAscii = true

    var inputDecoder = Encoding()
    loop:
    while true {
      switch inputDecoder.decode(&input) {
      case .scalarValue(let us):
        if us.value > 0x7f {
          isAscii = false
        }
        count += width(us)
      case .emptyInput:
        break loop
      case .error:
        if !repairingIllFormedSequences {
          return nil
        }
        isAscii = false
        count += width(UnicodeScalar(0xfffd)!)
      }
    }
    return (count, isAscii)
  }
}

// Unchecked init to avoid precondition branches in hot code paths where we
// already know the value is a valid unicode scalar.
extension UnicodeScalar {
  /// Create an instance with numeric value `value`, bypassing the regular
  /// precondition checks for code point validity.
  internal init(_unchecked value: UInt32) {
    _sanityCheck(value < 0xD800 || value > 0xDFFF,
      "high- and low-surrogate code points are not valid Unicode scalar values")
    _sanityCheck(value <= 0x10FFFF, "value is outside of Unicode codespace")

    self._value = value
  }
}

extension UnicodeCodec where CodeUnit : UnsignedInteger {
  public static func _nullCodeUnitOffset(in input: UnsafePointer<CodeUnit>) -> Int {
    var length = 0
    while input[length] != 0 {
      length += 1
    }
    return length
  }
}

extension UnicodeCodec {
  public static func _nullCodeUnitOffset(in input: UnsafePointer<CodeUnit>) -> Int {
    fatalError("_nullCodeUnitOffset(in:) implementation should be provided")
  }
}

@available(*, unavailable, renamed: "UnicodeCodec")
public typealias UnicodeCodecType = UnicodeCodec

extension UnicodeCodec {
  @available(*, unavailable, renamed: "encode(_:into:)")
  public static func encode(
    _ input: UnicodeScalar,
    output put: (CodeUnit) -> Void
  ) {
    Builtin.unreachable()
  }
}

@available(*, unavailable, message: "use 'transcode(_:from:to:stoppingOnError:into:)'")
public func transcode<Input, InputEncoding, OutputEncoding>(
  _ inputEncoding: InputEncoding.Type, _ outputEncoding: OutputEncoding.Type,
  _ input: Input, _ output: (OutputEncoding.CodeUnit) -> Void,
  stopOnError: Bool
) -> Bool
  where
  Input : IteratorProtocol,
  InputEncoding : UnicodeCodec,
  OutputEncoding : UnicodeCodec,
  InputEncoding.CodeUnit == Input.Element {
  Builtin.unreachable()
}

extension UTF16 {
  @available(*, unavailable, message: "use 'transcodedLength(of:decodedAs:repairingIllFormedSequences:)'")
  public static func measure<Encoding, Input>(
    _: Encoding.Type, input: Input, repairIllFormedSequences: Bool
  ) -> (Int, Bool)?
    where
    Encoding : UnicodeCodec,
    Input : IteratorProtocol,
    Encoding.CodeUnit == Input.Element {
    Builtin.unreachable()
  }
}

/// A namespace for Unicode utilities.
internal enum _Unicode {}


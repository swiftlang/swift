//===----------------------------------------------------------------------===//
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


// Conversions between different Unicode encodings.  Note that UTF-16 and
// UTF-32 decoding are *not* currently resilient to erroneous data.

/// The result of one Unicode decoding step.
///
/// A unicode scalar value, an indication that no more unicode scalars
/// are available, or an indication of a decoding error.
public enum UnicodeDecodingResult {
  case Result(UnicodeScalar)
  case EmptyInput
  case Error

  /// Returns `true` if `self` indicates no more unicode scalars are
  /// available.
  @warn_unused_result
  public func isEmptyInput() -> Bool {
    switch self {
    case .EmptyInput:
      return true
    default:
      return false
    }
  }
}

/// A Unicode [encoding scheme](http://www.unicode.org/glossary/#character_encoding_scheme).
///
/// Consists of an underlying [code unit](http://www.unicode.org/glossary/#code_unit) and functions to
/// translate between sequences of these code units and [unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value).
public protocol UnicodeCodecType {

  /// A type that can hold [code unit](http://www.unicode.org/glossary/#code_unit) values for this
  /// encoding.
  associatedtype CodeUnit

  init()

  /// Start or continue decoding a UTF sequence.
  ///
  /// In order to decode a code unit sequence completely, this function should
  /// be called repeatedly until it returns `UnicodeDecodingResult.EmptyInput`.
  /// Checking that the generator was exhausted is not sufficient.  The decoder
  /// can have an internal buffer that is pre-filled with data from the input
  /// generator.
  ///
  /// Because of buffering, it is impossible to find the corresponding position
  /// in the generator for a given returned `UnicodeScalar` or an error.
  ///
  /// - parameter next: A generator of code units to be decoded.
  mutating func decode<
    G : GeneratorType where G.Element == CodeUnit
  >(next: inout G) -> UnicodeDecodingResult

  /// Encode a `UnicodeScalar` as a series of `CodeUnit`s by
  /// calling `output` on each `CodeUnit`.
  static func encode(input: UnicodeScalar, @noescape output: (CodeUnit) -> Void)
}

/// A codec for [UTF-8](http://www.unicode.org/glossary/#UTF_8).
public struct UTF8 : UnicodeCodecType {

  /// A type that can hold [code unit](http://www.unicode.org/glossary/#code_unit) values for this
  /// encoding.
  public typealias CodeUnit = UInt8

  public init() {}

  /// Lookahead buffer used for UTF-8 decoding.  New bytes are inserted at LSB,
  /// and bytes are read at MSB.
  var _decodeLookahead: UInt32 = 0

  /// Flags with layout: `0bxxxx_yyyy`.
  ///
  /// `xxxx` is the EOF flag.  It means that the input generator has signaled
  /// end of sequence.  Out of the four bits, only one bit can be set.  The bit
  /// position specifies how many bytes have been consumed from the lookahead
  /// buffer already.  A value of `1000` means that there are `yyyy` bytes in
  /// the buffer, `0100` means that there are `yyyy - 1` bytes, `0010` --
  /// `yyyy - 2`, `0001` -- `yyyy - 3`.
  ///
  /// `yyyy` specifies how many bytes are valid in the lookahead buffer.  Value
  /// is expressed in unary code.  Valid values: `1111` (4), `0111` (3),
  /// `0011` (2), `0001` (1), `0000` (0).
  ///
  /// This representation is crafted to allow one to consume a byte from a
  /// buffer with a shift, and update flags with a single-bit right shift.
  var _lookaheadFlags: UInt8 = 0


  /// Returns `true` if the LSB bytes in `buffer` are a well-formed UTF-8 code
  /// unit sequence. The lowest byte is considered the first code unit.
  ///
  /// - Requires: There is at least one used byte in `buffer`, and the unused
  /// space in `buffer` is filled with some value not matching the UTF-8
  /// continuation byte form (`0b10xxxxxx`).
  @warn_unused_result
  public // @testable
  static func _isValidUTF8(buffer: UInt32) -> Bool {

    if _fastPath(buffer & 0x80 == 0) {
      return true // 0x00 -- 0x7f: 1-byte sequences (ASCII).
    }

    // Determine sequence length using high 5 bits of 1st byte. We use a
    // look-up table to branch less. 1-byte sequences are handled above.
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
    case (0, 0): // 2-byte sequence.
      // Require 10xx xxxx  110x xxxx.
      if buffer & 0xc0e0 != 0x80c0 { return false }
      // Disallow xxxx xxxx  xxx0 000x (<= 7 bits case).
      if buffer & 0x001e == 0x0000 { return false }
      return true
    case (0, 1): // 3-byte sequence.
      // Require 10xx xxxx  10xx xxxx  1110 xxxx.
      if buffer & 0xc0c0f0 != 0x8080e0 { return false }
      // Disallow xxxx xxxx  xx0x xxxx  xxxx 0000 (<= 11 bits case).
      if buffer & 0x00200f == 0x000000 { return false }
      // Disallow xxxx xxxx  xx1x xxxx  xxxx 1101 (surrogate code points).
      if buffer & 0x00200f == 0x00200d { return false }
      return true
    case (1, 0): // 4-byte sequence.
      // Require 10xx xxxx  10xx xxxx  10xx xxxx  1111 0xxx.
      if buffer & 0xc0c0c0f8 != 0x808080f0 { return false }
      // Disallow xxxx xxxx  xxxx xxxx  xx00 xxxx  xxxx x000 (<= 16 bits case).
      if buffer & 0x00003007 == 0x00000000 { return false }
      // Case xxxx xxxx  xxxx xxxx  xxxx xxxx  xxxx x1xx.
      if buffer & 0x00000004 == 0x00000004 {
        // Require xxxx xxxx  xxxx xxxx  xx00 xxxx  xxxx xx00 (<= 0x10FFFF).
        if buffer & 0x00003003 != 0x00000000 { return false }
      }
      return true
    default: // Invalid sequence.
      return false
    }
  }

  /// Given an ill-formed sequence, find the length of its maximal subpart.
  @inline(never)
  @warn_unused_result
  static func _findMaximalSubpartOfIllFormedUTF8Sequence(
      buffer: UInt32, validBytes: UInt8) -> UInt8 {
    var buffer = buffer
    var validBytes = validBytes
    // This function is '@inline(never)' because it is used only in the error
    // handling path.

    // Clear EOF flag, we don't care about it.
    validBytes &= 0b0000_1111

    _sanityCheck(validBytes != 0,
        "input buffer should not be empty")
    _sanityCheck(!UTF8._isValidUTF8(buffer),
        "input sequence should be ill-formed UTF-8")

    // Unicode 6.3.0, D93b:
    //
    //     Maximal subpart of an ill-formed subsequence: The longest code unit
    //     subsequence starting at an unconvertible offset that is either:
    //     a. the initial subsequence of a well-formed code unit sequence, or
    //     b. a subsequence of length one.

    // Perform case analysis.  See Unicode 6.3.0, Table 3-7. Well-Formed UTF-8
    // Byte Sequences.

    let cu0 = UInt8(buffer & 0xff)
    buffer >>= 8
    validBytes >>= 1
    if (cu0 >= 0xc2 && cu0 <= 0xdf) {
      // First byte is valid, but we know that this code unit sequence is
      // invalid, so the maximal subpart has to end after the first byte.
      return 1
    }

    if validBytes == 0 {
      return 1
    }

    let cu1 = UInt8(buffer & 0xff)
    buffer >>= 8
    validBytes >>= 1

    if (cu0 == 0xe0) {
      return (cu1 >= 0xa0 && cu1 <= 0xbf) ? 2 : 1
    }
    if (cu0 >= 0xe1 && cu0 <= 0xec) {
      return (cu1 >= 0x80 && cu1 <= 0xbf) ? 2 : 1
    }
    if (cu0 == 0xed) {
      return (cu1 >= 0x80 && cu1 <= 0x9f) ? 2 : 1
    }
    if (cu0 >= 0xee && cu0 <= 0xef) {
      return (cu1 >= 0x80 && cu1 <= 0xbf) ? 2 : 1
    }
    if (cu0 == 0xf0) {
      if (cu1 >= 0x90 && cu1 <= 0xbf) {
        if validBytes == 0 {
          return 2
        }

        let cu2 = UInt8(buffer & 0xff)
        return (cu2 >= 0x80 && cu2 <= 0xbf) ? 3 : 2
      }
      return 1
    }
    if (cu0 >= 0xf1 && cu0 <= 0xf3) {
      if (cu1 >= 0x80 && cu1 <= 0xbf) {
        if validBytes == 0 {
          return 2
        }

        let cu2 = UInt8(buffer & 0xff)
        return (cu2 >= 0x80 && cu2 <= 0xbf) ? 3 : 2
      }
      return 1
    }
    if (cu0 == 0xf4) {
      if (cu1 >= 0x80 && cu1 <= 0x8f) {
        if validBytes == 0 {
          return 2
        }

        let cu2 = UInt8(buffer & 0xff)
        return (cu2 >= 0x80 && cu2 <= 0xbf) ? 3 : 2
      }
      return 1
    }

    _sanityCheck((cu0 >= 0x80 && cu0 <= 0xc1) || cu0 >= 0xf5,
        "case analysis above should have handled all valid first bytes")

    // There are no well-formed sequences that start with these bytes.  Maximal
    // subpart is defined to have length 1 in these cases.
    return 1
  }

  /// Start or continue decoding a UTF sequence.
  ///
  /// In order to decode a code unit sequence completely, this function should
  /// be called repeatedly until it returns `UnicodeDecodingResult.EmptyInput`.
  /// Checking that the generator was exhausted is not sufficient.  The decoder
  /// can have an internal buffer that is pre-filled with data from the input
  /// generator.
  ///
  /// Because of buffering, it is impossible to find the corresponding position
  /// in the generator for a given returned `UnicodeScalar` or an error.
  ///
  /// - parameter next: A generator of code units to be decoded.
  public mutating func decode<
    G : GeneratorType where G.Element == CodeUnit
  >(next: inout G) -> UnicodeDecodingResult {
    // If the EOF flag is not set, fill the lookahead buffer from the input
    // generator.
    if _lookaheadFlags & 0b1111_0000 == 0 {
      // Add more bytes into the buffer until we have 4.
      while _lookaheadFlags != 0b0000_1111 {
        if let codeUnit = next.next() {
          _decodeLookahead = (_decodeLookahead << 8) | UInt32(codeUnit)
          _lookaheadFlags = (_lookaheadFlags << 1) | 1
        } else {
          // Set the EOF flag.
          switch _lookaheadFlags & 0b0000_1111 {
          case 0b1111:
            _sanityCheckFailure("should have not entered buffer refill loop")
          case 0b0111:
            _lookaheadFlags |= 0b0100_0000
          case 0b0011:
            _lookaheadFlags |= 0b0010_0000
          case 0b0001:
            _lookaheadFlags |= 0b0001_0000
          case 0b0000:
            _lookaheadFlags |= 0b1000_0000
            return .EmptyInput
          default:
            _sanityCheckFailure("bad value in _lookaheadFlags")
          }
          break
        }
      }
    }

    if _slowPath(_lookaheadFlags & 0b0000_1111 == 0) {
      return .EmptyInput
    }

    if _slowPath(_lookaheadFlags & 0b1111_0000 != 0) {
      // Reached EOF.  Restore the invariant: first unread byte is always at
      // MSB.
      switch _lookaheadFlags & 0b1111_0000 {
      case 0b1000_0000:
        break
      case 0b0100_0000:
        _decodeLookahead <<= 1 * 8
      case 0b0010_0000:
        _decodeLookahead <<= 2 * 8
      case 0b0001_0000:
        _decodeLookahead <<= 3 * 8
      default:
        _sanityCheckFailure("bad value in _lookaheadFlags")
      }
      _lookaheadFlags = (_lookaheadFlags & 0b0000_1111) | 0b1000_0000
    }

    // The first byte to read is located at MSB of `_decodeLookahead`.  Get a
    // representation of the buffer where we can read bytes starting from LSB.
    var buffer = _decodeLookahead.byteSwapped
    if _slowPath(!UTF8._isValidUTF8(buffer)) {
      // The code unit sequence is ill-formed.  According to Unicode
      // recommendation, replace the maximal subpart of ill-formed sequence
      // with one replacement character.
      _lookaheadFlags >>=
          UTF8._findMaximalSubpartOfIllFormedUTF8Sequence(buffer,
              validBytes: _lookaheadFlags)
      return .Error
    }

    // At this point we know that `buffer` starts with a well-formed code unit
    // sequence.  Decode it.
    //
    // When consuming bytes from the `buffer`, we just need to update
    // `_lookaheadFlags`.  The stored buffer in `_decodeLookahead` will be
    // shifted at the beginning of the next decoding cycle.
    let cu0 = UInt8(buffer & 0xff)
    buffer >>= 8
    _lookaheadFlags >>= 1

    if cu0 < 0x80 {
      // 1-byte sequences.
      return .Result(UnicodeScalar(UInt32(cu0)))
    }

    // Start with octet 1 (we'll mask off high bits later).
    var result = UInt32(cu0)

    let cu1 = UInt8(buffer & 0xff)
    buffer >>= 8
    _lookaheadFlags >>= 1
    result = (result << 6) | UInt32(cu1 & 0x3f)
    if cu0 < 0xe0 {
      // 2-byte sequences.
      return .Result(UnicodeScalar(result & 0x000007ff)) // 11 bits
    }

    let cu2 = UInt8(buffer & 0xff)
    buffer >>= 8
    _lookaheadFlags >>= 1
    result = (result << 6) | UInt32(cu2 & 0x3f)
    if cu0 < 0xf0 {
      // 3-byte sequences.
      return .Result(UnicodeScalar(result & 0x0000ffff)) // 16 bits
    }

    // 4-byte sequences.
    let cu3 = UInt8(buffer & 0xff)
    _lookaheadFlags >>= 1
    result = (result << 6) | UInt32(cu3 & 0x3f)
    return .Result(UnicodeScalar(result & 0x001fffff)) // 21 bits
  }

  /// Encode a `UnicodeScalar` as a series of `CodeUnit`s by
  /// calling `output` on each `CodeUnit`.
  public static func encode(
    input: UnicodeScalar,
    @noescape output put: (CodeUnit) -> Void
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
          put(UInt8(c | 0xF0)) // 11110xxx
        }
        put(buf1)
      }
      put(buf2)
    }
    put(buf3)
  }

  /// Returns `true` if `byte` is a continuation byte of the form
  /// `0b10xxxxxx`.
  @warn_unused_result
  public static func isContinuation(byte: CodeUnit) -> Bool {
    return byte & 0b11_00__0000 == 0b10_00__0000
  }

  var _value =  UInt8()
}

/// A codec for [UTF-16](http://www.unicode.org/glossary/#UTF_16).
public struct UTF16 : UnicodeCodecType {
  /// A type that can hold [code unit](http://www.unicode.org/glossary/#code_unit) values for this
  /// encoding.
  public typealias CodeUnit = UInt16

  public init() {}

  /// A lookahead buffer for one UTF-16 code unit.
  var _decodeLookahead: UInt32 = 0

  /// Flags with layout: `0b0000_00xy`.
  ///
  /// `y` is the EOF flag.
  ///
  /// `x` is set when `_decodeLookahead` contains a code unit.
  var _lookaheadFlags: UInt8 = 0

  /// Start or continue decoding a UTF sequence.
  ///
  /// In order to decode a code unit sequence completely, this function should
  /// be called repeatedly until it returns `UnicodeDecodingResult.EmptyInput`.
  /// Checking that the generator was exhausted is not sufficient.  The decoder
  /// can have an internal buffer that is pre-filled with data from the input
  /// generator.
  ///
  /// Because of buffering, it is impossible to find the corresponding position
  /// in the generator for a given returned `UnicodeScalar` or an error.
  ///
  /// - parameter next: A generator of code units to be decoded.
  public mutating func decode<
    G : GeneratorType where G.Element == CodeUnit
  >(input: inout G) -> UnicodeDecodingResult {
    if _lookaheadFlags & 0b01 != 0 {
      return .EmptyInput
    }

    // Note: maximal subpart of ill-formed sequence for UTF-16 can only have
    // length 1.  Length 0 does not make sense.  Neither does length 2 -- in
    // that case the sequence is valid.

    var unit0: UInt32
    if _fastPath(_lookaheadFlags & 0b10 == 0) {
      if let first = input.next() {
        unit0 = UInt32(first)
      } else {
        // Set EOF flag.
        _lookaheadFlags |= 0b01
        return .EmptyInput
      }
    } else {
      // Fetch code unit from the lookahead buffer and note this fact in flags.
      unit0 = _decodeLookahead
      _lookaheadFlags &= 0b01
    }

    // A well-formed pair of surrogates looks like this:
    // [1101 10ww wwxx xxxx] [1101 11xx xxxx xxxx]

    if _fastPath((unit0 >> 11) != 0b1101_1) {
      // Neither high-surrogate, nor low-surrogate -- sequence of 1 code unit,
      // decoding is trivial.
      return .Result(UnicodeScalar(unit0))
    }

    if _slowPath((unit0 >> 10) == 0b1101_11) {
      // `unit0` is a low-surrogate.  We have an ill-formed sequence.
      return .Error
    }

    // At this point we know that `unit0` is a high-surrogate.

    var unit1: UInt32
    if let second = input.next() {
      unit1 = UInt32(second)
    } else {
      // EOF reached.  Set EOF flag.
      _lookaheadFlags |= 0b01

      // We have seen a high-surrogate and EOF, so we have an ill-formed
      // sequence.
      return .Error
    }

    if _fastPath((unit1 >> 10) == 0b1101_11) {
      // `unit1` is a low-surrogate.  We have a well-formed surrogate pair.

      let result = 0x10000 + (((unit0 & 0x03ff) << 10) | (unit1 & 0x03ff))
      return .Result(UnicodeScalar(result))
    }

    // Otherwise, we have an ill-formed sequence.  These are the possible
    // cases:
    //
    // * `unit1` is a high-surrogate, so we have a pair of two high-surrogates.
    //
    // * `unit1` is not a surrogate.  We have an ill-formed sequence:
    //   high-surrogate followed by a non-surrogate.

    // Save the second code unit in the lookahead buffer.
    _decodeLookahead = unit1
    _lookaheadFlags |= 0b10
    return .Error
  }

  /// Try to decode one Unicode scalar, and return the actual number of code
  /// units it spanned in the input.  This function may consume more code
  /// units than required for this scalar.
  mutating func _decodeOne<
    G : GeneratorType where G.Element == CodeUnit
  >(input: inout G) -> (UnicodeDecodingResult, Int) {
    let result = decode(&input)
    switch result {
    case .Result(let us):
      return (result, UTF16.width(us))

    case .EmptyInput:
      return (result, 0)

    case .Error:
      return (result, 1)
    }
  }

  /// Encode a `UnicodeScalar` as a series of `CodeUnit`s by
  /// calling `output` on each `CodeUnit`.
  public static func encode(
    input: UnicodeScalar,
    @noescape output put: (CodeUnit) -> Void
  ) {
    let scalarValue: UInt32 = UInt32(input)

    if scalarValue <= UInt32(UInt16.max) {
      put(UInt16(scalarValue))
    }
    else {
      let lead_offset = UInt32(0xd800) - UInt32(0x10000 >> 10)
      put(UInt16(lead_offset + (scalarValue >> 10)))
      put(UInt16(0xdc00 + (scalarValue & 0x3ff)))
    }
  }

  var _value = UInt16()
}

/// A codec for [UTF-32](http://www.unicode.org/glossary/#UTF_32).
public struct UTF32 : UnicodeCodecType {
  /// A type that can hold [code unit](http://www.unicode.org/glossary/#code_unit) values for this
  /// encoding.
  public typealias CodeUnit = UInt32

  public init() {}

  /// Start or continue decoding a UTF sequence.
  ///
  /// In order to decode a code unit sequence completely, this function should
  /// be called repeatedly until it returns `UnicodeDecodingResult.EmptyInput`.
  /// Checking that the generator was exhausted is not sufficient.  The decoder
  /// can have an internal buffer that is pre-filled with data from the input
  /// generator.
  ///
  /// Because of buffering, it is impossible to find the corresponding position
  /// in the generator for a given returned `UnicodeScalar` or an error.
  ///
  /// - parameter next: A generator of code units to be decoded.
  public mutating func decode<
    G : GeneratorType where G.Element == CodeUnit
  >(input: inout G) -> UnicodeDecodingResult {
    return UTF32._decode(&input)
  }

  static func _decode<
    G : GeneratorType where G.Element == CodeUnit
  >(input: inout G) -> UnicodeDecodingResult {
    guard let x = input.next() else { return .EmptyInput }
    if _fastPath((x >> 11) != 0b1101_1 && x <= 0x10ffff) {
      return .Result(UnicodeScalar(x))
    } else {
      return .Error
    }
  }

  /// Encode a `UnicodeScalar` as a series of `CodeUnit`s by
  /// calling `output` on each `CodeUnit`.
  public static func encode(
    input: UnicodeScalar,
    @noescape output put: (CodeUnit) -> Void
  ) {
    put(UInt32(input))
  }
}

/// Translate `input`, in the given `InputEncoding`, into `output`, in
/// the given `OutputEncoding`.
///
/// - parameter stopOnError: Causes encoding to stop when an encoding
///   error is detected in `input`, if `true`.  Otherwise, U+FFFD
///   replacement characters are inserted for each detected error.
public func transcode<
  Input : GeneratorType,
  InputEncoding : UnicodeCodecType,
  OutputEncoding : UnicodeCodecType
  where InputEncoding.CodeUnit == Input.Element>(
  inputEncoding: InputEncoding.Type, _ outputEncoding: OutputEncoding.Type,
  _ input: Input, @noescape _ output: (OutputEncoding.CodeUnit) -> Void,
  stopOnError: Bool
) -> Bool {
  var input = input

  // NB.  It is not possible to optimize this routine to a memcpy if
  // InputEncoding == OutputEncoding.  The reason is that memcpy will not
  // substitute U+FFFD replacement characters for ill-formed sequences.

  var inputDecoder = inputEncoding.init()
  var hadError = false
  var scalar = inputDecoder.decode(&input)
  while !scalar.isEmptyInput() {
    switch scalar {
    case .Result(let us):
      OutputEncoding.encode(us, output: output)
    case .EmptyInput:
      _sanityCheckFailure("should not enter the loop when input becomes empty")
    case .Error:
      if stopOnError {
        return (hadError: true)
      } else {
        OutputEncoding.encode("\u{fffd}", output: output)
        hadError = true
      }
    }
    scalar = inputDecoder.decode(&input)
  }
  return hadError
}

/// Transcode UTF-16 to UTF-8, replacing ill-formed sequences with U+FFFD.
///
/// Returns the index of the first unhandled code unit and the UTF-8 data
/// that was encoded.
@warn_unused_result
internal func _transcodeSomeUTF16AsUTF8<
  Input : CollectionType
  where Input.Generator.Element == UInt16>(
  input: Input, _ startIndex: Input.Index
) -> (Input.Index, _StringCore.UTF8Chunk) {
  typealias UTF8Chunk = _StringCore.UTF8Chunk

  let endIndex = input.endIndex
  let utf8Max = sizeof(UTF8Chunk.self)
  var result: UTF8Chunk = 0
  var utf8Count = 0
  var nextIndex = startIndex
  while nextIndex != input.endIndex && utf8Count != utf8Max {
    let u = UInt(input[nextIndex])
    let shift = UTF8Chunk(utf8Count * 8)
    var utf16Length: Input.Index.Distance = 1

    if _fastPath(u <= 0x7f) {
      result |= UTF8Chunk(u) << shift
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
        } else if _slowPath(nextIndex.advancedBy(1) == endIndex) {
          // We have seen a high-surrogate and EOF, so we have an ill-formed
          // sequence.  Replace it with U+FFFD.
          r = 0xbdbfef
          scalarUtf8Length = 3
        } else {
          let unit1 = UInt(input[nextIndex.advancedBy(1)])
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
    nextIndex = nextIndex.advancedBy(utf16Length)
  }
  // FIXME: Annoying check, courtesy of <rdar://problem/16740169>
  if utf8Count < sizeofValue(result) {
    result |= ~0 << numericCast(utf8Count * 8)
  }
  return (nextIndex, result)
}

/// Instances of conforming types are used in internal `String`
/// representation.
public // @testable
protocol _StringElementType {
  @warn_unused_result
  static func _toUTF16CodeUnit(_: Self) -> UTF16.CodeUnit

  @warn_unused_result
  static func _fromUTF16CodeUnit(utf16: UTF16.CodeUnit) -> Self
}

extension UTF16.CodeUnit : _StringElementType {
  public // @testable
  static func _toUTF16CodeUnit(x: UTF16.CodeUnit) -> UTF16.CodeUnit {
    return x
  }
  public // @testable
  static func _fromUTF16CodeUnit(
    utf16: UTF16.CodeUnit
  ) -> UTF16.CodeUnit {
    return utf16
  }
}

extension UTF8.CodeUnit : _StringElementType {
  public // @testable
  static func _toUTF16CodeUnit(x: UTF8.CodeUnit) -> UTF16.CodeUnit {
    _sanityCheck(x <= 0x7f, "should only be doing this with ASCII")
    return UTF16.CodeUnit(x)
  }
  public // @testable
  static func _fromUTF16CodeUnit(
    utf16: UTF16.CodeUnit
  ) -> UTF8.CodeUnit {
    _sanityCheck(utf16 <= 0x7f, "should only be doing this with ASCII")
    return UTF8.CodeUnit(utf16)
  }
}

extension UTF16 {
  /// Returns the number of code units required to encode `x`.
  @warn_unused_result
  public static func width(x: UnicodeScalar) -> Int {
    return x.value <= 0xFFFF ? 1 : 2
  }

  /// Returns the high surrogate code unit of a [surrogate pair](http://www.unicode.org/glossary/#surrogate_pair) representing
  /// `x`.
  ///
  /// - Requires: `width(x) == 2`.
  @warn_unused_result
  public static func leadSurrogate(x: UnicodeScalar) -> UTF16.CodeUnit {
    _precondition(width(x) == 2)
    return UTF16.CodeUnit((x.value - 0x1_0000) >> (10 as UInt32)) + 0xD800
  }

  /// Returns the low surrogate code unit of a [surrogate pair](http://www.unicode.org/glossary/#surrogate_pair) representing
  /// `x`.
  ///
  /// - Requires: `width(x) == 2`.
  @warn_unused_result
  public static func trailSurrogate(x: UnicodeScalar) -> UTF16.CodeUnit {
    _precondition(width(x) == 2)
    return UTF16.CodeUnit(
      (x.value - 0x1_0000) & (((1 as UInt32) << 10) - 1)
    ) + 0xDC00
  }

  @warn_unused_result
  public static func isLeadSurrogate(x: CodeUnit) -> Bool {
    return 0xD800...0xDBFF ~= x
  }

  @warn_unused_result
  public static func isTrailSurrogate(x: CodeUnit) -> Bool {
    return 0xDC00...0xDFFF ~= x
  }

  public // @testable
  static func _copy<T : _StringElementType, U : _StringElementType>(
    source: UnsafeMutablePointer<T>,
    destination: UnsafeMutablePointer<U>, count: Int
  ) {
    if strideof(T.self) == strideof(U.self) {
      _memcpy(
        dest: UnsafeMutablePointer(destination),
        src: UnsafeMutablePointer(source),
        size: UInt(count) * UInt(strideof(U.self)))
    }
    else {
      for i in 0..<count {
        let u16 = T._toUTF16CodeUnit((source + i).memory)
        (destination + i).memory = U._fromUTF16CodeUnit(u16)
      }
    }
  }

  /// Returns the number of UTF-16 code units required for the given code unit
  /// sequence when transcoded to UTF-16, and a bit describing if the sequence
  /// was found to contain only ASCII characters.
  ///
  /// If `repairIllFormedSequences` is `true`, the function always succeeds.
  /// If it is `false`, `nil` is returned if an ill-formed code unit sequence is
  /// found in `input`.
  @warn_unused_result
  public static func measure<
      Encoding : UnicodeCodecType, Input : GeneratorType
      where Encoding.CodeUnit == Input.Element
  >(
    _: Encoding.Type, input: Input, repairIllFormedSequences: Bool
  ) -> (Int, Bool)? {
    var input = input
    var count = 0
    var isAscii = true

    var inputDecoder = Encoding()
    loop:
    while true {
      switch inputDecoder.decode(&input) {
      case .Result(let us):
        if us.value > 0x7f {
          isAscii = false
        }
        count += width(us)
      case .EmptyInput:
        break loop
      case .Error:
        if !repairIllFormedSequences {
          return nil
        }
        isAscii = false
        count += width(UnicodeScalar(0xfffd))
      }
    }
    return (count, isAscii)
  }
}

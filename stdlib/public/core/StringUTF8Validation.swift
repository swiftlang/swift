//===--- StringUTF8Validation.swift ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

private func _isUTF8MultiByteLeading(_ x: UInt8) -> Bool {
  return (0xC2...0xF4).contains(x)
}

private func _isNotOverlong_F0(_ x: UInt8) -> Bool {
  return (0x90...0xBF).contains(x)
}

private func _isNotInvalid_F4(_ x: UInt8) -> Bool {
  return UTF8.isContinuation(x) && x <= 0x8F
}

private func _isNotOverlong_E0(_ x: UInt8) -> Bool {
  return (0xA0...0xBF).contains(x)
}

private func _isNotInvalid_ED(_ x: UInt8) -> Bool {
  return UTF8.isContinuation(x) && x <= 0x9F
}

internal struct UTF8ExtraInfo: Equatable {
  public var isASCII: Bool
}

@inline(never) // slow-path
private func _diagnoseInvalidUTF8MultiByteLeading(
  _ x: UInt8
) -> _UTF8EncodingErrorKind {
  _internalInvariant(x >= 0x80)
  _internalInvariant(!_isUTF8MultiByteLeading(x))
  switch x {
  case 0x80...0xBF:
    return .unexpectedContinuationByte
  case 0xC0..<0xC2:
    return .overlongEncodingByte
  default:
    _internalInvariant(x > 0xF4)
    return .invalidNonSurrogateCodePointByte
  }
}

internal enum UTF8ValidationResult {
  case success(UTF8ExtraInfo)
  case error(
    kind: _UTF8EncodingErrorKind, toBeReplaced: Range<Int>
  )
}

// FIXME: refactor other parts of stdlib to avoid this dumb mirror enum
//
// Mirror of UTF8.ValidationError.Kind, available on 6.1
internal struct _UTF8EncodingErrorKind: Error, Sendable, Hashable
// TODO: embedded?, Codable
  , RawRepresentable {
  internal var rawValue: UInt8

  @available(SwiftStdlib 6.2, *)
  internal var _publicKind: UTF8.ValidationError.Kind {
    .init(rawValue: self.rawValue)!
  }

  @inlinable
  internal init(rawValue: UInt8) {
    self.rawValue = rawValue
  }

  /// A continuation byte (`10xxxxxx`) outside of a multi-byte sequence
  @_alwaysEmitIntoClient
  internal static var unexpectedContinuationByte: Self {
    .init(rawValue: 0)
  }

  /// A byte in a surrogate code point (`U+D800..U+DFFF`) sequence
  @_alwaysEmitIntoClient
  internal static var surrogateCodePointByte: Self {
    .init(rawValue: 1)
  }

  /// A byte in an invalid, non-surrogate code point (`>U+10FFFF`) sequence
  @_alwaysEmitIntoClient
  internal static var invalidNonSurrogateCodePointByte: Self {
    .init(rawValue: 2)
  }

  /// A byte in an overlong encoding sequence
  @_alwaysEmitIntoClient
  internal static var overlongEncodingByte: Self {
    .init(rawValue: 3)
  }

  /// A multi-byte sequence that is the start of a valid multi-byte scalar
  /// but is cut off before ending correctly
  @_alwaysEmitIntoClient
  internal static var truncatedScalar: Self {
    .init(rawValue: 4)
  }
}

extension UTF8ValidationResult: Equatable {}

internal func validateUTF8(_ buf: UnsafeBufferPointer<UInt8>) -> UTF8ValidationResult {
  if unsafe _allASCII(buf) {
    return .success(UTF8ExtraInfo(isASCII: true))
  }

  var iter = unsafe buf.makeIterator()
  var lastValidIndex = buf.startIndex

  @inline(__always) func guarantee(
    _ f: (UInt8) -> Bool,
    _ err: _UTF8EncodingErrorKind
  ) throws(_UTF8EncodingErrorKind) {
    guard let cu = unsafe iter.next() else {
      throw .truncatedScalar
    }
    guard f(cu) else {
      throw err
    }
  }
  @inline(__always) func guaranteeContinuation(
  ) throws(_UTF8EncodingErrorKind) {
    try guarantee(UTF8.isContinuation, .truncatedScalar)
  }

  func _legacyInvalidLengthCalculation(_ _buffer: (_storage: UInt32, ())) -> Int {
    // function body copied from UTF8.ForwardParser._invalidLength
    if _buffer._storage               & 0b0__1100_0000__1111_0000
                                     == 0b0__1000_0000__1110_0000 {
      // 2-byte prefix of 3-byte sequence. The top 5 bits of the decoded result
      // must be nonzero and not a surrogate
      let top5Bits = _buffer._storage & 0b0__0010_0000__0000_1111
      if top5Bits != 0 && top5Bits   != 0b0__0010_0000__0000_1101 { return 2 }
    }
    else if _buffer._storage                & 0b0__1100_0000__1111_1000
                                           == 0b0__1000_0000__1111_0000
    {
      // Prefix of 4-byte sequence. The top 5 bits of the decoded result
      // must be nonzero and no greater than 0b0__0100_0000
      let top5bits = UInt16(_buffer._storage & 0b0__0011_0000__0000_0111)
      if top5bits != 0 && top5bits.byteSwapped <= 0b0__0000_0100__0000_0000 {
        return _buffer._storage   & 0b0__1100_0000__0000_0000__0000_0000
                                 == 0b0__1000_0000__0000_0000__0000_0000 ? 3 : 2
      }
    }
    return 1
  }

  func _legacyNarrowIllegalRange(buf: Slice<UnsafeBufferPointer<UInt8>>) -> Range<Int> {
    var reversePacked: UInt32 = 0
    if let third = unsafe buf.dropFirst(2).first {
      reversePacked |= UInt32(third)
      reversePacked <<= 8
    }
    if let second = unsafe buf.dropFirst().first {
      reversePacked |= UInt32(second)
      reversePacked <<= 8
    }
    unsafe reversePacked |= UInt32(buf.first!)
    let _buffer: (_storage: UInt32, x: ()) = (reversePacked, ())
    let invalids = _legacyInvalidLengthCalculation(_buffer)
    return unsafe buf.startIndex ..< buf.startIndex + invalids
  }

  func findInvalidRange(_ buf: Slice<UnsafeBufferPointer<UInt8>>) -> Range<Int> {
    var endIndex = unsafe buf.startIndex
    var iter = unsafe buf.makeIterator()
    _ = unsafe iter.next()
    while let cu = unsafe iter.next(), UTF8.isContinuation(cu) {
      endIndex += 1
      // Unicode's Maximal subpart of an ill-formed subsequence will yield
      // at most 3 bytes of error.
      if unsafe buf.distance(from: buf.startIndex, to: endIndex) >= 3 {
        break
      }
    }
    let illegalRange = unsafe Range(buf.startIndex...endIndex)
    unsafe _internalInvariant(illegalRange.clamped(to: (buf.startIndex..<buf.endIndex)) == illegalRange,
                 "illegal range out of full range")
    // FIXME: Remove the call to `_legacyNarrowIllegalRange` and return `illegalRange` directly
    return unsafe _legacyNarrowIllegalRange(buf: buf[illegalRange])
  }

  do throws(_UTF8EncodingErrorKind) {

    /*
    The table of valid UTF-8 is:

     ╔════════════════════╦════════╦════════╦════════╦════════╗
     ║    Scalar value    ║ Byte 0 ║ Byte 1 ║ Byte 2 ║ Byte 3 ║
     ╠════════════════════╬════════╬════════╬════════╬════════╣
     ║ U+0000..U+007F     ║ 00..7F ║        ║        ║        ║
     ║ U+0080..U+07FF     ║ C2..DF ║ Contin ║        ║        ║
     ║ U+0800..U+0FFF     ║ E0     ║ A0..BF ║ Contin ║        ║
     ║ U+1000..U+CFFF     ║ E1..EC ║ Contin ║ Contin ║        ║
     ║ U+D000..U+D7FF     ║ ED     ║ 80..9F ║ Contin ║        ║
     ║ U+E000..U+FFFF     ║ EE..EF ║ Contin ║ Contin ║        ║
     ║ U+10000..U+3FFFF   ║ F0     ║ 90..BF ║ Contin ║ Contin ║
     ║ U+40000..U+FFFFF   ║ F1..F3 ║ Contin ║ Contin ║ Contin ║
     ║ U+100000..U+10FFFF ║ F4     ║ 80..8F ║ Contin ║ Contin ║
     ╚════════════════════╩════════╩════════╩════════╩════════╝

     "Contin" is any continuation byte, i.e. 80..BF or 10xxxxxx
     */
    var isASCII = true
    while let cu = unsafe iter.next() {
      if UTF8.isASCII(cu) { lastValidIndex &+= 1; continue }
      isASCII = false
      if _slowPath(!_isUTF8MultiByteLeading(cu)) {
        throw _diagnoseInvalidUTF8MultiByteLeading(cu)
      }
      switch cu {
      case 0xC2...0xDF:
        try guaranteeContinuation()
        lastValidIndex &+= 2
      case 0xE0:
        try guarantee(_isNotOverlong_E0, .overlongEncodingByte)
        try guaranteeContinuation()
        lastValidIndex &+= 3
      case 0xE1...0xEC:
        try guaranteeContinuation()
        try guaranteeContinuation()
        lastValidIndex &+= 3
      case 0xED:
        try guarantee(_isNotInvalid_ED, .surrogateCodePointByte)
        try guaranteeContinuation()
        lastValidIndex &+= 3
      case 0xEE...0xEF:
        try guaranteeContinuation()
        try guaranteeContinuation()
        lastValidIndex &+= 3
      case 0xF0:
        try guarantee(_isNotOverlong_F0, .overlongEncodingByte)
        try guaranteeContinuation()
        try guaranteeContinuation()
        lastValidIndex &+= 4
      case 0xF1...0xF3:
        try guaranteeContinuation()
        try guaranteeContinuation()
        try guaranteeContinuation()
        lastValidIndex &+= 4
      case 0xF4:
        try guarantee(
          _isNotInvalid_F4, .invalidNonSurrogateCodePointByte)
        try guaranteeContinuation()
        try guaranteeContinuation()
        lastValidIndex &+= 4
      default:
        Builtin.unreachable()
      }
    }
    return .success(UTF8ExtraInfo(isASCII: isASCII))
  } catch {
    return unsafe .error(
      kind: error,
      toBeReplaced: findInvalidRange(buf[lastValidIndex...]))
  }
}

internal func repairUTF8(_ input: UnsafeBufferPointer<UInt8>, firstKnownBrokenRange: Range<Int>) -> String {
  _internalInvariant(!input.isEmpty, "empty input doesn't need to be repaired")
  _internalInvariant(firstKnownBrokenRange.clamped(to: input.indices) == firstKnownBrokenRange)
  // During this process, `remainingInput` contains the remaining bytes to process. It's split into three
  // non-overlapping sub-regions:
  //
  //  1. `goodChunk` (may be empty) containing bytes that are known good UTF-8 and can be copied into the output String
  //  2. `brokenRange` (never empty) the next range of broken bytes,
  //  3. the remainder (implicit, will become the next `remainingInput`)
  //
  // At the beginning of the process, the `goodChunk` starts at the beginning and extends to just before the first
  // known broken byte. The known broken bytes are covered in the `brokenRange` and everything following that is
  // the remainder.
  // We then copy the `goodChunk` into the target buffer and append a UTF8 replacement character. `brokenRange` is
  // skipped (replaced by the replacement character) and we restart the same process. This time, `goodChunk` extends
  // from the byte after the previous `brokenRange` to the next `brokenRange`.
  var result = _StringGuts()
  let replacementCharacterCount = Unicode.Scalar._replacementCharacter.withUTF8CodeUnits { $0.count }
  result.reserveCapacity(input.count + 5 * replacementCharacterCount) // extra space for some replacement characters

  var brokenRange: Range<Int> = firstKnownBrokenRange
  var remainingInput = unsafe input
  repeat {
    _internalInvariant(!brokenRange.isEmpty, "broken range empty")
    _internalInvariant(!remainingInput.isEmpty, "empty remaining input doesn't need to be repaired")
    let goodChunk = unsafe remainingInput[..<brokenRange.startIndex]

    // very likely this capacity reservation does not actually do anything because we reserved space for the entire
    // input plus up to five replacement characters up front
    result.reserveCapacity(result.count + remainingInput.count + replacementCharacterCount)

    // we can now safely append the next known good bytes and a replacement character
    unsafe result.appendInPlace(unsafe UnsafeBufferPointer(rebasing: goodChunk),
                         isASCII: false /* appending replacement character anyway, so let's not bother */)
    Unicode.Scalar._replacementCharacter.withUTF8CodeUnits {
      unsafe result.appendInPlace($0, isASCII: false)
    }

    unsafe remainingInput = unsafe UnsafeBufferPointer(rebasing: remainingInput[brokenRange.endIndex...])
    switch unsafe validateUTF8(remainingInput) {
    case .success:
      unsafe result.appendInPlace(remainingInput, isASCII: false)
      return String(result)
    case .error(_, let newBrokenRange):
      brokenRange = newBrokenRange
    }
  } while !remainingInput.isEmpty
  return String(result)
}

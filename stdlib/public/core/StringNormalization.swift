//===--- StringNormalization.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

internal enum _Normalization {
  // ICU's NFC unorm2 instance
  //
  // TODO(String performance): Should we cache one on TLS? Is this an expensive
  // call?
  internal static var _nfcNormalizer: OpaquePointer = {
    var err = __swift_stdlib_U_ZERO_ERROR
    let normalizer = __swift_stdlib_unorm2_getNFCInstance(&err)
    guard err.isSuccess else {
      // This shouldn't be possible unless some deep (unrecoverable) system
      // invariants are violated
      fatalError("Unable to talk to ICU")
    }
    return normalizer
  }()

  // When normalized in NFC, some segments may expand in size (e.g. some non-BMP
  // musical notes). This expansion is capped by the maximum expansion factor of
  // the normal form. For NFC, that is 3x.
  internal static let _maxNFCExpansionFactor = 3
}

extension Unicode.Scalar {
  // Normalization boundary - a place in a string where everything left of the
  // boundary can be normalized independently from everything right of the
  // boundary. The concatenation of each result is the same as if the entire
  // string had been normalized as a whole.
  //
  // Normalization segment - a sequence of code units between two normalization
  // boundaries (without any boundaries in the middle). Note that normalization
  // segments can, as a process of normalization, expand, contract, and even
  // produce new sub-segments.

  // Whether this scalar value always has a normalization boundary before it.
  @inline(__always) // common fast-path
  internal var _hasNormalizationBoundaryBefore: Bool {
    // Fast-path: All scalars up through U+02FF are NFC and have boundaries
    // before them
    if self.value < 0x300 { return true }

    _internalInvariant(Int32(exactly: self.value) != nil, "top bit shouldn't be set")
    let value = Int32(bitPattern: self.value)
    return 0 != __swift_stdlib_unorm2_hasBoundaryBefore(
      _Normalization._nfcNormalizer, value)
  }
  @inline(__always) // common fast-path
  internal var _isNFCQCYes: Bool {
    // Fast-path: All scalars up through U+02FF are NFC and have boundaries
    // before them
    if self.value < 0x300 { return true }

    return __swift_stdlib_u_getIntPropertyValue(
      Builtin.reinterpretCast(value), __swift_stdlib_UCHAR_NFC_QUICK_CHECK
    ) == 1
  }

  // Quick check if a scalar is NFC and a segment starter
  internal var _isNFCStarter: Bool {
    // Otherwise, consult the properties
    return self._hasNormalizationBoundaryBefore && self._isNFCQCYes
  }
}

internal func _tryNormalize(
  _ input: UnsafeBufferPointer<UInt16>,
  into outputBuffer:
    UnsafeMutablePointer<_Normalization._SegmentOutputBuffer>
) -> Int? {
  return _tryNormalize(input, into: _castOutputBuffer(outputBuffer))
}
internal func _tryNormalize(
  _ input: UnsafeBufferPointer<UInt16>,
  into outputBuffer: UnsafeMutableBufferPointer<UInt16>
) -> Int? {
  var err = __swift_stdlib_U_ZERO_ERROR
  let count = __swift_stdlib_unorm2_normalize(
    _Normalization._nfcNormalizer,
    input.baseAddress._unsafelyUnwrappedUnchecked,
    numericCast(input.count),
    outputBuffer.baseAddress._unsafelyUnwrappedUnchecked,
    numericCast(outputBuffer.count),
    &err
  )
  guard err.isSuccess else {
    // The output buffer needs to grow
    return nil
  }
  return numericCast(count)
}

internal enum NormalizationResult {
  case success(SuccessResult)
  case bufferTooSmall(BufferResizeRequest) // The size needed to normalize the rest of the string
  
  struct SuccessResult {
    var amountFilled: Int
    var nextReadPosition: String.Index
  }
  struct BufferResizeRequest {
    var newOutputBufferSize: Int
    var newPreNormalScratchBufferSize: Int
    var newPostNormalScratchBufferSize: Int
  }
  
  static func bufferTooSmall(count: Int) -> NormalizationResult {
    let outputBufferSize = count * 9
    let preNormalBufferSize = count
    let postNormalBufferSize = count * 3
    let resizeRequest = BufferResizeRequest(
      newOutputBufferSize: outputBufferSize, 
      newPreNormalScratchBufferSize: preNormalBufferSize,
      newPostNormalScratchBufferSize: postNormalBufferSize
    )
    return .bufferTooSmall(resizeRequest)
  }
  
  static func success(
    amountFilled filled: Int, nextReadPosition index: String.Index
  ) -> NormalizationResult {
    let successResult = SuccessResult(amountFilled: filled, nextReadPosition: index)
    return .success(successResult)
  }
}

func unimplemented() -> Never { fatalError("Unimplemented function called") }

internal func fastFill(
  _ sourceBuffer: UnsafeBufferPointer<UInt8>,
  _ outputBuffer: UnsafeMutableBufferPointer<UInt8>
) -> (Int, Int) {
  // Quick check if a scalar is NFC and a segment starter
  @inline(__always) func isNFCStarter(_ scalar: Unicode.Scalar) -> Bool {
    // Fast-path: All scalars up through U+02FF are NFC and have boundaries
    // before them
    if scalar.value < 0x300 { return true }

    // Otherwise, consult the properties
    return scalar._hasNormalizationBoundaryBefore && scalar._isNFCQCYes
  }
  
  var outputBufferThreshold: Int {
    return outputBuffer.count - 4
  }

  // TODO: Additional fast-path: All CCC-ascending NFC_QC segments are NFC
  // TODO: Just freakin do normalization and don't bother with ICU
  var outputCount = 0
  let outputEnd = outputBufferThreshold
  var inputCount = 0
  let inputEnd = sourceBuffer.count
  while inputCount < inputEnd && outputCount < outputEnd {
    // TODO: Slightly faster code-unit scan for latiny (<0xCC)

    // Check scalar-based fast-paths
    let (scalar, len) = _decodeScalar(sourceBuffer, startingAt: inputCount)
    _internalInvariant(inputCount &+ len <= inputEnd)

    if _slowPath(
         !sourceBuffer.hasNormalizationBoundary(before: inputCount &+ len)
      || !isNFCStarter(scalar)
    ) {
      break
    }
    inputCount &+= len

    for cu in UTF8.encode(scalar)._unsafelyUnwrappedUnchecked {
      outputBuffer[outputCount] = cu
      outputCount &+= 1
    }

    _internalInvariant(inputCount == outputCount,
      "non-normalizing UTF-8 fast path should be 1-to-1 in code units")
  }
  return (inputCount, outputCount)
}

internal func transcodeToUTF16(
  _ sourceBuffer: UnsafeBufferPointer<UInt8>,
  into outputBuffer: UnsafeMutableBufferPointer<UInt16>
) -> (Int, Int)? {
  var readIndex = 0
  var writeIndex = 0
  let outputCount = outputBuffer.count
  let sourceCount = sourceBuffer.count
  
  while readIndex < sourceCount {
    let (scalar, length) = _decodeScalar(sourceBuffer, startingAt: readIndex)
    
    if scalar._hasNormalizationBoundaryBefore && readIndex != 0 {
      break
    }
    
    readIndex += length
    
    for cu in scalar.utf16 {
      if writeIndex < outputCount {
        outputBuffer[writeIndex] = cu
        writeIndex += 1
      } else {
        return nil
      }
    }
  }
  
  return (readIndex, writeIndex)
}

internal func transcodeToUTF8(
  _ sourceBuffer: UnsafeBufferPointer<UInt16>,
  into outputBuffer: UnsafeMutableBufferPointer<UInt8>
) -> (Int, Int)? {
  var readIndex = 0
  var writeIndex = 0
  let outputCount = outputBuffer.count
  let sourceCount = sourceBuffer.count
  
  while readIndex < sourceCount {
    let (scalar, length) = _decodeScalar(sourceBuffer, startingAt: readIndex)
    //we don't need to check for normalization boundaries here because we are only transcoding
    //a single segment at this point
    
    readIndex += length
    guard scalar.withUTF8CodeUnits({ utf8 in 
      for cu in utf8 {
        if writeIndex < outputCount {
          outputBuffer[writeIndex] = cu
          writeIndex += 1
        } else {
          return false
        }
      }
      return true
    }) else {
      return nil
    }
  }
  return (readIndex, writeIndex)
}

internal func fastNormalize(
  readIndex: String.Index,
  guts: _StringGuts,
  outputBuffer: UnsafeMutableBufferPointer<UInt8>,
  icuInputBuffer: UnsafeMutableBufferPointer<UInt16>,
  icuOutputBuffer: UnsafeMutableBufferPointer<UInt16>
) -> NormalizationResult {
  _internalInvariant(guts.isFastUTF8)
  return guts.withFastUTF8 { sourceBuffer in
    let sourceCount = sourceBuffer.count - readIndex.encodedOffset
    let start = readIndex.encodedOffset
    let rebasedSourceBuffer = UnsafeBufferPointer(rebasing: sourceBuffer[start...])
    do {
      let (read, filled) = fastFill(rebasedSourceBuffer, outputBuffer)
        if filled > 0 {
          let nextIndex = readIndex.encoded(offsetBy: read)
          _internalInvariant(guts.isOnUnicodeScalarBoundary(nextIndex))

          return .success(amountFilled: filled, nextReadPosition: nextIndex)
        }
    }
    guard let (read, filled) = transcodeToUTF16(rebasedSourceBuffer, into: icuInputBuffer) else {
      return .bufferTooSmall(count: sourceBuffer.count)
    }
    
    let nextIndex = readIndex.encoded(offsetBy: read)
    _internalInvariant(guts.isOnUnicodeScalarBoundary(nextIndex))
    
    let rebasedICUInputBuffer = UnsafeBufferPointer(rebasing: icuInputBuffer[..<filled])
    return sharedNormalize(
      sourceCount, nextIndex, outputBuffer, rebasedICUInputBuffer, icuOutputBuffer
    )
  }
}

internal func foreignNormalize(
  readIndex: String.Index,
  guts: _StringGuts,
  outputBuffer: UnsafeMutableBufferPointer<UInt8>,
  icuInputBuffer: UnsafeMutableBufferPointer<UInt16>,
  icuOutputBuffer: UnsafeMutableBufferPointer<UInt16>
) -> NormalizationResult {
  let sourceCount = guts.count - readIndex.encodedOffset
  guard let (read, filled) = foreignFill(readIndex, guts, into: icuInputBuffer) else {
    return .bufferTooSmall(count: guts.count)
  }
  
  let nextIndex = readIndex.encoded(offsetBy: read)
  _internalInvariant(guts.isOnUnicodeScalarBoundary(nextIndex))
  
  let rebasedICUInputBuffer = UnsafeBufferPointer(rebasing: icuInputBuffer[..<filled])
  return sharedNormalize(
    sourceCount, nextIndex, outputBuffer, rebasedICUInputBuffer, icuOutputBuffer
  )
}

func foreignFill(
  _ readIndex: String.Index,
  _ guts: _StringGuts,
  into outputBuffer: UnsafeMutableBufferPointer<UInt16>
) -> (Int, Int)? {
  var index = readIndex
  var writeIndex = 0
  let outputCount = outputBuffer.count
  let cachedEndIndex = guts.endIndex
  while index != cachedEndIndex {
    let (scalar, length) = guts.foreignErrorCorrectedScalar(startingAt: index)
    if scalar._hasNormalizationBoundaryBefore && index != readIndex {
      break
    }
    
    index = index.encoded(offsetBy: length)
    
    for cu in scalar.utf16 {
      if writeIndex < outputCount {
        outputBuffer[writeIndex] = cu
        writeIndex += 1
      } else {
        return nil
      }
    }
  }
  return (index.encodedOffset - readIndex.encodedOffset, writeIndex)
}

private func sharedNormalize(
  _ sourceCount: Int,
  _ nextIndex: String.Index,
  _ outputBuffer: UnsafeMutableBufferPointer<UInt8>,
  _ icuInputBuffer: UnsafeBufferPointer<UInt16>,
  _ icuOutputBuffer: UnsafeMutableBufferPointer<UInt16>
) -> NormalizationResult {
  guard let normalized = _tryNormalize(icuInputBuffer, into: icuOutputBuffer) else {
    return .bufferTooSmall(count: sourceCount)
  }
  
  guard let (_, transcoded) = transcodeToUTF8(
    UnsafeBufferPointer<UInt16>(rebasing: icuOutputBuffer[..<normalized]),
    into: outputBuffer
  ) else {
    return .bufferTooSmall(count: sourceCount)
  }

  return .success(amountFilled: transcoded, nextReadPosition: nextIndex)
}

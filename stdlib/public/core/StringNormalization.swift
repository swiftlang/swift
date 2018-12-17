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
  internal static let _maxUTF16toUTF8ExpansionFactor = 3
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

extension UnsafeBufferPointer where Element == UInt8 {
  internal func isOnUnicodeScalarBoundary(_ index: Int) -> Bool {
    guard index < count else {
      return true
    }
    return !_isContinuation(self[index])
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

internal struct NormalizationResult {
  var amountFilled: Int
  var nextReadPosition: String.Index
  var reallocatedBuffers: Bool
}

private func fastFill(
  _ sourceBuffer: UnsafeBufferPointer<UInt8>,
  _ outputBuffer: UnsafeMutableBufferPointer<UInt8>
) -> (Int, Int)? {
  let outputBufferThreshold = outputBuffer.count - 4
  
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
      || !scalar._isNFCStarter
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
  return outputCount > 0 ? (inputCount, outputCount) : nil
}

// Transcodes a single segment from the source buffer to the outputBuffer as UTF16
private func transcodeSegmentToUTF16(
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

//transcodes the UTF16 segment stored in soureceBuffer into the outputBuffer as UTF8
private func transcodeValidUTF16ToUTF8(
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
    
    for cu in UTF8.encode(scalar)._unsafelyUnwrappedUnchecked {
      if writeIndex < outputCount {
        outputBuffer[writeIndex] = cu
        writeIndex &+= 1
      } else {
        return nil
      }
    }
  }
  return (readIndex, writeIndex)
}

internal func _reallocateBuffers(
  sourceCount count: Int,
  outputBuffer: inout UnsafeMutableBufferPointer<UInt8>,
  icuInputBuffer: inout UnsafeMutableBufferPointer<UInt16>,
  icuOutputBuffer: inout UnsafeMutableBufferPointer<UInt16>
) {
  let output = count * _Normalization._maxNFCExpansionFactor * _Normalization._maxUTF16toUTF8ExpansionFactor
  let icuInput = count
  let icuOutput = count * _Normalization._maxNFCExpansionFactor
  outputBuffer = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: output)
  icuInputBuffer = UnsafeMutableBufferPointer<UInt16>.allocate(capacity: icuInput)
  icuOutputBuffer = UnsafeMutableBufferPointer<UInt16>.allocate(capacity: icuOutput)
}

internal func _fastNormalize(
  readIndex: String.Index,
  sourceBuffer: UnsafeBufferPointer<UInt8>,
  outputBuffer: inout UnsafeMutableBufferPointer<UInt8>,
  icuInputBuffer: inout UnsafeMutableBufferPointer<UInt16>,
  icuOutputBuffer: inout UnsafeMutableBufferPointer<UInt16>
) -> NormalizationResult {
  let start = readIndex.encodedOffset
  let rebasedSourceBuffer = UnsafeBufferPointer(rebasing: sourceBuffer[start...])
  if let (read, filled) = fastFill(rebasedSourceBuffer, outputBuffer) {
    let nextIndex = readIndex.encoded(offsetBy: read)
    _internalInvariant(sourceBuffer.isOnUnicodeScalarBoundary(nextIndex.encodedOffset))
    
    return NormalizationResult(
      amountFilled: filled, nextReadPosition: nextIndex, reallocatedBuffers: false
    )
  }
  guard let (read, filled) = transcodeSegmentToUTF16(rebasedSourceBuffer, into: icuInputBuffer)
  else {
    return _fastReallocateBuffers(
      readIndex: readIndex,
      sourceBuffer: sourceBuffer,
      outputBuffer: &outputBuffer,
      icuInputBuffer: &icuInputBuffer,
      icuOutputBuffer: &icuOutputBuffer
    )
  }
  
  let nextIndex = readIndex.encoded(offsetBy: read)
  _internalInvariant(sourceBuffer.isOnUnicodeScalarBoundary(nextIndex.encodedOffset))
  
  let rebasedICUInputBuffer = UnsafeBufferPointer(rebasing: icuInputBuffer[..<filled])
  guard let normalized = _tryNormalize(rebasedICUInputBuffer, into: icuOutputBuffer) else {
    return _fastReallocateBuffers(
      readIndex: readIndex,
      sourceBuffer: sourceBuffer,
      outputBuffer: &outputBuffer,
      icuInputBuffer: &icuInputBuffer,
      icuOutputBuffer: &icuOutputBuffer
    )
  }
  
  guard let (_, transcoded) = transcodeValidUTF16ToUTF8(
    UnsafeBufferPointer<UInt16>(rebasing: icuOutputBuffer[..<normalized]),
    into: outputBuffer
  ) else {
    return _fastReallocateBuffers(
      readIndex: readIndex,
      sourceBuffer: sourceBuffer,
      outputBuffer: &outputBuffer,
      icuInputBuffer: &icuInputBuffer,
      icuOutputBuffer: &icuOutputBuffer
    )
  }
  return NormalizationResult(
    amountFilled: transcoded, nextReadPosition: nextIndex, reallocatedBuffers: false
  )
}

internal func _foreignNormalize(
  readIndex: String.Index,
  endIndex: String.Index,
  guts: _StringGuts,
  outputBuffer: inout UnsafeMutableBufferPointer<UInt8>,
  icuInputBuffer: inout UnsafeMutableBufferPointer<UInt16>,
  icuOutputBuffer: inout UnsafeMutableBufferPointer<UInt16>
) -> NormalizationResult {
  guard let (read, filled) = foreignFill(
    readIndex: readIndex.encodedOffset, endIndex: endIndex.encodedOffset, guts, into: icuInputBuffer
  ) else {
    return _foreignReallocateBuffers(
      readIndex: readIndex,
      endIndex: endIndex,
      guts: guts,
      outputBuffer: &outputBuffer,
      icuInputBuffer: &icuInputBuffer,
      icuOutputBuffer: &icuOutputBuffer
    )
  }
  
  let nextIndex = readIndex.encoded(offsetBy: read)
  _internalInvariant(guts.isOnUnicodeScalarBoundary(nextIndex))
  
  let rebasedICUInputBuffer = UnsafeBufferPointer(rebasing: icuInputBuffer[..<filled])
  guard let normalized = _tryNormalize(rebasedICUInputBuffer, into: icuOutputBuffer) else {
    return _foreignReallocateBuffers(
      readIndex: readIndex,
      endIndex: endIndex,
      guts: guts,
      outputBuffer: &outputBuffer,
      icuInputBuffer: &icuInputBuffer,
      icuOutputBuffer: &icuOutputBuffer
    )
  }
  
  guard let (_, transcoded) = transcodeValidUTF16ToUTF8(
    UnsafeBufferPointer<UInt16>(rebasing: icuOutputBuffer[..<normalized]),
    into: outputBuffer
  ) else {
    return _foreignReallocateBuffers(
      readIndex: readIndex,
      endIndex: endIndex,
      guts: guts,
      outputBuffer: &outputBuffer,
      icuInputBuffer: &icuInputBuffer,
      icuOutputBuffer: &icuOutputBuffer
    )
  }

  return NormalizationResult(
    amountFilled: transcoded, nextReadPosition: nextIndex, reallocatedBuffers: false
  )
}

internal func _foreignReallocateBuffers(
  readIndex: String.Index,
  endIndex: String.Index,
  guts: _StringGuts,
  outputBuffer: inout UnsafeMutableBufferPointer<UInt8>,
  icuInputBuffer: inout UnsafeMutableBufferPointer<UInt16>,
  icuOutputBuffer: inout UnsafeMutableBufferPointer<UInt16>
) -> NormalizationResult {
  let sourceCount = endIndex.encodedOffset - readIndex.encodedOffset
  _reallocateBuffers(
    sourceCount: sourceCount,
    outputBuffer: &outputBuffer,
    icuInputBuffer: &icuInputBuffer,
    icuOutputBuffer: &icuOutputBuffer
  )
  var result = _foreignNormalize(
    readIndex: readIndex,
    endIndex: endIndex,
    guts: guts,
    outputBuffer: &outputBuffer,
    icuInputBuffer: &icuInputBuffer,
    icuOutputBuffer: &icuOutputBuffer
  )
  result.reallocatedBuffers = true
  return result
}

internal func _fastReallocateBuffers(
  readIndex: String.Index,
  sourceBuffer: UnsafeBufferPointer<UInt8>,
  outputBuffer: inout UnsafeMutableBufferPointer<UInt8>,
  icuInputBuffer: inout UnsafeMutableBufferPointer<UInt16>,
  icuOutputBuffer: inout UnsafeMutableBufferPointer<UInt16>
) -> NormalizationResult {
  let sourceCount = sourceBuffer.count - readIndex.encodedOffset
  _reallocateBuffers(
    sourceCount: sourceCount,
    outputBuffer: &outputBuffer,
    icuInputBuffer: &icuInputBuffer,
    icuOutputBuffer: &icuOutputBuffer
  )
  var result = _fastNormalize(
    readIndex: readIndex,
    sourceBuffer: sourceBuffer,
    outputBuffer: &outputBuffer,
    icuInputBuffer: &icuInputBuffer,
    icuOutputBuffer: &icuOutputBuffer
  )
  result.reallocatedBuffers = true
  return result
}

private func foreignFill(
  readIndex gutsReadIndex: Int,
  endIndex gutsEndIndex: Int,
  _ guts: _StringGuts,
  into outputBuffer: UnsafeMutableBufferPointer<UInt16>
) -> (Int, Int)? {
  var readIndex = gutsReadIndex
  var outputWriteIndex = 0
  let outputCount = outputBuffer.count
  while readIndex != gutsEndIndex {
    let (scalar, length) = guts.errorCorrectedScalar(startingAt: readIndex)
    if scalar._hasNormalizationBoundaryBefore && readIndex != gutsReadIndex {
      break
    }
    
    readIndex += length
    
    for cu in scalar.utf16 {
      if outputWriteIndex < outputCount {
        outputBuffer[outputWriteIndex] = cu
        outputWriteIndex += 1
      } else {
        return nil
      }
    }
  }
  return (readIndex - gutsReadIndex, outputWriteIndex)
}

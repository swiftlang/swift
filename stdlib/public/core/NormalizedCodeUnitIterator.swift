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

extension _Normalization {
  internal typealias _SegmentOutputBuffer = _FixedArray16<UInt16>
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
  internal var _hasNormalizationBoundaryBefore: Bool {
    _sanityCheck(Int32(exactly: self.value) != nil, "top bit shouldn't be set")
    let value = Int32(bitPattern: self.value)
    return 0 != __swift_stdlib_unorm2_hasBoundaryBefore(
      _Normalization._nfcNormalizer, value)
  }
  internal var _isNFCQCYes: Bool {
    return __swift_stdlib_u_getIntPropertyValue(
      Builtin.reinterpretCast(value), __swift_stdlib_UCHAR_NFC_QUICK_CHECK
    ) == 1
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

//
// Pointer casting helpers
//
@inline(__always)
private func _unsafeMutableBufferPointerCast<T, U>(
  _ ptr: UnsafeMutablePointer<T>,
  _ count: Int,
  to: U.Type = U.self
) -> UnsafeMutableBufferPointer<U> {
  return UnsafeMutableBufferPointer(
    start: UnsafeMutableRawPointer(ptr).assumingMemoryBound(to: U.self),
    count: count
  )
}
@inline(__always)
private func _unsafeBufferPointerCast<T, U>(
  _ ptr: UnsafePointer<T>,
  _ count: Int,
  to: U.Type = U.self
) -> UnsafeBufferPointer<U> {
  return UnsafeBufferPointer(
    start: UnsafeRawPointer(ptr).assumingMemoryBound(to: U.self),
    count: count
  )
}

internal func _castOutputBuffer(
  _ ptr: UnsafeMutablePointer<_Normalization._SegmentOutputBuffer>,
  endingAt endIdx: Int = _Normalization._SegmentOutputBuffer.capacity
) -> UnsafeMutableBufferPointer<UInt16> {
  let bufPtr: UnsafeMutableBufferPointer<UInt16> =
    _unsafeMutableBufferPointerCast(
      ptr, _Normalization._SegmentOutputBuffer.capacity)
  return UnsafeMutableBufferPointer<UInt16>(rebasing: bufPtr[..<endIdx])
}
internal func _castOutputBuffer(
  _ ptr: UnsafePointer<_Normalization._SegmentOutputBuffer>,
  endingAt endIdx: Int = _Normalization._SegmentOutputBuffer.capacity
) -> UnsafeBufferPointer<UInt16> {
  let bufPtr: UnsafeBufferPointer<UInt16> =
    _unsafeBufferPointerCast(
      ptr, _Normalization._SegmentOutputBuffer.capacity)
  return UnsafeBufferPointer<UInt16>(rebasing: bufPtr[..<endIdx])
}

extension _StringGuts {
  internal func foreignHasNormalizationBoundary(
    before index: String.Index
  ) -> Bool {
    let offset = index.encodedOffset
    if offset == 0 || offset == count {
      return true
    }

    let scalar = foreignErrorCorrectedScalar(startingAt: index).0
    return scalar._hasNormalizationBoundaryBefore
  }
}
extension UnsafeBufferPointer where Element == UInt8 {
  internal func hasNormalizationBoundary(before index: Int) -> Bool {
    if index == 0 || index == count {
      return true
    }

    assert(!_isContinuation(self[index]))

    let cu = _decodeScalar(self, startingAt: index).0
    return cu._hasNormalizationBoundaryBefore
  }
}

internal struct _NormalizedUTF8CodeUnitIterator: IteratorProtocol {
  internal typealias CodeUnit = UInt8

  var utf16Iterator: _NormalizedUTF16CodeUnitIterator
  var utf8Buffer = _FixedArray4<CodeUnit>(allZeros:())
  var bufferIndex = 0
  var bufferCount = 0

  internal init(foreign guts: _StringGuts, range: Range<String.Index>) {
    _sanityCheck(guts.isForeign)
    utf16Iterator = _NormalizedUTF16CodeUnitIterator(guts, range)
  }

  internal init(_ buffer: UnsafeBufferPointer<UInt8>, range: Range<Int>) {
    utf16Iterator = _NormalizedUTF16CodeUnitIterator(buffer, range)
  }

  internal mutating func next() -> UInt8? {
    if bufferIndex == bufferCount {
      bufferIndex = 0
      bufferCount = 0

      guard let cu = utf16Iterator.next() else {
        return nil
      }

      var array = _FixedArray2<UInt16>()
      array.append(cu)
      if _isSurrogate(cu) {
        guard let nextCU = utf16Iterator.next() else {
          fatalError("unpaired surrogate")
        }

        array.append(nextCU)
      }
      let iterator = array.makeIterator()
      _ = transcode(iterator, from: UTF16.self, to: UTF8.self,
        stoppingOnError: false) { codeUnit in
          _sanityCheck(bufferCount < 4)
          _sanityCheck(bufferIndex < 4)

          utf8Buffer[bufferIndex] = codeUnit
          bufferIndex += 1
          bufferCount += 1
      }
      bufferIndex = 0
    }

    defer { bufferIndex += 1 }

    return utf8Buffer[bufferIndex]
  }

  internal mutating func compare(
    with other: _NormalizedUTF8CodeUnitIterator
  ) -> _StringComparisonResult {
    var mutableOther = other

    for cu in self {
      if let otherCU = mutableOther.next() {
        let result = _lexicographicalCompare(cu, otherCU)
        if result == .equal {
          continue
        } else {
          return result
        }
      } else {
        //other returned nil, we are greater
        return .greater
      }
    }

    //we ran out of code units, either we are equal, or only we ran out and
    //other is greater
    if let _ = mutableOther.next() {
      return .less
    } else {
      return .equal
    }
  }
}

extension _NormalizedUTF8CodeUnitIterator: Sequence { }

internal
struct _NormalizedUTF16CodeUnitIterator: IteratorProtocol {
  internal typealias CodeUnit = UInt16
  var segmentBuffer = _FixedArray16<CodeUnit>(allZeros:())
  var overflowBuffer: [CodeUnit]? = nil
  var normalizationBuffer: [CodeUnit]? = nil
  var source: _SegmentSource

  var segmentBufferIndex = 0
  var segmentBufferCount = 0
  var overflowBufferIndex = 0
  var overflowBufferCount = 0

  init(_ guts: _StringGuts, _ range: Range<String.Index>) {
    source = _ForeignStringGutsSource(guts, range)
  }

  init(_ buffer: UnsafeBufferPointer<UInt8>, _ range: Range<Int>) {
    source = _UTF8BufferSource(buffer, range)
  }

  struct _UTF8BufferSource: _SegmentSource {
    var remaining: Int {
      return range.count - index
    }
    var isEmpty: Bool {
      return remaining <= 0
    }
    var buffer: UnsafeBufferPointer<UInt8>
    var index: Int
    var range: Range<Int>

    init(_ buffer: UnsafeBufferPointer<UInt8>, _ range: Range<Int>) {
      self.buffer = buffer
      self.range = range
      index = range.lowerBound
    }

    mutating func tryFill(
      into output: UnsafeMutableBufferPointer<UInt16>
    ) -> Int? {
      var outputIndex = 0
      let originalIndex = index
      repeat {
        guard !isEmpty else {
          break
        }

        guard outputIndex < output.count else {
          //The buff isn't big enough for the current segment
          index = originalIndex
          return nil
        }

        let (cu, len) = _decodeScalar(buffer, startingAt: index)
        let utf16 = cu.utf16
        switch utf16.count {
        case 1:
          output[outputIndex] = utf16[0]
          outputIndex += 1
        case 2:
          if outputIndex+1 >= output.count {
            index = originalIndex
            return nil
          }
          output[outputIndex] = utf16[0]
          output[outputIndex+1] = utf16[1]
          outputIndex += 2
        default:
          _conditionallyUnreachable()
        }
        index = index &+ len
      } while !buffer.hasNormalizationBoundary(before: index)
      return outputIndex
    }
  }

  struct _ForeignStringGutsSource: _SegmentSource {
    var remaining: Int {
      return range.upperBound.encodedOffset - index.encodedOffset
    }
    var isEmpty: Bool {
      return index >= range.upperBound
    }
    var guts: _StringGuts
    var index: String.Index
    var range: Range<String.Index>

    init(_ guts: _StringGuts, _ range: Range<String.Index>) {
      self.guts = guts
      self.range = range
      index = range.lowerBound
    }

    mutating func tryFill(
      into output: UnsafeMutableBufferPointer<UInt16>
    ) -> Int? {
      var outputIndex = 0
      let originalIndex = index
      repeat {
        guard index != range.upperBound else {
          break
        }

        guard outputIndex < output.count else {
          //The buffer isn't big enough for the current segment
          index = originalIndex
          return nil
        }

        let (scalar, len) = guts.foreignErrorCorrectedScalar(startingAt: index)
        output[outputIndex] = scalar.utf16[0]
        outputIndex += 1
        index = index.nextEncoded
        if len == 2 {
          output[outputIndex] = scalar.utf16[1]
          outputIndex += 1
          index = index.nextEncoded
        }
      } while !guts.foreignHasNormalizationBoundary(before: index)

      return outputIndex
    }
  }

  mutating func next() -> UInt16? {
    if segmentBufferCount == segmentBufferIndex {
      segmentBuffer = _FixedArray16<CodeUnit>(allZeros:())
      segmentBufferCount = 0
      segmentBufferIndex = 0
    }

    if overflowBufferCount == overflowBufferIndex {
      overflowBufferCount = 0
      overflowBufferIndex = 0
    }

    if source.isEmpty
    && segmentBufferCount == 0
    && overflowBufferCount == 0 {
      // Our source of code units to normalize is empty and our buffers from
      // previous normalizations are also empty.
      return nil
    }
    if segmentBufferCount == 0 && overflowBufferCount == 0 {
      //time to fill a buffer if possible. Otherwise we are done, return nil
      // Normalize segment, and then compare first code unit
      var intermediateBuffer = _FixedArray16<CodeUnit>(allZeros:())
      if overflowBuffer == nil,
         let filled = source.tryFill(into: &intermediateBuffer)
      {
        guard let count = _tryNormalize(
          _castOutputBuffer(&intermediateBuffer,
          endingAt: filled),
          into: &segmentBuffer
        )
        else {
          fatalError("Output buffer was not big enough, this should not happen")
        }
        segmentBufferCount = count
      } else {
        if overflowBuffer == nil {
          let size = source.remaining * _Normalization._maxNFCExpansionFactor
          overflowBuffer = Array(repeating: 0, count: size)
          normalizationBuffer = Array(repeating:0, count: size)
        }

        guard let count = normalizationBuffer!.withUnsafeMutableBufferPointer({
          (normalizationBufferPtr) -> Int? in
          guard let filled = source.tryFill(into: normalizationBufferPtr)
          else {
            fatalError("Invariant broken, buffer should have space")
          }
          return overflowBuffer!.withUnsafeMutableBufferPointer {
            (overflowBufferPtr) -> Int? in
            return _tryNormalize(
              UnsafeBufferPointer(rebasing: normalizationBufferPtr[..<filled]),
              into: overflowBufferPtr
            )
          }
        }) else {
          fatalError("Invariant broken, overflow buffer should have space")
        }

        overflowBufferCount = count
      }
    }

    //exactly one of the buffers should have code units for us to return
    _sanityCheck((segmentBufferCount == 0)
              != ((overflowBuffer?.count ?? 0) == 0))

    if segmentBufferIndex < segmentBufferCount {
      let index = segmentBufferIndex
      segmentBufferIndex += 1
      return segmentBuffer[index]
    } else if overflowBufferIndex < overflowBufferCount {
      _sanityCheck(overflowBufferIndex < overflowBuffer!.count)
      let index = overflowBufferIndex
      overflowBufferIndex += 1
      return overflowBuffer![index]
    } else {
        return nil
    }
  }
}

protocol _SegmentSource {
  var remaining: Int { get }
  var isEmpty: Bool { get }
  mutating func tryFill(into: UnsafeMutableBufferPointer<UInt16>) -> Int?
}

extension _SegmentSource {
  mutating func tryFill(
    into output: UnsafeMutablePointer<_Normalization._SegmentOutputBuffer>
  ) -> Int? {
    return tryFill(into: _castOutputBuffer(output))
  }
}

internal struct _NormalizedUTF8CodeUnitIterator_2: Sequence, IteratorProtocol {
  private var outputBuffer = _SmallBuffer<UInt8>()
  private var outputPosition = 0
  private var outputBufferCount = 0

  private var gutsSlice: _StringGutsSlice
  private var readPosition: String.Index

  private var _backupIsEmpty = false

  internal init(_ sliced: _StringGutsSlice) {
    self.gutsSlice = sliced
    self.readPosition = self.gutsSlice.range.lowerBound
  }

  internal mutating func next() -> UInt8? {
    return _next()
  }
}

extension _NormalizedUTF8CodeUnitIterator_2 {
  // The thresdhold we try to stay within while filling. Always leaves enough
  // code units at the end to finish a scalar, but not necessarily enough to
  // finish a segment.
  private var outputBufferThreshold: Int {
    return outputBuffer.capacity - 4
  }

  private var outputBufferEmpty: Bool {
    return outputPosition == outputBufferCount
  }
  private var outputBufferFull: Bool {
    return outputBufferCount >= outputBufferThreshold
  }

  private var inputBufferEmpty: Bool {
    return gutsSlice.range.isEmpty
  }
}

extension _NormalizedUTF8CodeUnitIterator_2 {
  @_effects(releasenone)
  private mutating func _next() -> UInt8? {
    defer { _fixLifetime(self) }
    if _slowPath(outputBufferEmpty) {
      if _slowPath(inputBufferEmpty) {
        return nil
      }
      fill()
      if _slowPath(outputBufferEmpty) {
        //_sanityCheck(inputBufferEmpty)
        return nil
      }
    }
    _sanityCheck(!outputBufferEmpty)

    _sanityCheck(outputPosition < outputBufferCount)
    let result = outputBuffer[outputPosition]
    outputPosition &+= 1
    return result
  }

  // Try to fill from the start without using ICU's normalizer. Returns number
  // of code units filled in.
  @inline(__always)
  @_effects(releasenone)
  private mutating func fastPathFill() -> (numRead: Int, numWritten: Int) {
    // Quick check if a scalar is NFC and a segment starter
    @inline(__always) func isNFCStarter(_ scalar: Unicode.Scalar) -> Bool {
      // Fast-path: All scalars up through U+02FF are NFC and have boundaries
      // before them
      if scalar.value < 0x300 { return true }

      // Otherwise, consult the properties
      return scalar._hasNormalizationBoundaryBefore && scalar._isNFCQCYes
    }

    // TODO: Additional fast-path: All CCC-ascending NFC_QC segments are NFC
    // TODO: Just freakin do normalization and don't bother with ICU
    var outputCount = 0
    let outputEnd = outputBufferThreshold
    var inputCount = 0
    let inputEnd = gutsSlice.count
    if _fastPath(gutsSlice.isFastUTF8) {
      gutsSlice.withFastUTF8 { utf8 in
        while inputCount < inputEnd && outputCount < outputEnd {
          // TODO: Slightly faster code-unit scan for latiny (<0xCC)

          // Check scalar-based fast-paths
          let (scalar, len) = _decodeScalar(utf8, startingAt: inputCount)
          _sanityCheck(inputCount &+ len <= inputEnd)

          if _slowPath(
               !utf8.hasNormalizationBoundary(before: inputCount &+ len)
            || !isNFCStarter(scalar)
          ) {
            break 
          }
          inputCount &+= len

          for cu in UTF8.encode(scalar)._unsafelyUnwrappedUnchecked {
            outputBuffer[outputCount] = cu
            outputCount &+= 1
          }

          _sanityCheck(inputCount == outputCount,
            "non-normalizing UTF-8 fast path should be 1-to-1 in code units")
        }
      }
    } else { // Foreign
      while inputCount < inputEnd && outputCount < outputEnd {
        let startIdx = gutsSlice.range.lowerBound.encoded(
          offsetBy: inputCount)
        let (scalar, len) = gutsSlice.foreignErrorCorrectedScalar(
          startingAt: startIdx)
        _sanityCheck(inputCount &+ len <= inputEnd)

        if _slowPath(
             !gutsSlice.foreignHasNormalizationBoundary(
               before: startIdx.encoded(offsetBy: len))
          || !isNFCStarter(scalar)
        ) {
          break 
        }
        inputCount &+= len

        for cu in UTF8.encode(scalar)._unsafelyUnwrappedUnchecked {
          outputBuffer[outputCount] = cu
          outputCount &+= 1
        }

        _sanityCheck(inputCount <= outputCount,
          "non-normalizing UTF-16 fast path shoule be 1-to-many in code units")
      }
    }
    return (inputCount, outputCount)
  }

  @_effects(releasenone)
  private mutating func fill() {
    _sanityCheck(outputBufferEmpty)

    let priorInputCount = gutsSlice._offsetRange.count

    outputPosition = 0
    let (inputCount, outputCount) = fastPathFill()
    self.outputBufferCount = outputCount

    // Check if we filled in any, and adjust our scanning range appropriately
    if inputCount > 0 {
      _sanityCheck(outputCount > 0)
      gutsSlice._offsetRange = Range(uncheckedBounds: (
        gutsSlice._offsetRange.lowerBound + inputCount,
        gutsSlice._offsetRange.upperBound))
      _sanityCheck(gutsSlice._offsetRange.count >= 0)
      return
    }

    let remaining: Int = gutsSlice.withNFCCodeUnitsIterator {
      var nfc = $0
      while !outputBufferFull, let cu = nfc.next() {
        outputBuffer[outputBufferCount] = cu
        outputBufferCount &+= 1
      }
      return nfc.utf16Iterator.source.remaining
    }

    if !(outputBufferCount == 0 || remaining < priorInputCount) {
      // TODO: _sanityCheck(outputBufferCount == 0 || remaining < priorInputCount)
    }

    gutsSlice._offsetRange = Range(uncheckedBounds: (
      gutsSlice._offsetRange.lowerBound + (priorInputCount - remaining),
      gutsSlice._offsetRange.upperBound))

    _sanityCheck(outputBufferFull || gutsSlice._offsetRange.isEmpty)
    _sanityCheck(gutsSlice._offsetRange.count >= 0)
  }

  @_effects(readonly)
  internal mutating func compare(
    with other: _NormalizedUTF8CodeUnitIterator_2
  ) -> _StringComparisonResult {
    var iter = self
    var mutableOther = other

    while let cu = iter.next() {
      if let otherCU = mutableOther.next() {
        let result = _lexicographicalCompare(cu, otherCU)
        if result == .equal {
          continue
        } else {
          return result
        }
      } else {
        //other returned nil, we are greater
        return .greater
      }
    }

    //we ran out of code units, either we are equal, or only we ran out and
    //other is greater
    if let _ = mutableOther.next() {
      return .less
    } else {
      return .equal
    }
  }
}



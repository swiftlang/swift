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

    let cu = foreignErrorCorrectedUTF16CodeUnit(at: index)
    return Unicode.Scalar(cu)?._hasNormalizationBoundaryBefore ?? false
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

  var utf16Iterator: _NormalizedCodeUnitIterator
  var utf8Buffer = _FixedArray4<CodeUnit>(allZeros:())
  var bufferIndex = 0
  var bufferCount = 0

  internal init(_ guts: _StringGuts, range: Range<String.Index>) {
    utf16Iterator = _NormalizedCodeUnitIterator(guts, range)
  }

  internal init(_ buffer: UnsafeBufferPointer<UInt8>, range: Range<Int>) {
    utf16Iterator = _NormalizedCodeUnitIterator(buffer, range)
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
  ) -> _StringComparison {
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
struct _NormalizedCodeUnitIterator: IteratorProtocol {
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

  mutating func compare(
    with other: _NormalizedCodeUnitIterator
  ) -> _StringComparison {
    var mutableOther = other
    for cu in IteratorSequence(self) {
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

        let (cu, nextIndex) = _decodeScalar(buffer, startingAt: index)
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
        index = nextIndex
      } while !buffer.hasNormalizationBoundary(before: index)
      return outputIndex
    }
  }

  struct _ForeignStringGutsSource: _SegmentSource {
    var remaining: Int {
      // not exact since we skip invalid CUs but it's just to get an approximate
      // size for allocating the buffer. Use isEmpty to determine if we're done
      // reading code from the source
      return guts.count - index.encodedOffset
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

        let cu = guts.foreignErrorCorrectedUTF16CodeUnit(at: index)
        output[outputIndex] = cu
        index = index._next()
        outputIndex += 1
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


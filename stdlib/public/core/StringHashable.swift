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

extension String : Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  public func hash(into hasher: inout Hasher) {
    if _fastPath(self._guts.isNFCFastUTF8) {
      self._guts.withFastUTF8 {
        hasher.combine(bytes: UnsafeRawBufferPointer($0))
      }
      hasher.combine(0xFF as UInt8) // terminator
      return
    }

    _gutsSlice._normalizedHash(into: &hasher)
  }
}

extension StringProtocol {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @_specialize(where Self == String)
  @_specialize(where Self == Substring)
  public func hash(into hasher: inout Hasher) {
    _gutsSlice._normalizedHash(into: &hasher)
  }
}

extension _StringGutsSlice {
  @inline(never) // slow-path
  internal func _normalizedHash(into hasher: inout Hasher) {
    if self.isNFCFastUTF8 {
      self.withFastUTF8 {
        hasher.combine(bytes: UnsafeRawBufferPointer($0))
      }
    } else {
      var output = _FixedArray16<UInt8>(allZeros: ())
      var icuInput = _FixedArray16<UInt16>(allZeros: ())
      var icuOutput = _FixedArray16<UInt16>(allZeros: ())
      if _fastPath(self.isFastUTF8) {
        self.withFastUTF8 {
          _fastNormalizeHashImpl(
            sourceBuffer: $0,
            outputBuffer: _castOutputBuffer(&output),
            icuInputBuffer: _castOutputBuffer(&icuInput),
            icuOutputBuffer: _castOutputBuffer(&icuOutput),
            hasher: &hasher
          )
        }
      } else {
        _foreignNormalizeHashImpl(
          outputBuffer: _castOutputBuffer(&output),
          icuInputBuffer: _castOutputBuffer(&icuInput),
          icuOutputBuffer: _castOutputBuffer(&icuOutput),
          hasher: &hasher
        )
      }
    }

    hasher.combine(0xFF as UInt8) // terminator
  }
  
  internal func _fastNormalizeHashImpl(
    sourceBuffer: UnsafeBufferPointer<UInt8>,
    outputBuffer: UnsafeMutableBufferPointer<UInt8>,
    icuInputBuffer: UnsafeMutableBufferPointer<UInt16>,
    icuOutputBuffer: UnsafeMutableBufferPointer<UInt16>,
    hasher: inout Hasher
  ) {
    var outputBuffer = outputBuffer
    var icuInputBuffer = icuInputBuffer
    var icuOutputBuffer = icuOutputBuffer
    
    var index = String.Index(encodedOffset: 0)
    let cachedEndIndex = String.Index(encodedOffset: sourceBuffer.count)
    var hasBufferOwnership = false
    while index < cachedEndIndex {
      let result = _fastNormalize(
        readIndex: index,
        sourceBuffer: sourceBuffer,
        outputBuffer: &outputBuffer,
        icuInputBuffer: &icuInputBuffer,
        icuOutputBuffer: &icuOutputBuffer
      )
      for i in 0..<result.amountFilled {
        hasher.combine(outputBuffer[i])
      }
      _internalInvariant(result.nextReadPosition != index)
      index = result.nextReadPosition
      if result.reallocatedBuffers {
        _internalInvariant(!hasBufferOwnership)
        hasBufferOwnership = true
      }
    }
    if hasBufferOwnership {
      outputBuffer.deallocate()
      icuInputBuffer.deallocate()
      icuOutputBuffer.deallocate()
    }
  }
  
  internal func _foreignNormalizeHashImpl(
    outputBuffer: UnsafeMutableBufferPointer<UInt8>,
    icuInputBuffer: UnsafeMutableBufferPointer<UInt16>,
    icuOutputBuffer: UnsafeMutableBufferPointer<UInt16>,
    hasher: inout Hasher
  ) {
    var outputBuffer = outputBuffer
    var icuInputBuffer = icuInputBuffer
    var icuOutputBuffer = icuOutputBuffer
    
    var index = self.range.lowerBound
    let cachedEndIndex = self.range.upperBound
    
    var hasBufferOwnership = false
    while index < cachedEndIndex {
      let result = _foreignNormalize(
        readIndex: index,
        endIndex: cachedEndIndex,
        guts: self._guts,
        outputBuffer: &outputBuffer,
        icuInputBuffer: &icuInputBuffer,
        icuOutputBuffer: &icuOutputBuffer
      )
      for i in 0..<result.amountFilled {
        hasher.combine(outputBuffer[i])
      }
      _internalInvariant(result.nextReadPosition != index)
      index = result.nextReadPosition
      if result.reallocatedBuffers {
        _internalInvariant(!hasBufferOwnership)
        hasBufferOwnership = true
      }
    }
    if hasBufferOwnership {
      outputBuffer.deallocate()
      icuInputBuffer.deallocate()
      icuOutputBuffer.deallocate()
    }
  }
}


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
      _normalizeHashImpl(
        outputBuffer: _castOutputBuffer(&output),
        icuInputBuffer: _castOutputBuffer(&icuInput),
        icuOutputBuffer: _castOutputBuffer(&icuOutput),
        hasher: &hasher
      )
    }

    hasher.combine(0xFF as UInt8) // terminator
  }
  
  internal func _normalizeHashImpl(
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
    
    let normalize: (String.Index, _StringGuts, UnsafeMutableBufferPointer<UInt8>,
      UnsafeMutableBufferPointer<UInt16>, UnsafeMutableBufferPointer<UInt16>
    ) -> NormalizationResult
    if _fastPath(self.isFastUTF8) {
      normalize = fastNormalize
    } else {
      normalize = foreignNormalize
    }
    
    var bufferCleanup: (() -> ())? = nil
    
    while index != cachedEndIndex {
      let result = normalize(index, self._guts, outputBuffer, icuInputBuffer, icuOutputBuffer)
      switch result {
      case let .success(r):
        for i in 0..<r.amountFilled {
          hasher.combine(outputBuffer[i])
        }
        index = r.nextReadPosition
      case let .bufferTooSmall(resize):
        _internalInvariant(bufferCleanup == nil)
        outputBuffer = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: resize.output)
        icuInputBuffer = UnsafeMutableBufferPointer<UInt16>.allocate(capacity: resize.icuInput)
        icuOutputBuffer = UnsafeMutableBufferPointer<UInt16>.allocate(capacity: resize.icuOutput)
        bufferCleanup = {
          outputBuffer.deallocate()
          icuInputBuffer.deallocate()
          icuOutputBuffer.deallocate()
        }
      }
    }
    if let cleanup = bufferCleanup {
      cleanup()
    }
  }
}


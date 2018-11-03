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
  // TODO(UTF8 perf): Should we cache one on TLS? Is this an expensive call?
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

extension String {
  // TODO(UTF8 perf): Change into a lazy sequence with fast-paths...
  @inline(never) // slow-path
  internal func _normalize() -> Array<UInt8> {
    func _tryNormalize(
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
    let transcoded = Array(self.utf16)
    let normalized: Array<UInt16> = transcoded.withUnsafeBufferPointer {
      (inputBufPtr) -> Array<UInt16> in
      var output = Array<UInt16>(
        repeating: 0,
        count: 1 + inputBufPtr.count * _Normalization._maxNFCExpansionFactor)
      let lenOpt = output.withUnsafeMutableBufferPointer { outputBufPtr in
        return _tryNormalize(inputBufPtr, into: outputBufPtr)
      }
      guard let len = lenOpt else {
        _sanityCheckFailure("normalization beyond max expansion factor")
      }
      _sanityCheck(len <= output.count)
      output.removeLast(output.count - len)
      return output
    }
    var codeUnits = Array<UInt8>()
    codeUnits.reserveCapacity(normalized.count)
    _ = transcode(
      normalized.makeIterator(),
      from: UTF16.self,
      to: UTF8.self,
      stoppingOnError: false,
      into: { codeUnits.append($0) })
    return codeUnits
  }
}
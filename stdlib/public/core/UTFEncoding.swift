//===--- UTFEncoding.swift - Common guts of the big 3 UnicodeEncodings ----===//
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
//
//  These components would be internal if it were possible to use internal
//  protocols to supply public conformance requirements.
//
//===----------------------------------------------------------------------===//


public protocol _UTFParser {
  associatedtype Encoding : _UnicodeEncoding

  func _parseMultipleCodeUnits() -> (isValid: Bool, bitCount: UInt8)
  func _bufferedScalar(bitCount: UInt8) -> Encoding.EncodedScalar
  
  var _buffer: _UIntBuffer<Encoding.CodeUnit> { get set }
}

extension _UTFParser
where Encoding.EncodedScalar : RangeReplaceableCollection {

  @inlinable
  @inline(__always)
  public mutating func parseScalar<I : IteratorProtocol>(
    from input: inout I
  ) -> Unicode.ParseResult<Encoding.EncodedScalar>
    where I.Element == Encoding.CodeUnit {

    // Bufferless single-scalar fastpath.
    if _fastPath(_buffer.isEmpty) {
      guard let codeUnit = input.next() else { return .emptyInput }
      // ASCII, return immediately.
      if Encoding._isScalar(codeUnit) {
        return .valid(Encoding.EncodedScalar(CollectionOfOne(codeUnit)))
      }
      // Non-ASCII, proceed to buffering mode.
      _buffer.append(codeUnit)
    } else if Encoding._isScalar(
      Encoding.CodeUnit(truncatingIfNeeded: _buffer._storage)
    ) {
      // ASCII in _buffer.  We don't refill the buffer so we can return
      // to bufferless mode once we've exhausted it.
      let codeUnit = Encoding.CodeUnit(truncatingIfNeeded: _buffer._storage)
      _buffer.remove(at: _buffer.startIndex)
      return .valid(Encoding.EncodedScalar(CollectionOfOne(codeUnit)))
    }
    // Buffering mode.
    // Fill buffer back to 4 bytes (or as many as are left in the iterator).
    repeat {
      if let codeUnit = input.next() {
        _buffer.append(codeUnit)
      } else {
        if _buffer.isEmpty { return .emptyInput }
        break // We still have some bytes left in our buffer.
      }
    } while _buffer.count < _buffer.capacity

    // Find one unicode scalar.
    let (isValid, scalarBitCount) = _parseMultipleCodeUnits()
    _internalInvariant(scalarBitCount % numericCast(Encoding.CodeUnit.bitWidth) == 0)
    _internalInvariant(1...4 ~= scalarBitCount / 8)
    _internalInvariant(scalarBitCount <= _buffer._bitCount)
    
    // Consume the decoded bytes (or maximal subpart of ill-formed sequence).
    let encodedScalar = _bufferedScalar(bitCount: scalarBitCount)
    
    _buffer._storage = UInt32(
      // widen to 64 bits so that we can empty the buffer in the 4-byte case
      truncatingIfNeeded: UInt64(_buffer._storage) &>> scalarBitCount)
      
    _buffer._bitCount = _buffer._bitCount &- scalarBitCount

    if _fastPath(isValid) {
      return .valid(encodedScalar)
    }
    return .error(
      length: Int(scalarBitCount / numericCast(Encoding.CodeUnit.bitWidth)))
  }
}

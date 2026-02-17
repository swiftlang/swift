//===----------------------------------------------------------------------===//
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

@inlinable @inline(__always) // top-level fastest-paths
@_effects(readonly)
internal func _stringCompare(
  _ lhs: _StringGuts, _ rhs: _StringGuts, expecting: _StringComparisonResult
) -> Bool {
  if lhs.rawBits == rhs.rawBits { return expecting == .equal }
  return _stringCompareWithSmolCheck(lhs, rhs, expecting: expecting)
}

@usableFromInline
@_effects(readonly)
internal func _stringCompareWithSmolCheck(
  _ lhs: _StringGuts, _ rhs: _StringGuts, expecting: _StringComparisonResult
) -> Bool {
  // ASCII small-string fast-path:
  if lhs.isSmallASCII && rhs.isSmallASCII {
    let lhsRaw = lhs.asSmall._storage
    let rhsRaw = rhs.asSmall._storage

    if lhsRaw.0 != rhsRaw.0 {
      return _lexicographicalCompare(
        lhsRaw.0.bigEndian, rhsRaw.0.bigEndian, expecting: expecting)
    }
    return _lexicographicalCompare(
      lhsRaw.1.bigEndian, rhsRaw.1.bigEndian, expecting: expecting)
  }

  return _stringCompareInternal(lhs, rhs, expecting: expecting)
}

@inline(never) // Keep `_stringCompareWithSmolCheck` fast-path fast
@usableFromInline
@_effects(readonly)
internal func _stringCompareInternal(
  _ lhs: _StringGuts, _ rhs: _StringGuts, expecting: _StringComparisonResult
) -> Bool {
  guard _fastPath(lhs.isFastUTF8 && rhs.isFastUTF8) else {
    return _stringCompareSlow(lhs, rhs, expecting: expecting)
  }

  let isNFC = lhs.isNFC && rhs.isNFC
  return unsafe lhs.withFastUTF8 { lhsUTF8 in
    return unsafe rhs.withFastUTF8 { rhsUTF8 in
      return unsafe _stringCompareFastUTF8(
        lhsUTF8, rhsUTF8, expecting: expecting, bothNFC: isNFC)
    }
  }
}

@inlinable @inline(__always) // top-level fastest-paths
@_effects(readonly)
internal func _stringCompare(
  _ lhs: _StringGuts, _ lhsRange: Range<Int>,
  _ rhs: _StringGuts, _ rhsRange: Range<Int>,
  expecting: _StringComparisonResult
) -> Bool {
  if lhs.rawBits == rhs.rawBits && lhsRange == rhsRange {
    return expecting == .equal
  }
  return _stringCompareInternal(
    lhs, lhsRange, rhs, rhsRange, expecting: expecting)
}

@usableFromInline
@_effects(readonly)
internal func _stringCompareInternal(
  _ lhs: _StringGuts, _ lhsRange: Range<Int>,
  _ rhs: _StringGuts, _ rhsRange: Range<Int>,
  expecting: _StringComparisonResult
) -> Bool {
  guard _fastPath(lhs.isFastUTF8 && rhs.isFastUTF8) else {
    return _stringCompareSlow(
      lhs, lhsRange, rhs, rhsRange, expecting: expecting)
  }

  let isNFC = lhs.isNFC && rhs.isNFC
  return unsafe lhs.withFastUTF8(range: lhsRange) { lhsUTF8 in
    return unsafe rhs.withFastUTF8(range: rhsRange) { rhsUTF8 in
      return unsafe _stringCompareFastUTF8(
        lhsUTF8, rhsUTF8, expecting: expecting, bothNFC: isNFC)
    }
  }
}

@_effects(readonly)
internal func _stringCompareFastUTF8(
  _ utf8Left: UnsafeBufferPointer<UInt8>,
  _ utf8Right: UnsafeBufferPointer<UInt8>,
  expecting: _StringComparisonResult,
  bothNFC: Bool
) -> Bool {
  if _fastPath(bothNFC) {
    /*
     If we know both Strings are NFC *and* we're just checking
     equality, then we can early-out without looking at the contents
     if the UTF8 counts are different (without the NFC req, equal 
     characters can have different counts). It might be nicer to do 
     this in _binaryCompare, but we have the information about what 
     operation we're trying to do at this level.
     */
    if expecting == .equal && utf8Left.count != utf8Right.count {
        return false
    }
    let cmp = unsafe _binaryCompare(utf8Left, utf8Right)
    return _lexicographicalCompare(cmp, 0, expecting: expecting)
  }

  return unsafe _stringCompareFastUTF8Abnormal(
    utf8Left, utf8Right, expecting: expecting)
}

@_effects(readonly)
private func _stringCompareFastUTF8Abnormal(
  _ utf8Left: UnsafeBufferPointer<UInt8>,
  _ utf8Right: UnsafeBufferPointer<UInt8>,
  expecting: _StringComparisonResult
) -> Bool {
  // Do a binary-equality prefix scan, to skip over long common prefixes.
  guard let diffIdx = unsafe _findDiffIdx(utf8Left, utf8Right) else {
    // We finished one of our inputs.
    //
    // TODO: This gives us a consistent and good ordering, but technically it
    // could differ from our stated ordering if combination with a prior scalar
    // did not produce a greater-value scalar. Consider checking normality.
    return _lexicographicalCompare(
      utf8Left.count, utf8Right.count, expecting: expecting)
  }

  let scalarDiffIdx = unsafe _scalarAlign(utf8Left, diffIdx)
  unsafe _internalInvariant(scalarDiffIdx == _scalarAlign(utf8Right, diffIdx))

  let (leftScalar, leftLen) = unsafe _decodeScalar(utf8Left, startingAt: scalarDiffIdx)
  let (rightScalar, rightLen) = unsafe _decodeScalar(
    utf8Right, startingAt: scalarDiffIdx)

  // Very frequent fast-path: point of binary divergence is a NFC single-scalar
  // segment. Check that we diverged at the start of a segment, and the next
  // scalar is both NFC and its own segment.
  if unsafe _fastPath(
    leftScalar._isNFCStarter && rightScalar._isNFCStarter &&
    utf8Left.hasNormalizationBoundary(before: scalarDiffIdx &+ leftLen) &&
    utf8Right.hasNormalizationBoundary(before: scalarDiffIdx &+ rightLen)
  ) {
    guard expecting == .less else {
      // We diverged
      _internalInvariant(expecting == .equal)
      return false
    }
    return _lexicographicalCompare(
      leftScalar.value, rightScalar.value, expecting: .less)
  }

  // Back up to the nearest normalization boundary before doing a slow
  // normalizing compare.
  let boundaryIdx = unsafe Swift.min(
    _findBoundary(utf8Left, before: diffIdx),
    _findBoundary(utf8Right, before: diffIdx))
  _internalInvariant(boundaryIdx <= diffIdx)

  return unsafe _stringCompareSlow(
    UnsafeBufferPointer(rebasing: utf8Left[boundaryIdx...]),
    UnsafeBufferPointer(rebasing: utf8Right[boundaryIdx...]),
    expecting: expecting)
}

@_effects(readonly)
private func _stringCompareSlow(
  _ lhs: _StringGuts, _ rhs: _StringGuts, expecting: _StringComparisonResult
) -> Bool {
  return _stringCompareSlow(
    lhs, 0..<lhs.count, rhs, 0..<rhs.count, expecting: expecting)
}

@_effects(readonly)
private func _stringCompareSlow(
  _ lhs: _StringGuts, _ lhsRange: Range<Int>,
  _ rhs: _StringGuts, _ rhsRange: Range<Int>,
  expecting: _StringComparisonResult
) -> Bool {
  // TODO: Just call the normalizer directly with range

  return _StringGutsSlice(lhs, lhsRange).compare(
    with: _StringGutsSlice(rhs, rhsRange),
    expecting: expecting)
}

@_effects(readonly)
private func _stringCompareSlow(
  _ leftUTF8: UnsafeBufferPointer<UInt8>,
  _ rightUTF8: UnsafeBufferPointer<UInt8>,
  expecting: _StringComparisonResult
) -> Bool {
  // TODO: Just call the normalizer directly

  let left = unsafe _StringGutsSlice(_StringGuts(leftUTF8, isASCII: false))
  let right = unsafe _StringGutsSlice(_StringGuts(rightUTF8, isASCII: false))
  return left.compare(with: right, expecting: expecting)
}

// Return the point of binary divergence. If they have no binary difference
// (even if one is longer), returns nil.
@_effects(readonly)
private func _findDiffIdx(
  _ left: UnsafeBufferPointer<UInt8>, _ right: UnsafeBufferPointer<UInt8>
) -> Int? {
  let count = Swift.min(left.count, right.count)
  var idx = 0
  while idx < count {
    guard unsafe left[_unchecked: idx] == right[_unchecked: idx] else {
      return idx
    }
    idx &+= 1
  }
  return nil
}

@_effects(readonly)
@inline(__always)
private func _lexicographicalCompare<I: FixedWidthInteger>(
  _ lhs: I, _ rhs: I, expecting: _StringComparisonResult
) -> Bool {
  return expecting == .equal ? lhs == rhs : lhs < rhs
}

@_effects(readonly)
private func _findBoundary(
  _ utf8: UnsafeBufferPointer<UInt8>, before: Int
) -> Int {
  var idx = before
  _internalInvariant(idx >= 0)

  // End of string is a normalization boundary
  guard idx < utf8.count else {
    _internalInvariant(before == utf8.count)
    return utf8.count
  }

  // Back up to scalar boundary
  while unsafe UTF8.isContinuation(utf8[_unchecked: idx]) {
    idx &-= 1
  }

  while true {
    if idx == 0 { return 0 }

    let scalar = unsafe _decodeScalar(utf8, startingAt: idx).0

    if scalar._isNFCStarter {
      return idx
    }

    unsafe idx &-= _utf8ScalarLength(utf8, endingAt: idx)
  }
  fatalError()
}

@frozen
@usableFromInline
internal enum _StringComparisonResult {
  case equal
  case less

  @inlinable @inline(__always)
  internal init(signedNotation int: Int) {
    _internalInvariant(int <= 0)
    self = int == 0 ? .equal : .less
  }

  @inlinable @inline(__always)
  static func ==(
    _ lhs: _StringComparisonResult, _ rhs: _StringComparisonResult
  ) -> Bool {
    switch (lhs, rhs) {
      case (.equal, .equal): return true
      case (.less, .less): return true
      default: return false
    }
  }
}

// Perform a binary comparison of bytes in memory. Return value is negative if
// less, 0 if equal, positive if greater.
@_effects(readonly)
internal func _binaryCompare<UInt8>(
  _ lhs: UnsafeBufferPointer<UInt8>, _ rhs: UnsafeBufferPointer<UInt8>
) -> Int {
  var cmp = unsafe Int(truncatingIfNeeded:
    _swift_stdlib_memcmp(
      lhs.baseAddress._unsafelyUnwrappedUnchecked,
      rhs.baseAddress._unsafelyUnwrappedUnchecked,
      Swift.min(lhs.count, rhs.count)))
  if cmp == 0 {
    cmp = lhs.count &- rhs.count
  }
  return cmp
}

// Double dispatch functions
extension _StringGutsSlice {
  @_effects(readonly)
  internal func compare(
    with other: _StringGutsSlice, expecting: _StringComparisonResult
  ) -> Bool {
    if _fastPath(self.isFastUTF8 && other.isFastUTF8) {
      Builtin.onFastPath() // aggressively inline / optimize
      let isEqual = unsafe self.withFastUTF8 { utf8Self in
        return unsafe other.withFastUTF8 { utf8Other in
          return unsafe 0 == _binaryCompare(utf8Self, utf8Other)
        }
      }
      if isEqual { return expecting == .equal }
    }

    return _slowCompare(with: other, expecting: expecting)
  }

  @inline(never) // opaque slow-path
  @_effects(readonly)
  internal func _slowCompare(
    with other: _StringGutsSlice,
    expecting: _StringComparisonResult
  ) -> Bool {
    var iter1 = Substring(self).unicodeScalars._internalNFC.makeIterator()
    var iter2 = Substring(other).unicodeScalars._internalNFC.makeIterator()

    var scalar1: Unicode.Scalar? = nil
    var scalar2: Unicode.Scalar? = nil

    while true {
      scalar1 = iter1.next()
      scalar2 = iter2.next()

      if scalar1 == nil || scalar2 == nil {
        break
      }

      if scalar1 == scalar2 {
        continue
      }

      if scalar1! < scalar2! {
        return expecting == .less
      } else {
        return false
      }
    }

    // If both of them ran out of scalars, then these are completely equal.
    if scalar1 == nil, scalar2 == nil {
      return expecting == .equal
    }

    // Otherwise, one of these strings has more scalars, so the one with less
    // scalars is considered "less" than.
    if end < other.end {
      return expecting == .less
    }

    return false
  }
}

fileprivate extension RawSpan {
  @inline(__always)
  func decodeUTF8(ofWidth width: Int, at index: Int, in bytes: RawSpan) -> UInt32? {
    guard index < bytes.byteOffsets.upperBound &- width else {
      return nil
    }
    var l1 = unsafe UInt32(bytes.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 0, as: UInt8.self))
    switch width {
    case 1:
      break
    case 2:
      let l2 = unsafe UInt32(bytes.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 1, as: UInt8.self))
      l1 =  (l1 & 0b00011111) << 6
      l1 |= (l2 & 0b00111111)
    case 3:
      let l2 = unsafe UInt32(bytes.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 1, as: UInt8.self))
      let l3 = unsafe UInt32(bytes.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 2, as: UInt8.self))
      l1 =  (l1 & 0b00011111) << 12
      l1 |= (l2 & 0b00111111) << 6
      l1 |= (l3 & 0b00111111)
    default:
      let l2 = unsafe UInt32(bytes.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 1, as: UInt8.self))
      let l3 = unsafe UInt32(bytes.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 2, as: UInt8.self))
      let l4 = unsafe UInt32(bytes.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 3, as: UInt8.self))
      l1 =  (l1 & 0b00011111) << 18
      l1 |= (l2 & 0b00111111) << 12
      l1 |= (l3 & 0b00111111) << 6
      l1 |= (l4 & 0b00111111)
    }
    return l1
  }
  
  @inline(__always)
  func decodeUTF16(ofWidth width: Int, at index: Int, in bytes: RawSpan) -> UInt32? {
    _debugPrecondition(width == 2 || width == 4)
    guard index < bytes.byteOffsets.upperBound &- width else {
      return nil
    }
    let r1 = unsafe bytes.unsafeLoadUnaligned(fromUncheckedByteOffset: index, as: UInt16.self)
    if width == 2 {
      return UInt32(r1)
    }
    if !Unicode.UTF16.isLeadSurrogate(r1) {
      return nil
    }
    let r2 = unsafe bytes.unsafeLoadUnaligned(fromUncheckedByteOffset: index &+ 2, as: UInt16.self)
    if !Unicode.UTF16.isTrailSurrogate(r2) {
      return nil
    }
    return Unicode.UTF16._decodeSurrogates(r1, r2).value
  }
  
  @inline(__always)
  func contentsAreTriviallyIdentical(to rhs: RawSpan) -> Bool? {
    if self.isIdentical(to: rhs) {
      return true
    }
    if self.byteCount == 0 {
      return rhs.byteCount == 0
    }
    if rhs.byteCount == 0 {
      return false //already covered lhs.byteCount == 0 above
    }
    return nil //can't decide trivially
  }
}

fileprivate func isEqual(
  utf8Bytes lhs: RawSpan,
  utf16Bytes rhs: RawSpan
) -> Bool {
    // If the UTF8 buffer is entirely 3 byte characters, that will be 2 bytes in UTF16
    let minimumUTF16CountForUTF8 = (lhs.byteCount * 2) / 3
    if rhs.byteCount < minimumUTF16CountForUTF8 {
      return false
    }
    // On the other hand if it's entirely 1 byte characters, that'll double in size
    let maximumUTF16CountForUTF8 = lhs.byteCount * 2
    if rhs.byteCount > maximumUTF16CountForUTF8 {
      return false
    }
    
    var lhsIdx = 0
    var rhsIdx = 0
    
    while true {
      guard let firstUTF8Byte = lhs.decodeUTF8(ofWidth: 1, at: lhsIdx, in: lhs) else {
        // If we've consumed everything in both buffers without finding an inequality, we're equal
        return lhsIdx &+ 1 == lhs.byteOffsets.upperBound && rhsIdx &+ 1 == rhs.byteOffsets.upperBound
      }
      let utf8Width: Int
      let utf16Width: Int
      if firstUTF8Byte < 128 {
        utf8Width = 1
        utf16Width = 2
        guard let r = rhs.decodeUTF16(ofWidth: utf16Width, at: rhsIdx, in: rhs) else {
          return false
        }
        if r != firstUTF8Byte {
          return false
        }
      } else {
        utf8Width = _utf8ScalarLength(UInt8(truncatingIfNeeded: firstUTF8Byte))
        utf16Width = utf8Width == 4 ? 4 : 2
        guard
          let l = lhs.decodeUTF8(ofWidth: utf8Width, at: lhsIdx, in: lhs),
          let r = rhs.decodeUTF16(ofWidth: utf16Width, at: rhsIdx, in: rhs) else {
          return false
        }
        if (l != r) {
          return false
        }
      }
      lhsIdx &+= utf8Width
      rhsIdx &+= utf16Width
    }
}

@inline(__always)
fileprivate func isEqual(
  bytes lhs: RawSpan,
  bytes rhs: RawSpan
) -> Bool {
  _debugPrecondition(lhs.byteCount > 0)
  if lhs.byteCount != rhs.byteCount {
    return false
  }
  return lhs.withUnsafeBytes { lhsBuffer in
    return rhs.withUnsafeBytes { rhsBuffer in
      return 0 == memcmp(
        lhsBuffer.baseAddress.unsafelyUnwrapped,
        rhsBuffer.baseAddress.unsafelyUnwrapped,
        lhs.byteCount
      )
    }
  }
}

fileprivate func isEqual(
  asciiBytes lhs: RawSpan,
  utf16Bytes rhs: RawSpan
) -> Bool {
    if lhs.byteCount * 2 != rhs.byteCount {
      return false
    }
    for lhsIdx in lhs.byteOffsets {
      // Bounds checking handled by looping over `byteOffsets`
      let l = unsafe lhs.unsafeLoadUnaligned(fromUncheckedByteOffset: lhsIdx, as: UInt8.self)
      // Bounds checking handled by the count * 2 verification earlier
      let r = unsafe UInt8(truncatingIfNeeded: rhs.unsafeLoadUnaligned(fromUncheckedByteOffset: lhsIdx, as: UInt16.self))
      if l != r {
        return false
      }
    }
    return true
}

internal func isEqual<LHSEncoding: _UnicodeEncoding, RHSEncoding: _UnicodeEncoding>(
  bytes lhs: RawSpan,
  encoding lhsEnc: LHSEncoding.Type,
  bytes rhs: RawSpan,
  encoding rhsEnc: RHSEncoding.Type
) -> Bool {
  if let trivialCheck = lhs.contentsAreTriviallyIdentical(to: rhs) {
    return trivialCheck
  }
  // ASCII == UTF8 can just use memcmp
  if (lhsEnc == rhsEnc) ||
      (lhsEnc == Unicode.ASCII.self && rhsEnc == Unicode.UTF8.self) ||
      (lhsEnc == Unicode.UTF8.self && rhsEnc == Unicode.ASCII.self) {
    return isEqual(bytes: lhs, bytes: rhs)
  }
  if lhsEnc == Unicode.UTF8.self && rhsEnc == Unicode.UTF16.self {
    return isEqual(utf8Bytes: lhs, utf16Bytes: rhs)
  }
  if lhsEnc == Unicode.UTF16.self && rhsEnc == Unicode.UTF8.self {
    return isEqual(utf8Bytes: rhs, utf16Bytes: lhs)
  }
  if lhsEnc == Unicode.ASCII.self && rhsEnc == Unicode.UTF16.self {
    return isEqual(asciiBytes: lhs, utf16Bytes: rhs)
  }
  if lhsEnc == Unicode.UTF16.self && rhsEnc == Unicode.ASCII.self {
    return isEqual(asciiBytes: rhs, utf16Bytes: lhs)
  }
  fatalError("Unsupported combination of encodings")
}

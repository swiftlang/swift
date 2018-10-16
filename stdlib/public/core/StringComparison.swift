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

// TODO: pick values that give us the best branching pattern
@usableFromInline // FIXME(sil-serialize-all)
internal
enum _GutsClassification: UInt {
  case smallUTF8 = 0
  case irregular = 1
  case regularASCII = 2
  case regularUTF16 = 4
}

extension _StringGuts {
  @inlinable
  var classification: _GutsClassification {
    if _isSmall { return .smallUTF8 }
    if _isContiguous { return isASCII ? .regularASCII : .regularUTF16 }
    return .irregular
  }
}

// HACK: This gets rid of some retains/releases that was slowing down the
// memcmp fast path for comparing ascii strings. rdar://problem/37473470
@inline(never) // @outlined
@_effects(readonly)
@usableFromInline // @opaque
internal
func _compareUnicode(
  _ lhs: _StringGuts._RawBitPattern, _ rhs: _StringGuts._RawBitPattern
) -> Int {
  let left = _StringGuts(rawBits: lhs)
  let right = _StringGuts(rawBits: rhs)

  switch (left.classification, right.classification) {
    // Both small: fast in-register comparison
    case (.smallUTF8, .smallUTF8):
      return left._smallUTF8String._compare(right._smallUTF8String).rawValue

    // Either irregular: branch to opaque code
    case (.irregular, _):
      return left._asOpaque()._compare(right).rawValue
    case (_, .irregular):
      return right._asOpaque()._compare(left).flipped.rawValue

    // One small, other contiguous in memory
    case (.smallUTF8, _):
      return left._smallUTF8String._compare(_contiguous: right).rawValue
    case (_, .smallUTF8):
      return right._smallUTF8String._compare(
        _contiguous: left
      ).flipped.rawValue

    // Both contiguous
    case (.regularASCII, _):
      return left._unmanagedASCIIView._compare(_contiguous: right).rawValue
    case (.regularUTF16, _):
      return left._unmanagedUTF16View._compare(_contiguous: right).rawValue
  }
}

@inline(never) // @outlined
@_effects(readonly)
@usableFromInline // @opaque
internal
func _compareUnicode(
  _ lhs: _StringGuts._RawBitPattern, _ leftRange: Range<Int>,
  _ rhs: _StringGuts._RawBitPattern, _ rightRange: Range<Int>
) -> Int {
  let left = _StringGuts(rawBits: lhs)
  let right = _StringGuts(rawBits: rhs)

  switch (left.classification, right.classification) {
    // Both small: fast in-register comparison
    case (.smallUTF8, .smallUTF8):
      return left._smallUTF8String[leftRange]._compare(
        right._smallUTF8String[rightRange]
      ).rawValue

    // Either irregular: branch to opaque code
    case (.irregular, _):
      return left._asOpaque()[leftRange]._compare(right, rightRange).rawValue
    case (_, .irregular):
      return right._asOpaque()[rightRange]._compare(
        left, leftRange
      ).flipped.rawValue

    // One small, other contiguous in memory
    case (.smallUTF8, _):
      return left._smallUTF8String[leftRange]._compare(
        _contiguous: right, rightRange
      ).rawValue
    case (_, .smallUTF8):
      return right._smallUTF8String[rightRange]._compare(
        _contiguous: left, leftRange
      ).flipped.rawValue

    // Both contiguous
    case (.regularASCII, _):
      return left._unmanagedASCIIView[leftRange]._compare(
        _contiguous: right, rightRange
      ).rawValue
    case (.regularUTF16, _):
      return left._unmanagedUTF16View[leftRange]._compare(
        _contiguous: right, rightRange
      ).rawValue
  }
}

// TODO: coalesce many of these into a protocol to simplify the code

extension _SmallUTF8String {
  @inlinable
  func _compare(_ other: _SmallUTF8String) -> _Ordering {
#if arch(i386) || arch(arm)
    _conditionallyUnreachable()
#else
    // TODO: Ensure normality when adding UTF-8 support
    _sanityCheck(self.isASCII && other.isASCII, "Need to ensure normality")
    if self._storage == other._storage { return .equal }
    for i in 0..<Swift.min(self.count, other.count) {
      if self[i] < other[i] { return .less }
      if self[i] > other[i] { return .greater }
    }
    return self.count < other.count ? .less : .greater
#endif // 64-bit
  }
  func _compare(_contiguous other: _StringGuts) -> _Ordering {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    _sanityCheck(other._isContiguous)
    if other.isASCII {
      // TODO: fast in-register comparison
      return self._compare(other._unmanagedASCIIView)
    }
    return self._compare(other._unmanagedUTF16View)
#endif // 64-bit
  }

  func _compare(
    _contiguous other: _StringGuts, _ otherRange: Range<Int>
  ) -> _Ordering {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    _sanityCheck(other._isContiguous)
    if other.isASCII {
      return self._compare(other._unmanagedASCIIView[otherRange])
    }
    return self._compare(other._unmanagedUTF16View[otherRange])
#endif // 64-bit
  }

  func _compare(_ other: _UnmanagedString<UInt8>) -> _Ordering {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    if _fastPath(self.isASCII) {
      return self.withUnmanagedASCII { selfView in
        return _Ordering(
          signedNotation: selfView.compareASCII(to: other))
      }
    }
    return self.withUnmanagedUTF16 { $0._compare(other) }
#endif // 64-bit
  }

  func _compare(_ other: _UnmanagedString<UInt16>) -> _Ordering {
#if arch(i386) || arch(arm)
    unsupportedOn32bit()
#else
    if _fastPath(self.isASCII) {
      return self.withUnmanagedASCII { $0._compare(other) }
    }
    return self.withUnmanagedUTF16 { $0._compare(other) }
#endif // 64-bit    
  }
}

extension _UnmanagedString where CodeUnit == UInt8 {
  func _compare(_contiguous other: _StringGuts) -> _Ordering {
    _sanityCheck(other._isContiguous)
    if other.isASCII {
      return self._compare(other._unmanagedASCIIView)
    }
    return self._compare(other._unmanagedUTF16View)
  }
  func _compare(
    _contiguous other: _StringGuts, _ otherRange: Range<Int>
  ) -> _Ordering {
    _sanityCheck(other._isContiguous)
    if other.isASCII {
      return self._compare(other._unmanagedASCIIView[otherRange])
    }
    return self._compare(other._unmanagedUTF16View[otherRange])
  }

  func _compare(_ other: _UnmanagedString<UInt8>) -> _Ordering {
    fatalError("Should have hit the ascii comp in StringComparable.compare")
  }
  func _compare(_ other: _UnmanagedString<UInt16>) -> _Ordering {
    return self._compareStringsPreLoop(other)
  }
}

extension _UnmanagedString where CodeUnit == UInt16 {
  func _compare(_contiguous other: _StringGuts) -> _Ordering {
    _sanityCheck(other._isContiguous)
    if other.isASCII {
      return self._compare(other._unmanagedASCIIView)
    }
    return self._compare(other._unmanagedUTF16View)
  }
  func _compare(
    _contiguous other: _StringGuts, _ otherRange: Range<Int>
  ) -> _Ordering {
    _sanityCheck(other._isContiguous)
    if other.isASCII {
      return self._compare(other._unmanagedASCIIView[otherRange])
    }
    return self._compare(other._unmanagedUTF16View[otherRange])
  }

  func _compare(_ other: _UnmanagedString<UInt8>) -> _Ordering {
    return other._compare(self).flipped
  }
  func _compare(_ other: _UnmanagedString<UInt16>) -> _Ordering {
    return self._compareStringsPreLoop(other)
  }
}

extension _UnmanagedOpaqueString {
  func _compare(_ other: _StringGuts) -> _Ordering {
    return self._compareOpaque(other)
  }
  func _compare(_ other: _StringGuts, _ otherRange: Range<Int>) -> _Ordering {
    return self._compareOpaque(other, otherRange)
  }
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

internal let _leadingSurrogateBias: UInt16 = 0xd800
internal let _trailingSurrogateBias: UInt16 = 0xdc00
internal let _surrogateMask: UInt16 = 0xfc00

@inline(__always)
internal func _isSurrogate(_ cu: UInt16) -> Bool {
  return _isLeadingSurrogate(cu) || _isTrailingSurrogate(cu)
}

@inline(__always)
internal func _isLeadingSurrogate(_ cu: UInt16) -> Bool {
  // NOTE: Specifically match against the trailing surrogate mask, as it matches
  // more cases.
  return cu & _surrogateMask == _leadingSurrogateBias
}

@inline(__always)
internal func _isTrailingSurrogate(_ cu: UInt16) -> Bool {
  return cu & _surrogateMask == _trailingSurrogateBias
}
@inline(__always)
internal func _decodeSurrogatePair(
  leading high: UInt16, trailing low: UInt16
) -> UInt32 {
  _sanityCheck(_isLeadingSurrogate(high) && _isTrailingSurrogate(low))
  let hi10: UInt32 = UInt32(high) &- UInt32(_leadingSurrogateBias)
  _sanityCheck(hi10 < 1<<10, "I said high 10. Not high, like, 20 or something")
  let lo10: UInt32 = UInt32(low) &- UInt32(_trailingSurrogateBias)
  _sanityCheck(lo10 < 1<<10, "I said low 10. Not low, like, 20 or something")

  return ((hi10 &<< 10) | lo10) &+ 0x1_00_00
}

internal func _hasNormalizationBoundary(before cu: UInt16) -> Bool {
  guard !_isSurrogate(cu) else { return false }
  return UnicodeScalar(_unchecked: UInt32(cu))._hasNormalizationBoundaryBefore
}

//
// Pointer casting helpers
//
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

extension _FixedArray16 where T == UInt16 {
  mutating func fill(from other: _UnmanagedString<T>) {
    _sanityCheck(other.count < _FixedArray16<T>.capacity,
      "out of bounds fill")
    for i in 0..<other.count {
      self[i] = other[i]
    }
  }
}

@_frozen
@usableFromInline internal
enum _Ordering: Int, Equatable {
  case less = -1
  case equal = 0
  case greater = 1

  @usableFromInline internal
  var flipped: _Ordering {
    switch self {
      case .less: return .greater
      case .equal: return .equal
      case .greater: return .less
    }
  }

  @inline(__always)
  @usableFromInline internal
  init(signedNotation int: Int) {
    self = int < 0 ? .less : int == 0 ? .equal : .greater
  }
}

extension _UnmanagedString where CodeUnit == UInt8 {
  // TODO: These should be SIMD-ized
  internal func _findDiffIdx(_ other: _UnmanagedString<UInt16>) -> Int {
    let count = Swift.min(self.count, other.count)
    for idx in 0..<count {
      guard UInt16(self[idx]) == other[idx] else {
        return idx
      }
    }
    return count
  }
}

internal func _findDiffIdx(
  _ left: UnsafeBufferPointer<UInt8>,
  _ right: UnsafeBufferPointer<UInt16>
) -> Int {
  let count = Swift.min(left.count, right.count)
  for idx in 0..<count {
    guard UInt16(left[idx]) == right[idx] else {
      return idx
    }
  }
  return count
}

internal func _findDiffIdx<CodeUnit>(
  _ left: UnsafeBufferPointer<CodeUnit>,
  _ right: UnsafeBufferPointer<CodeUnit>
) -> Int where CodeUnit : FixedWidthInteger & UnsignedInteger {
  let count = Swift.min(left.count, right.count)
  for idx in 0..<count {
    guard left[idx] == right[idx] else {
      return idx
    }
  }
  return count
}

extension _UnmanagedString where CodeUnit : FixedWidthInteger & UnsignedInteger {
  internal func _findDiffIdx<CodeUnit>(
    _ other: _UnmanagedString<CodeUnit>
  ) -> Int {
    let count = Swift.min(self.count, other.count)
    for idx in 0..<count {
      guard self[idx] == other[idx] else {
        return idx
      }
    }
    return count
  }
}

extension _UnmanagedOpaqueString {
  internal func _findDiffIdx(_ other: _StringGuts, _ otherRange: Range<Int>
  ) -> Int {
    let count = Swift.min(self.count, otherRange.count)
    for idx in 0..<count {
      guard self[idx] == other.codeUnit(
        atCheckedOffset: idx + otherRange.lowerBound
      ) else {
        return idx
      }
    }
    return count
  }
}

internal func _lexicographicalCompare(_ lhs: Int, _ rhs: Int) -> _Ordering {
  // TODO: inspect code quality
  return lhs < rhs ? .less : (lhs > rhs ? .greater : .equal)
}

internal func _lexicographicalCompare(
  _ lhs: UInt16, _ rhs: UInt16
) -> _Ordering {
  return lhs < rhs ? .less : (lhs > rhs ? .greater : .equal)
}

internal func _lexicographicalCompare(
  _ leftHS: UnsafeBufferPointer<UInt16>,
  _ rightHS: UnsafeBufferPointer<UInt16>
) -> _Ordering {
  let count = Swift.min(leftHS.count, rightHS.count)

  let idx = _findDiffIdx(leftHS, rightHS)
  guard idx < count else {
    return _lexicographicalCompare(leftHS.count, rightHS.count)
  }
  let leftHSPtr = leftHS.baseAddress._unsafelyUnwrappedUnchecked
  let rightHSPtr = rightHS.baseAddress._unsafelyUnwrappedUnchecked
  return _lexicographicalCompare(leftHSPtr[idx], rightHSPtr[idx])
}

internal func _lexicographicalCompare(
  _ leftHS: UnsafeBufferPointer<UInt8>,
  _ rightHS: UnsafeBufferPointer<UInt16>
) -> _Ordering {
  let count = Swift.min(leftHS.count, rightHS.count)

  let idx = _findDiffIdx(leftHS, rightHS)
  guard idx < count else {
    return _lexicographicalCompare(leftHS.count, rightHS.count)
  }
  let leftHSPtr = leftHS.baseAddress._unsafelyUnwrappedUnchecked
  let rightHSPtr = rightHS.baseAddress._unsafelyUnwrappedUnchecked
  return _lexicographicalCompare(UInt16(leftHSPtr[idx]), rightHSPtr[idx])
}
@inline(__always)
internal func _lexicographicalCompare(
  _ leftHS: UnsafePointer<_Normalization._SegmentOutputBuffer>,
  leftCount: Int,
  _ rightHS: UnsafePointer<_Normalization._SegmentOutputBuffer>,
  rightCount: Int
) -> _Ordering {
  return _lexicographicalCompare(
    _castOutputBuffer(leftHS, endingAt: leftCount),
    _castOutputBuffer(rightHS, endingAt: rightCount))
}
@inline(__always)
internal func _lexicographicalCompare(
  _ leftHS: Array<UInt16>,
  _ rightHS: Array<UInt16>
) -> _Ordering {
  return leftHS.withUnsafeBufferPointer { leftPtr in
    return rightHS.withUnsafeBufferPointer { rightPtr in
      return _lexicographicalCompare(leftPtr, rightPtr)
    }
  }
}

internal func _parseRawScalar(
  _ buf: UnsafeMutablePointer<_Normalization._SegmentOutputBuffer>,
  startingFrom idx: Int = 0
) -> (UnicodeScalar, scalarEndIndex: Int) {
  return Swift._parseRawScalar(buffer: _castOutputBuffer(buf), startingFrom: idx)
}

internal func _parseRawScalar(
  buffer buf: UnsafeBufferPointer<UInt16>,
  startingFrom idx: Int = 0
) -> (UnicodeScalar, scalarEndIndex: Int) {
  let ptr = buf.baseAddress._unsafelyUnwrappedUnchecked
  _sanityCheck(idx >= 0 && idx < buf.count, "out of bounds index")
  let cu: UInt16 = ptr[idx]
  if _slowPath(idx+1 == buf.count) {
    return (UnicodeScalar(_unchecked: UInt32(cu)), idx+1)
  }
  guard _isLeadingSurrogate(cu) else {
    return (UnicodeScalar(_unchecked: UInt32(cu)), idx+1)
  }
  let nextCu: UInt16 = ptr[idx+1]
  guard _isTrailingSurrogate(nextCu) else {
    // Invalid surrogate pair: just return the invalid value
    return (UnicodeScalar(_unchecked: UInt32(cu)), idx+1)
  }

  // Decode
  let value: UInt32 = _decodeSurrogatePair(leading: cu, trailing: nextCu)
  _sanityCheck(Int32(exactly: value) != nil, "top bit shouldn't be set")
  return (UnicodeScalar(_unchecked: value), idx+2)
}

extension _UnmanagedOpaqueString {
  internal func _parseRawScalar(
    startingFrom idx: Int = 0
  ) -> (UnicodeScalar, scalarEndIndex: Int) {
    var buffer = _FixedArray2<UInt16>(allZeros:())
    if idx+1 < self.count {
      buffer[0] = self[idx]
      buffer[1] = self[idx+1]

      let bufferPointer = _unsafeBufferPointerCast(
        &buffer, 2, to: UInt16.self
      )
      return Swift._parseRawScalar(buffer: bufferPointer, startingFrom: 0)
    } else {
      buffer[0] = self[idx]

      let bufferPointer = _unsafeBufferPointerCast(
        &buffer, 1, to: UInt16.self
      )
      return Swift._parseRawScalar(buffer: bufferPointer, startingFrom: 0)
    }
  }
}

extension _UnmanagedString where CodeUnit == UInt16 {
  internal func _parseRawScalar(
    startingFrom idx: Int = 0
  ) -> (UnicodeScalar, scalarEndIndex: Int) {
    _sanityCheck(idx >= 0 && idx < self.count, "out of bounds index")
    let cu = self[idx]
    if _slowPath(idx+1 == self.count) {
      return (UnicodeScalar(_unchecked: UInt32(cu)), idx+1)
    }
    guard _isLeadingSurrogate(cu) else {
      return (UnicodeScalar(_unchecked: UInt32(cu)), idx+1)
    }
    let nextCu = self[idx+1]
    guard _isTrailingSurrogate(nextCu) else {
      // Invalid surrogate pair: just return the invalid value
      return (UnicodeScalar(_unchecked: UInt32(cu)), idx+1)
    }

    // Decode
    let value: UInt32 = _decodeSurrogatePair(leading: cu, trailing: nextCu)
    _sanityCheck(Int32(exactly: value) != nil, "top bit shouldn't be set")
    return (UnicodeScalar(_unchecked: value), idx+2)
  }

  internal func _reverseParseRawScalar(
    endingAt idx: Int // one-past-the-end
  ) -> (UnicodeScalar, scalarStartIndex: Int) {
    _sanityCheck(idx > 0 && idx <= self.count, "out of bounds end index")

    // Corner case: leading un-paired surrogate
    if _slowPath(idx == 1) {
      return (UnicodeScalar(_unchecked: UInt32(self[0])), 0)
    }

    let cu = self[idx-1]
    guard _isTrailingSurrogate(cu) else {
      return (UnicodeScalar(_unchecked: UInt32(cu)), idx-1)
    }
    let priorCU = self[idx-2]
    guard _isLeadingSurrogate(priorCU) else {
      return (UnicodeScalar(_unchecked: UInt32(cu)), idx-1)
    }

    // Decode
    let value: UInt32 = _decodeSurrogatePair(leading: priorCU, trailing: cu)
    _sanityCheck(Int32(exactly: value) != nil, "top bit shouldn't be set")
    return (UnicodeScalar(_unchecked: value), idx-2)
  }

  internal func _tryNormalize(
    into outputBuffer: UnsafeMutablePointer<_Normalization._SegmentOutputBuffer>
  ) -> Int? {
    return self._tryNormalize(into: _castOutputBuffer(outputBuffer))
  }

  internal func _tryNormalize(
    into outputBuffer: UnsafeMutableBufferPointer<UInt16>
  ) -> Int? {
    var err = __swift_stdlib_U_ZERO_ERROR
    let count = __swift_stdlib_unorm2_normalize(
      _Normalization._nfcNormalizer,
      self.start,
      numericCast(self.count),
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

  internal func _slowNormalize() -> [UInt16] {
    _sanityCheck(self.count > 0, "called on empty string")

    let canary = self.count * _Normalization._maxNFCExpansionFactor
    var count = self.count
    while true {
      var result = Array<UInt16>(repeating: 0, count: count)
      if let length = result.withUnsafeMutableBufferPointer({ (bufPtr) -> Int? in
        return self._tryNormalize(into: bufPtr)
      }) {
        result.removeLast(count - length)
        return result
      }
      // Otherwise, we need to grow
      guard count <= canary else {
        fatalError("Invariant broken: Max decomposition factor insufficient")
      }
      count *= 2
    }
  }
}

internal func _tryNormalize(
  _ input: UnsafeBufferPointer<UInt16>,
  into outputBuffer: UnsafeMutablePointer<_Normalization._SegmentOutputBuffer>
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

extension _UnmanagedString where CodeUnit == UInt8 {
  @inlinable // FIXME(sil-serialize-all)
  internal func compareASCII(to other: _UnmanagedString<UInt8>) -> Int {
    // FIXME Results should be the same across all platforms.
    if self.start == other.start {
      return (self.count &- other.count).signum()
    }
    var cmp = Int(truncatingIfNeeded:
      _swift_stdlib_memcmp(
        self.rawStart, other.rawStart,
        Swift.min(self.count, other.count)))
    if cmp == 0 {
      cmp = self.count &- other.count
    }
    return cmp.signum()
  }
}

extension _UnmanagedOpaqueString {
  @inline(never) // @outlined
  @usableFromInline
  internal
  func _compareOpaque(_ other: _StringGuts) -> _Ordering {
    return self._compareOpaque(other, 0..<other.count)
  }

  @inline(never) // @outlined
  @usableFromInline
  internal
  func _compareOpaque(
    _ other: _StringGuts, _ otherRange: Range<Int>
  ) -> _Ordering {
    //
    // Do a fast Latiny comparison loop; bail if that proves insufficient.
    //
    // The vast majority of the time, seemingly-non-contiguous Strings are
    // really ASCII strings that were bridged improperly. E.g., unknown nul-
    // termination of an all-ASCII file loaded by String.init(contentsOfFile:).
    //

    let selfCount = self.count
    let otherCount = otherRange.count
    let count = Swift.min(selfCount, otherCount)
    let idx = self._findDiffIdx(other, otherRange)
    if idx == count {
      return _lexicographicalCompare(selfCount, otherCount)
    }

    let selfCU = self[idx]
    let otherCU = other.codeUnit(atCheckedOffset: idx + otherRange.lowerBound)

    //
    // Fast path: if one is ASCII, we can often compare the code units directly.
    //
    let selfIsASCII = selfCU <= 0x7F
    let otherIsASCII = otherCU <= 0x7F

    let selfIsSingleSegmentScalar =
        self.hasNormalizationBoundary(after: idx)
        && _hasNormalizationBoundary(before: selfCU)
    let otherIsSingleSegmentScalar =
        other.hasNormalizationBoundary(after: idx)
        && _hasNormalizationBoundary(before: otherCU)

    if _fastPath(selfIsASCII || otherIsASCII) {
      _sanityCheck(idx < selfCount && idx < otherCount,
        "Should be caught by check against min-count")
      // Check if next CU is <0x300, or if we're in a
      // "_isNormalizedSuperASCII" case. 99.9% of the time, we're here because
      // the non-contig string is ASCII. We never want to hit the pathological
      // path for those.

      if selfIsASCII && otherIsASCII {
        if selfIsSingleSegmentScalar && otherIsSingleSegmentScalar {
          return _lexicographicalCompare(selfCU, otherCU)
        }

        return self._compareOpaquePathological(
          other, otherRange, startingFrom: Swift.max(0, idx-1))
      }

      if selfIsASCII && selfIsSingleSegmentScalar
      && self._parseRawScalar(startingFrom: idx).0._isNormalizedSuperASCII {
         return .less
      } else if otherIsASCII && otherIsSingleSegmentScalar
      && self._parseRawScalar(startingFrom: idx).0._isNormalizedSuperASCII {
         return .greater
       }
    }

    return self._compareOpaquePathological(
      other, otherRange, startingFrom: Swift.max(0, idx-1)
    )
  }

  @inline(never)
  func _compareOpaquePathological(
    _ other: _StringGuts, _ otherRange: Range<Int>,
    startingFrom: Int
  ) -> _Ordering {
    // Compare by pulling in a segment at a time, normalizing then comparing
    // individual code units
    var selfIterator = _NormalizedCodeUnitIterator(self[startingFrom...])
    return selfIterator.compare(with:
      _NormalizedCodeUnitIterator(other, otherRange, startIndex: startingFrom)
    )
  }
}

extension UnicodeScalar {
  internal func _normalize(
    into outputBuffer: UnsafeMutablePointer<_Normalization._SegmentOutputBuffer>
  ) -> Int {
    // Implementation: Perform the normalization on an input buffer and output
    // buffer.
    func impl(
      _ input: UnsafeMutablePointer<_FixedArray2<UInt16>>,
      count: Int,
      into output: UnsafeMutablePointer<_Normalization._SegmentOutputBuffer>
    ) -> Int {
      let inputBuffer = _unsafeBufferPointerCast(
        input, count, to: UInt16.self
      )
      let outputBuffer = _unsafeMutableBufferPointerCast(
        output, _FixedArray8<UInt16>.capacity, to: UInt16.self
      )
      return _tryNormalize(
        inputBuffer, into: outputBuffer
      )._unsafelyUnwrappedUnchecked
    }

    var inBuffer = _FixedArray2<UInt16>(allZeros:())
    var inLength = 0
    for cu in self.utf16 {
      inBuffer[inLength] = cu
      inLength += 1
    }

    return impl(&inBuffer, count: inLength, into: outputBuffer)
  }

  internal static let maxValue = 0x0010_FFFF
}

private struct _UnicodeScalarExceptions {
  fileprivate let _multiSegmentExpanders: Set<UInt32>
  fileprivate let _normalizedASCIIStarter: Array<UInt32>

  @inline(__always)
  init() {
    var msExpanders = Set<UInt32>()
    msExpanders.reserveCapacity(16)
    var normalizedASCIIStarter = Array<UInt32>()
    normalizedASCIIStarter.reserveCapacity(8)

    for rawValue in 0..<UnicodeScalar.maxValue {
      guard let scalar = UnicodeScalar(rawValue) else { continue }

      // Fast path: skip unassigned code points
      guard scalar.properties.generalCategory != .unassigned else { continue }

      // Fast path: skip unless QC_FCD=no
      if _fastPath(!scalar.properties.isFullCompositionExclusion) {
        continue
      }

      var outBuffer = _Normalization._SegmentOutputBuffer(allZeros:())
      let length = scalar._normalize(into: &outBuffer)

      // See if this normalized to have an ASCII starter
      if _slowPath(outBuffer[0] <= 0x7F) {
        normalizedASCIIStarter.append(scalar.value)
      }

      // See if this normalizes to multiple segments
      var i = 0
      while i < length {
        let (innerScalar, nextI) = _parseRawScalar(&outBuffer, startingFrom: i)
        if _slowPath(i != 0 && innerScalar._hasNormalizationBoundaryBefore) {
          guard innerScalar._hasNormalizationBoundaryBefore else {
            fatalError(
              "Unicode invariant violated: non-starter multi-segment expander")
          }
          msExpanders.insert(scalar.value)
          break
        }
        i = nextI
      }
    }

    self._multiSegmentExpanders = msExpanders
    self._normalizedASCIIStarter = normalizedASCIIStarter
  }
}
private let _unicodeScalarExceptions: _UnicodeScalarExceptions = {
  return _UnicodeScalarExceptions()
}()

extension UnicodeScalar {
  // Multi-Segment Expanders - Unicode defines "expanding canonical
  // decompositions", where even in NFC a single scalar expands to multiple
  // scalars. A small subset (currently 12 scalars circa Unicode 10) of these
  // will expand into multiple normalization segments, breaking any kind of
  // segment-by- segment logic or processing even under NFC. These are a subset
  // of what is identified by the UCD as "composition exclusion" scalars. Since
  // we don't have access to a UCD (available only at runtime), we go through
  // ICU which lumps those and even more as "Full Composition Exclusions". Of
  // the many full composition exclusions, this set (created once at runtime as
  // this can change with Unicode version) tracks just those that can expand
  // into multiple normalization segments.
  internal var _isMultiSegmentExpander: Bool {
    return _unicodeScalarExceptions._multiSegmentExpanders.contains(self.value)
  }

  // Whether, post-normalization, this scalar definitely compares greater than
  // any ASCII scalar. This is true for all super-ASCII scalars that are not
  // ASCII Normalized Starters.
  //
  // ASCII Normalized Starters - A handful of scalars normalize to have ASCII
  // starters, e.g. Greek question mark ";". As of Unicode 10 there are 3 (all
  // from Unicode 1.1 originally) and more are unlikely. But, there could be
  // more in future versions, so determine at runtime.
  internal var _isNormalizedSuperASCII: Bool {
    if _slowPath(
      _unicodeScalarExceptions._normalizedASCIIStarter.contains(self.value)
    ) {
      return false
    }
    return self.value > 0x7F
  }
}

extension _UnmanagedString where CodeUnit == UInt8 {
  @usableFromInline
  internal func _compareStringsPreLoop(
    _ other: _UnmanagedString<UInt16>
  ) -> _Ordering {
    let count = Swift.min(self.count, other.count)

    //
    // Fast scan until we find a difference
    //
    let idx = self._findDiffIdx(other)
    guard idx < count else {
      return _lexicographicalCompare(self.count, other.count)
    }
    let otherCU = other[idx]

    //
    // Fast path: if other is super-ASCII post-normalization, we must be less.
    // If other is ASCII and a single-scalar segment, we have our answer.
    //
    if otherCU > 0x7F {
      if _fastPath(
        other._parseRawScalar(startingFrom: idx).0._isNormalizedSuperASCII
      ) {
        return .less
      }

      // Rare pathological case, e.g. Kelvin symbol
      var selfIterator = _NormalizedCodeUnitIterator(self)
      return selfIterator.compare(with: _NormalizedCodeUnitIterator(other))
    }

    let selfASCIIChar = UInt16(self[idx])
    _sanityCheck(selfASCIIChar != otherCU, "should be different")
    if idx+1 == other.count {
      return _lexicographicalCompare(selfASCIIChar, otherCU)
    }
    if _fastPath(other.hasNormalizationBoundary(after: idx)) {
      return _lexicographicalCompare(selfASCIIChar, otherCU)
    }

    //
    // Otherwise, need to normalize the segment and then compare
    //
    return _compareStringsPostSuffix(
      selfASCIIChar: selfASCIIChar, otherUTF16WithLeadingASCII: other[idx...]
    )
  }
}

extension _StringGuts {
  internal func hasNormalizationBoundary(after index: Int) -> Bool {
    let nextIndex = index + 1
    if nextIndex >= self.count {
      return true
    }

    let nextCU = self.codeUnit(atCheckedOffset: nextIndex)
    return _hasNormalizationBoundary(before: nextCU)
  }
}

extension _UnmanagedOpaqueString {
  internal func hasNormalizationBoundary(after index: Int) -> Bool {
    let nextIndex = index + 1
    if nextIndex >= self.count {
      return true
    }

    let nextCU = self[nextIndex]
    return _hasNormalizationBoundary(before: nextCU)
  }
}

extension _UnmanagedString where CodeUnit == UInt16 {
  internal func hasNormalizationBoundary(after index: Int) -> Bool {
    let nextIndex = index + 1
    if nextIndex >= self.count {
      return true
    }

    let nextCU = self[nextIndex]
    return _hasNormalizationBoundary(before: nextCU)
  }
}

extension BidirectionalCollection where Element == UInt16, SubSequence == Self {
  internal func hasNormalizationBoundary(after index: Index) -> Bool {
    let nextIndex = self.index(after: index)
    if nextIndex == self.endIndex {
      return true
    }

    let nextCU = self[nextIndex]
    return _hasNormalizationBoundary(before: nextCU)
  }
}

@inline(never) // @outlined
private func _compareStringsPostSuffix(
  selfASCIIChar: UInt16,
  otherUTF16WithLeadingASCII: _UnmanagedString<UInt16>
) -> _Ordering {
  let otherCU = otherUTF16WithLeadingASCII[0]
  _sanityCheck(otherCU <= 0x7F, "should be ASCII, otherwise no need to call")

  let segmentEndIdx = otherUTF16WithLeadingASCII._findNormalizationSegmentEnd(
    startingFrom: 0)
  let segment = otherUTF16WithLeadingASCII[..<segmentEndIdx]

  // Fast path: If prenormal, we're done.
  if _Normalization._prenormalQuickCheckYes(segment) {
    return _lexicographicalCompare(selfASCIIChar, otherCU)
  }

  // Normalize segment, and then compare first code unit
  var outputBuffer = _Normalization._SegmentOutputBuffer(allZeros:())
  if _fastPath(
    segment._tryNormalize(into: &outputBuffer) != nil
  ) {
    return _lexicographicalCompare(selfASCIIChar, outputBuffer[0])
  }
  return _lexicographicalCompare(selfASCIIChar, segment._slowNormalize()[0])
}

extension _UnmanagedString where CodeUnit == UInt16 {
  //
  // Find the end of the normalization segment
  //
  internal func _findNormalizationSegmentEnd(startingFrom idx: Int) -> Int {
    let count = self.count
    _sanityCheck(idx < count, "out of bounds")

    // Normalization boundaries are best queried before known starters. Advance
    // past one scalar first.
    var (_, segmentEndIdx) = self._parseRawScalar(startingFrom: idx)
    while segmentEndIdx < count {
      let (scalar, nextIdx) = self._parseRawScalar(startingFrom: segmentEndIdx)
      if scalar._hasNormalizationBoundaryBefore {
        break
      }
      segmentEndIdx = nextIdx
    }
    return segmentEndIdx
  }

  internal func _findNormalizationSegmentStart(
    endingAt idx: Int // one-past-the-end
  ) -> Int {
    var idx = idx
    let count = self.count
    _sanityCheck(idx > 0 && idx <= count, "out of bounds")

    while idx > 0 {
      let (scalar, priorIdx) = _reverseParseRawScalar(endingAt: idx)
      idx = priorIdx
      if scalar._hasNormalizationBoundaryBefore {
        break
      }
    }
    return idx
  }

  internal func _findNormalizationSegment(spanning idx: Int) -> (Int, Int) {
    var idx = idx

    // Corner case: if we're sub-surrogate, back up
    if _slowPath(
      idx > 0
      && _isTrailingSurrogate(self[idx])
      && _isLeadingSurrogate(self[idx-1])
    ) {
      idx -= 1
    }
    let segmentEnd = self._findNormalizationSegmentEnd(startingFrom: idx)

    // Find the start
    if _slowPath(idx == 0) {
      return (0, segmentEnd)
    }

    // Check current scalar
    if self._parseRawScalar(startingFrom: idx).0._hasNormalizationBoundaryBefore {
      return (idx, segmentEnd)
    }

    // Reverse parse until we found the segment start
    let segmentStart = self._findNormalizationSegmentStart(endingAt: idx)

    return (segmentStart, segmentEnd)
  }

  // Wether the segment identified by `idx` is prenormal.
  //
  // Scalar values below 0x300 are special: normalization segments containing only
  // one such scalar are trivially prenormal under NFC. Most Latin-derived scripts
  // can be represented entirely by <0x300 scalar values, meaning that many user
  // strings satisfy this prenormal check. We call sub-0x300 scalars "Latiny" (not
  // official terminology).
  //
  // The check is effectively:
  //   1) Whether the current scalar <0x300, AND
  //   2) Whether the current scalar comprises the entire segment
  //
  internal func _isLatinyPrenormal(idx: Int
  ) -> Bool {
    _sanityCheck(idx < self.count, "out of bounds")

    let cu = self[idx]
    if _slowPath(cu >= 0x300) {
      return false
    }
    if _slowPath(idx+1 == self.count) {
      return true
    }

    let nextCU = self[idx+1]
    return nextCU < 0x300 || _hasNormalizationBoundary(before: nextCU)
  }

  @usableFromInline
  internal
  func _compareStringsPreLoop(
    _ other: _UnmanagedString<UInt16>
  ) -> _Ordering {
    let count = Swift.min(self.count, other.count)

    //
    // Fast scan until we find a diff
    //
    let idx = _findDiffIdx(other)
    guard idx < count else {
      return _lexicographicalCompare(self.count, other.count)
    }
    let selfCU = self[idx]
    let otherCU = other[idx]

    //
    // Fast path: sub-0x300 single-scalar segments can be compared directly
    //
    if _fastPath(
      _isLatinyPrenormal(idx: idx)
      && other._isLatinyPrenormal(idx: idx)
    ) {
      return _lexicographicalCompare(selfCU, otherCU)
    }

    return self._compareStringsSuffix(other: other, randomIndex: idx)
  }

  //Is the shorter of the two parameters a prefix of the other parameter?
  private func shorterPrefixesOther(
    _ other: _UnmanagedString<UInt16>
  ) -> Bool {
    if self.count == other.count {
      return false
    }

    let minimumLength = Swift.min(self.count, other.count)
    for i in 0..<minimumLength {
      if self[i] != other[i] {
        return false
      }
    }
    return true
  }

  private func _compareStringsSuffix(
    other: _UnmanagedString<UInt16>,
    randomIndex: Int
  ) -> _Ordering {
    let count = Swift.min(self.count, other.count)
    let selfCU = self[randomIndex]
    let otherCU = other[randomIndex]
    _sanityCheck(randomIndex >= 0 && randomIndex < count, "out of bounds")
    _sanityCheck(selfCU != otherCU, "should be called at a point of difference")

    //
    // Find the segment surrounding the random index passed in. This may involve
    // some back tracking to the nearest normalization boundary. Once we've
    // identified the segment, we can normalize and continue comparision.
    //
    // NOTE: We need to back-track for both self and other. Even though prefixes
    // are binary equal, the point of difference might be at the start of a new
    // segment for one and in the middle of the prior segment for the other. In
    // which case, we will want to effectively compare the two consecutive
    // segments together.
    //
    let (selfSegmentStartIdx, selfSegmentEndIdx) =
      self._findNormalizationSegment(spanning: randomIndex)
    let (otherSegmentStartIdx, otherSegmentEndIdx) =
      other._findNormalizationSegment(spanning: randomIndex)
    let comparisonStartIdx = Swift.min(selfSegmentStartIdx, otherSegmentStartIdx)


    //
    // Fast path: if both are prenormal, we have our answer
    //
    let selfSegment = self[comparisonStartIdx..<selfSegmentEndIdx]
    let otherSegment = other[comparisonStartIdx..<otherSegmentEndIdx]
    let selfSegmentPrenormal = _Normalization._prenormalQuickCheckYes(selfSegment)
    let otherSegmentPrenormal = _Normalization._prenormalQuickCheckYes(
      otherSegment)
    if selfSegmentPrenormal && otherSegmentPrenormal {
      return _lexicographicalCompare(selfCU, otherCU)
    }

    //
    // Pathological case: multi-segment expanders ruin segment-by-segment
    // processing.
    //
    // NOTE: Multi-segment expanders are (at least up til Unicode 10) always the
    // beginning of a normalization segment (i.e. they are starters). This is very
    // unlikely to change in the future, as new non-starter scalars that normalize
    // to pre-existing scalars would have to produce a starter. We validate this
    // fact on constructing our MultiSegmentExpander set, so we can rely on it
    // here.
    //
    if _slowPath(
       selfSegment._parseRawScalar().0._isMultiSegmentExpander
    || otherSegment._parseRawScalar().0._isMultiSegmentExpander
    ) {
      return self[comparisonStartIdx...]._compareStringsPathological(
        other: other[comparisonStartIdx...]
      )
    }

    //
    // Normalize segments and compare. If they still differ, we have our answer.
    //
    var selfOutputBuffer = _Normalization._SegmentOutputBuffer(allZeros:())
    var otherOutputBuffer = _Normalization._SegmentOutputBuffer(allZeros:())
    let selfSegmentLengthOpt: Int?
    let otherSegmentLengthOpt: Int?
    if selfSegmentPrenormal {
      selfOutputBuffer.fill(from: selfSegment)
      selfSegmentLengthOpt = selfSegment.count
    } else {
      selfSegmentLengthOpt = selfSegment._tryNormalize(into: &selfOutputBuffer)
    }
    if otherSegmentPrenormal {
      otherOutputBuffer.fill(from: otherSegment)
      otherSegmentLengthOpt = otherSegment.count
    } else {
      otherSegmentLengthOpt = otherSegment._tryNormalize(into: &otherOutputBuffer)
    }

    if _slowPath(selfSegmentLengthOpt == nil || otherSegmentLengthOpt == nil) {
      // If we couldn't normalize a segment into a generously large stack buffer,
      // we have a pathological String.
      return self[comparisonStartIdx...]._compareStringsPathological(
        other: other[comparisonStartIdx...]
      )
    }
    let selfLength = selfSegmentLengthOpt._unsafelyUnwrappedUnchecked
    let otherLength = otherSegmentLengthOpt._unsafelyUnwrappedUnchecked

    if Swift.shorterPrefixesOther(
      &selfOutputBuffer, selfLength,
      &otherOutputBuffer, otherLength)
    {
      let selfSlice = self[selfSegmentStartIdx...]
      let otherSlice = other[otherSegmentStartIdx...]
      return selfSlice._compareStringsPathological(other: otherSlice)
    }

    let comp = _lexicographicalCompare(
      &selfOutputBuffer, leftCount: selfLength,
      &otherOutputBuffer, rightCount: otherLength)
    if _fastPath(comp != .equal) {
      return comp
    }

    //
    // If they compare equal after normalization, we may have equal strings that
    // differ in form, e.g. NFC vs NFD strings. Or, we may have strings that
    // differ in form that also will differ later on. Either way, segment-by-
    // segment processing incurs significant overhead. We'd rather do larger
    // chunks of work at a time (e.g. ~1KB of text at a time). For now, we eagerly
    // process the entire strings, as chunking properly without guarantees of
    // normality is tricky (and expensive at times as well).
    //
    // NOTE: We could add a chunking path. It is hard to do correctly, because
    // Unicode. It's especially hard to test, because Unicode. It's hard to ensure
    // lasting correctness, because Unicode. (Also, sometimes it's impossible, but
    // that's what _compareStringsPathological is for.) However, it helps for very
    // long strings that differ in the middle. We might want this one day... but
    // not today.
    //
    // TODO: An additional (or even repeated) reapplying of the algorithm,
    // including the binary diff scan, could greatly benefit strings that only
    // sparsely differ in normality (penalizing strings that densely differ in
    // normality). This would add complexity, but with compelling data could be an
    // alternative to chunking.
    //
    return self[selfSegmentEndIdx...]._compareStringsPathological(
      other: other[otherSegmentEndIdx...]
    )
  }

  private func _compareStringsPathological(
    other: _UnmanagedString<UInt16>
  ) -> _Ordering {
    var selfIterator = _NormalizedCodeUnitIterator(self)
    return selfIterator.compare(with:
      _NormalizedCodeUnitIterator(other)
    )
  }
}

private func shorterPrefixesOther(
  _ selfBuffer: UnsafePointer<_Normalization._SegmentOutputBuffer>,
  _ selfLength: Int,
  _ otherBuffer: UnsafePointer<_Normalization._SegmentOutputBuffer>,
  _ otherLength: Int
) -> Bool {
  return shorterPrefixesOther(
    _castOutputBuffer(selfBuffer, endingAt: selfLength),
    _castOutputBuffer(otherBuffer, endingAt: otherLength)
  )
}

//Is the shorter of the two parameters a prefix of the other parameter?
private func shorterPrefixesOther(
  _ selfBuffer: UnsafeBufferPointer<UInt16>,
  _ otherBuffer: UnsafeBufferPointer<UInt16>
) -> Bool {
  if selfBuffer.count == otherBuffer.count {
    return false
  }

  let minimumLength = Swift.min(selfBuffer.count, otherBuffer.count)
  for i in 0..<minimumLength {
    if selfBuffer[i] != otherBuffer[i] {
      return false
    }
  }
  return true
}


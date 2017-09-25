// RUN: %target-build-swift %s -swift-version 3 -g -O -o %T/StringComparison
// RUN: %target-run %T/StringComparison
// REQUIRES: executable_test
// REQUIRES: objc_interop

import SwiftShims
import Darwin

//
// Print debugging helpers
//
extension UnsignedInteger {
  var hex: String { return String(self, radix: 16) }
}
extension UnicodeScalar {
  var hex: String { return String(self.value, radix: 16) }
}
extension Collection where Element: UnsignedInteger {
  var hex: String {
    return "[" + self.map { $0.hex }.joined(separator:", ") + "]"
  }
}

//
// HACK HACK HACK: Turn _sanityCheck into whether *this* file is debug or not
//
func _sanityCheck(
  _ condition: @autoclosure () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = #file, line: UInt = #line
) {
  _debugPrecondition(condition, message, file: file, line: line)  
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

// TODO(Recursive protocol constraints): Use Base.Element instead of UIntX
extension RandomAccessSlice where Base == UnsafeBufferPointer<UInt16> {
  var rebased: UnsafeBufferPointer<UInt16> {
    return UnsafeBufferPointer(rebasing: self)
  }
}
extension RandomAccessSlice where Base == UnsafeBufferPointer<UInt8> {
  var rebased: UnsafeBufferPointer<UInt8> {
    return UnsafeBufferPointer(rebasing: self)
  }
}
extension MutableRandomAccessSlice
where Base == UnsafeMutableBufferPointer<UInt16> {
  var rebased: UnsafeMutableBufferPointer<UInt16> {
    return UnsafeMutableBufferPointer(rebasing: self)
  }
}

print("[String Normalization]")
defer { print("[done]") }

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

  let result = ((hi10 &<< 10) | lo10) &+ 0x1_00_00
  // print("""
  //   hi10: \(hex(hi10)), lo10: \(hex(lo10)) ==> \(hex(result))
  // """)

  return result
}

internal func _normalizationBoundary(beforeCodeUnit cu: UInt16) -> Bool {
  guard !_isSurrogate(cu) else { return false }
  return UnicodeScalar(_unchecked: UInt32(cu))._hasNormalizationBoundaryBefore
}

//
// Pointer casting helpers
//
internal func _castOutputBuffer(
  _ ptr: UnsafeMutablePointer<_Normalization._SegmentOutputBuffer>,
  endingAt endIdx: Int = _Normalization._SegmentOutputBuffer._arraySize
) -> UnsafeMutableBufferPointer<UInt16> {
  let bufPtr: UnsafeMutableBufferPointer<UInt16> =
    _unsafeMutableBufferPointerCast(
      ptr, _Normalization._SegmentOutputBuffer._arraySize)
  return bufPtr[..<endIdx].rebased
}
internal func _castOutputBuffer(
  _ ptr: UnsafePointer<_Normalization._SegmentOutputBuffer>,
  endingAt endIdx: Int = _Normalization._SegmentOutputBuffer._arraySize
) -> UnsafeBufferPointer<UInt16> {
  let bufPtr: UnsafeBufferPointer<UInt16> =
    _unsafeBufferPointerCast(
      ptr, _Normalization._SegmentOutputBuffer._arraySize)
  return bufPtr[..<endIdx].rebased
}
internal func _castOutputBuffer(
  _ ptr: UnsafeMutablePointer<_Normalization._ChunkOutputBuffer>,
  endingAt endIdx: Int = _Normalization._ChunkOutputBuffer._arraySize
) -> UnsafeMutableBufferPointer<UInt16> {
  let bufPtr: UnsafeMutableBufferPointer<UInt16> =
    _unsafeMutableBufferPointerCast(
      ptr, _Normalization._ChunkOutputBuffer._arraySize)
  return bufPtr[..<endIdx].rebased
}
internal func _castOutputBuffer(
  _ ptr: UnsafePointer<_Normalization._ChunkOutputBuffer>,
  endingAt endIdx: Int = _Normalization._ChunkOutputBuffer._arraySize
) -> UnsafeBufferPointer<UInt16> {
  let bufPtr: UnsafeBufferPointer<UInt16> =
    _unsafeBufferPointerCast(
      ptr, _Normalization._ChunkOutputBuffer._arraySize)
  return bufPtr[..<endIdx].rebased
}

extension _FixedArray16 {
  mutating func fill(_ other: UnsafeBufferPointer<T>) {
    _sanityCheck(other.count < _FixedArray16<T>._arraySize,
      "out of bounds fill")
    for i in 0..<other.count {
      self[i] = other[i]
    }
  }
}

enum _Ordering {
  case less
  case equal
  case greater

  var flipped: _Ordering {
    switch self {
      case .less: return .greater
      case .equal: return .equal
      case .greater: return .less
    }
  }

  @inline(__always)
  init(signedNotation int: Int) {
    self = int < 0 ? .less : int == 0 ? .equal : .greater
  }
}

// TODO: These should be SIMD-ized
//
// TODO: drop the @inline(never), useful for now to inspect code quality
//
@inline(never)
internal func _findDiffIdx(
  _ leftBuffer: UnsafeBufferPointer<UInt8>,
  _ rightBuffer: UnsafeBufferPointer<UInt16>
) -> Int {
  let leftPtr = leftBuffer.baseAddress._unsafelyUnwrappedUnchecked
  let rightPtr = rightBuffer.baseAddress._unsafelyUnwrappedUnchecked

  let count = Swift.min(leftBuffer.count, rightBuffer.count)
  for idx in 0..<count {
    guard UInt16(leftPtr[idx]) == rightPtr[idx] else {
      return idx
    }
  }
  return count
}
@inline(never)
internal func _findDiffIdx(
  _ leftBuffer: UnsafeBufferPointer<UInt16>,
  _ rightBuffer: UnsafeBufferPointer<UInt16>
) -> Int {
  let leftPtr = leftBuffer.baseAddress._unsafelyUnwrappedUnchecked
  let rightPtr = rightBuffer.baseAddress._unsafelyUnwrappedUnchecked

  let count = Swift.min(leftBuffer.count, rightBuffer.count)
  for idx in 0..<count {
    guard leftPtr[idx] == rightPtr[idx] else {
      return idx
    }
  }
  return count
}

@inline(__always)
internal func _lexicographicalCompare(_ lhs: Int, _ rhs: Int) -> _Ordering {
  // TODO: inspect code quality
  return lhs < rhs ? .less : (lhs > rhs ? .greater : .equal)
}
@inline(__always)
internal func _lexicographicalCompare(
  _ lhs: UInt16, _ rhs: UInt16
) -> _Ordering {
  return lhs < rhs ? .less : (lhs > rhs ? .greater : .equal)
}
// @inline(never) // TODO: temporary, to inspect code quality
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
// @inline(never) // TODO: temporary, to inspect code quality
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
  _ buf: UnsafeBufferPointer<UInt16>,
  startingFrom idx: Int = 0
) -> (UInt32, scalarEndIndex: Int) {
  let ptr = buf.baseAddress._unsafelyUnwrappedUnchecked
  _sanityCheck(idx >= 0 && idx < buf.count, "out of bounds index")
  let cu: UInt16 = ptr[idx]
  if _slowPath(idx+1 == buf.count) {
    return (UInt32(cu), idx+1)
  }
  guard _isLeadingSurrogate(cu) else {
    return (UInt32(cu), idx+1)
  }
  let nextCu: UInt16 = ptr[idx+1]
  guard _isTrailingSurrogate(nextCu) else {
    // Invalid surrogate pair: just return the invalid value
    return (UInt32(cu), idx+1)
  }

  // print("""
  //   leading: \(hex(cu)), trailing: \(hex(nextCu))
  // """)

  // Decode
  let value: UInt32 = _decodeSurrogatePair(leading: cu, trailing: nextCu)
  _sanityCheck(Int32(exactly: value) != nil, "top bit shouldn't be set")
  return (value, idx+2)
}
internal func _reverseParseRawScalar(
  _ buf: UnsafeBufferPointer<UInt16>,
  endingAt idx: Int // one-past-the-end
) -> (UInt32, scalarStartIndex: Int) {
  _sanityCheck(idx > 0 && idx <= buf.count, "out of bounds end index")
  let ptr = buf.baseAddress._unsafelyUnwrappedUnchecked

  // Corner case: leading un-paired surrogate
  if _slowPath(idx == 1) {
    return (UInt32(ptr[0]), 0)
  }

  let cu: UInt16 = ptr[idx-1]
  guard _isTrailingSurrogate(cu) else {
    return (UInt32(cu), idx-1)
  }
  let priorCU: UInt16 = ptr[idx-2]
  guard _isLeadingSurrogate(priorCU) else {
    return (UInt32(cu), idx-1)
  }

  // Decode
  let value: UInt32 = _decodeSurrogatePair(leading: priorCU, trailing: cu)
  _sanityCheck(Int32(exactly: value) != nil, "top bit shouldn't be set")
  return (value, idx-2)
}

internal func _tryNormalize(
  _ input: UnsafeBufferPointer<UInt16>,
  into outputBuffer: UnsafeMutablePointer<_Normalization._SegmentOutputBuffer>
) -> Int? {
  return _tryNormalize(input, into: _castOutputBuffer(outputBuffer))
}
internal func _tryNormalize(
  _ input: UnsafeBufferPointer<UInt16>,
  into outputBuffer: UnsafeMutablePointer<_Normalization._ChunkOutputBuffer>
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
internal func _slowNormalize(
  _ input: UnsafeBufferPointer<UInt16>
) -> [UInt16] {
  _sanityCheck(input.count > 0, "called on empty buffer")

  let canary = input.count * _Normalization._maxNFCExpansionFactor
  var count = input.count
  while true {
    var result = Array<UInt16>(repeating: 0, count: count)
    if let length = result.withUnsafeMutableBufferPointer({ (bufPtr) -> Int? in
      return _tryNormalize(input, into: bufPtr)
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


extension UnicodeScalar {
  internal var _isSegmentExpander: Bool {
    // Fast path: skip unassigned code points
    guard self._isDefined else { return false }

    // Fast path: skip unless QC_FCD=no
    guard self._hasFullCompExclusion else { return false }

    // Try to expand this scalar and see if it now contains multiple segments
    typealias InputBuffer = _FixedArray2<UInt16>
    typealias OutputBuffer = _FixedArray8<UInt16>
    var outBuffer = OutputBuffer(allZeros:())

    var inBuffer = InputBuffer(allZeros:())
    var inLength = 0
    for cu in self.utf16 {
      inBuffer[inLength] = cu
      inLength += 1
    }

    func normalize(
      _ input: UnsafeMutablePointer<InputBuffer>,
      count: Int,
      into output: UnsafeMutablePointer<OutputBuffer>,
      capacity: Int
    ) -> Int {
      let inputBuffer = _unsafeBufferPointerCast(
        input, count, to: UInt16.self
      )
      let outputBuffer = _unsafeMutableBufferPointerCast(
        output, capacity, to: UInt16.self
      )
      return _tryNormalize(
        inputBuffer, into: outputBuffer
      )._unsafelyUnwrappedUnchecked
    }

    func impl(_ buffer: UnsafePointer<OutputBuffer>, count: Int) -> Bool {
      let buffer = _unsafeBufferPointerCast(buffer, count, to: UInt16.self)
      var i = 0
      while i < buffer.count {
        let (value, nextI) = _parseRawScalar(buffer, startingFrom: i)
        if i != 0 
        && UnicodeScalar(_unchecked: value)._hasNormalizationBoundaryBefore {
          return true
        }
        i = nextI
      }
      return false
    }

    let length = normalize(
      &inBuffer, count: inLength, into: &outBuffer, capacity: outBuffer.count)

    return impl(&outBuffer, count: length)
  }
}

// Multi-Segment Expanders - Unicode defines "expanding canonical
// decompositions", where even in NFC a single scalar expands to multiple
// scalars. A small subset (currently 12 scalars circa Unicode 10) of these will
// expand into multiple normalization segments, breaking any kind of segment-by-
// segment logic or processing even under NFC. These are a subset of what is
// identified by the UCD as "composition exclusion" scalars. Since we don't have
// access to a UCD (available only at runtime), we go through ICU which lumps
// those and even more as "Full Composition Exclusions". Of the many full
// composition exclusions, this set (created once at runtime as this can change
// with Unicode version) tracks just those that can expand into multiple
// normalization segments.
private let _multiSegmentExpanders: Set<UInt32> = {
  var result = Set<UInt32>()
  result.reserveCapacity(16)
  let maxScalarValue = 0x0010_FFFF
  for i in 0..<maxScalarValue {
    guard let value = UnicodeScalar(i) else { continue }
    if _slowPath(value._isSegmentExpander) {
      result.insert(value.value)
    }
  }
  return result
}()

extension UnicodeScalar {
  var _isMultiSegmentExpander: Bool {
    return _multiSegmentExpanders.contains(self.value)
  }
}

//
// Find the end of the normalization segment
//
internal func _findNormalizationSegmentEnd(
  _ buf: UnsafeBufferPointer<UInt16>,
  startingFrom idx: Int
) -> Int {
  let count = buf.count
  _sanityCheck(idx < count, "out of bounds")

  // Normalization boundaries are best queried before known starters. Advance
  // past one scalar first.
  var (_, segmentEndIdx) = _parseRawScalar(buf, startingFrom: idx)
  while segmentEndIdx < count {
    let (scalar, nextIdx) = _parseRawScalar(buf, startingFrom: segmentEndIdx)
    segmentEndIdx = nextIdx
    if UnicodeScalar(_unchecked: scalar)._hasNormalizationBoundaryBefore {
      break
    }
  }
  return segmentEndIdx
}
internal func _findNormalizationSegmentStart(
  _ buf: UnsafeBufferPointer<UInt16>,
  endingAt idx: Int // one-past-the-end
) -> Int {
  var idx = idx
  let count = buf.count
  _sanityCheck(idx > 0 && idx <= count, "out of bounds")

  while idx > 0 {
    let (scalar, priorIdx) = _reverseParseRawScalar(buf, endingAt: idx)
    idx = priorIdx
    if UnicodeScalar(_unchecked: scalar)._hasNormalizationBoundaryBefore {
      break
    }
  }
  return idx
}

internal func _findNormalizationSegment(
  _ buf: UnsafeBufferPointer<UInt16>,
  spanning idx: Int
) -> (Int, Int) {
  let ptr = buf.baseAddress._unsafelyUnwrappedUnchecked
  var idx = idx

  // Corner case: if we're sub-surrogate, back up
  if _slowPath(
    idx > 0 && _isTrailingSurrogate(ptr[idx]) && _isLeadingSurrogate(ptr[idx-1])
  ) {
    idx -= 1
  }
  let segmentEnd = _findNormalizationSegmentEnd(buf, startingFrom: idx)

  // Find the start
  if _slowPath(idx == 0) {
    return (0, segmentEnd)
  }

  // Check current scalar
  if UnicodeScalar(
    _unchecked: _parseRawScalar(buf, startingFrom: idx).0
  )._hasNormalizationBoundaryBefore {
    return (idx, segmentEnd)
  }

  // Reverse parse until we found the segment start
  let segmentStart = _findNormalizationSegmentStart(buf, endingAt: idx)

  return (segmentStart, segmentEnd)
}

// For testing purposes only. Not to be used for anything performance sensitive
internal func _naiveNormalize(_ string: String) -> String {
  let cus = Array(string.utf16)
  return cus.withUnsafeBufferPointer { (cuBuffer) -> String in
    return String(decoding: _slowNormalize(cuBuffer), as: UTF16.self)
  }
}
internal func _naiveNormalize(_ codeUnits: Array<UInt16>) -> Array<UInt16> {
  return codeUnits.withUnsafeBufferPointer { (cuBuffer) -> Array<UInt16> in
    return _slowNormalize(cuBuffer)
  }
}

//
// TODO: drop the @inline(never), useful for now to inspect code quality
//
//
// @inline(never)
internal func _isLatinyPrenormal(
  _ buffer: UnsafeBufferPointer<UInt16>, idx: Int
) -> Bool {
  _sanityCheck(idx < buffer.count, "out of bounds")
  let ptr = buffer.baseAddress._unsafelyUnwrappedUnchecked

  let cu = ptr[idx]
  guard cu < 0x300 else { return false }

  if idx+1 == buffer.count {
    return true
  }

  let nextCU = ptr[idx+1]
  return nextCU < 0x300 || _normalizationBoundary(beforeCodeUnit: nextCU)
}

//
// TODO: drop the @inline(never), useful for now to inspect code quality
//
//
// @inline(never)
private func _compareStringsPreLoop(
  selfASCII: UnsafeBufferPointer<UInt8>,
  otherUTF16: UnsafeBufferPointer<UInt16>
) -> _Ordering {
  let selfASCIIPtr = selfASCII.baseAddress._unsafelyUnwrappedUnchecked
  let otherUTF16Ptr = otherUTF16.baseAddress._unsafelyUnwrappedUnchecked
  let count = Swift.min(selfASCII.count, otherUTF16.count)

  //
  // Fast scan until we find a difference
  //
  let idx = _findDiffIdx(selfASCII, otherUTF16)
  guard idx < count else {
    return _lexicographicalCompare(selfASCII.count, otherUTF16.count)
  }
  let otherCU = otherUTF16Ptr[idx]

  //
  // Fast path: if other is non-ASCII, we must be less because:
  //   1) If other hasBoundaryBefore, it is a new super-ASCII segment
  //   2) Otherwise, it might modify prior value, but even then prior value must
  //      be a super-ASCII segment.
  //
  if otherCU > 0x7F {
    return .less
  }

  //
  // Fast path: If otherCU is entire segment, we're done.
  //
  let selfASCIIChar = UInt16(selfASCIIPtr[idx])
  _sanityCheck(selfASCIIChar != otherCU, "should be different")
  if idx+1 == otherUTF16.count {
    return _lexicographicalCompare(selfASCIIChar, otherCU)
  }
  let otherNextCU = otherUTF16Ptr[idx+1]
  if _normalizationBoundary(beforeCodeUnit: otherNextCU) {
    return _lexicographicalCompare(selfASCIIChar, otherCU)
  }

  //
  // Otherwise, need to normalize the segment and then compare
  //
  return _compareStringsPostSuffix(
    selfASCIIChar: selfASCIIChar, otherUTF16: otherUTF16[idx...].rebased)
}

@inline(never)
private func _compareStringsPostSuffix(
  selfASCIIChar: UInt16,
  otherUTF16: UnsafeBufferPointer<UInt16>
) -> _Ordering {
  let otherUTF16Ptr = otherUTF16.baseAddress._unsafelyUnwrappedUnchecked
  let otherCU = otherUTF16Ptr[0]
  _sanityCheck(otherCU <= 0x7F, "should be ASCII, otherwise no need to call")

  let segmentEndIdx = _findNormalizationSegmentEnd(otherUTF16, startingFrom: 0)
  let segment = otherUTF16[..<segmentEndIdx].rebased

  // Fast path: If prenormal, we're done.
  if _Normalization._prenormalQuickCheckYes(segment) {
    return _lexicographicalCompare(selfASCIIChar, otherCU)
  }

  // Normalize segment, and then compare first code unit
  var outputBuffer = _Normalization._SegmentOutputBuffer(allZeros:())
  if _fastPath(
    _tryNormalize(segment, into: &outputBuffer) != nil
  ) {
    return _lexicographicalCompare(selfASCIIChar, outputBuffer[0])
  }
  return _lexicographicalCompare(selfASCIIChar, _slowNormalize(segment)[0])
}

//
// TODO: drop the @inline(never), useful for now to inspect code quality
//
//
// @inline(never)
private func _compareStringsPreLoop(
  selfUTF16: UnsafeBufferPointer<UInt16>,
  otherUTF16: UnsafeBufferPointer<UInt16>
) -> _Ordering {
  let selfUTF16Ptr = selfUTF16.baseAddress._unsafelyUnwrappedUnchecked
  let otherUTF16Ptr = otherUTF16.baseAddress._unsafelyUnwrappedUnchecked
  let count = Swift.min(selfUTF16.count, otherUTF16.count)

  //
  // Fast scan until we find a diff
  //
  let idx = _findDiffIdx(selfUTF16, otherUTF16)
  guard idx < count else {
    return _lexicographicalCompare(selfUTF16.count, otherUTF16.count)
  }
  let selfCU = selfUTF16Ptr[idx]
  let otherCU = otherUTF16Ptr[idx]

  //
  // Fast path: sub-0x300 single-scalar segments can be compared directly
  //
  if _fastPath(
    _isLatinyPrenormal(selfUTF16, idx: idx)
    && _isLatinyPrenormal(otherUTF16, idx: idx)
  ) {
    return _lexicographicalCompare(selfCU, otherCU)
  }

  // print("""
  //   comparing \(selfUTF16.hex) to \(otherUTF16.hex)
  //   """)

  return _compareStringsSuffix(
    selfUTF16: selfUTF16, otherUTF16: otherUTF16, randomIndex: idx)
}

private func _compareStringsSuffix(
  selfUTF16: UnsafeBufferPointer<UInt16>,
  otherUTF16: UnsafeBufferPointer<UInt16>,
  randomIndex: Int
) -> _Ordering {
  let selfUTF16Ptr = selfUTF16.baseAddress._unsafelyUnwrappedUnchecked
  let otherUTF16Ptr = otherUTF16.baseAddress._unsafelyUnwrappedUnchecked
  let count = Swift.min(selfUTF16.count, otherUTF16.count)
  let selfCU = selfUTF16Ptr[randomIndex]
  let otherCU = otherUTF16Ptr[randomIndex]
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
  let (selfSegmentStartIdx, selfSegmentEndIdx) = _findNormalizationSegment(
    selfUTF16, spanning: randomIndex)
  let (otherSegmentStartIdx, otherSegmentEndIdx) = _findNormalizationSegment(
    otherUTF16, spanning: randomIndex)
  let comparisonStartIdx = Swift.min(selfSegmentStartIdx, otherSegmentStartIdx)


  //
  // Fast path: if both are prenormal, we have our answer
  //
  let selfSegment = selfUTF16[comparisonStartIdx..<selfSegmentEndIdx].rebased
  let otherSegment = otherUTF16[comparisonStartIdx..<otherSegmentEndIdx].rebased
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
  if _slowPath(
     UnicodeScalar(
      _unchecked: _parseRawScalar(selfSegment).0)._isMultiSegmentExpander
  || UnicodeScalar(
    _unchecked: _parseRawScalar(otherSegment).0)._isMultiSegmentExpander
  ) {
    return _compareStringsPathological(
      selfUTF16: selfUTF16[comparisonStartIdx...].rebased,
      otherUTF16: otherUTF16[comparisonStartIdx...].rebased)
  }

  //
  // Normalize segments and compare. If they still differ, we have our answer.
  //
  var selfOutputBuffer = _Normalization._SegmentOutputBuffer(allZeros:())
  var otherOutputBuffer = _Normalization._SegmentOutputBuffer(allZeros:())
  let selfSegmentLengthOpt: Int?
  let otherSegmentLengthOpt: Int?
  if selfSegmentPrenormal {
    selfOutputBuffer.fill(selfSegment)
    selfSegmentLengthOpt = selfSegment.count
  } else {
    selfSegmentLengthOpt = _tryNormalize(selfSegment, into: &selfOutputBuffer)
  }
  if otherSegmentPrenormal {
    otherOutputBuffer.fill(otherSegment)
    otherSegmentLengthOpt = otherSegment.count
  } else {
    otherSegmentLengthOpt = _tryNormalize(otherSegment, into: &otherOutputBuffer)
  }
  if _slowPath(selfSegmentLengthOpt == nil || otherSegmentLengthOpt == nil) {
    // If we couldn't normalize a segment into a generously large stack buffer,
    // we have a pathological String.
    return _compareStringsPathological(
      selfUTF16: selfUTF16[comparisonStartIdx...].rebased,
      otherUTF16: otherUTF16[comparisonStartIdx...].rebased)
  }
  let selfLength = selfSegmentLengthOpt._unsafelyUnwrappedUnchecked
  let otherLength = otherSegmentLengthOpt._unsafelyUnwrappedUnchecked

  // print("""
  //   comparing \(selfOutputBuffer.hex)[..<\(selfLength)] \
  //   to \(otherOutputBuffer.hex)[..<\(otherLength)]
  //   """)

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
  return _compareStringsPathological(
    selfUTF16: selfUTF16[selfSegmentEndIdx...].rebased,
    otherUTF16: otherUTF16[otherSegmentEndIdx...].rebased)
}

private func _compareStringsPathological(
  selfUTF16: UnsafeBufferPointer<UInt16>,
  otherUTF16: UnsafeBufferPointer<UInt16>
) -> _Ordering {
  if _Normalization._prenormalQuickCheckYes(selfUTF16) {
    return _compareStringsPathological(
      normalizedSelfUTF16: selfUTF16, otherUTF16: otherUTF16)
  }

  //
  // Normalize self, then proceed. Attempt to do so with a stack buffer first,
  // otherwise use an Array.
  //
  if selfUTF16.count < _Normalization._ChunkOutputBuffer._arraySize {
    var selfBuffer = _Normalization._ChunkOutputBuffer(allZeros:())
    if let length = _tryNormalize(selfUTF16, into: &selfBuffer) {
      return _compareStringsPathological(
        normalizedSelfUTF16: _castOutputBuffer(&selfBuffer, endingAt: length),
        otherUTF16: otherUTF16)
    }
  }
  return _slowNormalize(selfUTF16).withUnsafeBufferPointer {
    (selfBufPtr) -> _Ordering in 
    return _compareStringsPathological(
      normalizedSelfUTF16: selfBufPtr, otherUTF16: otherUTF16)
  }
}

private func _compareStringsPathological(
  normalizedSelfUTF16: UnsafeBufferPointer<UInt16>,
  otherUTF16: UnsafeBufferPointer<UInt16>
) -> _Ordering {
  if _Normalization._prenormalQuickCheckYes(otherUTF16) {
    return _lexicographicalCompare(normalizedSelfUTF16, otherUTF16)
  }

  //
  // Normalize other, then proceed. Attempt to do so with a stack buffer first,
  // otherwise use an Array.
  //
  if otherUTF16.count < _Normalization._ChunkOutputBuffer._arraySize {
    var otherBuffer = _Normalization._ChunkOutputBuffer(allZeros:())
    if let length = _tryNormalize(otherUTF16, into: &otherBuffer) {
      return _lexicographicalCompare(
        normalizedSelfUTF16, _castOutputBuffer(&otherBuffer, endingAt: length))
    }
  }
  return _slowNormalize(otherUTF16).withUnsafeBufferPointer {
    (otherBufPtr) -> _Ordering in
    return _lexicographicalCompare(normalizedSelfUTF16, otherBufPtr)
  }
}

extension String {
  // A super slow reference implementation for correctness checking purposes
  // only
  func _compareStringsReference(_ other: String) -> _Ordering {
    let selfCodeUnits = Array(self.utf16)
    let otherCodeUnits = Array(other.utf16)
    return _lexicographicalCompare(
      _naiveNormalize(selfCodeUnits), _naiveNormalize(otherCodeUnits))
  }

}

extension String {
  @inline(__always)
  func _spanPrenormalLatiny(
    _ codeUnits: UnsafeBufferPointer<UInt16>
  ) -> Int {
    for i in 0..<codeUnits.count {
      guard codeUnits[i] < 0x300 else { return i }
    }
    return codeUnits.count
  }

  var _unsafeASCII : UnsafeBufferPointer<UInt8> {
    _sanityCheck(_core.isASCII && _core._unmanagedASCII != nil)
    return UnsafeBufferPointer(
      start: _core._baseAddress._unsafelyUnwrappedUnchecked.assumingMemoryBound(
        to: UInt8.self),
      count: _core.count
    )
  }
  var _unsafeUTF16 : UnsafeBufferPointer<UInt16> {
    _sanityCheck(!_core.isASCII && _core._unmanagedUTF16 != nil)
    return UnsafeBufferPointer(
      start: _core._baseAddress._unsafelyUnwrappedUnchecked.assumingMemoryBound(
        to: UInt16.self),
      count: _core.count
    )
  }

  @inline(__always)
  func _comparePreLoop(_ other: String) -> _Ordering {
    guard self._core._baseAddress != nil
      && other._core._baseAddress != nil
    else {
      fatalError("TODO: non-contig strings. mostly ascii in practice...")
    }

    // Referential equality means String equality
    if self._core._baseAddress != nil
    && self._core._baseAddress == other._core._baseAddress
    && self._core.count == other._core.count {
      return .equal
    }

    switch (self._core.isASCII, other._core.isASCII) {
    case (true, true):
      return _Ordering(signedNotation: self._compareASCII(other))
    case (true, false):
      return _compareStringsPreLoop(
        selfASCII: self._unsafeASCII,
        otherUTF16: other._unsafeUTF16)
    case (false, true):
      // Same compare, just invert result
      return _compareStringsPreLoop(
        selfASCII: other._unsafeASCII,
        otherUTF16: self._unsafeUTF16
      ).flipped
    case (false, false):
      return _compareStringsPreLoop(
        selfUTF16: self._unsafeUTF16,
        otherUTF16: other._unsafeUTF16)
      // return _compareFast(other)
    }
  }
}

///
/// Tests
///
enum Tests {
  static let exploders: Array<UnicodeScalar> = [
    "\u{0344}", "\u{0958}", "\u{0959}", "\u{095A}", "\u{095B}", "\u{095C}",
    "\u{095D}", "\u{095E}", "\u{095F}", "\u{09DC}", "\u{09DD}", "\u{09DF}",
    "\u{0A33}", "\u{0A36}", "\u{0A59}", "\u{0A5A}", "\u{0A5B}", "\u{0A5E}",
    "\u{0B5C}", "\u{0B5D}", "\u{0F43}", "\u{0F4D}", "\u{0F52}", "\u{0F57}",
    "\u{0F5C}", "\u{0F69}", "\u{0F73}", "\u{0F75}", "\u{0F76}", "\u{0F78}",
    "\u{0F81}", "\u{0F93}", "\u{0F9D}", "\u{0FA2}", "\u{0FA7}", "\u{0FAC}",
    "\u{0FB9}", "\u{2ADC}", "\u{FA6C}", "\u{FACF}", "\u{FAD0}", "\u{FAD1}",
    "\u{FAD5}", "\u{FAD6}", "\u{FAD7}", "\u{FB1D}", "\u{FB1F}", "\u{FB2A}",
    "\u{FB2B}", "\u{FB2E}", "\u{FB2F}", "\u{FB30}", "\u{FB31}", "\u{FB32}",
    "\u{FB33}", "\u{FB34}", "\u{FB35}", "\u{FB36}", "\u{FB38}", "\u{FB39}",
    "\u{FB3A}", "\u{FB3B}", "\u{FB3C}", "\u{FB3E}", "\u{FB40}", "\u{FB41}",
    "\u{FB43}", "\u{FB44}", "\u{FB46}", "\u{FB47}", "\u{FB48}", "\u{FB49}",
    "\u{FB4A}", "\u{FB4B}", "\u{FB4C}", "\u{FB4D}", "\u{FB4E}", "\u{0001D15E}",
    "\u{0001D15F}", "\u{0001D1BB}", "\u{0001D1BC}",
    "\u{FB2C}", "\u{FB2D}", "\u{0001D160}", "\u{0001D161}", "\u{0001D162}",
    "\u{0001D163}", "\u{0001D164}", "\u{0001D1BD}", "\u{0001D1BE}",
    "\u{0001D1BF}", "\u{0001D1C0}",
  ]

  @inline(never)
  static func test() {
    var passed = true
    defer { guard passed else { fatalError("FAIL") } }

    print("[testing]")
    defer { print("[done]") }
    func expect(_ lhs: String, _ rhs: String, _ order: _Ordering) {
      if lhs._comparePreLoop(rhs) != order {
        _sanityCheck(lhs._compareStringsReference(rhs) == order, "bad test")
        print("FAIL: expected \(order) for \(lhs) compared to \(rhs)")
        print("""
          Details:
            lhs: \(lhs.utf16.hex), normalized: \(_naiveNormalize(lhs).utf16.hex)
            rhs: \(rhs.utf16.hex), normalized: \(_naiveNormalize(rhs).utf16.hex)
          """)
        passed = false
      }
    }
    func expectCmp(_ strs: [String], expecting order: _Ordering) {
      assert(strs.count >= 2)
      for i in 0..<strs.count {
        for j in i+1..<strs.count {
          expect(strs[i], strs[j], order)
          expect(strs[j], strs[i], order.flipped)
        }
      }
    }
    func expectEQ(_ strs: String...) {
      expectCmp(strs, expecting: .equal)
    }
    func expectLT(_ strs: String...) {
      expectCmp(strs, expecting: .less)
    }
    func expectGT(_ strs: String...) {
      expectCmp(strs, expecting: .greater)
    }

    expectEQ("", "")
    expectLT("", "Z", "a", "b", "c", "\u{00c5}", "á")

    expectEQ("abcdefg", "abcdefg")
    expectLT("abcdefg", "abcdefgh", "abcdefghi")

    expectEQ("á", "\u{0061}\u{0301}")
    expectLT("à", "\u{0061}\u{0301}", "â", "\u{e3}", "a\u{0308}")

    // Exploding scalars AND exploding segments
    expectEQ("\u{fa2}", "\u{fa1}\u{fb7}")
    expectEQ(
      "\u{fa2}\u{fa2}\u{fa2}\u{fa2}",
      "\u{fa1}\u{fb7}\u{fa1}\u{fb7}\u{fa1}\u{fb7}\u{fa1}\u{fb7}")
    expectEQ(
      "\u{fa2}\u{fa2}\u{fa2}\u{fa2}\u{fa2}\u{fa2}\u{fa2}\u{fa2}",
      "\u{fa1}\u{fb7}\u{fa1}\u{fb7}\u{fa1}\u{fb7}\u{fa1}\u{fb7}\u{fa1}\u{fb7}\u{fa1}\u{fb7}\u{fa1}\u{fb7}\u{fa1}\u{fb7}")
    expectEQ(
      "a\u{fa2}\u{fa2}a\u{fa2}\u{fa2}\u{fa2}\u{fa2}\u{fa2}\u{fa2}",
      "a\u{fa1}\u{fb7}\u{fa1}\u{fb7}a\u{fa1}\u{fb7}\u{fa1}\u{fb7}\u{fa1}\u{fb7}\u{fa1}\u{fb7}\u{fa1}\u{fb7}\u{fa1}\u{fb7}")

    expectEQ("😀", "😀")
    expectEQ("\u{2f9df}", "\u{8f38}")
    expectLT(
      "a",
      "\u{2f9df}", // D87E DDDF as written, but normalizes to 8f38
      "\u{2f9df}\u{2f9df}", // D87E DDDF as written, but normalizes to 8f38
      "👨🏻", // D83D DC68 D83C DFFB
      "👨🏻‍⚕️", // D83D DC68 D83C DFFB 200D 2695 FE0F
      "👩‍⚕️", // D83D DC69 200D 2695 FE0F
      "👩🏾", // D83D DC69 D83C DFFE
      "👩🏾‍⚕", // D83D DC69 D83C DFFE 200D 2695 FE0F
      "😀", // D83D DE00
      "😅", // D83D DE05
      "🧀" // D83E DDC0 -- aka a really big scalar
    )

    expectEQ("\u{f90b}", "\u{5587}")

    expectEQ("\u{212b}", "\u{00c5}")
    expectLT(
      "A",
      "a",
      "aa",
      "ae",
      "ae🧀",
      "az",
      "aze\u{300}",
      "ae\u{301}",
      "ae\u{301}ae\u{301}",
      "ae\u{301}ae\u{301}ae\u{301}",
      "ae\u{301}ae\u{301}ae\u{301}ae\u{301}",
      "ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}",
      "ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}",
      "ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}",
      "ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}",
      "ae\u{302}",
      "ae\u{302}{303}",
      "ae\u{302}🧀",
      "ae\u{303}",
      "\u{f90b}\u{f90c}\u{f90d}", // Normalizes to BMP scalars
      "🧀", // D83E DDC0 -- aka a really big scalar
      "\u{FFEE}" // half width CJK dot
    )
  }
}

Tests.test()

///
/// Benchmarks
///
extension String {
  func lines() -> [String] {
    return self.split(separator: "\n").map { String($0) }
  }
}

struct Workload {
  static let N = 100

  let name: String
  let payload: [String]
  var scaleMultiplier: Double

  init(name: String, payload: [String], scaleMultiplier: Double = 1.0) {
    self.name = name
    self.payload = payload
    self.scaleMultiplier = scaleMultiplier
  }

  var tripCount: Int {
    return Int(Double(Workload.N) * scaleMultiplier)
  }

  static let ascii = Workload(
    name: "ASCII",
    payload: """
      woodshed
      lakism
      gastroperiodynia
      afetal
      Casearia
      ramsch
      Nickieben
      undutifulness
      decorticate
      neognathic
      mentionable
      tetraphenol
      pseudonymal
      dislegitimate
      Discoidea
      criminative
      disintegratory
      executer
      Cylindrosporium
      complimentation
      Ixiama
      Araceae
      silaginoid
      derencephalus
      Lamiidae
      marrowlike
      ninepin
      trihemimer
      semibarbarous
      heresy
      existence
      fretless
      Amiranha
      handgravure
      orthotropic
      Susumu
      teleutospore
      sleazy
      shapeliness
      hepatotomy
      exclusivism
      stifler
      cunning
      isocyanuric
      pseudepigraphy
      carpetbagger
      unglory
      """.lines(),
      scaleMultiplier: 0.25
  )

  static let latin1 = Workload(
    name: "Latin1",
    payload: """
      café
      résumé
      caférésumé
      ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º
      1+1=3
      ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹
      ¡¢£¤¥¦§¨©ª«¬­®
      »¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍ
      ÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãä
      åæçèéêëìíîïðñò
      ÎÏÐÑÒÓÔÕÖëìíîïðñò
      óôõö÷øùúûüýþÿ
      123.456£=>¥
      123.456
      """.lines()
  )
  static let fastPrenormal = Workload(
    name: "FastPrenormal",
    payload: """
      ĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥ
      ĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸ
      ĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲ
      ųŴŵŶŷŸŹźŻżŽžſƀƁƂƃƄƅƆ
      ƇƈƉƊƋƌƍƎƏƐƑƒƓƔƕƖƗƘƙƚƛƜƝƞƟƠơƢƣƤƥƦƧƨƩƪƫƬƭƮƯưƱƲƳƴƵƶƷƸƹƺƻƼƽƾƿǀ
      Ƈ
      ǁǂǃǄǅǆǇǈǉǊǋǌǍǎǏǐǑǒǓǔǕǖ
      ǗǘǙǚǛǜǝǞǟǠǡǢǣǤǥǦǧǨǩǪǫǬǭǮǯǰǱǲǳǴǵǶǷǸǹǺǻǼǽǾǿȀȁȂȃȄȅȆȇȈȉȊȋȌȍȎȏȐȑ
      ȒȓȔȕȖȗȘșȚțȜȝȞȟȠȡȢȣȤȥȦȧȨȩȪȫȬ
      ȒȓȔȕȖȗȘșȚțȜȝȞȟȠȡȢȣȤȥȦȧȨȩȪȫȬǲǳǴǵǶǷǸǹǺǻǼǽǾǿȀȁȂȃȄȅȆȇȈȉȊȋȌȍȎȏȐȑ
      ȭȮȯȰȱȲȳȴȵȶȷȸȹȺȻȼȽȾȿɀɁɂɃɄɅɆɇɈɉɊɋɌɍɎɏɐɑɒɓɔɕɖɗɘəɚɛɜɝɞɟɠɡɢɣɤɥɦɧɨɩɪɫɬɭɮɯɰ
      ɱɲɳɴɵɶɷɸɹɺɻɼɽɾɿʀʁʂʃʄ
      ɱɲɳɴɵɶɷɸɹɺɻɼɽɾɿʀʁʂʃ
      ʅʆʇʈʉʊʋʌʍʎʏʐʑʒʓʔʕʖʗʘʙʚʛʜʝʞʟʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯʰ
      ʱʲʳʴʵʶʷʸʹʺʻʼʽʾʿˀˁ˂˃˄˅ˆˇˈˉˊˋˌˍˎˏːˑ˒˓˔˕˖˗˘˙˚˛˜˝˞˟ˠˡˢˣˤ˥˦
      ˧˨˩˪˫ˬ˭ˮ˯˰˱˲˳˴˵˶˷˸˹˺˻˼˽˾
      """.lines()
  )
  static let slowerPrenormal = Workload(
    name: "SlowerPrenormal",
    payload: """
      Swiftに大幅な改良が施され、
      安定していてしかも
      直感的に使うことができる
      向けプログラミング言語になりました。
      이번 업데이트에서는 강력하면서도
      \u{201c}Hello\u{2010}world\u{2026}\u{201d}
      平台的编程语言
      功能强大且直观易用
      而本次更新对其进行了全面优化
      в чащах юга жил-был цитрус
      \u{300c}\u{300e}今日は\u{3001}世界\u{3002}\u{300f}\u{300d}
      но фальшивый экземпляр
      """.lines()
  )
  // static let slowestPrenormal = """
  //   """.lines()
  static let nonBMPSlowestPrenormal = Workload(
    name: "NonBMPSlowestPrenormal",
    payload: """
      𓀀𓀤𓁓𓁲𓃔𓃗
      𓀀𓀁𓀂𓀃𓀄𓀇𓀈𓀉𓀊𓀋𓀌𓀍𓀎𓀏𓀓𓀔𓀕𓀖𓀗𓀘𓀙𓀚𓀛𓀜𓀞𓀟𓀠𓀡𓀢𓀣
      𓀤𓀥𓀦𓀧𓀨𓀩𓀪𓀫𓀬𓀭
      𓁡𓁢𓁣𓁤𓁥𓁦𓁧𓁨𓁩𓁫𓁬𓁭𓁮𓁯𓁰𓁱𓁲𓁳𓁴𓁵𓁶𓁷𓁸
      𓁹𓁺𓁓𓁔𓁕𓁻𓁼𓁽𓁾𓁿
      𓀀𓀁𓀂𓀃𓀄𓃒𓃓𓃔𓃕𓃻𓃼𓃽𓃾𓃿𓄀𓄁𓄂𓄃𓄄𓄅𓄆𓄇𓄈𓄉𓄊𓄋𓄌𓄍𓄎
      𓂿𓃀𓃁𓃂𓃃𓃄𓃅
      𓃘𓃙𓃚𓃛𓃠𓃡𓃢𓃣𓃦𓃧𓃨𓃩𓃬𓃭𓃮𓃯𓃰𓃲𓃳𓃴𓃵𓃶𓃷𓃸
      𓃘𓃙𓃚𓃛𓃠𓃡𓃢𓃣𓃦𓃧𓃨𓃩𓃬𓃭𓃮𓃯𓃰𓃲𓃳𓃴𓃵𓃶𓃷
      𓀀𓀁𓀂𓀃𓀄𓆇𓆈𓆉𓆊𓆋𓆌𓆍𓆎𓆏𓆐𓆑𓆒𓆓𓆔𓆗𓆘𓆙𓆚𓆛𓆝𓆞𓆟𓆠𓆡𓆢𓆣𓆤
      𓆥𓆦𓆧𓆨𓆩𓆪𓆫𓆬𓆭𓆮𓆯𓆰𓆱𓆲𓆳𓆴𓆵𓆶𓆷𓆸𓆹𓆺𓆻
      """.lines()
  )
  static let emoji = Workload(
    name: "Emoji",
    payload: """
      👍👩‍👩‍👧‍👧👨‍👨‍👦‍👦🇺🇸🇨🇦🇲🇽👍🏻👍🏼👍🏽👍🏾👍🏿
      👍👩‍👩‍👧‍👧👨‍👨‍👦‍👦🇺🇸🇨🇦🇲🇽👍🏿👍🏻👍🏼👍🏽👍🏾
      😀🧀😀😃😄😁🤣😂😅😆
      😺🎃🤖👾😸😹😻😼😾😿🙀😽🙌🙏🤝👍✌🏽
      ☺️😊😇🙂😍😌😉🙃😘😗😙😚😛😝😜
      😋🤑🤗🤓😎😒😏🤠🤡😞😔😟😕😖😣☹️🙁😫😩😤😠😑😐😶😡😯
      😦😧😮😱😳😵😲😨😰😢😥
      😪😓😭🤤😴🙄🤔🤥🤧🤢🤐😬😷🤒🤕😈💩👺👹👿👻💀☠️👽
      """.lines()
  )

  static let abnormal = Workload(
    name: "Abnormal",
    payload: """
    ae\u{301}ae\u{301}ae\u{302}ae\u{303}ae\u{304}ae\u{305}ae\u{306}ae\u{307}
    ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{301}ae\u{300}
    \u{f900}\u{f901}\u{f902}\u{f903}\u{f904}\u{f905}\u{f906}\u{f907}\u{f908}\u{f909}\u{f90a}
    \u{f90b}\u{f90c}\u{f90d}\u{f90e}\u{f90f}\u{f910}\u{f911}\u{f912}\u{f913}\u{f914}\u{f915}\u{f916}\u{f917}\u{f918}\u{f919}
    \u{f900}\u{f91a}\u{f91b}\u{f91c}\u{f91d}\u{f91e}\u{f91f}\u{f920}\u{f921}\u{f922}
    """.lines()
  )
  // static let pathological = """
  //   """.lines()
  // static let zalgo = """
  //   """.lines()
    // ṭ̴̵̶̷̸̢̧̨̡̛̤̥̦̩̪̫̬̭̮̯̰̖̗̘̙̜̝̞̟̠̱̲̳̹̺̻̼͇͈͉͍͎̀́̂̃̄̅̆̇̈̉ͣͤ̊̋̌̍̎̏̐̑̒̓̔̽̾̿̀́͂̓̈́͆͊͋͌̕̚͜͟͢͝͞͠͡ͅ͏͓͔͕͖͙͚͐͑͒͗͛ͣͤͥͦͧͨͩͪͫͬͭͮ͘͜͟͢͝͞͠͡
    // h̀́̂̃
    // è͇͈͉͍͎́̂̃̄̅̆̇̈̉͊͋͌͏̡̢̧̨̛͓͔͕͖͙͚̖̗̘̙̜̝̞̟̠̣̤̥̦̩̪̫̬̭͇͈͉͍͎͐͑͒͗͛̊̋̌̍̎̏̐̑̒̓̔̀́͂̓̈́͆͊͋͌͘̕̚͜͟͝͞͠ͅ͏͓͔͕͖͐͑͒
    // q̴̵̶̷̸̡̢̧̨̛̖̗̘̙̜̝̞̟̠̣̤̥̦̩̪̫̬̭̮̯̰̱̲̳̹̺̻̼͇̀́̂̃̄̅̆̇̈̉̊̋̌̍̎̏̐̑̒̓̔̽̾̿̀́͂̓̈́͆̕̚ͅ
    // ư̴̵̶̷̸̗̘̙̜̹̺̻̼͇͈͉͍͎̽̾̿̀́͂̓̈́͆͊͋͌̚ͅ͏͓͔͕͖͙͚͐͑͒͗͛ͣͤͥͦͧͨͩͪͫͬͭͮ͘͜͟͢͝͞͠͡
    // ì̡̢̧̨̝̞̟̠̣̤̥̦̩̪̫̬̭̮̯̰̹̺̻̼͇͈͉͍͎́̂̃̄̉̊̋̌̍̎̏̐̑̒̓̽̾̿̀́͂̓̈́͆͊͋͌ͅ͏͓͔͕͖͙͐͑͒͗ͬͭͮ͘
    // c̴̵̶̷̸̡̢̧̨̛̖̗̘̙̜̝̞̟̠̣̤̥̦̩̪̫̬̭̮̯̰̱̲̳̹̺̻̼̀́̂̃̄̔̽̾̿̀́͂̓̈́͆ͣͤͥͦͧͨͩͪͫͬͭͮ̕̚͢͡ͅ
    // k̴̵̶̷̸̡̢̧̨̛̖̗̘̙̜̝̞̟̠̣̤̥̦̩̪̫̬̭̮̯̰̱̲̳̹̺̻̼͇͈͉͍͎̀́̂̃̄̅̆̇̈̉̊̋̌̍̎̏̐̑̒̓̔̽̾̿̀́͂̓̈́͆͊͋͌̕̚ͅ͏͓͔͕͖͙͚͐͑͒͗͛ͣͤͥͦͧͨͩͪͫͬͭͮ͘͜͟͢͝͞͠͡
    // b̴̵̶̷̸̡̢̛̗̘̙̜̝̞̟̠̹̺̻̼͇͈͉͍͎̽̾̿̀́͂̓̈́͆͊͋͌̚ͅ͏͓͔͕͖͙͚͐͑͒͗͛ͣͤͥͦͧͨͩͪͫͬͭͮ͘͜͟͢͝͞͠͡
    // ŗ̴̵̶̷̸̨̛̩̪̫̯̰̱̲̳̹̺̻̼̬̭̮͇̗̘̙̜̝̞̟̤̥̦͉͍͎̽̾̿̀́͂̓̈́͆͊͋͌̚ͅ͏̡̢͓͔͕͖͙͚̠̣͐͑͒͗͛ͣͤͥͦͧͨͩͪͫͬͭͮ͘͜͟͢͝͞͠͡
    // o
    // w̗̘͇͈͉͍͎̓̈́͆͊͋͌ͅ͏̛͓͔͕͖͙͚̙̜̹̺̻̼͐͑͒͗͛ͣͤͥͦ̽̾̿̀́͂ͧͨͩͪͫͬͭͮ͘̚͜͟͢͝͞͠͡
    // n͇͈͉͍͎͊͋͌ͧͨͩͪͫͬͭͮ͏̛͓͔͕͖͙͚̗̘̙̜̹̺̻̼͐͑͒͗͛ͣͤͥͦ̽̾̿̀́͂̓̈́͆ͧͨͩͪͫͬͭͮ͘̚͜͟͢͝͞͠͡ͅ
    // f̛̗̘̙̜̹̺̻̼͇͈͉͍͎̽̾̿̀́͂̓̈́͆͊͋͌̚ͅ͏͓͔͕͖͙͚͐͑͒͗͛ͣͤͥͦ͘͜͟͢͝͞͠͡
    // ơ̗̘̙̜̹̺̻̼͇͈͉͍͎̽̾̿̀́͂̓̈́͆͊͋͌̚ͅ͏͓͔͕͖͙͚͐͑͒͗͛ͥͦͧͨͩͪͫͬͭͮ͘
    // xͣͤͥͦͧͨͩͪͫͬͭͮ
    // """.lines()

  static let allScalars: Workload = {
    let maxScalarValue = 0x0010_FFFF
    let allScalars = (0...maxScalarValue).flatMap { UnicodeScalar($0) }
    return Workload(
      name: "All Unicode Scalars",
      payload: [String(allScalars.map { Character($0) })],
      scaleMultiplier: 1.0
    )
  }()

  static var all: [Workload] = {
    return [
      .ascii, .latin1, .fastPrenormal, .slowerPrenormal,
      .nonBMPSlowestPrenormal, .emoji, .abnormal
    ]
  }()

  static func concat(_ workloads: [Workload]) -> Workload {
    return Workload(
      name: "<concatinated>",
      payload: Array(workloads.map { $0.payload }.joined()),
      scaleMultiplier: 1.0)// ??? // 0.1) // ???
  }

  static var allConcat: Workload = { concat(all) }()

  static func crossProduct(_ left: Workload, _ right: Workload) -> Workload {
    let name = left.name + "_X_" + right.name
    var payload: [String] = []
    // TODO: scale differently?
    let multiplier = left.scaleMultiplier * right.scaleMultiplier
    for lhs in left.payload {
      for rhs in right.payload {
        payload.append(lhs + rhs)
      }
    }
    return Workload(name: name, payload: payload, scaleMultiplier: multiplier)
  }

  static var allCrossProduct: [Workload] = {
    var result: [Workload] = []
    for prefix in all {
      for suffix in all {
        if prefix.name == suffix.name {
          continue
        }
        result.append(crossProduct(prefix, suffix))
      }
    }
    return result
  }()
  static var allCrossProductConcat: Workload = { concat(allCrossProduct) }()

  static var allCrossProductCrossProduct: [Workload] = {
    var result: [Workload] = []
    for prefix in allCrossProduct {
      for suffix in allCrossProduct {
        if prefix.name == suffix.name {
          continue
        }
        result.append(crossProduct(prefix, suffix))
      }
    }
    return result
  }()
}

import Dispatch
enum Benchmarks {
  @inline(__always)
  static func time<T>(_ _caller : String = #function, _ block: () -> T) -> T {
    print("\(_caller) execution time: ...", terminator: "")
    let start = DispatchTime.now()
    let res = block()
    let end = DispatchTime.now()
    let milliseconds = (
      end.uptimeNanoseconds - start.uptimeNanoseconds) / 1_000_000
    print("\u{8}\u{8}\u{8}\(milliseconds)(ms)")
    return res
  }

  @inline(never)
  public static func crossComparePreLoop(
    _ workload: Workload
  ) -> Int {
    var count = 0
    let tripCount = workload.tripCount
    let payload = workload.payload
    let name = workload.name + " (new)"
    return Benchmarks.time(name) {
      for _ in 0..<tripCount {
        for s1 in payload {
          for s2 in payload {
            let cmp = s1._comparePreLoop(s2)
            count += cmp == .less ? 1 : 0
          }
        }
      }
      return count
    }
  }

  @inline(never)
  public static func crossCompareOld(
    _ workload: Workload
  ) -> Int {
    var count = 0
    let tripCount = workload.tripCount
    let payload = workload.payload
    let name = workload.name + " (old)"
    return Benchmarks.time(name) {
      for _ in 0..<tripCount {
        for s1 in payload {
          for s2 in payload {
            let cmp = s1 < s2
            count += cmp ? 1 : 0
          }
        }
      }
      return count
    }
  }

  @inline(never)
  public static func benchmark(_ workload: Workload) -> Bool {
    // let c1 = Benchmarks.crossCompare(workload)
    let c2 = Benchmarks.crossCompareOld(workload)
    let c3 = Benchmarks.crossComparePreLoop(workload)
    return c2 == c3
  }

  static func run() {
    print("[benchmarking (N=\(Workload.N))]")
    defer { print( "[done]") }
    var passed = true
    for workload in [Workload.allConcat] {
      var workload = workload
      workload.scaleMultiplier *= 100.0
      // guard workload.name == "Emoji_X_Latin1" else { continue }
      // passed = passed && benchmark(workload)
    }
    for workload in Workload.all {
      var workload = workload
      workload.scaleMultiplier *= 100.0
      // guard workload.name == "Emoji_X_Latin1" else { continue }
      passed = passed && benchmark(workload)
    }
    for workload in Workload.allCrossProduct {
      // guard workload.name == "Emoji_X_Latin1" else { continue }
      passed = passed && benchmark(workload)
    }

    guard passed else { fatalError("benchmark failed") }
  }

  @inline(never)
  static func timeFastLatiny(
    _ workload: Workload
  ) -> Int {
    let name = workload.name
    let payload = workload.payload
    return time(name) {
      var count = 0
      for str in payload {
        for _ in 0..<Workload.N {
          for cu in str._core {
            count += cu < 0x300 ? 1 : 0
          }
        }
      }
      return count
    }
  }

  @inline(never)
  static func timeGraphemeWalk(
    _ workload: Workload
  ) -> Int {
    let name = workload.name + " (grapheme walk)"
    let payload = workload.payload
    return time(name) {
      var count = 0
      for str in payload {
        for _ in 0..<Workload.N {
          for _ in str {
            count += 1
          }
        }
      }
      return count
    }
  }

  @inline(never)
  static func timeSegmentWalk(
    _ workload: Workload
  ) -> Int {
    let name = workload.name + " (segment walk)"
    let payload = workload.payload
    return time(name) {
      var count = 0
      for str in payload {
        for _ in 0..<Workload.N {
          for scalar in str.unicodeScalars {
            if scalar._hasNormalizationBoundaryBefore {
              count += 1
            }
          }
        }
      }
      return count
    }
  }

  @inline(never)
  static func timeSegmentWalk_2(
    _ workload: Workload
  ) -> Int {
    let name = workload.name + " (segment walk)"
    let payload = workload.payload
    return time(name) {
      var count = 0
      for str in payload {
        for _ in 0..<Workload.N {
          for scalar in str.unicodeScalars {
            if scalar._hasNormalizationBoundaryBefore {
              count += 1
            }
          }
        }
      }
      return count
    }
  }


  @inline(never)
  static func timeCompExclusion(
    _ workload: Workload
  ) -> Int {
    let name = workload.name + " (comp exclusion)"
    let payload = workload.payload
    return time(name) {
      var count = 0
      for str in payload {
        for _ in 0..<Workload.N {
          for scalar in str.unicodeScalars {
            if scalar._hasFullCompExclusion {
              count += 1
            }
          }
        }
      }
      return count
    }
  }

  @inline(never)
  static func time_isSegmentExpander(
    _ workload: Workload
  ) -> Int {
    let name = workload.name + " (one of those)"
    let payload = workload.payload
    return time(name) {
      var count = 0
      for str in payload {
        for _ in 0..<Workload.N {
          for scalar in str.unicodeScalars {
            if scalar._isSegmentExpander {
              count += 1
            }
          }
        }
      }
      return count
    }
  }

  @inline(never)
  public static func benchmarkAdHoc(_ workload: Workload) -> Int {
    var count = 0
    count += Benchmarks.timeFastLatiny(workload)
    count += Benchmarks.timeGraphemeWalk(workload)
    count += Benchmarks.timeSegmentWalk(workload)
    count += Benchmarks.timeSegmentWalk_2(workload)
    count += Benchmarks.timeCompExclusion(workload)
    count += Benchmarks.time_isSegmentExpander(workload)
    return count
  }

  static func adHoc() {
    var count = 0
    print("[ad hoc]")
    defer { print( "[done]") }
    _ = benchmarkAdHoc(Workload.allConcat)
    // benchmarkAdHoc(Workload.allScalars)

  }
}

Benchmarks.run()
// Benchmarks.adHoc()


@inline(never)
func benchmark_findTheOnes() -> Set<UInt32> {
  var result = Set<UInt32>()
  result.reserveCapacity(16)
  let maxScalarValue = 0x0010_FFFF
  for _ in 0..<Workload.N {
    for i in 0..<maxScalarValue {
      guard let value = UnicodeScalar(i) else { continue }
      if _slowPath(value._isSegmentExpander) {
        result.insert(value.value)
      }
    }
  }
  return result
}

let theOnes_2 = Benchmarks.time(
  "finding the segment expanders N=\(Workload.N)") {
  () -> Set<UInt32> in
  return benchmark_findTheOnes()
}
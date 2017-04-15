//===--- StringNormalization.swift ----------------------------------------===//
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

//
// A "normalization segment" is a sequence that starts with a unicode scalar
// value which has a normalization boundary before it, and includes all
// subsequent unicode scalar values who do not.
//
// A "native index" is an index into the original String's code units. It can be
// interchanged with many of the other String indices. This is what is
// represented by UnicodeView's "EncodedOffset" Int64.
//

extension UInt16 : _DefaultConstructible {}

// TODO: Figure out a more sensible size
public typealias UTF16CodeUnitBuffer =
  UnboundCapacity<_BoundedCapacity<_FixedArray8<UInt16>>>

//
// Useful thresholds
//

// The first combining code unit
let _firstCombiningCodeUnit: UInt16 = 0x300

// The first code unit that, when the sole member of a normalization segment, is
// non-normal.
let _firstIsolatedNonNormalCodeUnit: UInt16 = 0x340

// The last code unit below the surrogates that, when the sole member of a
// normalization segment, is non-normal.
let _lastIsolatedSubSurrogateNonNormalCodeUnit: UInt16 = 0x2ADC

// The first surrogate code unit
let _firstSurrogateCodeUnit: UInt16 = 0xD800

// A normalization segment that is FCC-normalized. This is a collection of
// normalized UTF16 code units.
//
// Note that this is not a UnicodeView. Indices into a normalized segment are
// not native indices, and do not necessarily correspond to any particular code
// unit as they may of undergone composition or decomposition, which in turn may
// also re-order code units. Thus, FCCNormalizedSegments are not suitable for
// queries needing sub-segment granularity. However, FCCNormalizedSegments are
// suitable for segment-level-or-coarser granularity queries, which include any
// grapheme-level queries as segments are sub- grapheme.
//
// TODO: verify above statement about segments being sub-grapheme, that is make
// sure it is not possible to have segments span graphemes.
//
// TODO: Explore coalescing small segments together
public struct FCCNormalizedSegment : BidirectionalCollection {
  let buffer: UTF16CodeUnitBuffer

  // The corresponding native begin/end indices for this segment
  let nativeStart: Int
  let nativeEnd: Int

  public init(_ buffer: UTF16CodeUnitBuffer, nativeStart: Int, nativeEnd: Int) {
    self.buffer = buffer
    self.nativeStart = nativeStart
    self.nativeEnd = nativeEnd
  }

  // Init for making empty/sentinel segments
  public init(sentinelValue value: Int) {
    self.buffer = UTF16CodeUnitBuffer()
    self.nativeStart = value
    self.nativeEnd = value
  }

  public typealias Index = Int // UTF16CodeUnitBuffer.Index

  public var startIndex : Index {
    return buffer.startIndex
  }
  public var endIndex : Index {
    return buffer.endIndex
  }
  public subscript(i: Index) -> UInt16 {
    return buffer[i]
  }
  public func index(after i: Index) -> Index {
    return buffer.index(after: i)
  }
  public func index(before i: Index) -> Index {
    return buffer.index(before: i)
  }

  public typealias SubSequence = UnicodeViewSlice<FCCNormalizedSegment>
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }
}

extension FCCNormalizedSegment {
  // Perform normalization on the buffer
  public init(
    normalizing buffer: UTF16CodeUnitBuffer, nativeStart: Int, nativeEnd: Int
  ) {
    // Fast pre-normalized checks (worth doing before calling over to ICU)
    if _fastPath(FCCNormalizedSegment.isPreNormalized(buffer)) {
      self.init(buffer, nativeStart: nativeStart, nativeEnd: nativeEnd)
      return
    }
    let normalizedBuffer = FCCNormalizedSegment.normalized(buffer)
    self.init(normalizedBuffer, nativeStart: nativeStart, nativeEnd: nativeEnd)
  }

  internal static func normalized(
    _ buffer: UTF16CodeUnitBuffer
  ) -> UTF16CodeUnitBuffer {
    _sanityCheck(buffer.count > 0, "How did this happen? Failed precondition?")

    // Ask ICU to normalize
    //
    // FIXME: withMutableArray kind of defeats the purpose of the small
    // buffer :-(
    var buffer = buffer
    buffer.withMutableArray { (array: inout [UInt16]) -> () in
      array.withUnsafeBufferPointer {
        // TODO: Just reserving one or two extra up front. If we're segment-
        // based, should be finite number of possible decomps.
        let originalCount = buffer.count
        while true {
          var error = __swift_stdlib_U_ZERO_ERROR
          let usedCount = __swift_stdlib_unorm2_normalize(
            // FIXME: check valid force-unwrap
            _fccNormalizer, $0.baseAddress!, numericCast($0.count),
            &array, numericCast(array.count), &error)
          if __swift_stdlib_U_SUCCESS(error) {
            array.removeLast(array.count - numericCast(usedCount))
            return
          }
          _sanityCheck(
            error == __swift_stdlib_U_BUFFER_OVERFLOW_ERROR,
            "Unknown normalization error")

          // Maximum number of NFC to FCC decompositions for a single unicode
          // scalar value
          //
          // TODO: what is this really? Should be much less
          let maxDecompSize = 8

          // Very loose canary to check that we haven't grown exceedingly large
          // (indicative of logic error). Loose by assuming that every original
          // character could be decomposed the maximum number of times. Without
          // this, an error would loop until we run out of memory or the array
          // is larger than 2^32 on 64bit platforms.
          _sanityCheck(buffer.count < originalCount*maxDecompSize)

          // extend array storage by 25%
          array.append(
            contentsOf: repeatElement(0, count: (array.count + 3) >> 2))
        }
      }
    }

    return buffer
  }

  static func isPreNormalized(_ buffer: UTF16CodeUnitBuffer) -> Bool {
    if _fastPath(buffer.count == 0) {
      return true
    }
    if _fastPath(buffer.count == 1) {
      return FCCNormalizedSegment.isPreNormalized(
        isolatedUTF16CodeUnit: buffer[0])
    }

    // TODO: There are much better pre-normalized checks we can do than just
    // checking that all CUs are independently pre-normalized.

    // As a quick check, see if we're just a coalesced buffer of pre-normalized
    // scalars.
    for cu in buffer {
      // TODO: Other checks
      guard _fastPath(cu < _firstCombiningCodeUnit) else {
        return false
      }
    }
    return true
  }

  // A pre-normalized check for single-code-unit normalization segment
  static func isPreNormalized(isolatedUTF16CodeUnit cu: UInt16) -> Bool {
    // For single-code-unit normalization segments, the vast majority of the
    // sub-surrogate BMP is already FCC normalized (~70 exceptions). Perform
    // some quick checks over the largest ranges.

    // The earliest exception is _firstIsolatedNonNormalCodeUnit,
    if _fastPath(cu < _firstIsolatedNonNormalCodeUnit) {
      return true
    }

    // The last exception, before the surrogate code units, is 0x2ADC, the
    // FORKING supplemental mathematical operator.
    if _fastPath(
      cu > _lastIsolatedSubSurrogateNonNormalCodeUnit
      && cu < _firstSurrogateCodeUnit) {
      return true
    }

    // TODO: Worth checking the more narrow ranges?
    return false
  }
}

extension FCCNormalizedSegment : UnicodeView {
  public func nativeIndex(_ i: AnyUnicodeIndex) -> Index {
    if (i.encodedOffset != nativeStart) {
      // Can't translate indices
      //
      // TODO: Should this trigger an error?
      return endIndex
    }

    if case .fccNormalizedUTF16(_, let offsetInSegment) = i {
      return offsetInSegment
    }
    return startIndex
  }

  public func anyIndex(_ i: Index) -> AnyUnicodeIndex {
    return .fccNormalizedUTF16(
      offsetOfSegment: numericCast(nativeStart),
      offsetInSegment: i)
  }
}

// Ask ICU if the given unicode scalar value has a normalization boundary before
// it, that is it begins a new normalization segment.
public func _hasBoundary(before value: UInt32) -> Bool {
  return __swift_stdlib_unorm2_hasBoundaryBefore(_fccNormalizer, value) != 0
}

// TODO: explore using hasBoundary(after:), and whether that will identify
// finer-grained segments.

public struct FCCNormalizedLazySegments<
  CodeUnits : BidirectionalCollection,
  FromEncoding : UnicodeEncoding
>
where
  CodeUnits.Index == CodeUnits.SubSequence.Index,
  CodeUnits.SubSequence : BidirectionalCollection,
  CodeUnits.SubSequence == CodeUnits.SubSequence.SubSequence,
  CodeUnits.Iterator.Element == CodeUnits.SubSequence.Iterator.Element,
  CodeUnits.Iterator.Element == FromEncoding.EncodedScalar.Iterator.Element
{
  let codeUnits: CodeUnits

  public init(
    _ codeUnits: CodeUnits,
    _: FromEncoding.Type = FromEncoding.self
  ) {
    self.codeUnits = codeUnits
  }

  public init(_ unicodeView: _UnicodeViews<CodeUnits, FromEncoding>) {
    self.init(unicodeView.codeUnits)
  }

  // Find the segment that terminates at `endingAt`. Optimized for backwards
  // traversal.
  internal func priorSegment(
    endingAt end: CodeUnits.Index
  ) -> Index {
    precondition(end != codeUnits.startIndex)

    // Decode scalars backwards, including them until we after we find one that
    // has a boundary before it (and include that one).
    var start = end
    while start != codeUnits.startIndex {
      // Include this scalar
      let (scalarValue: value, startIndex: scalarStart, e)
        = decodeOne(priorTo: start)
      _sanityCheck(e == start, "Internal inconsistency in decodeOne")

      // Include this scalar
      start = scalarStart

      if _hasBoundary(before: value) {
        // We're done
        break
      }
    }

    return Index(formSegment(from: start, until: end))
  }

  // Find the segment that starts with `startingAt`. Optimized for forwards
  // traversal.
  internal func nextSegment(
    startingAt start: CodeUnits.Index
  ) -> Index {
    if start == codeUnits.endIndex {
      return endIndex
    }

    // Parse the first scalar, it will always be in the segment
    var (scalarValue: value, startIndex: s, endIndex: end)
      = decodeOne(from: start)
    _sanityCheck(start == codeUnits.startIndex ||
                 _hasBoundary(before: value), "Not on segment boundary")
    _sanityCheck(s == start, "Internal inconsistency in decodeOne")

    // Include any subsequent scalars that don't have boundaries before them
    while end != codeUnits.endIndex {
      let (scalarValue: value, startIndex: s, endIndex: scalarEnd)
        = decodeOne(from: end)
      _sanityCheck(s == end, "Internal inconsistency in decodeOne")

      if _hasBoundary(before: value) {
        // Exclude this scalar
        break
      }

      // Include this scalar
      end = scalarEnd
    }

    return Index(formSegment(from: start, until: end))
  }

  internal func castCUIndex(_ idx: CodeUnits.Index) -> Int {
    return numericCast(codeUnits.distance(from: codeUnits.startIndex, to: idx))
  }
  internal func formCUIndex(_ offset: Int) -> CodeUnits.Index {
    return codeUnits.index(atOffset: offset)
  }

  // Normalize a segment. Indices must be on scalar boundaries.
  internal func formSegment(
    from start: CodeUnits.Index,
    until end: CodeUnits.Index
  ) -> FCCNormalizedSegment {
    precondition(start != end, "TODO: should we just have empty segment?")

    let utf16CodeUnits = unicodeView(
      from: start, until: end
    ).scalarsTranscoded(
      to: UTF16.self
    )

    // TODO: Find way to re-use the storage, maybe iterator pattern?
    var buffer = UTF16CodeUnitBuffer(utf16CodeUnits.lazy.joined())

    // Fast pre-normalized checks (worth doing before calling over to ICU)
    if _fastPath(FCCNormalizedSegment.isPreNormalized(buffer)) {
      return FCCNormalizedSegment(
        buffer, nativeStart: castCUIndex(start), nativeEnd: castCUIndex(end))
    }

    _sanityCheck(buffer.count > 0, "How did this happen? Failed precondition?")

    // Ask ICU to normalize
    //
    // FIXME: withMutableArray kind of defeats the purpose of the small
    // buffer :-(
    buffer.withMutableArray { (array: inout [UInt16]) -> () in
      array.withUnsafeBufferPointer {
        // TODO: Just reserving one or two extra up front. If we're segment-
        // based, should be finite number of possible decomps.
        let originalCount = buffer.count
        while true {
          var error = __swift_stdlib_U_ZERO_ERROR
          let usedCount = __swift_stdlib_unorm2_normalize(
            // FIXME: check valid force-unwrap
            _fccNormalizer, $0.baseAddress!, numericCast($0.count),
            &array, numericCast(array.count), &error)
          if __swift_stdlib_U_SUCCESS(error) {
            array.removeLast(array.count - numericCast(usedCount))
            return
          }
          _sanityCheck(
            error == __swift_stdlib_U_BUFFER_OVERFLOW_ERROR,
            "Unknown normalization error")

          // Maximum number of NFC to FCC decompositions for a single unicode
          // scalar value
          //
          // TODO: what is this really? Should be much less
          let maxDecompSize = 8

          // Very loose canary to check that we haven't grown exceedingly large
          // (indicative of logic error). Loose by assuming that every original
          // character could be decomposed the maximum number of times. Without
          // this, an error would loop until we run out of memory or the array
          // is larger than 2^32 on 64bit platforms.
          _sanityCheck(buffer.count < originalCount*maxDecompSize)

          // extend array storage by 25%
          array.append(
            contentsOf: repeatElement(0, count: (array.count + 3) >> 2))
        }
      }
    }

    return FCCNormalizedSegment(
      buffer, nativeStart: castCUIndex(start), nativeEnd: castCUIndex(end))
  }

  // Decode one or more code units, returning the unicode scalar value and the
  // indices spanning the code units parsed. `from` should be on scalar boundary
  internal func decodeOne(from start: CodeUnits.Index)
    -> (scalarValue: UInt32,
        startIndex: CodeUnits.Index,
        endIndex: CodeUnits.Index) {
    precondition(start != codeUnits.endIndex, "Given empty slice")

    let encodedScalar = unicodeView(from: start).encodedScalars.first!
    return (scalarValue: encodedScalar.utf32[0],
            startIndex: start,
            endIndex: codeUnits.index(start, offsetBy: numericCast(encodedScalar.count)))
  }

  // As decodeOne(from:), but in reverse. `priorTo` is the index after the last
  // code unit in the scalar, i.e. it is exclusive.
  internal func decodeOne(priorTo end: CodeUnits.Index)
    -> (scalarValue: UInt32,
        startIndex: CodeUnits.Index,
        endIndex: CodeUnits.Index) {
    precondition(end != codeUnits.startIndex, "Given empty slice")

    let encodedScalar = unicodeView(until: end).encodedScalars.last!
    return (scalarValue: encodedScalar.utf32[0],
            startIndex: codeUnits.index(end, offsetBy: -numericCast(encodedScalar.count)),
            endIndex: end)
  }

  // Get the rest of the Unicode view
  internal func unicodeView(
    from start: CodeUnits.Index? = nil,
    until end: CodeUnits.Index? = nil
  ) -> _UnicodeViews<CodeUnits.SubSequence, FromEncoding> {
    let end = end ?? codeUnits.endIndex
    let start = start ?? codeUnits.startIndex
    return _UnicodeViews(codeUnits[start..<end], FromEncoding.self)
  }
}

extension FCCNormalizedLazySegments : BidirectionalCollection {
  // TODO?: This is really more like an iterator...
  public struct Index : Comparable {
    let segment: FCCNormalizedSegment

    public static func <(lhs: Index, rhs: Index) -> Bool {
      if lhs.segment.nativeStart < rhs.segment.nativeStart {
        // Our ends should be ordered similarly, unless lhs is the last index
        // before endIndex and rhs is the endIndex.
        _sanityCheck(
          lhs.segment.nativeEnd < rhs.segment.nativeEnd ||
          rhs.segment.nativeStart == rhs.segment.nativeEnd,
          "overlapping segments?")

        return true
      }

      return false
    }

    public static func ==(lhs: Index, rhs: Index) -> Bool {
      if lhs.segment.nativeStart == rhs.segment.nativeStart {
        _sanityCheck(
          lhs.segment.nativeEnd == rhs.segment.nativeEnd,
          "overlapping segments?")
        return true
      }

      return false
    }

    public init(_ segment: FCCNormalizedSegment) {
      self.segment = segment
    }
  }

  // TODO: formIndex(after:) that re-uses storage

  public var startIndex: Index {
    return nextSegment(startingAt: codeUnits.startIndex)
  }
  public var endIndex: Index {
    return Index(
      FCCNormalizedSegment(sentinelValue: numericCast(codeUnits.count)))
  }

  public func index(after idx: Index) -> Index {
    return nextSegment(
      startingAt: formCUIndex(idx.segment.nativeEnd)
    )
  }
  public func index(before idx: Index) -> Index {
    return priorSegment(
      endingAt: formCUIndex(idx.segment.nativeStart)
    )
  }
  public subscript(position: Index) -> FCCNormalizedSegment {
    return position.segment
  }

  public typealias SubSequence = BidirectionalSlice<FCCNormalizedLazySegments>
}

extension _UnicodeViews {
  public struct FCCNormalizedUTF16View: BidirectionalCollectionWrapper {
    public typealias Base = FlattenBidirectionalCollection<
      FCCNormalizedLazySegments<CodeUnits, Encoding>
    >
    public var base: Base
    public typealias Index = Base.Index
    public typealias IndexDistance = Base.IndexDistance
    public typealias Self_ = FCCNormalizedUTF16View

    public init(_ unicodeView: _UnicodeViews<CodeUnits, Encoding>) {
      self.base = Base(FCCNormalizedLazySegments(unicodeView))
    }
  }

  public var fccNormalizedUTF16: FCCNormalizedUTF16View {
    return FCCNormalizedUTF16View(self)
  }
}

extension _UnicodeViews.FCCNormalizedUTF16View : UnicodeView {
  public func nativeIndex(_ i: AnyUnicodeIndex) -> Index {
    let segmentIdx = base._base.nextSegment(
      startingAt: base._base.codeUnits.index(
        base._base.codeUnits.startIndex,
        offsetBy: numericCast(i.encodedOffset)
      )
    )
    if _slowPath(segmentIdx == base._base.endIndex) {
      return endIndex
    }
    if case .fccNormalizedUTF16(_, let offsetInSegment) = i {
      return Index(segmentIdx, offsetInSegment)
    }
    return Index(segmentIdx, segmentIdx.segment.startIndex)
  }

  public func anyIndex(_ i: Index) -> AnyUnicodeIndex {
    return .fccNormalizedUTF16(
      offsetOfSegment: numericCast(i._outer.segment.nativeStart),
      offsetInSegment: i._inner ?? 0)
  }

  public typealias SubSequence = UnicodeViewSlice<Self_>
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }
}

internal func _makeFCCNormalizer() -> OpaquePointer {
  var err = __swift_stdlib_U_ZERO_ERROR;
  let ret = __swift_stdlib_unorm2_getInstance(
    nil, "nfc", __swift_stdlib_UNORM2_COMPOSE_CONTIGUOUS, &err)
  _precondition(err.isSuccess, "unexpected unorm2_getInstance failure")
  return ret!
}

// Michael NOTE: made public for prototype, should be internal
public var _fccNormalizer = _makeFCCNormalizer()


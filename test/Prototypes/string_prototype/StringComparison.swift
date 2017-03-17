// Prototype code for an eventual StringComparison.swift

import Swift
import SwiftShims

// TODO: put these on protocol decl, and have impls provide functionality
// instead
//
// The below establishes the concept of a *trivially-decodable* code unit.A
// trivially-decodable code unit can be decoded into its unicode scalar merely
// by zero-extending the value. It represents an entire unicode scalar all on
// its own. Trivially decodable code units can be more efficiently compared.
extension UnicodeEncoding
where
  EncodedScalar.Iterator.Element : UnsignedInteger
{
  // The trivial-decodable encodings: An encoding is trivially-decodable if
  // all possible code units expressed in that encoding are trivially-
  // decodable
  //
  // TODO: all code units, or all valid code units?
  static var isTriviallyDecodableEncoding: Bool {
    if (Self.self == Latin1.self) {
      return true
    }
    if (Self.self == UTF32.self) {
      // TODO: valid only? what about surrogate scalars (which are invalid)?
      //
      // For reference, Unicode 9.0,S3.9,D76 defined "Unicode scalar value" as
      // excluding surrogates. i.e.:
      //  ranges 0 to D7FF16 and E00016 to 10FFFF16, inclusive.

      return true
    }

    // TODO: others? valid variants?

    return false
  }

  // Whether the provided code unit is trivially decodable in this encoding.
  static func isTriviallyDecodable(_ codeUnit: CodeUnit) -> Bool {
    if isTriviallyDecodableEncoding {
      return true
    }

    // The encodings with thresholds: All code units below a certain threshold
    // are trivial
    if (Self.self == UTF8.self) {
      // Only ASCII
      return codeUnit < 0x80
    }
    if (Self.self == UTF16.self) {
      // All BMP scalars, that is anything not in a surrogate range.
      return codeUnit < 0xD800 || codeUnit > 0xDFFF
    }

    fatalError("unimplemented")
  }
}

// TODO: put these on protocol decl, and have impls provide functionality
// instead
//
// The below establishes the concept of a *starter code unit*, which is any code
// unit that begins a unicode scalar value whose canonical combining class in
// the UCD is zero. Such code units start a new sequence for the purposes of
// normalization and thus also serve as natural canonical ordering breaks.
extension UnicodeEncoding
where
  EncodedScalar.Iterator.Element : UnsignedInteger
{
  // Whether the given code unit is a starter code unit.
  // This function is meant as a quick check, involving very few operations, to
  // check for large and common ranges of starter code units.
  //
  // TODO: this will likely be a threshold value instead, for a simd fast path,
  // as well as a more exhaustive check for the scalar fast path.
  static func quickCheckStarter(
    _ codeUnit: CodeUnit
  ) -> Bool {
    // Trivial
    if (Self.self == Latin1.self) {
      return true
    }

    if (Self.self == UTF8.self) {
      // FIXME: instead, find the ranges of the leading byte that must fall
      // within our ranges. This might be subject to known-validity

      if _fastPath(codeUnit < 0x7f) {
        return true
      }

      // UTF8 preserves ordering of encoded unicode scalar values. If this code
      // unit begins a new sequence, return whether that sequence can only
      // represent normalization-sequence-starters.

      // TODO: check top two bits, then return codeUnit < 0xcc for our 0x300
      // threshold.
      return false
    }

    assert(Self.self == UTF16.self || Self.self == UTF32.self)

    // TODO: Determine strategy for unallocated or reserved code points. There
    // are very large ranges of common characters if we are allowed to treat
    // reserved and/or unallocated code points as being normalization-sequence-
    // starters: e.g. ~30k CJK characters, ~20k Middle Eastern and African, etc.

    return codeUnit < 0x300
  }
}

//
// We are still deciding on the most desirable normal form, but it might end up
// being NFC or maybe FCC. For now, I'm calling this "TODO-normal-form" aka
// "NFT" for normal form T[ODO]. When we decide, we replace all NFT with real
// normal form. Correctness assumptions are conservavite, which is assume some
// kind of composed-like form. We could hit more fast-paths more often if we
// settled on a decomposed form instead.
//

//
// Proposed sort order semantics for String in Swift 4: Lexicographic order of a
// string's normalized unicode scalar values.
//

// Mocked-up extra bits of info we might want from Unicode/_UnicodeViews
extension Unicode {
  // Whether the string only contains unicode scalar values in the Basic
  // Multilingual Plane (BMP). This is useful as it ensures trivial-decodability
  // for UTF16 as well as making code-unit-wise ordering reflect scalar value
  // ordering for normalized UTF16.
  func isBMP(scan: Bool = true) -> Bool {
    return false
  }
}


//
// Implementation-side, boils down to 4 general levels of comparision. These are
// presented from fastest (and most restricively applicable) to slowest (and
// most generally applicable), and thus in the order of preference if possible.
//
//  1) bitwise comparison: compare bits; roughly equivalent to C's memcmp.
//
//  2) zero-extend and compare: zero extend the smaller code unit and then
//  compare bits. Considered distinct for now from bitwise as it may differ
//  in simd vectorization capability.
//
//  3) scalar value comparison: decode into unicode scalar values, and compare
//  those. This is a decode-and-then-bit-compare. Implemented through a
//  transcoded view targeting (valid?) UTF32.
//
//  4) normalized scalar comparison: compare sequence of normalized scalar
//  values. Currently implemented as grapheme-based comparison (requiring
//  grapheme break logic which currently uses libICU), but that does more work
//  than strictly necessary. Need to make a normalized view, then we can just
//
//  Note on #4: Currently, this ends up shuttling through Character, and no
//  normalization is actually ever performed even for the scalar view. Thus
//  comparison is broken as it will have to use Swift 3 ordering semantics for
//  Character, which are a bastardization of UCA and ASCII-code-order.
//

//
// When they apply: please refer to draft spreadsheet with the decision matrix...
//

// Fast paths
//
// #1 needs no fast paths, it *is* the fastest path.
//
// #2 needs no fast paths. If #1 isn't possible, then #2 *is* the fastest path.
//
// #3 can do bitwise/zero-extension compare whenever both compared code units
//    are trivially-decodable.
//
// #4 can do bitwise/zero-extension compare whenever both compared code units
// are trivially-decodable AND the following code units are definitely
// starter code units.
//

//
// Sort order is preserved with consistent use of indices. For example:
//
//   1.  ă ́ ̠  (U+0103 U+0301 U+0320)
//   2.  ă ́ b  (U+0103 U+0301 U+0062)
//   3.  ă ́ 日 (U+0103 U+0301 U+65E5)
//
// These will be ordered #2, #1, #3. This is because the combining character
// U+0320 comes after 'b'. This seems undesirable at first, but it affords
// memcmp semantics and still preserves consistent ordering at grapheme,
// canonical-combining-sequence, and even scalar boundaries.
//
// That is, if substrings up to a particular index compare equivalently, then
// the sort order of the rest of the strings is the same sort order of the
// entire strings.
//
// This does expose a chance of user error if the user determines the scalar
// index of divergence, then tries to advance grapheme indices to that point to
// resume comparison. In that case, the graphme index may advance past the point
// of divergence if divergence occurrs within a grapheme. This would be a misuse
// of our indices and comparison semantics, and I can't think of a good way to
// address it. Note that it would also apply even at canonical-combining-
// sequence boundaries that were not also grapheme boundaries.
//
// TODO: given that lexicographic ordering is monoidal, figure out the fancy
// term for this property.
//


extension _UnicodeViews {
  // TODO: a properties struct formed by a single pass instead of the below.

  func isASCII(scan: Bool = true) -> Bool {
    // TODO: is US the right place for this?
    // TODO: do
    return false
  }
  func isLatin1(scan: Bool = true) -> Bool {
    // TODO: is US the right place for this?
    // TODO: do
    return false
  }
  func isBMP(scan: Bool = true) -> Bool {
    // TODO: is US the right place for this?
    // TODO: do
    return false
  }
  // Normal Form TODO, whatever normal form we decide on
  func isNFT(scan: Bool = true) -> Bool {
    // TODO: is US the right place for this?
    // TODO: do
    return false
  }
}

// TODO: replace with eventual comparison order
enum SortOrder { case before, same, after }

enum StringComparisonStrategy {
  case bits
  // TODO: worth refactoring to have associated values?
  case bitsWithSurrogatesGreater
  case zextBits
  case zextBitsWithSurrogatesGreater
  case unicodeScalarValues
  case normalizeThenCompareUnicodeScalarValues
}

func _determineComparisonStrategy<
  LHSCodeUnits: RandomAccessCollection,
  LHSEncoding: UnicodeEncoding,
  RHSCodeUnits: RandomAccessCollection,
  RHSEncoding: UnicodeEncoding
>(
  _ lhs: _UnicodeViews<LHSCodeUnits, LHSEncoding>,
  _ rhs: _UnicodeViews<RHSCodeUnits, RHSEncoding>
) -> StringComparisonStrategy
where
  LHSEncoding.CodeUnit: UnsignedInteger,
  RHSEncoding.CodeUnit: UnsignedInteger
  // LHSEncoding.CodeUnit: FixedWidthInteger,
  // RHSEncoding.CodeUnit: FixedWidthInteger
{
  // Without normalization, all bets are off
  guard lhs.isNFT() && rhs.isNFT() else {
    return .normalizeThenCompareUnicodeScalarValues
  }

  // Same encodings usually mean bitwise comparison except for UTF16 outside of
  // the BMP, as surrogate code points do not occur at the end of the BMP.
  if _fastPath(LHSEncoding.self == RHSEncoding.self) {
    if LHSEncoding.self == UTF16.self {
      if _fastPath(lhs.isBMP() && rhs.isBMP()) {
        return .bits
      }
      // So long as one is BMP, we can use a trick: bitwise using a surrogate
      // filter: when the other has a surrogate bitpattern, that CU is greater.
      if lhs.isBMP() || rhs.isBMP() {
        return .bitsWithSurrogatesGreater
      }

      return .unicodeScalarValues
    }

    return .bits
  }

  // Choose between bitwise and zextwise based on size of code units
  let bitwiseOrZextwise: () -> StringComparisonStrategy = {
    // FIXME: can't use bitWidth? UInt16 is not FixedWidthInteger???
    let sameSize = MemoryLayout<LHSEncoding.CodeUnit>.size
      == MemoryLayout<RHSEncoding.CodeUnit>.size
    return sameSize ? .bits : .zextBits
  }

  // ASCII is common subset of all encodings
  if lhs.isASCII() || rhs.isASCII() {
    return bitwiseOrZextwise()
  }

  // All other UTF8 combinations require decoding
  if LHSEncoding.self == UTF8.self || RHSEncoding.self == UTF8.self {
    return .unicodeScalarValues
  }

  // Latin1 is a common subset for everything but UTF8
  if lhs.isLatin1() || rhs.isLatin1() {
    return bitwiseOrZextwise()
  }

  // Final remaining options: UTF16 compared to UTF32, normalized
  assert(
    LHSEncoding.self == UTF16.self && RHSEncoding.self == UTF32.self
    || LHSEncoding.self == UTF32.self && RHSEncoding.self == UTF16.self
  )

  // BMP is common subset of UTF16 and UTF32
  if _fastPath(lhs.isBMP() || rhs.isBMP()) {
    if _fastPath(lhs.isBMP() && rhs.isBMP()) {
      return .zextBits
    }
    // Otherwise, we need to be careful about surrogates
    return .zextBitsWithSurrogatesGreater
  }

  // Otherwise, requires decoding
  return .unicodeScalarValues
}


enum PartialFastCompare<
  LHSIndex,
  RHSIndex
>
{
  case result(SortOrder)
  case moreProcessingRequired(LHSIndex, RHSIndex)
}

// TODO: this shouldn't be one monolithic function, but instead be specialized
// into the two cases where it's most applicable: pre-normalized-
// unicodeScalarValues compare and normalized-unicodeScalarValues compare
func _partialFastCompare<
  LHSCodeUnits: RandomAccessCollection,
  LHSEncoding: UnicodeEncoding,
  RHSCodeUnits: RandomAccessCollection,
  RHSEncoding: UnicodeEncoding
>
(
  _ lhs: _UnicodeViews<LHSCodeUnits, LHSEncoding>,
  _ rhs: _UnicodeViews<RHSCodeUnits, RHSEncoding>,
  preNormalized: Bool
) -> PartialFastCompare<LHSCodeUnits.Index, RHSCodeUnits.Index>
where
  LHSCodeUnits.Iterator.Element : UnsignedInteger,
  RHSCodeUnits.Iterator.Element : UnsignedInteger,
  LHSCodeUnits.SubSequence : RandomAccessCollection,
  LHSCodeUnits.SubSequence.Index == LHSCodeUnits.Index,
  LHSCodeUnits.SubSequence.SubSequence == LHSCodeUnits.SubSequence,
  LHSCodeUnits.SubSequence.Iterator.Element == LHSCodeUnits.Iterator.Element,
  RHSCodeUnits.SubSequence : RandomAccessCollection,
  RHSCodeUnits.SubSequence.Index == RHSCodeUnits.Index,
  RHSCodeUnits.SubSequence.SubSequence == RHSCodeUnits.SubSequence,
  RHSCodeUnits.SubSequence.Iterator.Element == RHSCodeUnits.Iterator.Element
{
  var lhsIdx = lhs.codeUnits.startIndex
  var rhsIdx = rhs.codeUnits.startIndex
  let lhsEndIdx = lhs.codeUnits.endIndex
  let rhsEndIdx = rhs.codeUnits.endIndex
  // Returns a value if termination conditions are satisfied, otherwise nil.
  // The value returned is the result of `compare`.
  // Captures the indices declared above
  func checkTerminationCondition() -> SortOrder? {
    // If both are consumed, success!
    if lhsIdx == lhsEndIdx && rhsIdx == rhsEndIdx {
      return .same
    }
    // One if finished, the other is not
    if lhsIdx == lhsEndIdx {
      return .before
    }
    if rhsIdx == rhsEndIdx {
      return .after
    }
    return nil
  }
  // Orders two numbers
  func order(_ lhs: UInt32, _ rhs: UInt32) -> SortOrder {
    if lhs < rhs {
      return .before
    }
    if lhs == rhs {
      return .same
    }
    return .after
  }
  // Check for starter status, if applicable.
  func checkLHSNextStarterCU(_ lhsNextIdx: LHSCodeUnits.Index) -> Bool {
    return preNormalized
      || lhsNextIdx == lhsEndIdx
      || LHSEncoding.quickCheckStarter(lhs.codeUnits[lhsNextIdx])
  }
  func checkRHSNextStarterCU(_ rhsNextIdx: RHSCodeUnits.Index) -> Bool {
    return preNormalized
      || rhsNextIdx == rhsEndIdx
      || RHSEncoding.quickCheckStarter(rhs.codeUnits[rhsNextIdx])
  }
  // Trivial: we're finished
  if let res = checkTerminationCondition() {
    return .result(res)
  }
  // Fast loop while possible
  while true {
    // Ensure mutual trivial decodability
    guard _slowPath(
      LHSEncoding.isTriviallyDecodable(lhs.codeUnits[lhsIdx]) &&
      RHSEncoding.isTriviallyDecodable(rhs.codeUnits[rhsIdx])) else {
      // FIXME: if non-pre-normalized, still faster to just decode on the fly
      // and continue so long as we can.
      // Need to fall out to decoded comparison
      return .moreProcessingRequired(lhsIdx, rhsIdx)
    }
    let lhsCU: UInt32 = numericCast(lhs.codeUnits[lhsIdx])
    let rhsCU: UInt32 = numericCast(rhs.codeUnits[rhsIdx])
    let lhsNextIdx = lhs.codeUnits.index(after: lhsIdx)
    let rhsNextIdx = rhs.codeUnits.index(after: rhsIdx)
    let lhsNextIsEnd = lhsNextIdx == lhsEndIdx
    let rhsNextIsEnd = rhsNextIdx == rhsEndIdx
    // If we're on the last code units, we can just compare directly and
    // return.
    if lhsNextIsEnd && rhsNextIsEnd {
      return .result(order(lhsCU, rhsCU))
    }
    // If we're not pre-normalized, then we need to ensure that the next code
    // units are starter code units.
    guard checkLHSNextStarterCU(lhsNextIdx)
       && checkRHSNextStarterCU(rhsNextIdx) else {
      return .moreProcessingRequired(lhsIdx, rhsIdx)
    }
    // Can now just compare code units
    if lhsCU != rhsCU {
      return .result(order(lhsCU, rhsCU))
    }
    if let res = checkTerminationCondition() {
      return .result(res)
    }
    // Continue
    lhsIdx = lhsNextIdx
    rhsIdx = rhsNextIdx
  }
}

extension Collection {
  func lexicographicCompare<
    OtherC: Collection
  >(_ other: OtherC) -> SortOrder
  where
    // Self.Iterator.Element: FixedWidthInteger,
    Self.Iterator.Element: UnsignedInteger,
    // OtherC.Iterator.Element: FixedWidthInteger,
    OtherC.Iterator.Element: UnsignedInteger
  {
    for (lhs, rhs) in zip(self, other) {
      let ord = lhs.ordered(with: rhs)
      guard ord == .same else {
        return ord
      }
    }
    return .same
  }

  func lexicographicCompare<
    OtherC: Collection
  >(
    _ other: OtherC,
    using order: (Self.Iterator.Element, OtherC.Iterator.Element) -> SortOrder
  ) -> SortOrder
  {
    for (lhs, rhs) in zip(self, other) {
      let ord = order(lhs, rhs)
      guard ord == .same else {
        return ord
      }
    }
    return .same
  }
}

extension UnsignedInteger {
  func ordered<Other: UnsignedInteger>(with other: Other) -> SortOrder {
    // FIXME: not overflow safe...
    let lhs = numericCast(self) as Int
    let rhs = numericCast(other) as Int
    if _fastPath(lhs == rhs) {
      return .same
    }

    return lhs < rhs ? .before : .after
  }
}

extension Character {
  func ordered(with other: Character) -> SortOrder {
    // FIXME: this will do UCA-based ordering :-(. We can't even use the unicode
    // scalars as those are not guaranteed to be canonicalized :-(. Such
    // sadness.
    if _fastPath(self == other) {
      return .same
    }
    return self < other ? .before : .after
  }
}

extension EncodedScalarProtocol {
  func ordered<Other: EncodedScalarProtocol>(with other: Other) -> SortOrder {
    let lhs = self.utf32[0]
    let rhs = other.utf32[0]
    return lhs.ordered(with: rhs)
  }
}

extension _UnicodeViews
where
  // CodeUnits.Index : UnsignedInteger,
  // FIXME: something like: CodeUnits.SubSequence == CodeUnits,
  // CodeUnits.Iterator.Element : FixedWidthInteger,
  CodeUnits.Iterator.Element : UnsignedInteger
{
  func ordered<
    OtherCodeUnits: RandomAccessCollection, OtherEncoding: UnicodeEncoding
  >(with other: _UnicodeViews<OtherCodeUnits, OtherEncoding>) -> SortOrder
  where
    OtherCodeUnits.Index == CodeUnits.Index,
    // OtherCodeUnits.SubSequence == OtherCodeUnits,
    // OtherCodeUnits.Iterator.Element : FixedWidthInteger,
    OtherCodeUnits.Iterator.Element : UnsignedInteger
  {
    let lhs = self
    let rhs = other
    switch _determineComparisonStrategy(lhs, rhs) {
    case .bits:
      // TODO: ensure this compiles to memcmp, otherwise rewrite as lower level
      return lhs.codeUnits.lexicographicCompare(rhs.codeUnits)

    case .bitsWithSurrogatesGreater:
      // TODO: ensure this compiles well, otherwise rewrite at lower level
      // TODO: better to have the known BMP on one side and the unknown on the
      // other.
      return lhs.codeUnits.lexicographicCompare(rhs.codeUnits) {
        lhsCU, rhsCU in
        // Check surrogate patterns
        guard Encoding.isTriviallyDecodable(lhsCU) else {
          return .after
        }
        guard OtherEncoding.isTriviallyDecodable(rhsCU) else {
          return .before
        }
        return lhsCU.ordered(with: rhsCU)
      }

    case .zextBits:
      // TODO: ensure this compiles well, otherwise rewrite at lower level
      return lhs.codeUnits.lexicographicCompare(rhs.codeUnits) {
        lhsCU, rhsCU in
        lhsCU.ordered(with: rhsCU)
      }

    case .zextBitsWithSurrogatesGreater:
      // TODO: ensure this compiles well, otherwise rewrite at lower level
      // TODO: better to have the known BMP on one side and the unknown on the
      // other.
      return lhs.codeUnits.lexicographicCompare(rhs.codeUnits) {
        lhsCU, rhsCU in
        // Check surrogate patterns
        guard Encoding.isTriviallyDecodable(lhsCU) else {
          return .after
        }
        guard OtherEncoding.isTriviallyDecodable(rhsCU) else {
          return .before
        }
        return lhsCU.ordered(with: rhsCU)
      }

    case .unicodeScalarValues:
      // First, get as far as we can with fast paths
      //
      // TODO: fast path function might actually just finish the comparison for
      // us, that is it can do the decoding and continue...
      let partialResult = _partialFastCompare(
        lhs, rhs, preNormalized: true
      )
      switch partialResult {
      case .result(let res):
        return res
      case .moreProcessingRequired(let lhsIdx, let rhsIdx):
        // FIXME: avoid the copy into array once I get constraints right...
        let lhsRest = _UnicodeViews_(
          lhs.codeUnits.suffix(from: lhsIdx).map { $0 }, Encoding.self
        )
        let rhsRest = _UnicodeViews_(
          rhs.codeUnits.suffix(from: rhsIdx).map { $0 }, OtherEncoding.self
        )
        // Fall back to decoding and comparing decoded scalars
        return lhsRest.scalars.lexicographicCompare(rhsRest.scalars) {
          lhsScalar, rhsScalar in
          return lhsScalar.ordered(with: rhsScalar)
        }
      }

    case .normalizeThenCompareUnicodeScalarValues:
      // First, get as far as we can with fast paths
      let partialResult = _partialFastCompare(
        lhs, rhs, preNormalized: false
      )
      switch partialResult {
      case .result(let res):
        return res
      case .moreProcessingRequired(let lhsIdx, let rhsIdx):
        // FIXME: avoid the copy into array once I get constraints right...
        let lhsRest = _UnicodeViews_(
          lhs.codeUnits.suffix(from: lhsIdx).map { $0 }, Encoding.self
        )
        let rhsRest = _UnicodeViews_(
          rhs.codeUnits.suffix(from: rhsIdx).map { $0 }, OtherEncoding.self
        )
        return lhsRest.fccNormalizedUTF16.lexicographicCompare(
          rhsRest.fccNormalizedUTF16)
      }
    }
  }
}

//
// A "normalization segment" is a sequence that starts with a unicode scalar
// value which has a normalization boundary before it, and includes all
// subsequent unicode scalar values who do not.
//
// A "native index" is an index into the original String's code units. It can be
// interchanged with many of the other String indices. TODO: I think this is
// what UnicodeView solves, phrase it in those terms.
//
//
//

// Michael TODO: Why doesn't the following work?
//
// extension UnsignedInteger : _DefaultConstructible {
//   init() { self = 0 }
// }

extension UInt16 : _DefaultConstructible {}

typealias UTF16CodeUnitBuffer =
  UnboundCapacity<_BoundedCapacity<_Array8<UInt16>>>

// A normalization segment that is FCC-normalized. This is a collection of
// normalized UTF16 code units.
//
// Note that indices into a normalized segment are not native indices, and do
// not necessarily correspond to any particular code unit as they may of
// undergone composition or decomposition. Thus, FCCNormalizedSegments are not
// suitable for queries needing sub-segment granularity. However,
// FCCNormalizedSegments are suitable for segment-level-or-coarser granularity
// queries, which include any grapheme-level queries as segments are sub-
// grapheme.
//
// TODO: Explore coalescing small segments together
struct FCCNormalizedSegment : BidirectionalCollection {
  let buffer: UTF16CodeUnitBuffer

  init(_ buffer: UTF16CodeUnitBuffer) {
    self.buffer = buffer
  }
  init() {
    self.buffer = UTF16CodeUnitBuffer()
  }

  typealias Index = UTF16CodeUnitBuffer.Index

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
  public typealias SubSequence = BidirectionalSlice<FCCNormalizedSegment>
}

// Initialize an UnboundCapacity from any sequence
extension UnboundCapacity {
  public init<S : Sequence>(_ elements: S)
  where S.Iterator.Element == Base.Iterator.Element {
    self.init()
    self.reserveCapacity(elements.underestimatedCount)
    for elt in elements {
      self.append(elt)
    }
  }
}

// Ask ICU if the given unicode scalar value has a normalization boundary before
// it, that is it begins a new normalization segment.
public func _hasBoundary(before value: UInt32) -> Bool {
  return __swift_stdlib_unorm2_hasBoundaryBefore(_fccNormalizer, value) != 0
}

struct FCCNormalizedLazySegments<
  CodeUnits : RandomAccessCollection,
  FromEncoding : UnicodeEncoding
> : BidirectionalCollection
where
  CodeUnits.Index == CodeUnits.SubSequence.Index,
  CodeUnits.SubSequence : RandomAccessCollection,
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

  private func priorSegment(
    endingAt end: CodeUnits.Index
  ) -> Index {
    _debug("priorSegment endingAt: end")
    precondition(end != codeUnits.startIndex)

    // Decode scalars backwards, including them until we after we find one that
    // has a boundary before it (and include that one).
    var start = end
    while start != codeUnits.startIndex {
      // Include this scalar
      let (scalarValue: value, startIndex: scalarStart, e)
        = decodeOne(priorTo: start)
      assert(e == start, "Internal inconsistency in decodeOne")

      // Include this scalar
      start = scalarStart

      if _hasBoundary(before: value) {
        // We're done
        break
      }
    }

    return Index(
      nativeStartIndex: start,
      nativeEndIndex: end,
      segment: formSegment(from: start, until: end)
    )
  }

  // Scan ahead and produce the next segment. `from` must be valid index (not
  // end).
  private func nextSegment(
    startingAt start: CodeUnits.Index
  ) -> Index {
    if start == codeUnits.endIndex {
      return endIndex
    }

    // Parse the first scalar, it will always be in the segment
    var (scalarValue: value, startIndex: s, endIndex: end)
      = decodeOne(from: start)
    assert(_hasBoundary(before: value), "Not on segment boundary")
    assert(s == start, "Internal inconsistency in decodeOne")

    // Include any subsequent scalars that don't have boundaries before them
    while end != codeUnits.endIndex {
      let (scalarValue: value, startIndex: s, endIndex: scalarEnd)
        = decodeOne(from: end)
      assert(s == end, "Internal inconsistency in decodeOne")

      if _hasBoundary(before: value) {
        // Exclude this scalar
        break
      }

      // Include this scalar
      end = scalarEnd
    }

    return Index(
      nativeStartIndex: start,
      nativeEndIndex: end,
      segment: formSegment(from: start, until: end)
    )
  }

  // Normalize a segment. Indices must be on scalar boundaries.
  private func formSegment(
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

    // Single scalar is trivial segment, no need to normalize
    //
    // TODO: fast pre-normalized checks (worth doing before calling over to
    //       ICU)
    //
    // TODO: non-BMP can hit this path too
    if end == codeUnits.index(after: start) {
      return FCCNormalizedSegment(buffer)
    }

    assert(buffer.count > 0, "How did this happen? Failed precondition?")

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
            _fccNormalizer, $0.baseAddress, numericCast($0.count),
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
          // (indicative of error). Loose by assuming that every original
          // character could be decomposed the maximum number of times. Without
          // this, an error would loop until we run out of memory or the array
          // is larger than 2^32 on 64bit platforms.
          //
          // FIXME: should go away by construction, or else be sanity check.
          assert(buffer.count < originalCount*maxDecompSize)

          // extend array storage by 25%
          array.append(
            contentsOf: repeatElement(0, count: (array.count + 3) >> 2))
        }
      }
    }

    return FCCNormalizedSegment(buffer)
  }

  // Decode one or more code units, returning the unicode scalar value and the
  // number of code units parsed. `startingFrom` should be on scalar boundary
  private func decodeOne(from start: CodeUnits.Index)
    -> (scalarValue: UInt32,
        startIndex: CodeUnits.Index,
        endIndex: CodeUnits.Index) {
    precondition(start != codeUnits.endIndex, "Given empty slice")

    let encodedScalar = unicodeView(from: start).scalars.first!
    return (scalarValue: encodedScalar.utf32[0],
            startIndex: start,
            endIndex: codeUnits.index(start, offsetBy: numericCast(encodedScalar.count)))
  }

  // As decodeOne, but in reverse. `priorTo` is the index after the last code
  // unit in the scalar, i.e. it is exclusive.
  private func decodeOne(priorTo end: CodeUnits.Index)
    -> (scalarValue: UInt32,
        startIndex: CodeUnits.Index,
        endIndex: CodeUnits.Index) {
    precondition(end != codeUnits.startIndex, "Given empty slice")

    let encodedScalar = unicodeView(until: end).scalars.last!
    return (scalarValue: encodedScalar.utf32[0],
            startIndex: codeUnits.index(end, offsetBy: -numericCast(encodedScalar.count)),
            endIndex: end)
  }

  // Get the rest of the Unicode view
  private func unicodeView(
    from start: CodeUnits.Index? = nil,
    until end: CodeUnits.Index? = nil
  ) -> _UnicodeViews<CodeUnits.SubSequence, FromEncoding> {
    let end = end ?? codeUnits.endIndex
    let start = start ?? codeUnits.startIndex
    return _UnicodeViews(codeUnits[start..<end], FromEncoding.self)
  }

  //
  // Be a BidirectionalCollection
  //

  // TODO?: This is really more like an iterator...
  struct Index: Comparable {
    // The corresponding native begin/end indices for this segment
    let nativeStartIndex: CodeUnits.Index
    let nativeEndIndex: CodeUnits.Index
    let segment: FCCNormalizedSegment

    public static func <(lhs: Index, rhs: Index) -> Bool {
      _debug("\(lhs.nativeStartIndex, lhs.nativeEndIndex) < \(rhs.nativeStartIndex, rhs.nativeEndIndex)")
      if lhs.nativeStartIndex < rhs.nativeStartIndex {
        // Our ends should be ordered similarly, unless lhs is the last index
        // before endIndex and rhs is the endIndex.
        assert(
          lhs.nativeEndIndex < rhs.nativeEndIndex ||
          rhs.nativeStartIndex == rhs.nativeEndIndex,
          "overlapping segments?")

        return true
      }

      return false
    }

    public static func ==(lhs: Index, rhs: Index) -> Bool {
      _debug("\(lhs.nativeStartIndex, lhs.nativeEndIndex) == \(rhs.nativeStartIndex, rhs.nativeEndIndex)")

      if lhs.nativeStartIndex == rhs.nativeStartIndex {
        assert(
          lhs.nativeEndIndex == rhs.nativeEndIndex,
          "overlapping segments?")

        return true
      }

      // assert(
      //   lhs.nativeEndIndex != rhs.nativeEndIndex
      //   "overlapping segments?")
      return false
    }
  }

  var startIndex: Index {
    _debug("startIndex")
    return nextSegment(startingAt: codeUnits.startIndex)
  }
  var endIndex: Index {
    _debug("endIndex")
    return Index(
      nativeStartIndex: codeUnits.endIndex,
      nativeEndIndex: codeUnits.endIndex,
      segment: FCCNormalizedSegment())
  }

  func index(after idx: Index) -> Index {
    _debug("index after: \(idx.nativeStartIndex, idx.nativeEndIndex)")
    return nextSegment(startingAt: idx.nativeEndIndex)
  }
  func index(before idx: Index) -> Index {
    _debug("index before: \(idx.nativeStartIndex, idx.nativeEndIndex)")
    return priorSegment(endingAt: idx.nativeStartIndex)
  }
  subscript(position: Index) -> FCCNormalizedSegment {
    _debug("subscript: \(position.nativeStartIndex, position.nativeEndIndex)")
    return position.segment
  }

  public typealias SubSequence = BidirectionalSlice<FCCNormalizedLazySegments>
}

typealias FCCNormalizedUTF16View_2<
  CodeUnits: RandomAccessCollection,
  Encoding: UnicodeEncoding
> = FlattenBidirectionalCollection<
  FCCNormalizedLazySegments<CodeUnits, Encoding>
>
where
  CodeUnits.Index == CodeUnits.SubSequence.Index,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence == CodeUnits.SubSequence.SubSequence,
  CodeUnits.Iterator.Element == CodeUnits.SubSequence.Iterator.Element,
  CodeUnits.Iterator.Element == Encoding.EncodedScalar.Iterator.Element


// struct FCCNormalizedUTF16View_2<
//   CodeUnits : RandomAccessCollection,
//   FromEncoding : UnicodeEncoding
// > : BidirectionalCollection
// where
//   CodeUnits.Index == CodeUnits.SubSequence.Index,
//   CodeUnits.SubSequence : RandomAccessCollection,
//   CodeUnits.SubSequence == CodeUnits.SubSequence.SubSequence,
//   CodeUnits.SubSequence == CodeUnits, // TODO: necessary?
//   CodeUnits.Iterator.Element == CodeUnits.SubSequence.Iterator.Element,
//   CodeUnits.Iterator.Element == FromEncoding.EncodedScalar.Iterator.Element
// {

// }


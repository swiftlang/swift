// Prototype code for an eventual StringComparison.swift

import Swift

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
      //
      // For efficiency and simplicity, currently implemented as a threshold.
      // But in theory, we could include scalars above surrogate ranges. But
      // that might be murkey for private use areas and might be more likely to
      // be affected by churn in future Unicode versions.
      //
      // TODO: cost/benefit investigation of more complex check here...
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
  // Whether the given code unit is a normalization-sequence-starter code unit.
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
// normal form.
//

// For now, sort order semantics are: the unicode scalar value order when in
// NFT. This may be refined more in the future as I learn more
// about Unicode.

// Mocked-up extra bits of info we might want from Unicode/UnicodeStorage
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
//  compare bits. Done using a lazy zero-extending view over the smaller-
//  bitwidth string.
//
//  3) scalar value comparison: compare the unicode scalar values, and compare
//  those. This is a decode-and-then-bit-compare. Implemented through a
//  transcoded view targeting (valid?) UTF32.
//
//  4) normalized scalar comparison: compare sequence of normalized scalar
//  values. Currently implemented as grapheme-based comparison (requiring
//  grapheme break logic which currently uses libICU), but that does more work
//  than strictly necessary. Need to make a normalized view, then we can just
//  compare across that. Fast path: so long as the next CUs begin a new
//  grapheme, can compare non-normalized: scalars as above and corresponding
//  fast-paths from above.
//


//
// TODO: notion of mutually-bitwise-comparable encodings, wherein strings in
// these encodings can be ordered through bitwise comparison alone. I need a
// better name, as this notion needs to be paired with NFT. This
// happens when the encodings are the same, or when they have the same bitsize
// and each of their contents are mutually-trivially-decodable.
//

//  TODO: notion of a equivalence-class-break, or a code unit that is definitely
// part of a different ...comparison unit?... than the one preceeding it. All
// grapheme breaks are equivalence-class-breaks, but some additional unicode
// scalars are as well...
//
//
//
// When they apply:
//
//  #1 can be used on entire strings when all of the following are satisfied:
//    * Both strings are in NFT
//    * Both strings have trivially-decodable code units
//
//  #2 can be used on entire strings when all of the following are satisfied:
//    * Both strings are in NFT
//    * Both strings have trivially-decodable code units
//
//  #3 can be used on entire strings when all of the following are satisfied:
//    * Both strings are in NFT
//
//  #4 is used for all the rest.
//
//  Note that this is a property of comparing two particular strings in two
//  particular encodings. The faster methods can situationally be used for
//  comparing segments of strings when some conditions are satisfied.
//
//
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
// normalization-sequence-starters.
//


//
// For the slow path, ideally we would have a normalized-scalar-view that we can
// iterate over to compare. Ideally, this would be done without needing to call
// out to ICU, but I don't know if we'd need full grapheme-break detection.
// Regardless, we'd need significant portions of the UCD bundled with the stdlib
// to avoid ICU.
//
// I mock up this functionality using Character and then stable-sorting
// constintuent scalars, but this is not necessarily producing a C normal form.
// I perform composition yet...
//

//
// String sort order in Swift, absent any locale, is the sort order of the
// string's sequence of NFT unicode scalar values, where normalization-sequences
// must be finished before moving on to the next unicode scalar value.
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
// resume comparison. In that case, the graphme index would advance past the
// point of divergence. This would be a misuse of our indices and comparison
// semantics, and I can't think of a good way to address it. Note that it would
// also apply even at canonical-combining-sequence boundaries that were not also
// grapheme boundaries.
//
// That is, the lexicographic invariant is preserved.
//
// TODO: given that lexicographic ordering is monoidal, figure out the fancy
// term for this property.
//


extension UnicodeStorage {
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

// TODO: This currently asserts in irgen::emitStructMetadata, need to
// investigate and file a JIRA.
//
// extension UnicodeStorage
// where CodeUnits.Iterator.Element: UnsignedInteger {
//   struct ZextView : BidirectionalCollection {

//     let codeUnits: CodeUnits

//     init(_ codeUnits: CodeUnits) {
//       self.codeUnits = codeUnits
//     }

//     public var startIndex: CodeUnits.Index {
//       return codeUnits.startIndex
//     }
//     public var endIndex: CodeUnits.Index {
//       return codeUnits.endIndex
//     }
//     public subscript(i: CodeUnits.Index) -> UInt32 {
//       return numericCast(codeUnits[i])
//     }
//     public func index(after i: CodeUnits.Index) -> CodeUnits.Index {
//       return codeUnits.index(after: i)
//     }
//     public func index(before i: CodeUnits.Index) -> CodeUnits.Index {
//       return codeUnits.index(before: i)
//     }
//     typealias SubSequence = BidirectionalSlice<ZextView>
//   }

//   var zextCodeUnits : ZextView { return ZextView(codeUnits) }

// }

// TODO: replace with eventual comparison order
enum SortOrder { case before, same, after }

enum StringComparisonStrategy {
  case bits
  // TODO: worth refactoring to have associated values
  case bitsWithSurrogatesGreater
  case zextBits
  case zextBitsWithSurrogatesGreater
  case unicodeScalarValues
  case normalizedUnicodeScalarValues
}

func determineComparisonStrategy<
  LHSCodeUnits: RandomAccessCollection,
  LHSEncoding: UnicodeEncoding,
  RHSCodeUnits: RandomAccessCollection,
  RHSEncoding: UnicodeEncoding
>(
  _ lhs: UnicodeStorage<LHSCodeUnits, LHSEncoding>,
  _ rhs: UnicodeStorage<RHSCodeUnits, RHSEncoding>
) -> StringComparisonStrategy
where
  LHSEncoding.CodeUnit: UnsignedInteger,
  RHSEncoding.CodeUnit: UnsignedInteger
  // LHSEncoding.CodeUnit: FixedWidthInteger,
  // RHSEncoding.CodeUnit: FixedWidthInteger
{
  // Without normalization, all bets are off
  guard lhs.isNFT() && rhs.isNFT() else {
    return .normalizedUnicodeScalarValues
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
func partialFastCompare<
  LHSCodeUnits: RandomAccessCollection,
  LHSEncoding: UnicodeEncoding,
  RHSCodeUnits: RandomAccessCollection,
  RHSEncoding: UnicodeEncoding
>
(
  _ lhs: UnicodeStorage<LHSCodeUnits, LHSEncoding>,
  _ rhs: UnicodeStorage<RHSCodeUnits, RHSEncoding>,
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

extension UnicodeStorage
where
  // CodeUnits.Index : UnsignedInteger,
  // FIXME: something like: CodeUnits.SubSequence == CodeUnits,
  // CodeUnits.Iterator.Element : FixedWidthInteger,
  CodeUnits.Iterator.Element : UnsignedInteger
{
  func ordered<
    OtherCodeUnits: RandomAccessCollection, OtherEncoding: UnicodeEncoding
  >(with other: UnicodeStorage<OtherCodeUnits, OtherEncoding>) -> SortOrder
  where
    OtherCodeUnits.Index == CodeUnits.Index,
    // OtherCodeUnits.SubSequence == OtherCodeUnits,
    // OtherCodeUnits.Iterator.Element : FixedWidthInteger,
    OtherCodeUnits.Iterator.Element : UnsignedInteger
  {
    let lhs = self
    let rhs = other
    switch determineComparisonStrategy(lhs, rhs) {
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
      let partialResult = partialFastCompare(
        lhs, rhs, preNormalized: true
      )
      switch partialResult {
        case .result(let res):
          return res
        case .moreProcessingRequired(let lhsIdx, let rhsIdx):
          // FIXME: avoid the copy into array once I get constraints right...
          let lhsRest = UnicodeStorage<[CodeUnits.Iterator.Element], Encoding>(
            lhs.codeUnits.suffix(from: lhsIdx).map { $0 }, Encoding.self
          )
          let rhsRest = UnicodeStorage<[OtherCodeUnits.Iterator.Element], OtherEncoding>(
            rhs.codeUnits.suffix(from: rhsIdx).map { $0 }, OtherEncoding.self
          )
          // Fall back to decoding and comparing decoded scalars
          return lhsRest.scalars.lexicographicCompare(rhsRest.scalars) {
            lhsScalar, rhsScalar in
            return lhsScalar.ordered(with: rhsScalar)
          }
      }

    case .normalizedUnicodeScalarValues:
      // First, get as far as we can with fast paths
      let partialResult = partialFastCompare(
        lhs, rhs, preNormalized: false
      )
      switch partialResult {
        case .result(let res):
          return res
        case .moreProcessingRequired(let lhsIdx, let rhsIdx):
          // FIXME: avoid the copy into array once I get constraints right...
          let lhsRest = UnicodeStorage<[CodeUnits.Iterator.Element], Encoding>(
            lhs.codeUnits.suffix(from: lhsIdx).map { $0 }, Encoding.self
          )
          let rhsRest = UnicodeStorage<[OtherCodeUnits.Iterator.Element], OtherEncoding>(
            rhs.codeUnits.suffix(from: rhsIdx).map { $0 }, OtherEncoding.self
          )
          // TODO: instead, use normalized scalar view
          return lhsRest.characters.lexicographicCompare(rhsRest.characters) {
            lhsChar, rhsChar in
            return lhsChar.ordered(with: rhsChar)
          }
      }
    }
  }
}


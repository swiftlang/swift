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
      return codeUnit < 0xD800
    }

    fatalError("unimplemented")
  }
}

// TODO: put these on protocol decl, and have impls provide functionality
// instead
//
// The below establishes the concept of a *normalization-sequence-starter code
// unit*, which is any code unit that begins a unicode scalar value whose
// canonical combining class in the UCD is zero. Such code units start a new
// sequence for the purposes of normalization and thus also serve as natural
// canonical ordering breaks.
extension UnicodeEncoding
where
  EncodedScalar.Iterator.Element : UnsignedInteger
{
  // Whether the given code unit is a normalization-sequence-starter code unit.
  // This function is meant as a quick check, involving very few operations, to
  // check for large and common ranges of starter code units.
  static func quickCheckNormalizationSequenceStarter(
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

// For now, sort order semantics are: the unicode scalar value order when in
// TODO-normal-form. This may be refined more in the future as I learn more
// about Unicode.

// Mocked-up extra bits of info we might want from Unicode/UnicodeStorage
extension Unicode {
  // Whether the string only contains unicode scalar values in the GMP plane.
  // This is useful as it ensures trivial-decodability for UTF16 as well as
  // making code-unit-wise ordering reflect scalar value ordering for normalized
  // UTF16.
  func isGMP(scan: Bool = true) -> Bool {
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
// better name, as this notion needs to be paired with TODO-normal-form. This
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
//    * Both strings are in TODO-normal-form
//    * Both strings are mutually-bitwise-comparable
//
//  #2 can be used on entire strings when all of the following are satisfied:
//    * Both strings are in TODO-normal-form
//    * Both strings are mutually-bitwise-comparable-modulo-zero-extension TODO:
//
//  #3 can be used on entire strings when all of the following are satisfied:
//    * Both strings are in TODO-normal-form
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




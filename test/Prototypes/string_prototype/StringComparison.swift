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

// TODO: notion of mutually-trivially-decodable, where each code unit is
// trivially-decodable in its own encoding as well as the other one. For
// example, ASCII code units are mutually-trivially-decodable between UTF8 and
// UTF16, but not Latin1 code units.


// For now, sort order semantics are: the unicode scalar value order when in
// TODO-normal-form. This may be refined more in the future as I learn more
// about Unicode.


//
// Implementation-side, boils down to 4 general levels of comparision. These are
// presented from fastest (and most restricively applicable) to slowest (and
// most generally applicable), and thus in the order of preference if possible.
//
//  1) bitwise comparison: compare bits; roughly equivalent to C's memcmp.
//
//  2) zero-extend and compare: zero extend the smaller code unit and then
//  compare bits. Done using a lazy zero-extending view.
//
//  3) scalar value comparison: compare the unicode scalar values, and sort
//  based on those. This is a decode-and-bit-compare. Implemented through a
//  transcoded view targeting (valud?) UTF32. Fast path: use bit/zext comparison
//  whenever both code units happen to be trivially-decodable.
//
//  4) normalized scalar comparison: compare sequence of normalized scalar
//  values. Currently implemented as grapheme-based comparison, but that does
//  more work than strictly necessary. Need to make a normalized view, then we
//  can just compare across that. Fast path: so long as the next CUs begin a new
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
// Fast paths
//
// #1 needs no fast paths, it *is* the fastest path.
//
// #2 needs no fast paths. If #1 isn't possible, then #2 *is* the fastest path.
//
// #3 can do bitwise/zero-extension compare whenever both compared code units
//    are mutually-trivially-decodable.
//
// #4 can do bitwise/zero-extension compare whenever both compared code units are mutually-trivially-decodable AND to following code units  definitely do
//
//

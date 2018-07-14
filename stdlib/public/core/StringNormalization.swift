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

extension Unicode {
  /// A normalization form for Unicode text.
  ///
  /// Unicode extended grapheme clusters can be equivalent to each other despite
  /// differences in their underlying representation. Normalization removes
  /// unwanted differences in representation; roughly speaking, a normalization
  /// form specifies which differences are unwanted and how they should be
  /// normalized.
  ///
  /// Two __canonically equivalent__ texts should have no differences in display
  /// formatting or interpretation. For example, Ä (00C4 LATIN CAPITAL LETTER A
  /// WITH DIAERESIS) is equivalent to Ä (0041 LATIN CAPITAL LETTER A + 0308
  /// COMBINING DIAERESIS).
  ///
  /// Two __compatibly equivalent__ texts may, in some contexts, have
  /// differences in display formatting or interpretation; for example, ½ (00BD
  /// VULGAR FRACTION ONE HALF) is compatibly equivalent to 1⁄2 (0031 DIGIT ONE
  /// + 2044 FRACTION SLASH + 0032 DIGIT TWO).
  /* non-frozen */ public enum NormalizationForm {
    /// Normalization Form D (NFD), canonical decomposition.
    ///
    /// This algorithm separates single code points into canonically equivalent
    /// sequences of code points, where possible.
    case nfd

    /// Normalization Form C (NFC), canonical decomposition followed by
    /// canonical composition.
    ///
    /// This algorithm combines canonically equivalent decomposed sequences of
    /// code points into their canonically equivalent single code points, where
    /// possible.
    case nfc

    /// Normalization Form KD (NFKD), compatibility decomposition.
    ///
    /// This algorithm separates single code points into compatibly equivalent
    /// sequences of code points, where possible; the decomposition may discard
    /// information important for formatting or interpretation in some contexts.
    case nfkd

    /// Normalization Form KC (NFKC), compatibility decomposition followed by
    /// canonical composition.
    ///
    /// This algorithm combines compatibly equivalent decomposed sequences of
    /// code points into their canonically equivalent single code points, where
    /// possible; the decomposition may discard information important for
    /// formatting or interpretation in some contexts.
    case nfkc

    /// "Fast C or D" (FCD), as described in Unicode Technical Note #5.
    ///
    /// FCD does not describe a unique form: two canonically equivalent texts
    /// normalized using this algorithm can be non-identical in their underlying
    /// representation. All NFD strings and most NFC strings fulfill the
    /// conditions of FCD form.
    case fcd

    /// "Fast C Contiguous" (FCC), as described in Unicode Technical Note #5.
    ///
    /// This algorithm is a modification of the NFC algorithm that eliminates
    /// discontiguous composition. For most texts, the FCC and NFC forms are
    /// identical.
    case fcc
  }
}

// An internal namespace for various normalization heuristics.
internal enum _Normalization {
  // Returns ICU's unorm2 instance corresponding to the given normalization
  // form.
  internal static func _normalizer(
    _ form: Unicode.NormalizationForm
  ) -> OpaquePointer {
    var err = __swift_stdlib_U_ZERO_ERROR
    let normalizer: OpaquePointer
    switch form {
    case .nfd: normalizer = __swift_stdlib_unorm2_getNFDInstance(&err)
    case .nfc: normalizer = __swift_stdlib_unorm2_getNFCInstance(&err)
    case .nfkd: normalizer = __swift_stdlib_unorm2_getNFKDInstance(&err)
    case .nfkc: normalizer = __swift_stdlib_unorm2_getNFKCInstance(&err)
    case .fcd: normalizer = __swift_stdlib_unorm2_getFCDInstance(&err)
    case .fcc: normalizer = __swift_stdlib_unorm2_getFCCInstance(&err)
    // @unknown default: fatalError("Unrecognized normalization form")
    }

    guard err.isSuccess else {
      // This shouldn't be possible unless some deep (unrecoverable) system
      // invariants are violated.
      fatalError("Unable to talk to ICU")
    }
    return normalizer
  }

  // Returns a Boolean value indicating whether this buffer of code units
  // satisfies the quickCheck=YES property for normality checking.
  //
  // ICU provides a quickCheck, which may yield YES, NO, or MAYBE; there are no
  // MAYBE values for NFD and NFKD. YES means that the string was determined to
  // be definitely normal. In practice, the majority of strings have this
  // property. Checking for YES is considerably faster than trying to
  // distinguish between NO and MAYBE.
  internal static func _prenormalQuickCheckYes(
    normalizer: OpaquePointer = _Normalization._nfcNormalizer,
    _ buffer: UnsafeBufferPointer<UInt16>
  ) -> Bool {
    var err = __swift_stdlib_U_ZERO_ERROR
    let length = __swift_stdlib_unorm2_spanQuickCheckYes(
      normalizer, buffer.baseAddress._unsafelyUnwrappedUnchecked,
      Int32(buffer.count), &err)

    guard err.isSuccess else {
      // This shouldn't be possible unless some deep (unrecoverable) system
      // invariants are violated.
      fatalError("Unable to talk to ICU")
    }
    return length == buffer.count
  }

  internal static func _prenormalQuickCheckYes(
    normalizer: OpaquePointer = _Normalization._nfcNormalizer,
    _ string: _UnmanagedString<UInt16>
  ) -> Bool {
    var err = __swift_stdlib_U_ZERO_ERROR
    let length = __swift_stdlib_unorm2_spanQuickCheckYes(
      normalizer, string.start, Int32(string.count), &err)

    guard err.isSuccess else {
      // This shouldn't be possible unless some deep (unrecoverable) system
      // invariants are violated.
      fatalError("Unable to talk to ICU")
    }
    return length == string.count
  }
}

extension _Normalization {
  internal static var _nfcNormalizer: OpaquePointer =
    _Normalization._normalizer(.nfc)

  // When normalized in NFC, some segments may expand in size (e.g. some non-BMP
  // musical notes). This expansion is capped by the maximum expansion factor of
  // the normal form. For NFC, that is 3x.
  //
  // See <https://unicode.org/faq/normalization.html#12>.
  internal static let _maxNFCExpansionFactor = 3

  // A small output buffer to use for normalizing a single normalization
  // segment. Fits all but pathological arbitrary-length segments (i.e. zalgo-
  // segments).
  internal typealias _SegmentOutputBuffer = _FixedArray16<UInt16>
}

extension UnicodeScalar {
  // Normalization boundary - a place in a string where everything left of the
  // boundary can be normalized independently from everything right of the
  // boundary. The concatenation of each result is the same as if the entire
  // string had been normalized as a whole.
  //
  // Normalization segment - a sequence of code units between two normalization
  // boundaries (without any boundaries in the middle). Note that normalization
  // segments can, as a process of normalization, expand, contract, and even
  // produce new sub-segments.

  // A Boolean value indicating Whether this scalar value always has a
  // normalization boundary before it.
  internal var _hasNormalizationBoundaryBefore: Bool {
    _sanityCheck(Int32(exactly: self.value) != nil, "top bit shouldn't be set")
    let value = Int32(bitPattern: self.value)
    return 0 != __swift_stdlib_unorm2_hasBoundaryBefore(
      _Normalization._nfcNormalizer, value)
  }
}

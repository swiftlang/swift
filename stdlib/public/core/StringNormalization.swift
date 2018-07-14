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
  /// A normalization form for strings.
  /* non-frozen */ public enum NormalizationForm {
    /// Unicode normalization form D (canonical decomposition).
    case nfd

    /// Unicode normalization form C (canonical decomposition followed by
    /// canonical composition).
    case nfc

    /// Unicode normalization form KD (compatibility decomposition).
    case nfkd

    /// Unicode normalization form KC (compatibility decomposition followed by
    /// canonical composition).
    case nfkc

    /// Unicode normalization form KC (compatibility decomposition followed by
    /// canonical composition) with case-folding.
    ///
    /// This algorithm is formalized by the Unicode standard. The expression
    /// `str.normalized(.nfkc).caseFolded().normalized(.nfkc)` approximates, but
    /// is not always identical to, `str.normalized(.nfkcCaseFold)`.
    case nfkcCaseFold

    /// "Fast C or D" (FCD) form, as described in Unicode Technical Note #5.
    ///
    /// FCD does not describe a unique form: two canonically equivalent strings
    /// normalized using this algorithm can be non-identical. All NFD strings
    /// and most NFC strings fulfill the conditions of FCD form.
    case fcd

    /// "Fast C Contiguous" (FCC) form, as described in Unicode Technical Note
    /// #5.
    ///
    /// This algorithm is a modification of the NFC algorithm that eliminates
    /// discontiguous composition. For most strings, the FCC and NFC forms are
    /// identical.
    case fcc
  }
}

// An internal namespace for various normalization heuristics.
internal enum _Normalization {
  // ICU's NFD unorm2 instance.
  internal static var _nfdNormalizer: OpaquePointer = {
    var err = __swift_stdlib_U_ZERO_ERROR
    let normalizer = __swift_stdlib_unorm2_getNFDInstance(&err)
    guard err.isSuccess else {
      // This shouldn't be possible unless some deep (unrecoverable) system
      // invariants are violated.
      fatalError("Unable to talk to ICU")
    }
    return normalizer
  }()

  // ICU's NFC unorm2 instance.
  internal static var _nfcNormalizer: OpaquePointer = {
    var err = __swift_stdlib_U_ZERO_ERROR
    let normalizer = __swift_stdlib_unorm2_getNFCInstance(&err)
    guard err.isSuccess else { fatalError("Unable to talk to ICU") }
    return normalizer
  }()

  // ICU's NFKD unorm2 instance.
  internal static var _nfkdNormalizer: OpaquePointer = {
    var err = __swift_stdlib_U_ZERO_ERROR
    let normalizer = __swift_stdlib_unorm2_getNFKDInstance(&err)
    guard err.isSuccess else { fatalError("Unable to talk to ICU") }
    return normalizer
  }()

  // ICU's NFKC unorm2 instance.
  internal static var _nfkcNormalizer: OpaquePointer = {
    var err = __swift_stdlib_U_ZERO_ERROR
    let normalizer = __swift_stdlib_unorm2_getNFKCInstance(&err)
    guard err.isSuccess else { fatalError("Unable to talk to ICU") }
    return normalizer
  }()

  // ICU's NFKCCasefold unorm2 instance.
  internal static var _nfkcCasefoldNormalizer: OpaquePointer = {
    var err = __swift_stdlib_U_ZERO_ERROR
    let normalizer = __swift_stdlib_unorm2_getNFKCCasefoldInstance(&err)
    guard err.isSuccess else { fatalError("Unable to talk to ICU") }
    return normalizer
  }()

    // ICU's FCD unorm2 instance.
  internal static var _fcdNormalizer: OpaquePointer = {
    var err = __swift_stdlib_U_ZERO_ERROR
    let normalizer = __swift_stdlib_unorm2_getFCDInstance(&err)
    guard err.isSuccess else { fatalError("Unable to talk to ICU") }
    return normalizer
  }()

  // ICU's FCC unorm2 instance.
  internal static var _fccNormalizer: OpaquePointer = {
    var err = __swift_stdlib_U_ZERO_ERROR
    let normalizer = __swift_stdlib_unorm2_getFCCInstance(&err)
    guard err.isSuccess else { fatalError("Unable to talk to ICU") }
    return normalizer
  }()

  // Whether this buffer of code units satisfies the quickCheck=YES property for
  // normality checking.
  //
  // ICU provides a quickCheck, which may yield "YES", "NO", or "MAYBE". YES
  // means that the string was determined to be definitely normal. In practice,
  // the majority of strings have this property. Checking for YES is
  // considerably faster than trying to distinguish between NO and MAYBE.
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

  // Whether this scalar value always has a normalization boundary before it.
  internal var _hasNormalizationBoundaryBefore: Bool {
    _sanityCheck(Int32(exactly: self.value) != nil, "top bit shouldn't be set")
    let value = Int32(bitPattern: self.value)
    return 0 != __swift_stdlib_unorm2_hasBoundaryBefore(
      _Normalization._nfcNormalizer, value)
  }
}

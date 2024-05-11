//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

internal typealias ScalarAndNormData = (
  scalar: Unicode.Scalar,
  normData: Unicode._CanonicalNormData
)

internal typealias ScalarAndCompatNormData = (
  scalar: Unicode.Scalar,
  normData: Unicode._CompatibilityNormData
)

extension Unicode {

  internal struct _CanonicalNormData {

    // A wrapper type over the normalization data value we receive when we
    // lookup a scalar's normalization information. The layout of the underlying
    // 16 bit value we receive is as follows:
    //
    // 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    // └──── CCC ────┘ └───────┘ └─┘ │
    //                     │      │  └── decomposed_QC
    //              Unused ┘      └── precomposed_QC
    //
    // NF(K)D_QC: This is a simple Yes/No on whether the scalar has canonical
    //            decomposition. Note: Yes is indicated via 0 instead of 1.
    //
    // NF(K)C_QC: This is either Yes/No/Maybe on whether the scalar is NFC quick
    //            check.
    //            - Yes, represented as 0, means the scalar can NEVER compose
    //              with another scalar previous to it.
    //            - No, represented as 1, means the scalar can NEVER appear
    //              within a well formed NFC string.
    //            - Maybe, represented as 2, means the scalar COULD appear
    //              with an NFC string, but further information is required
    //              to determine if that is the case.
    //
    // CCC: This is the canonical combining class property of a scalar that is
    //      used when sorting scalars of a normalization segment after NFD
    //      computation. A scalar with a CCC value of 128 can NEVER appear before
    //      a scalar with a CCC value of 100, unless there are normalization
    //      boundaries between them.
    //

    private let rawValue: UInt16

    private init(rawValue: UInt16) {
      self.rawValue = rawValue
    }

    private init(_ scalar: Unicode.Scalar, fastUpperbound: UInt32) {

      if _fastPath(scalar.value < fastUpperbound) {
        // CCC = 0, NFD_QC = Yes, NFC_QC = Yes.
        self.rawValue = 0
        return
      }

      var rawValue = _swift_stdlib_getNormData(scalar.value)
      if (0xAC00 ... 0xD7A3).contains(scalar.value) {
        // Because we don't store precomposed hangul in our NFD_QC data,
        // these will return true for NFD_QC when in fact they are not.
        // So set the bit to false.
        rawValue |= 0x1
      }
      self.rawValue = rawValue
    }

    /// Lookup the NormData for the given scalar.
    /// All fields are accurate.
    ///
    internal init(_ scalar: Unicode.Scalar) {
      self.init(scalar, fastUpperbound: 0xC0)
    }

    /// Lookup the NormData for the given scalar.
    /// Only the Canonical Combining Class and NFC\_QC fields are accurate.
    ///
    internal init(onlyCCCAndNFCQC scalar: Unicode.Scalar) {
      self.init(scalar, fastUpperbound: 0x300)
    }

    internal static var hangulLeadingConsonants: Self {
      // CCC = 0, NFD_QC = Yes, NFC_QC = Yes.
      Self(rawValue: 0)
    }

    internal static var hangulVowels: Self {
      // CCC = 0, NFD_QC = Yes, NFC_QC = Maybe.
      Self(rawValue: 0b0000_0000_0000_0100)
    }

    internal static var hangulTailConsonants: Self {
      // CCC = 0, NFD_QC = Yes, NFC_QC = Maybe.
      Self(rawValue: 0b0000_0000_0000_0100)
    }

    internal var ccc: UInt8 {
      UInt8(truncatingIfNeeded: rawValue >> 8)
    }

    internal var canonicalCombiningClass: Unicode.CanonicalCombiningClass {
      Unicode.CanonicalCombiningClass(rawValue: ccc)
    }

    internal var isNFDQC: Bool {
      rawValue & 0b0000_0001 == 0
    }

    // TODO: Replace with Tristate
    internal var isNFCQC: Bool {
      rawValue & 0b0000_0110 == 0
    }
  }


  internal struct _CompatibilityNormData {

    // Same layout as Unicode._CanonicalNormData.

    private let rawValue: UInt16

    private init(rawValue: UInt16) {
      self.rawValue = rawValue
    }

    /// Lookup the NormData for the given scalar.
    /// All fields are accurate.
    ///
    internal init(_ scalar: Unicode.Scalar) {

      if _fastPath(scalar.value < 0xA0) {
        // CCC = 0, NFKD_QC = Yes, NFKC_QC = Yes.
        self.rawValue = 0
        return
      }

      var rawValue = _swift_stdlib_getCompatibilityNormData(scalar.value)
      if (0xAC00 ... 0xD7A3).contains(scalar.value) {
        // Because we don't store precomposed hangul in our NFKD_QC data,
        // these will return true for NFKD_QC when in fact they are not.
        // So set the bit to false.
        rawValue |= 0x1
      }
      self.rawValue = rawValue
    }

    internal static var hangulLeadingConsonants: Self {
      // CCC = 0, NFD_QC = Yes, NFC_QC = Yes.
      Self(rawValue: 0)
    }

    internal static var hangulVowels: Self {
      // CCC = 0, NFD_QC = Yes, NFC_QC = Maybe.
      Self(rawValue: 0b0000_0000_0000_0100)
    }

    internal static var hangulTailConsonants: Self {
      // CCC = 0, NFD_QC = Yes, NFC_QC = Maybe.
      Self(rawValue: 0b0000_0000_0000_0100)
    }

    internal var ccc: UInt8 {
      UInt8(truncatingIfNeeded: rawValue >> 8)
    }

    internal var canonicalCombiningClass: Unicode.CanonicalCombiningClass {
      Unicode.CanonicalCombiningClass(rawValue: ccc)
    }

    internal var isNFKDQC: Bool {
      rawValue & 0b0000_0001 == 0
    }

    internal var isNFKCQC: Unicode.NFCQCResult {
      let v = UInt8(truncatingIfNeeded: (rawValue & 0b0000_0110) &>> 1)
      return Unicode.NFCQCResult(rawValue: v)
    }
  }
}

extension Unicode {

  @usableFromInline
  internal struct NFCQCResult: Equatable {
    internal var rawValue: UInt8

    @usableFromInline
    internal static var yes: Self   { Self(rawValue: 0) }
    @usableFromInline
    internal static var no: Self    { Self(rawValue: 1) }
    @usableFromInline
    internal static var maybe: Self { Self(rawValue: 2) }

    @usableFromInline
    internal static func == (lhs: Self, rhs: Self) -> Bool {
      lhs.rawValue == rhs.rawValue
    }
  }
}
extension Unicode._CanonicalNormData {
  internal var isNFCQC_Tristate: Unicode.NFCQCResult {
    let v = UInt8(truncatingIfNeeded: (rawValue & 0b0000_0110) &>> 1)
    return Unicode.NFCQCResult(rawValue: v)
  }
}

extension Unicode {

  internal typealias _CanonicalNormalizationBuffer = Unicode._NormDataBuffer<ScalarAndNormData>
  internal typealias _CompatibilityNormalizationBuffer = Unicode._NormDataBuffer<ScalarAndCompatNormData>

  /// A buffer for Normalization data.
  ///
  /// The buffer operates in two modes:
  ///
  /// 1. Elements are inserted using the `append` method
  ///    until the normalizer detects some kind of boundary.
  ///    If required, the contents may then be sorted.
  ///
  /// 2. Elements are consumed by using the `next` method.
  ///    Once it has started consuming elements, the normalizer
  ///    must not append anything until it has consumed the entire buffer
  ///    and `next` has returned `nil`.
  /// 
  internal struct _NormDataBuffer<T> {

    private var storage: [T] = []
    private var isReversed = false

    /// Resets the buffer to its initial state.
    ///
    /// Existing capacity is kept unless it exceeds the given maximum.
    ///
    internal mutating func reset(maximumCapacity: Int) {
      storage.removeAll(keepingCapacity: storage.capacity <= maximumCapacity)
      isReversed = false
    }

    /// Appends an element to the buffer.
    ///
    internal mutating func append(_ element: T) {
      _internalInvariant(!isReversed)
      storage.append(element)
    }

    /// Removes the first element from the buffer. 
    ///
    /// After calling this method, `append` becomes unavailable
    /// until the entire buffer has been consumed.
    ///
    internal mutating func next() -> T? {
      guard !storage.isEmpty else {
        isReversed = false
        return nil
      }
      if !isReversed {
        storage.reverse()
        isReversed = true
      }
      return storage.removeLast()
    }

    internal var isEmpty: Bool {
      storage.isEmpty
    }

    internal var last: T? {
      _internalInvariant(!isReversed)
      return storage.last
    }
  }
}

extension Unicode._CanonicalNormalizationBuffer {

  /// Sorts the buffer in canonical order.
  ///
  /// > Important: The buffer must only contain a single normalization segment.
  ///
  internal mutating func sort() {
    storage._insertionSort(within: storage.indices) {
      $0.normData.ccc < $1.normData.ccc
    }
  }
}

extension Unicode._CompatibilityNormalizationBuffer {

  /// Sorts the buffer in canonical order.
  ///
  /// The buffer may contain multiple normalization segments.
  /// Each segment (between starters) is sorted independently.
  ///
  internal mutating func sort() {
    var segmentStart = storage.startIndex
    var i = segmentStart
    while i < storage.endIndex {
      if storage[i].normData.canonicalCombiningClass == .notReordered {
        storage._insertionSort(within: segmentStart..<i) {
          $0.normData.ccc < $1.normData.ccc
        }
        segmentStart = i
      }
      storage.formIndex(after: &i)
    }
    storage._insertionSort(within: segmentStart..<storage.endIndex) {
      $0.normData.ccc < $1.normData.ccc
    }
  }
}


// -------------------------------------------------------------------------- //
// Decomposition mappings.                                                    //
// -------------------------------------------------------------------------- //


extension Unicode {

  internal struct _CanonicalDecomposition {

    internal let utf8: UnsafeBufferPointer<UInt8>

    internal init?(_ scalar: Unicode.Scalar) {

      // The layout of the underlying 32 bit value we receive is as follows:
      //
      //          Top 14 bits                   Bottom 18 bits
      //
      // 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      // └───────── Index ─────────┘ └───────── Hashed Scalar ─────────┘
      //
      // Index: This is the direct index into '_swift_stdlib_nfd_decompositions'
      //        that points to a size byte indicating the overall size of the
      //        UTF-8 decomposition string. Following the size byte is said string.
      //
      // Hashed Scalar: Because perfect hashing doesn't know the original set of
      //                keys it was hashed with, we store the original scalar in the
      //                decomposition entry so that we can guard against scalars
      //                who happen to hash to the same index.

      let rawValue = _swift_stdlib_getDecompositionEntry(scalar.value)
      let index = Int(truncatingIfNeeded: rawValue >> 18)
      let hashedScalar = (rawValue << 14) >> 14

      guard hashedScalar == scalar.value else {
        return nil
      }

      let decompositionsTable = _swift_stdlib_nfd_decompositions._unsafelyUnwrappedUnchecked

      // First byte of the decomposition entry is its size.
      let size = Int(truncatingIfNeeded: decompositionsTable[index])
      self.utf8 = UnsafeBufferPointer(
        start: decompositionsTable + index + 1 /* size byte */,
        count: size
      )
    }
  }

  internal struct _CompatibilityDecomposition {

    internal let utf8: UnsafeBufferPointer<UInt8>

    internal init?(_ scalar: Unicode.Scalar) {

      // Same layout as Unicode._CanonicalDecomposition.

      let rawValue = _swift_stdlib_getCompatibilityDecompositionEntry(scalar.value)
      let index = Int(truncatingIfNeeded: rawValue >> 18)
      let hashedScalar = (rawValue << 14) >> 14

      guard hashedScalar == scalar.value else {

        // The compatibility decomposition table is recursively expanded,
        // which includes considering canonical decompositions,
        // but it doesn't store its own copy of purely-canonical decompositions.

        guard let canonicalResult = _CanonicalDecomposition(scalar) else {
          return nil
        }

        self.utf8 = canonicalResult.utf8
        return
      }

      let decompositionsTable = _swift_stdlib_nfkd_decompositions._unsafelyUnwrappedUnchecked

      // First byte of the decomposition entry is its size.
      let size = Int(truncatingIfNeeded: decompositionsTable[index])
      self.utf8 = UnsafeBufferPointer(
        start: decompositionsTable + index + 1 /* size byte */,
        count: size
      )
    }
  }
}

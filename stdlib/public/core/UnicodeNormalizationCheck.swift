//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//


// ---------------------------------------------------------------------------//
// Entry Points
// ---------------------------------------------------------------------------//


extension Sequence<Unicode.Scalar> {

  /// Whether this sequence of scalars is in the given normalization form.
  ///
  /// Computing this result is often more efficient
  /// than normalizing and comparing.
  ///
  /// ```swift
  /// let aString = "café"
  ///
  /// aString.unicodeScalars.isNormalized(.nfd) // false
  /// aString.unicodeScalars.isNormalized(.nfc) // true
  /// ```
  ///
  // @available(SwiftStdlib 9999, *)
  @inlinable
  public consuming func isNormalized(
    _ form: Unicode.CanonicalNormalizationForm
  ) -> Bool {
    switch form {
    case .nfd: return _isNFD()
    case .nfc: return _isNFC()
    }
  }

  /// Whether this sequence of scalars is in the given normalization form.
  ///
  /// Computing this result is often more efficient
  /// than normalizing and comparing.
  ///
  /// ```swift
  /// let aString = "Henry Ⅷ"
  ///
  /// aString.unicodeScalars.isNormalized(.nfkd) // false
  /// aString.unicodeScalars.isNormalized(.nfc)  // true
  /// ```
  ///
  // @available(SwiftStdlib 9999, *)
  @inlinable
  public consuming func isNormalized(
    _ form: Unicode.CompatibilityNormalizationForm
  ) -> Bool {
    switch form {
    case .nfkd: return _isNFKD()
    case .nfkc: return _isNFKC()
    }
  }
}

extension Collection<Unicode.Scalar> {

  /// Whether this sequence of scalars is in the given normalization form.
  ///
  /// Computing this result is often more efficient
  /// than normalizing and comparing.
  ///
  /// ```swift
  /// let aString = "café"
  ///
  /// aString.unicodeScalars.isNormalized(.nfd) // false
  /// aString.unicodeScalars.isNormalized(.nfc) // true
  /// ```
  ///
  // @available(SwiftStdlib 9999, *)
  @inlinable
  public func isNormalized(
    _ form: Unicode.CanonicalNormalizationForm
  ) -> Bool {
    switch form {
    case .nfd: return _isNFD()
    case .nfc: return _isNFC()
    }
  }

  /// Whether this sequence of scalars is in the given normalization form.
  ///
  /// Computing this result is often more efficient
  /// than normalizing and comparing.
  ///
  /// ```swift
  /// let aString = "Henry Ⅷ"
  ///
  /// aString.unicodeScalars.isNormalized(.nfkd) // false
  /// aString.unicodeScalars.isNormalized(.nfc)  // true
  /// ```
  ///
  // @available(SwiftStdlib 9999, *)
  @inlinable
  public func isNormalized(
    _ form: Unicode.CompatibilityNormalizationForm
  ) -> Bool {
    switch form {
    case .nfkd: return _isNFKD()
    case .nfkc: return _isNFKC()
    }
  }
}

extension StringProtocol {

  /// Whether this string is in the given normalization form.
  ///
  /// Computing this result is often more efficient
  /// than normalizing and comparing.
  ///
  /// ```swift
  /// let aString = "café"
  ///
  /// aString.isNormalized(.nfd) // false
  /// aString.isNormalized(.nfc) // true
  /// ```
  ///
  // @available(SwiftStdlib 9999, *)
  @_specialize(where Self == String)
  @_specialize(where Self == Substring)
  public func isNormalized(
    _ form: Unicode.CanonicalNormalizationForm
  ) -> Bool {
    self.unicodeScalars.isNormalized(form)
  }

  /// Whether this string is in the given normalization form.
  ///
  /// Computing this result is often more efficient
  /// than normalizing and comparing.
  ///
  /// ```swift
  /// let aString = "Henry Ⅷ"
  ///
  /// aString.isNormalized(.nfkd) // false
  /// aString.isNormalized(.nfc)  // true
  /// ```
  ///
  // @available(SwiftStdlib 9999, *)
  @_specialize(where Self == String)
  @_specialize(where Self == Substring)
  public func isNormalized(
    _ form: Unicode.CompatibilityNormalizationForm
  ) -> Bool {
    self.unicodeScalars.isNormalized(form)
  }
}

extension Character {

  /// Whether this character is in the given normalization form.
  ///
  /// Computing this result is often more efficient
  /// than normalizing and comparing.
  ///
  /// ```swift
  /// let char: Character = "é"
  ///
  /// char.isNormalized(.nfd) // false
  /// char.isNormalized(.nfc) // true
  /// ```
  ///
  // @available(SwiftStdlib 9999, *)
  public func isNormalized(
    _ form: Unicode.CanonicalNormalizationForm
  ) -> Bool {
    self._str.isNormalized(form)
  }

  /// Whether this character is in the given normalization form.
  ///
  /// Computing this result is often more efficient
  /// than normalizing and comparing.
  ///
  /// ```swift
  /// let char: Character = "Ⅷ"
  ///
  /// char.isNormalized(.nfkd) // false
  /// char.isNormalized(.nfc)  // true
  /// ```
  ///
  // @available(SwiftStdlib 9999, *)
  public func isNormalized(
    _ form: Unicode.CompatibilityNormalizationForm
  ) -> Bool {
    self._str.isNormalized(form)
  }
}


// ---------------------------------------------------------------------------//
// NFD (Sequence only)
// ---------------------------------------------------------------------------//


extension Unicode.Scalar {

  // ABI barrier for data required by the NFD Quick Check algorithm.

  // @available(SwiftStdlib 9999, *)
  @usableFromInline
  internal typealias _NFDQuickCheckData = (
    CCC: Unicode.CanonicalCombiningClass,
    isNFDQC: Bool
  )

  // @available(SwiftStdlib 9999, *)
  @usableFromInline
  internal func _getNFDQuickCheckData() -> _NFDQuickCheckData {
    let normdata = Unicode._CanonicalNormData(self)
    return (normdata.canonicalCombiningClass, normdata.isNFDQC)
  }
}

extension Sequence<Unicode.Scalar> {

  // @available(SwiftStdlib 9999, *)
  @inlinable
  internal consuming func _isNFD() -> Bool {

    var lastCanonicalClass = Unicode.CanonicalCombiningClass.notReordered
    var iter = (consume self).makeIterator()

    while let scalar = iter.next() {
      let scalarInfo = scalar._getNFDQuickCheckData()

      if scalarInfo.CCC != .notReordered,
         lastCanonicalClass > scalarInfo.CCC {
        return false
      }

      guard scalarInfo.isNFDQC else {
        return false
      }

      lastCanonicalClass = scalarInfo.CCC
    }
    return true
  }
}


// ---------------------------------------------------------------------------//
// NFKD (Sequence only)
// ---------------------------------------------------------------------------//


extension Unicode.Scalar {

  // ABI barrier for data required by the NFKD Quick Check algorithm.

  // @available(SwiftStdlib 9999, *)
  @usableFromInline
  internal typealias _NFKDQuickCheckData = (
    CCC: Unicode.CanonicalCombiningClass,
    isNFKDQC: Bool
  )

  // @available(SwiftStdlib 9999, *)
  @usableFromInline
  internal func _getNFKDQuickCheckData() -> _NFKDQuickCheckData {
    let normdata = Unicode._CompatibilityNormData(self)
    return (normdata.canonicalCombiningClass, normdata.isNFKDQC)
  }
}

extension Sequence<Unicode.Scalar> {

  // @available(SwiftStdlib 9999, *)
  @inlinable
  internal consuming func _isNFKD() -> Bool {

    var lastCanonicalClass = Unicode.CanonicalCombiningClass.notReordered
    var iter = (consume self).makeIterator()

    while let scalar = iter.next() {
      let scalarInfo = scalar._getNFKDQuickCheckData()

      if scalarInfo.CCC != .notReordered,
         lastCanonicalClass > scalarInfo.CCC {
        return false
      }

      guard scalarInfo.isNFKDQC else {
        return false
      }

      lastCanonicalClass = scalarInfo.CCC
    }
    return true
  }
}


// ---------------------------------------------------------------------------//
// NFC (Collection and Sequence)
// ---------------------------------------------------------------------------//


extension Unicode.Scalar {

  // ABI barrier for data required by the NFC Quick Check algorithm.

  // @available(SwiftStdlib 9999, *)
  @usableFromInline
  internal typealias _NFCQuickCheckData = (
    CCC: Unicode.CanonicalCombiningClass,
    isNFCQC: Unicode.NFCQCResult
  )

  // @available(SwiftStdlib 9999, *)
  @usableFromInline
  internal func _getNFCQuickCheckData() -> _NFCQuickCheckData {
    let normdata = Unicode._CanonicalNormData(onlyCCCAndNFCQC: self)
    return (normdata.canonicalCombiningClass, normdata.isNFCQC_Tristate)
  }
}

extension Collection<Unicode.Scalar> {

  // @available(SwiftStdlib 9999, *)
  @inlinable @inline(never)
  internal func _isNFC() -> Bool {

    var segmentStart = startIndex
    var needsSlowCheck = false
    var lastCanonicalClass = Unicode.CanonicalCombiningClass.notReordered
    var normalizer = Unicode.NFCNormalizer()

    var i = segmentStart
    while i < endIndex {
      let scalar = self[i]
      let scalarInfo = scalar._getNFCQuickCheckData()

      if scalarInfo.CCC != .notReordered,
         lastCanonicalClass > scalarInfo.CCC {
        return false
      }

      switch scalarInfo.isNFCQC {
      case .no:
        return false
      case .yes:
        if scalarInfo.CCC == .notReordered {
          if _slowPath(needsSlowCheck) {
            guard _isNFC_sequence_slow(
              segment: self[segmentStart..<i],
              normalizer: &normalizer
            ) else {
              return false
            }
          }
          needsSlowCheck = false
          segmentStart = i
        }
        lastCanonicalClass = scalarInfo.CCC
        formIndex(after: &i)
      default:
        _internalInvariant(scalarInfo.isNFCQC == .maybe)
        lastCanonicalClass = scalarInfo.CCC
        formIndex(after: &i)
        needsSlowCheck = true
      }
    }

    if _slowPath(needsSlowCheck) {
      return _isNFC_sequence_slow(
        segment: self[segmentStart...],
        normalizer: &normalizer
      )
    }

    return true
  }
}

extension Sequence<Unicode.Scalar> {

  // @available(SwiftStdlib 9999, *)
  @inlinable @inline(never)
  internal consuming func _isNFC() -> Bool {

    // Because Sequence is potentially single-pass,
    // we need to keep the current normalization segment in a buffer
    // in case we later hit a "maybe" scalar.
    var segment = [Unicode.Scalar]()
    var needsSlowCheck = false
    var lastCanonicalClass = Unicode.CanonicalCombiningClass.notReordered
    var normalizer = Unicode.NFCNormalizer()

    // Array needs 16 bytes for count + capacity on 64-bit,
    // so in total this should be a 128 byte allocation.
    segment.reserveCapacity(28)

    var iter = (consume self).makeIterator()
    while let scalar = iter.next() {
      let scalarInfo = scalar._getNFCQuickCheckData()

      if scalarInfo.CCC != .notReordered,
         lastCanonicalClass > scalarInfo.CCC {
        return false
      }

      switch scalarInfo.isNFCQC {
      case .no:
        return false
      case .yes:
        if scalarInfo.CCC == .notReordered {
          if _slowPath(needsSlowCheck) {
            guard _isNFC_sequence_slow(
              segment: segment,
              normalizer: &normalizer
            ) else {
              return false
            }
          }
          needsSlowCheck = false
          segment.removeAll(keepingCapacity: true)
        }
        lastCanonicalClass = scalarInfo.CCC
        segment.append(scalar)
      default:
        _internalInvariant(scalarInfo.isNFCQC == .maybe)
        lastCanonicalClass = scalarInfo.CCC
        segment.append(scalar)
        needsSlowCheck = true
      }
    }

    if _slowPath(needsSlowCheck) {
      return _isNFC_sequence_slow(
        segment: segment,
        normalizer: &normalizer
      )
    }

    return true
  }
}

// @available(SwiftStdlib 9999, *)
@inlinable
internal func _isNFC_sequence_slow(
  segment: some Collection<Unicode.Scalar>,
  normalizer: inout Unicode.NFCNormalizer
) -> Bool {

  normalizer.reset()

  var original = segment.makeIterator()
  var input = segment.makeIterator()
  while let s = normalizer.resume(consuming: &input) ?? normalizer.flush() {
    guard original.next() == s else {
      return false
    }
  }
  return (original.next() == nil)
}


// ---------------------------------------------------------------------------//
// NFKC (Collection and Sequence)
// ---------------------------------------------------------------------------//


extension Unicode.Scalar {

  // ABI barrier for data required by the NFKC Quick Check algorithm.

  // @available(SwiftStdlib 9999, *)
  @usableFromInline
  internal typealias _NFKCQuickCheckData = (
    CCC: Unicode.CanonicalCombiningClass,
    isNFKCQC: Unicode.NFCQCResult
  )

  // @available(SwiftStdlib 9999, *)
  @usableFromInline
  internal func _getNFKCQuickCheckData() -> _NFKCQuickCheckData {
    let normdata = Unicode._CompatibilityNormData(self)
    return (normdata.canonicalCombiningClass, normdata.isNFKCQC)
  }
}

extension Collection<Unicode.Scalar> {

  // @available(SwiftStdlib 9999, *)
  @inlinable @inline(never)
  internal func _isNFKC() -> Bool {

    var segmentStart = startIndex
    var needsSlowCheck = false
    var lastCanonicalClass = Unicode.CanonicalCombiningClass.notReordered
    var normalizer = Unicode.NFKCNormalizer()

    var i = segmentStart
    while i < endIndex {
      let scalar = self[i]
      let scalarInfo = scalar._getNFKCQuickCheckData()

      if scalarInfo.CCC != .notReordered,
         lastCanonicalClass > scalarInfo.CCC {
        return false
      }

      switch scalarInfo.isNFKCQC {
      case .no:
        return false
      case .yes:
        if scalarInfo.CCC == .notReordered {
          if _slowPath(needsSlowCheck) {
            guard _isNFKC_sequence_slow(
              segment: self[segmentStart..<i],
              normalizer: &normalizer
            ) else {
              return false
            }
          }
          needsSlowCheck = false
          segmentStart = i
        }
        lastCanonicalClass = scalarInfo.CCC
        formIndex(after: &i)
      default:
        _internalInvariant(scalarInfo.isNFKCQC == .maybe)
        lastCanonicalClass = scalarInfo.CCC
        formIndex(after: &i)
        needsSlowCheck = true
      }
    }

    if _slowPath(needsSlowCheck) {
      return _isNFKC_sequence_slow(
        segment: self[segmentStart...],
        normalizer: &normalizer
      )
    }

    return true
  }
}

extension Sequence<Unicode.Scalar> {

  // @available(SwiftStdlib 9999, *)
  @inlinable @inline(never)
  internal consuming func _isNFKC() -> Bool {

    // Because Sequence is potentially single-pass,
    // we need to keep the current normalization segment in a buffer
    // in case we later hit a "maybe" scalar.
    var segment = [Unicode.Scalar]()
    var needsSlowCheck = false
    var lastCanonicalClass = Unicode.CanonicalCombiningClass.notReordered
    var normalizer = Unicode.NFKCNormalizer()

    // Array needs 16 bytes for count + capacity on 64-bit,
    // so in total this should be a 128 byte allocation.
    segment.reserveCapacity(28)

    var iter = (consume self).makeIterator()
    while let scalar = iter.next() {
      let scalarInfo = scalar._getNFKCQuickCheckData()

      if scalarInfo.CCC != .notReordered,
         lastCanonicalClass > scalarInfo.CCC {
        return false
      }

      switch scalarInfo.isNFKCQC {
      case .no:
        return false
      case .yes:
        if scalarInfo.CCC == .notReordered {
          if _slowPath(needsSlowCheck) {
            guard _isNFKC_sequence_slow(
              segment: segment,
              normalizer: &normalizer
            ) else {
              return false
            }
          }
          needsSlowCheck = false
          segment.removeAll(keepingCapacity: true)
        }
        lastCanonicalClass = scalarInfo.CCC
        segment.append(scalar)
      default:
        _internalInvariant(scalarInfo.isNFKCQC == .maybe)
        lastCanonicalClass = scalarInfo.CCC
        segment.append(scalar)
        needsSlowCheck = true
      }
    }

    if _slowPath(needsSlowCheck) {
      return _isNFKC_sequence_slow(
        segment: segment,
        normalizer: &normalizer
      )
    }

    return true
  }
}

// @available(SwiftStdlib 9999, *)
@inlinable
internal func _isNFKC_sequence_slow(
  segment: some Collection<Unicode.Scalar>,
  normalizer: inout Unicode.NFKCNormalizer
) -> Bool {

  normalizer.reset()

  var original = segment.makeIterator()
  var input = segment.makeIterator()
  while let s = normalizer.resume(consuming: &input) ?? normalizer.flush() {
    guard original.next() == s else {
      return false
    }
  }
  return (original.next() == nil)
}

@available(SwiftStdlib 6.1, *)
extension UTF8Span {
  /// Returns whether the validated contents were all-ASCII. This is checked at
  /// initialization time and remembered.
  @_alwaysEmitIntoClient
  public var isASCII: Bool {
    0 != _countAndFlags & Self._asciiBit
  }


  /// Returns whether the contents are known to be NFC. This is not
  /// always checked at initialization time and is set by `checkForNFC`.
  @_unavailableInEmbedded
  @_alwaysEmitIntoClient
  public var isKnownNFC: Bool {
    0 != _countAndFlags & Self._nfcBit
  }

  /// Returns whether the contents are a null-terminated C string. If true, there
  /// is a guaranteed null byte after the end of `count` and no null bytes stored
  /// within the span
  @_alwaysEmitIntoClient
  public var isNullTerminatedCString: Bool {
    0 != _countAndFlags & Self._nullTerminatedCStringBit
  }

  // Takes a paremeter because `extracting` may need to un-set the bit
  @_alwaysEmitIntoClient
  internal mutating func _setIsNullTerminatedCString(_ value: Bool) {
    if value {
      _countAndFlags |= Self._nullTerminatedCStringBit
    } else {
      _countAndFlags &= ~Self._nullTerminatedCStringBit
    }
    _invariantCheck()
  }

  // Set the isASCII bit to true (also isNFC)
  @_alwaysEmitIntoClient
  internal mutating func _setIsASCII() {
    self._countAndFlags |= Self._asciiBit | Self._nfcBit
  }

  /// Do a scan checking for whether the contents are in Normal Form C.
  /// When the contents are in NFC, canonical equivalence checks are much
  /// faster.
  ///
  /// `quickCheck` will check for a subset of NFC contents using the 
  /// NFCQuickCheck algorithm, which is faster than the full normalization
  /// algorithm. However, it cannot detect all NFC contents.
  ///
  /// Updates the `isKnownNFC` bit.
  @_unavailableInEmbedded
  public mutating func checkForNFC(
    quickCheck: Bool
  ) -> Bool {
    if isKnownNFC { return true }

    if quickCheck {
      var cur = 0
      while cur < count {
        let (s, next) = decodeNextScalar(cur)
        cur = next
        if s.value < 0x300 {
          continue
        }
        // TODO: Check (internal) Unicode NFCQuickCheck=YES property
        return false
      }
      self._countAndFlags |= Self._nfcBit
      return true
    }

    // TODO: use faster internal algorithm
    let normalized = _str._nfcCodeUnits
    guard unsafeBaseAddress._urbp(
      0..<count
    ).elementsEqual(normalized) else {
      return false
    }

    self._countAndFlags |= Self._nfcBit
    return true
  }

  /// Returns whether every `Character` (i.e. grapheme cluster)
  /// is known to be comprised of a single `Unicode.Scalar`.
  ///
  /// This is not always checked at initialization time. It is set by
  /// `checkForSingleScalarCharacters`.
  @_unavailableInEmbedded
  @_alwaysEmitIntoClient
  public var isKnownSingleScalarCharacters: Bool {
    0 != _countAndFlags & Self._singleScalarCharactersBit
  }

  /// Do a scan, checking whether every `Character` (i.e. grapheme cluster)
  /// is comprised of only a single `Unicode.Scalar`. When a span contains
  /// only single-scalar characters, character operations are much faster.
  ///
  /// `quickCheck` will check for a subset of single-scalar character contents
  /// using a faster algorithm than the full grapheme breaking algorithm.
  /// However, it cannot detect all single-scalar `Character` contents.
  ///
  /// Updates the `isKnownSingleScalarCharacters` bit.
  @_unavailableInEmbedded
  public mutating func checkForSingleScalarCharacters(
    quickCheck: Bool
  ) -> Bool {
    if isKnownSingleScalarCharacters { return true }

    if quickCheck {
      var idx = 0
      var currentScalar: Unicode.Scalar? = nil
      while idx < count {
        let (scalar, next) = decodeNextScalar(idx)

        if let cur = currentScalar {
          guard true == _quickHasGraphemeBreakBetween(cur, scalar) else {
            return false
          }
        }

        currentScalar = scalar
        idx = next
      }

      self._countAndFlags |= Self._singleScalarCharactersBit
      return true
    }

    var idx = 0
    while idx < count {
      let nextIdx = nextCharacterStart(uncheckedAssumingAligned: idx)
      guard nextIdx == nextScalarStart(idx) else {
        return false
      }
      idx = nextIdx
    }

    self._countAndFlags |= Self._singleScalarCharactersBit
    return true
  }
}

@available(SwiftStdlib 6.1, *)
extension UTF8Span {
  @_alwaysEmitIntoClient @inline(__always)
  internal static var _asciiBit: UInt64 {
    0x8000_0000_0000_0000
  }

  @_alwaysEmitIntoClient @inline(__always)
  internal static var _nfcBit: UInt64 {
    0x4000_0000_0000_0000
  }

  @_alwaysEmitIntoClient @inline(__always)
  internal static var _singleScalarCharactersBit: UInt64 {
    0x2000_0000_0000_0000
  }

  @_alwaysEmitIntoClient @inline(__always)
  internal static var _nullTerminatedCStringBit: UInt64 {
    0x1000_0000_0000_0000
  }

  @_alwaysEmitIntoClient @inline(__always)
  internal static var _countMask: UInt64 {
    0x00FF_FFFF_FFFF_FFFF
  }

  @_alwaysEmitIntoClient @inline(__always)
  internal static var _flagsMask: UInt64 {
    0xFF00_0000_0000_0000
  }

  @_alwaysEmitIntoClient
  public var count: Int {
    Int(truncatingIfNeeded: _countAndFlags & Self._countMask)
  }

//  @_alwaysEmitIntoClient @inline(__always)
//  internal var _end: UnsafeRawPointer {
//    unsafeBaseAddress.advanced(by: _byteCount)
//  }
}



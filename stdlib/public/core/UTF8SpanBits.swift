@available(SwiftStdlib 6.1, *)
extension UTF8Span {
  /// Returns whether contents are known to be all-ASCII. A return value of
  /// `true` means that all code units are ASCII. A return value of `false`
  /// means there _may_ be non-ASCII content.
  ///
  /// ASCII-ness is checked and remembered during UTF-8 validation, so this
  /// is often equivalent to is-ASCII, but there are some situations where
  /// we might return `false` even when the content happens to be all-ASCII.
  ///
  /// For example, a UTF-8 span generated from a `String` that at some point
  /// contained non-ASCII content would report false for `isKnownASCII`, even
  /// if that String had subsequent mutation operations that removed any
  /// non-ASCII content.
  @_alwaysEmitIntoClient
  public var isKnownASCII: Bool {
    0 != _countAndFlags & Self._asciiBit
  }

  /// Do a scan checking for whether the contents are all-ASCII.
  ///
  /// Updates the `isKnownASCII` bit if contents are all-ASCII.
  public mutating func checkForASCII() -> Bool {
    fatalError()
  }

  /// Returns whether the contents are known to be NFC. This is not
  /// always checked at initialization time and is set by `checkForNFC`.
  // TODO: should this be @_unavailableInEmbedded
  @_alwaysEmitIntoClient
  public var isKnownNFC: Bool {
    0 != _countAndFlags & Self._nfcBit
  }

  // Set the isKnownASCII bit to true (also isNFC)
  @_alwaysEmitIntoClient
  internal mutating func _setIsASCII() {
    self._countAndFlags |= Self._asciiBit | Self._nfcBit
  }

  // Set the isKnownNFC bit to true (also isNFC)
  @_alwaysEmitIntoClient
  internal mutating func _setIsNFC() {
    self._countAndFlags |= Self._nfcBit
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
        let (s, next) = _decodeNextScalar(uncheckedAssumingAligned: cur)
        cur = next
        if s.value < 0x300 {
          continue
        }
        TODO("Refactor and adapt _StringGutsSlice._fastNFCCheck()")
      }
      self._countAndFlags |= Self._nfcBit
      return true
    }

    // TODO: use faster internal algorithm
    let normalized = _str._nfcCodeUnits
    guard _start()._urbp(
      0..<count
    ).elementsEqual(normalized) else {
      return false
    }

    self._countAndFlags |= Self._nfcBit
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



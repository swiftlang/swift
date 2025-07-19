// TODO: comment header


@available(SwiftStdlib 6.2, *)
extension UTF8Span {
  /// Whether this span has the same bytes as `other`.
  ///
  /// - Complexity: O(n)
  @_alwaysEmitIntoClient
  public func bytesEqual(to other: some Sequence<UInt8>) -> Bool {
    unsafe _withUnsafeBufferPointer { unsafe $0.elementsEqual(other) }
  }

  /// Whether this span has the same `Unicode.Scalar`s as `other`.
  ///
  /// - Complexity: O(n)
  @_alwaysEmitIntoClient
  public func unicodeScalarsEqual(
    to other: some Sequence<Unicode.Scalar>
  ) -> Bool {
    // TODO: We don't need to decode our code units, we can just match
    // against their scalars' encoded bytes

    var scalars = makeUnicodeScalarIterator()
    var otherScalars = other.makeIterator()
    while let s = scalars.next() {
      guard let otherS = otherScalars.next(), s == otherS else {
        return false
      }
    }
    guard scalars.next() == nil else {
      return false
    }
    return true
  }

  /// Whether this span has the same `Character`s as `other`.
  ///
  /// - Complexity: O(n)
  @_unavailableInEmbedded
  @_alwaysEmitIntoClient
  public func charactersEqual(
    to other: some Sequence<Character>
  ) -> Bool {
    var chars = makeCharacterIterator()
    var otherChars = other.makeIterator()
    while let c = chars.next() {
      guard let otherC = otherChars.next(), c == otherC else {
        return false
      }
    }
    guard chars.next() == nil else {
      return false
    }
    return true
  }
}

@available(SwiftStdlib 6.2, *)
extension UTF8Span {
  /// Whether `self` is equivalent to `other` under Unicode Canonical
  /// Equivalence.
  ///
  /// - Complexity: O(n)
  public func isCanonicallyEquivalent(
    to other: UTF8Span
  ) -> Bool {
    unsafe self._withUnsafeBufferPointer { selfBufPtr in
      unsafe other._withUnsafeBufferPointer { otherBufPtr in
        unsafe _stringCompareFastUTF8(
          selfBufPtr,
          otherBufPtr,
          expecting: .equal,
          bothNFC: self.isKnownNFC && other.isKnownNFC)
      }
    }
  }

  /// Whether `self` orders less than `other` under Unicode Canonical
  /// Equivalence using normalized code-unit order (in NFC).
  ///
  /// - Complexity: O(n)
  public func isCanonicallyLessThan(
    _ other: UTF8Span
  ) -> Bool {
    unsafe self._withUnsafeBufferPointer { selfBufPtr in
      unsafe other._withUnsafeBufferPointer { otherBufPtr in
        unsafe _stringCompareFastUTF8(
          selfBufPtr,
          otherBufPtr,
          expecting: .less,
          bothNFC: self.isKnownNFC && other.isKnownNFC)
      }
    }
  }
}

// // FIXME: remove
// @available(SwiftStdlib 6.2, *)
// extension UTF8Span {
//   public static func ~=(_ lhs: StaticString, _ rhs: UTF8Span) -> Bool {
//     return lhs.withUTF8Buffer { str in
//       rhs._withUnsafeBufferPointer { span in
//         str.elementsEqual(span)
//       }
//     }
//   }
// }



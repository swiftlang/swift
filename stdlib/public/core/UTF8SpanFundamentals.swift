// Core Scalar API
@available(SwiftStdlib 6.1, *)
extension UTF8Span {
  /// Whether `i` is on a boundary between Unicode scalar values.
  @_alwaysEmitIntoClient
  internal func isScalarAligned(_ i: Int) -> Bool {
    if i == count || i == 0 { return true }
    precondition(boundsCheck(i))
    return isScalarAligned(unchecked: i)
  }

  /// Whether `i` is on a boundary between Unicode scalar values.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func isScalarAligned(unchecked i: Int) -> Bool {
    if i == count || i == 0 { return true }
    _internalInvariant(boundsCheck(i))
    return unsafeBaseAddress._isScalarAligned(i)
  }

  /// Whether `range`'s bounds are aligned to `Unicode.Scalar` boundaries.
  @_alwaysEmitIntoClient
  internal func isScalarAligned(_ range: Range<Int>) -> Bool {
    isScalarAligned(range.lowerBound) && isScalarAligned(range.upperBound)
  }

  /// Whether `range`'s bounds are aligned to `Unicode.Scalar` boundaries.
  ///
  /// This function does not validate that `range` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func isScalarAligned(unchecked range: Range<Int>) -> Bool {
    isScalarAligned(unchecked: range.lowerBound)
    && isScalarAligned(unchecked: range.upperBound)
  }

  /// Returns the start of the next `Unicode.Scalar` after the one starting at
  /// `i`, or the end of the span if `i` denotes the final scalar.
  ///
  /// `i` must be scalar-aligned.
  @_alwaysEmitIntoClient
  internal func nextScalarStart(_ i: Int) -> Int {
    precondition(boundsCheck(i))
    return nextScalarStart(unchecked: i)
  }

  /// Returns the start of the next `Unicode.Scalar` after the one starting at
  /// `i`, or the end of the span if `i` denotes the final scalar.
  ///
  /// `i` must be scalar-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func nextScalarStart(unchecked i: Int) -> Int {
    _internalInvariant(boundsCheck(i))
    precondition(isScalarAligned(i))
    return nextScalarStart(uncheckedAssumingAligned: i)
  }

  /// Returns the start of the next `Unicode.Scalar` after the one starting at
  /// `i`, or the end of the span if `i` denotes the final scalar.
  ///
  /// `i` must be scalar-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  ///
  /// This function does not validate that `i` is scalar-aligned; this is an
  /// unsafe operation if `i` isn't.
  @_alwaysEmitIntoClient
  internal func nextScalarStart(
    uncheckedAssumingAligned i: Int
  ) -> Int {
    _internalInvariant(boundsCheck(i))
    _internalInvariant(isScalarAligned(i))
    return unsafeBaseAddress._nextScalarStart(i)
  }

  /// Returns the start of the `Unicode.Scalar` ending at `i`, i.e. the scalar
  /// before the one starting at `i` or the last scalar if `i` is the end of
  /// the span.
  ///
  /// `i` must be scalar-aligned.
  @_alwaysEmitIntoClient
  internal func previousScalarStart(_ i: Int) -> Int {
    precondition(boundsCheck(i&-1))
    return previousScalarStart(unchecked: i)
  }

  /// Returns the start of the `Unicode.Scalar` ending at `i`, i.e. the scalar
  /// before the one starting at `i` or the last scalar if `i` is the end of
  /// the span.
  ///
  /// `i` must be scalar-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func previousScalarStart(unchecked i: Int) -> Int {
    _internalInvariant(boundsCheck(i&-1))
    precondition(isScalarAligned(i))
    return previousScalarStart(uncheckedAssumingAligned: i)
  }

  /// Returns the start of the `Unicode.Scalar` ending at `i`, i.e. the scalar
  /// before the one starting at `i` or the last scalar if `i` is the end of
  /// the span.
  ///
  /// `i` must be scalar-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  ///
  ///
  /// This function does not validate that `i` is scalar-aligned; this is an
  /// unsafe operation if `i` isn't.
  @_alwaysEmitIntoClient
  internal func previousScalarStart(
    uncheckedAssumingAligned i: Int
  ) -> Int {
    _internalInvariant(boundsCheck(i&-1))
    _internalInvariant(isScalarAligned(i))
    return unsafeBaseAddress._previousScalarStart(i)
  }

  /// Decode the `Unicode.Scalar` starting at `i`. Return it and the start of
  /// the next scalar.
  ///
  /// `i` must be scalar-aligned.
  @_alwaysEmitIntoClient
  internal func decodeNextScalar(
    _ i: Int
  ) -> (Unicode.Scalar, nextScalarStart: Int) {
    precondition(boundsCheck(i))
    return decodeNextScalar(unchecked: i)
  }

  /// Decode the `Unicode.Scalar` starting at `i`. Return it and the start of
  /// the next scalar.
  ///
  /// `i` must be scalar-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func decodeNextScalar(
    unchecked i: Int
  ) -> (Unicode.Scalar, nextScalarStart: Int) {
    _internalInvariant(boundsCheck(i))
    precondition(isScalarAligned(i))
    return decodeNextScalar(uncheckedAssumingAligned: i)
  }

  /// Decode the `Unicode.Scalar` starting at `i`. Return it and the start of
  /// the next scalar.
  ///
  /// `i` must be scalar-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  ///
  ///
  /// This function does not validate that `i` is scalar-aligned; this is an
  /// unsafe operation if `i` isn't.
  @_alwaysEmitIntoClient
  internal func decodeNextScalar(
    uncheckedAssumingAligned i: Int
  ) -> (Unicode.Scalar, nextScalarStart: Int) {
    _internalInvariant(boundsCheck(i))
    _internalInvariant(isScalarAligned(i))
    return unsafeBaseAddress._decodeScalar(startingAt: i)
  }

  /// Decode the `Unicode.Scalar` ending at `i`, i.e. the previous scalar.
  /// Return it and the start of that scalar.
  ///
  /// `i` must be scalar-aligned.
  @_alwaysEmitIntoClient
  internal func decodePreviousScalar(
    _ i: Int
  ) -> (Unicode.Scalar, previousScalarStart: Int) {
    precondition(boundsCheck(i &- 1))
    return decodePreviousScalar(unchecked: i)
  }

  /// Decode the `Unicode.Scalar` ending at `i`, i.e. the previous scalar.
  /// Return it and the start of that scalar.
  ///
  /// `i` must be scalar-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func decodePreviousScalar(
    unchecked i: Int
  ) -> (Unicode.Scalar, previousScalarStart: Int) {
    _internalInvariant(boundsCheck(i &- 1))
    precondition(isScalarAligned(i))
    return decodePreviousScalar(uncheckedAssumingAligned: i)
  }

  /// Decode the `Unicode.Scalar` ending at `i`, i.e. the previous scalar.
  /// Return it and the start of that scalar.
  ///
  /// `i` must be scalar-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  ///
  ///
  /// This function does not validate that `i` is scalar-aligned; this is an
  /// unsafe operation if `i` isn't.
  @_alwaysEmitIntoClient
  internal func decodePreviousScalar(
    uncheckedAssumingAligned i: Int
  ) -> (Unicode.Scalar, previousScalarStart: Int) {
    _internalInvariant(boundsCheck(i &- 1))
    _internalInvariant(isScalarAligned(i))
    return unsafeBaseAddress._decodeScalar(endingAt: i)
  }
}

// Derived Scalar API
@available(SwiftStdlib 6.1, *)
extension UTF8Span {
  /// Find the nearest scalar-aligned position `<= i`.
  @_alwaysEmitIntoClient
  internal func scalarAlignBackwards(_ i: Int) -> Int {
    precondition(boundsCheck(i))
    return scalarAlignBackwards(unchecked: i)
  }

  /// Find the nearest scalar-aligned position `<= i`.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func scalarAlignBackwards(unchecked i: Int) -> Int {
    _internalInvariant(boundsCheck(i))
    return unsafeBaseAddress._scalarAlign(i)
  }

  /// Find the nearest scalar-aligned position `>= i`.
  @_alwaysEmitIntoClient
  internal func scalarAlignForwards(_ i: Int) -> Int {
    var i = i
    while _slowPath(!isScalarAligned(i)) {
      i &+= 1
    }
    return i
  }

  /// Find the nearest scalar-aligned position `>= i`.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func scalarAlignForwards(unchecked i: Int) -> Int {
    var i = i
    while _slowPath(!isScalarAligned(unchecked: i)) {
      i &+= 1
    }
    return i
  }
}

// Core Character API
@available(SwiftStdlib 6.1, *)
extension UTF8Span {
  // TODO: Single-scalar fast paths

  /// Whether `i` is on a boundary between `Character`s (i.e. grapheme
  /// clusters).
  @_alwaysEmitIntoClient
  internal func isCharacterAligned(_ i: Int) -> Bool {
    if i == count || i == 0 { return true }
    precondition(boundsCheck(i))
    return isCharacterAligned(unchecked: i)
  }

  /// Whether `i` is on a boundary between `Character`s (i.e. grapheme
  /// clusters).
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func isCharacterAligned(unchecked i: Int) -> Bool {
    if i == count || i == 0 { return true }
    _internalInvariant(boundsCheck(i))
    return unsafeBaseAddress._isCharacterAligned(i, limitedBy: count)
  }

  /// Returns the start of the next `Character` (i.e. grapheme cluster) after
  /// the one  starting at `i`, or the end of the span if `i` denotes the final
  /// `Character`.
  ///
  /// `i` must be `Character`-aligned.
  @_alwaysEmitIntoClient
  internal func nextCharacterStart(_ i: Int) -> Int {
    precondition(boundsCheck(i))
    return nextCharacterStart(unchecked: i)
  }

  /// Returns the start of the next `Character` (i.e. grapheme cluster) after
  /// the one  starting at `i`, or the end of the span if `i` denotes the final
  /// `Character`.
  ///
  /// `i` must be `Character`-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func nextCharacterStart(unchecked i: Int) -> Int {
    _internalInvariant(boundsCheck(i))
    precondition(isCharacterAligned(i))
    return nextCharacterStart(uncheckedAssumingAligned: i)
  }

  /// Returns the start of the next `Character` (i.e. grapheme cluster) after
  /// the one  starting at `i`, or the end of the span if `i` denotes the final
  /// `Character`.
  ///
  /// `i` must be `Character`-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  ///
  /// This function does not validate that `i` is `Character`-aligned; this is
  /// an unsafe operation if `i` isn't.
  @_alwaysEmitIntoClient
  internal func nextCharacterStart(
    uncheckedAssumingAligned i: Int
  ) -> Int {
    _internalInvariant(boundsCheck(i))
    _internalInvariant(isCharacterAligned(i))
    return unsafeBaseAddress._nextCharacterStart(i, limitedBy: count)
  }

  /// Returns the start of the `Character` (i.e. grapheme cluster) ending at
  /// `i`, i.e. the `Character` before the one starting at `i` or the last
  /// `Character` if `i` is the end of the span.
  ///
  /// `i` must be `Character`-aligned.
  @_alwaysEmitIntoClient
  internal func previousCharacterStart(_ i: Int) -> Int {
    precondition(boundsCheck(i&-1))
    return previousCharacterStart(unchecked: i)
  }

  /// Returns the start of the `Character` (i.e. grapheme cluster) ending at
  /// `i`, i.e. the `Character` before the one starting at `i` or the last
  /// `Character` if `i` is the end of the span.
  ///
  /// `i` must be `Character`-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func previousCharacterStart(unchecked i: Int) -> Int {
    _internalInvariant(boundsCheck(i&-1))
    precondition(isCharacterAligned(i))
    return previousCharacterStart(uncheckedAssumingAligned: i)
  }

  /// Returns the start of the `Character` (i.e. grapheme cluster) ending at
  /// `i`, i.e. the `Character` before the one starting at `i` or the last
  /// `Character` if `i` is the end of the span.
  ///
  /// `i` must be `Character`-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  ///
  /// This function does not validate that `i` is `Character`-aligned; this is
  /// an unsafe operation if `i` isn't.
  @_alwaysEmitIntoClient
  internal func previousCharacterStart(
    uncheckedAssumingAligned i: Int
  ) -> Int {
    _internalInvariant(boundsCheck(i&-1))
    _internalInvariant(isCharacterAligned(i))
    return unsafeBaseAddress._previousCharacterStart(i, limitedBy: count)
  }

  /// Decode the `Character` starting at `i` Return it and the start of the
  /// next `Character`.
  ///
  /// `i` must be `Character`-aligned.
  @_alwaysEmitIntoClient
  internal func decodeNextCharacter(
    _ i: Int
  ) -> (Character, nextCharacterStart: Int) {
    precondition(boundsCheck(i))
    return decodeNextCharacter(unchecked: i)
  }

  /// Decode the `Character` starting at `i` Return it and the start of the
  /// next `Character`.
  ///
  /// `i` must be `Character`-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func decodeNextCharacter(
    unchecked i: Int
  ) -> (Character, nextCharacterStart: Int) {
    _internalInvariant(boundsCheck(i))
    precondition(isCharacterAligned(i))
    return decodeNextCharacter(uncheckedAssumingAligned: i)
  }

  /// Decode the `Character` starting at `i` Return it and the start of the
  /// next `Character`.
  ///
  /// `i` must be `Character`-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  ///
  /// This function does not validate that `i` is `Character`-aligned; this is
  /// an unsafe operation if `i` isn't.
  @_alwaysEmitIntoClient
  internal func decodeNextCharacter(
    uncheckedAssumingAligned i: Int
  ) -> (Character, nextCharacterStart: Int) {
    _internalInvariant(boundsCheck(i))
    _internalInvariant(isCharacterAligned(i))
    return unsafeBaseAddress._decodeCharacter(
      startingAt: i, limitedBy: count)
  }

  /// Decode the `Character` (i.e. grapheme cluster) ending at `i`, i.e. the
  /// previous `Character`. Return it and the start of that `Character`.
  ///
  /// `i` must be `Character`-aligned.
  @_alwaysEmitIntoClient
  internal func decodePreviousCharacter(_ i: Int) -> (Character, Int) {
    precondition(boundsCheck(i &- 1))
    return decodePreviousCharacter(unchecked: i)
  }

  /// Decode the `Character` (i.e. grapheme cluster) ending at `i`, i.e. the
  /// previous `Character`. Return it and the start of that `Character`.
  ///
  /// `i` must be `Character`-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func decodePreviousCharacter(
    unchecked i: Int
  ) -> (Character, Int) {
    _internalInvariant(boundsCheck(i &- 1))
    precondition(isCharacterAligned(i))
    return decodePreviousCharacter(uncheckedAssumingAligned: i)
  }

  /// Decode the `Character` (i.e. grapheme cluster) ending at `i`, i.e. the
  /// previous `Character`. Return it and the start of that `Character`.
  ///
  /// `i` must be `Character`-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  ///
  /// This function does not validate that `i` is `Character`-aligned; this is
  /// an unsafe operation if `i` isn't.
  @_alwaysEmitIntoClient
  internal func decodePreviousCharacter(
    uncheckedAssumingAligned i: Int
  ) -> (Character, Int) {
    _internalInvariant(boundsCheck(i &- 1))
    _internalInvariant(isCharacterAligned(i))
    return unsafeBaseAddress._decodeCharacter(
      endingAt: i, limitedBy: count)
  }

}

// Derived Character API
@available(SwiftStdlib 6.1, *)
extension UTF8Span {
  /// Find the nearest `Character` (i.e. grapheme cluster)-aligned position
  /// that is `<= i`.
  @_alwaysEmitIntoClient
  internal func characterAlignBackwards(_ i: Int) -> Int {
    precondition(i == count || boundsCheck(i))
    return characterAlignBackwards(unchecked: i)
  }

  /// Find the nearest `Character` (i.e. grapheme cluster)-aligned position
  /// that is `<= i`.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func characterAlignBackwards(unchecked i: Int) -> Int {
    _internalInvariant(i == count || boundsCheck(i))
    var i = i
    while _slowPath(!isCharacterAligned(unchecked: i)) {
      i &-= 1
    }
    return i
  }

  /// Find the nearest `Character` (i.e. grapheme cluster)-aligned position
  /// that is `>= i`.
  @_alwaysEmitIntoClient
  internal func characterAlignForwards(_ i: Int) -> Int {
    precondition(i == count || boundsCheck(i))
    return characterAlignForwards(unchecked: i)
  }

  /// Find the nearest `Character` (i.e. grapheme cluster)-aligned position
  /// that is `>= i`.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  @_alwaysEmitIntoClient
  internal func characterAlignForwards(unchecked i: Int) -> Int {
    _internalInvariant(i == count || boundsCheck(i))
    var i = i
    while _slowPath(!isCharacterAligned(unchecked: i)) {
      i &+= 1
    }
    return i
  }
}

// TODO: internal?
@available(SwiftStdlib 6.1, *)
extension UTF8Span {
  /// Whether `i` is in bounds
  @_alwaysEmitIntoClient
  internal func boundsCheck(_ i: Int) -> Bool {
    i >= 0 && i < count
  }
  /// Whether `bounds` is in bounds
  @_alwaysEmitIntoClient
  internal func boundsCheck(_ bounds: Range<Int>) -> Bool {
    boundsCheck(bounds.lowerBound)
    && boundsCheck(bounds.upperBound &- 1)
  }
}

// Future work: UTF-16 support when we get views

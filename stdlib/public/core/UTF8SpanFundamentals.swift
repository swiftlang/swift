// Core Scalar API
@available(SwiftStdlib 6.2, *)
extension UTF8Span {
  /// Whether `i` is on a boundary between Unicode scalar values.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  internal func _isScalarAligned(unchecked i: Int) -> Bool {
    if i == count || i == 0 { return true }
    _internalInvariant(_boundsCheck(i))
    return unsafe _start()._isScalarAligned(i)
  }

  /// Returns the start of the `Unicode.Scalar` ending at `i`, i.e. the scalar
  /// before the one starting at `i` or the last scalar if `i` is the end of
  /// the span.
  ///
  /// `i` must be scalar-aligned.
  internal func _previousScalarStart(_ i: Int) -> Int {
    _precondition(_boundsCheck(i&-1))
    return _previousScalarStart(unchecked: i)
  }

  /// Returns the start of the `Unicode.Scalar` ending at `i`, i.e. the scalar
  /// before the one starting at `i` or the last scalar if `i` is the end of
  /// the span.
  ///
  /// `i` must be scalar-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  internal func _previousScalarStart(unchecked i: Int) -> Int {
    _internalInvariant(_boundsCheck(i&-1))
    _precondition(_isScalarAligned(unchecked: i))
    return _previousScalarStart(uncheckedAssumingAligned: i)
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
  internal func _previousScalarStart(
    uncheckedAssumingAligned i: Int
  ) -> Int {
    _internalInvariant(_boundsCheck(i&-1))
    _internalInvariant(_isScalarAligned(unchecked: i))
    return unsafe _start()._previousScalarStart(i)
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
  internal func _decodeNextScalar(
    uncheckedAssumingAligned i: Int
  ) -> (Unicode.Scalar, nextScalarStart: Int) {
    _internalInvariant(_boundsCheck(i))
    _internalInvariant(_isScalarAligned(unchecked: i))
    return unsafe _start()._decodeScalar(startingAt: i)
  }

  /// Decode the `Unicode.Scalar` ending at `i`, i.e. the previous scalar.
  /// Return it and the start of that scalar.
  ///
  /// `i` must be scalar-aligned.
  internal func _decodePreviousScalar(
    _ i: Int
  ) -> (Unicode.Scalar, previousScalarStart: Int) {
    _precondition(_boundsCheck(i &- 1))
    return _decodePreviousScalar(unchecked: i)
  }

  /// Decode the `Unicode.Scalar` ending at `i`, i.e. the previous scalar.
  /// Return it and the start of that scalar.
  ///
  /// `i` must be scalar-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  internal func _decodePreviousScalar(
    unchecked i: Int
  ) -> (Unicode.Scalar, previousScalarStart: Int) {
    _internalInvariant(_boundsCheck(i &- 1))
    _precondition(_isScalarAligned(unchecked: i))
    return _decodePreviousScalar(uncheckedAssumingAligned: i)
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
  internal func _decodePreviousScalar(
    uncheckedAssumingAligned i: Int
  ) -> (Unicode.Scalar, previousScalarStart: Int) {
    _internalInvariant(_boundsCheck(i &- 1))
    _internalInvariant(_isScalarAligned(unchecked: i))
    return unsafe _start()._decodeScalar(endingAt: i)
  }
}

// Derived Scalar API
@available(SwiftStdlib 6.2, *)
extension UTF8Span {
  /// Find the nearest scalar-aligned position `<= i`.
  internal func _scalarAlignBackwards(_ i: Int) -> Int {
    if i == count || i == 0 { return i }

    _precondition(_boundsCheck(i))
    return unsafe _start()._scalarAlign(i)
  }

  /// Find the nearest scalar-aligned position `>= i`.
  internal func _scalarAlignForwards(_ i: Int) -> Int {
    if i == count || i == 0 { return i }

    _precondition(_boundsCheck(i))
    var i = i
    while _slowPath(!_isScalarAligned(unchecked: i)) {
      i &+= 1
    }
    return i
  }

  /// Find the nearest scalar-aligned position `>= i`.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  internal func _scalarAlignForwards(unchecked i: Int) -> Int {
    if i == count || i == 0 { return i }

    var i = i
    while _slowPath(!_isScalarAligned(unchecked: i)) {
      i &+= 1
    }
    return i
  }
}

// Core Character API
@available(SwiftStdlib 6.2, *)
extension UTF8Span {
  /// Returns the start of the next `Character` (i.e. grapheme cluster) after
  /// the one  starting at `i`, or the end of the span if `i` denotes the final
  /// `Character`.
  ///
  /// `i` must be `Character`-aligned.
  internal func _nextCharacterStart(_ i: Int) -> Int {
    _precondition(_boundsCheck(i))
    return _nextCharacterStart(unchecked: i)
  }

  /// Returns the start of the next `Character` (i.e. grapheme cluster) after
  /// the one  starting at `i`, or the end of the span if `i` denotes the final
  /// `Character`.
  ///
  /// `i` must be `Character`-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  internal func _nextCharacterStart(unchecked i: Int) -> Int {
    _internalInvariant(_boundsCheck(i))
    _precondition(_isScalarAligned(unchecked: i))
    return _nextCharacterStart(uncheckedAssumingAligned: i)
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
  internal func _nextCharacterStart(
    uncheckedAssumingAligned i: Int
  ) -> Int {
    _internalInvariant(_boundsCheck(i))
    _internalInvariant(_isScalarAligned(unchecked: i))
    return unsafe _start()._nextCharacterStart(i, limitedBy: count)
  }

  /// Returns the start of the `Character` (i.e. grapheme cluster) ending at
  /// `i`, i.e. the `Character` before the one starting at `i` or the last
  /// `Character` if `i` is the end of the span.
  ///
  /// `i` must be `Character`-aligned.
  internal func _previousCharacterStart(_ i: Int) -> Int {
    _precondition(_boundsCheck(i&-1))
    return _previousCharacterStart(unchecked: i)
  }

  /// Returns the start of the `Character` (i.e. grapheme cluster) ending at
  /// `i`, i.e. the `Character` before the one starting at `i` or the last
  /// `Character` if `i` is the end of the span.
  ///
  /// `i` must be `Character`-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  internal func _previousCharacterStart(unchecked i: Int) -> Int {
    _internalInvariant(_boundsCheck(i&-1))
    _precondition(_isScalarAligned(unchecked: i))
    return _previousCharacterStart(uncheckedAssumingAligned: i)
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
  internal func _previousCharacterStart(
    uncheckedAssumingAligned i: Int
  ) -> Int {
    _internalInvariant(_boundsCheck(i&-1))
    _internalInvariant(_isScalarAligned(unchecked: i))
    return unsafe _start()._previousCharacterStart(i, limitedBy: count)
  }

  /// Decode the `Character` starting at `i` Return it and the start of the
  /// next `Character`.
  ///
  /// `i` must be `Character`-aligned.
  internal func _decodeNextCharacter(
    _ i: Int
  ) -> (Character, nextCharacterStart: Int) {
    _precondition(_boundsCheck(i))
    return _decodeNextCharacter(unchecked: i)
  }

  /// Decode the `Character` starting at `i` Return it and the start of the
  /// next `Character`.
  ///
  /// `i` must be `Character`-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  internal func _decodeNextCharacter(
    unchecked i: Int
  ) -> (Character, nextCharacterStart: Int) {
    _internalInvariant(_boundsCheck(i))
    _precondition(_isScalarAligned(unchecked: i))
    return _decodeNextCharacter(uncheckedAssumingAligned: i)
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
  internal func _decodeNextCharacter(
    uncheckedAssumingAligned i: Int
  ) -> (Character, nextCharacterStart: Int) {
    _internalInvariant(_boundsCheck(i))
    _internalInvariant(_isScalarAligned(unchecked: i))
    return unsafe _start()._decodeCharacter(
      startingAt: i, limitedBy: count)
  }

  /// Decode the `Character` (i.e. grapheme cluster) ending at `i`, i.e. the
  /// previous `Character`. Return it and the start of that `Character`.
  ///
  /// `i` must be `Character`-aligned.
  internal func _decodePreviousCharacter(_ i: Int) -> (Character, Int) {
    _precondition(_boundsCheck(i &- 1))
    return _decodePreviousCharacter(unchecked: i)
  }

  /// Decode the `Character` (i.e. grapheme cluster) ending at `i`, i.e. the
  /// previous `Character`. Return it and the start of that `Character`.
  ///
  /// `i` must be `Character`-aligned.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  internal func _decodePreviousCharacter(
    unchecked i: Int
  ) -> (Character, Int) {
    _internalInvariant(_boundsCheck(i &- 1))
    _precondition(_isScalarAligned(unchecked: i))
    return _decodePreviousCharacter(uncheckedAssumingAligned: i)
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
  internal func _decodePreviousCharacter(
    uncheckedAssumingAligned i: Int
  ) -> (Character, Int) {
    _internalInvariant(_boundsCheck(i &- 1))
    _internalInvariant(_isScalarAligned(unchecked: i))
    return unsafe _start()._decodeCharacter(
      endingAt: i, limitedBy: count)
  }

}

// TODO: internal?
@available(SwiftStdlib 6.2, *)
extension UTF8Span {
  /// Whether `i` is in bounds
  @_alwaysEmitIntoClient
  internal func _boundsCheck(_ i: Int) -> Bool {
    i >= 0 && i < count
  }
  /// Whether `bounds` is in bounds
  @_alwaysEmitIntoClient
  internal func _boundsCheck(_ bounds: Range<Int>) -> Bool {
    _boundsCheck(bounds.lowerBound)
    && _boundsCheck(bounds.upperBound &- 1)
  }
}

// Future work: UTF-16 support when we get views



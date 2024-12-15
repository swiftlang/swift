// Core Scalar API
@available(SwiftStdlib 6.2, *)
extension UTF8Span {
  // /// Whether `i` is on a boundary between Unicode scalar values.
  // internal func _isScalarAligned(_ i: Int) -> Bool {
  //   if i == count || i == 0 { return true }
  //   precondition(_boundsCheck(i))
  //   return _isScalarAligned(unchecked: i)
  // }

  /// Whether `i` is on a boundary between Unicode scalar values.
  ///
  /// This function does not validate that `i` is within the span's bounds;
  /// this is an unsafe operation.
  internal func _isScalarAligned(unchecked i: Int) -> Bool {
    if i == count || i == 0 { return true }
    _internalInvariant(_boundsCheck(i))
    return unsafe _start()._isScalarAligned(i)
  }

  // internal func _roundDownToScalarBoundary(i: Int) -> Int {
  //   if i == count || i == 0 { return i }
  //   precondition(_boundsCheck(i))

  // }

  // /// Whether `range`'s bounds are aligned to `Unicode.Scalar` boundaries.
  // internal func _isScalarAligned(_ range: Range<Int>) -> Bool {
  //   _isScalarAligned(range.lowerBound) && _isScalarAligned(range.upperBound)
  // }

  // /// Whether `range`'s bounds are aligned to `Unicode.Scalar` boundaries.
  // ///
  // /// This function does not validate that `range` is within the span's bounds;
  // /// this is an unsafe operation.
  // internal func _isScalarAligned(unchecked range: Range<Int>) -> Bool {
  //   _isScalarAligned(unchecked: range.lowerBound)
  //   && _isScalarAligned(unchecked: range.upperBound)
  // }

  // /// Returns the start of the next `Unicode.Scalar` after the one starting at
  // /// `i`, or the end of the span if `i` denotes the final scalar.
  // ///
  // /// `i` must be scalar-aligned.
  // internal func _nextScalarStart(_ i: Int) -> Int {
  //   precondition(_boundsCheck(i))
  //   return _nextScalarStart(unchecked: i)
  // }

  // /// Returns the start of the next `Unicode.Scalar` after the one starting at
  // /// `i`, or the end of the span if `i` denotes the final scalar.
  // ///
  // /// `i` must be scalar-aligned.
  // ///
  // /// This function does not validate that `i` is within the span's bounds;
  // /// this is an unsafe operation.
  // internal func _nextScalarStart(unchecked i: Int) -> Int {
  //   _internalInvariant(_boundsCheck(i))
  //   precondition(_isScalarAligned(i))
  //   return _nextScalarStart(uncheckedAssumingAligned: i)
  // }

  // /// Returns the start of the next `Unicode.Scalar` after the one starting at
  // /// `i`, or the end of the span if `i` denotes the final scalar.
  // ///
  // /// `i` must be scalar-aligned.
  // ///
  // /// This function does not validate that `i` is within the span's bounds;
  // /// this is an unsafe operation.
  // ///
  // /// This function does not validate that `i` is scalar-aligned; this is an
  // /// unsafe operation if `i` isn't.
  // @_alwaysEmitIntoClient
  // internal func nextScalarStart(
  //   uncheckedAssumingAligned i: Int
  // ) -> Int {
  //   _internalInvariant(_boundsCheck(i))
  //   _internalInvariant(_isScalarAligned(i))
  //   return _start()._nextScalarStart(i)
  // }

  /// Returns the start of the `Unicode.Scalar` ending at `i`, i.e. the scalar
  /// before the one starting at `i` or the last scalar if `i` is the end of
  /// the span.
  ///
  /// `i` must be scalar-aligned.
  internal func _previousScalarStart(_ i: Int) -> Int {
    precondition(_boundsCheck(i&-1))
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
    precondition(_isScalarAligned(unchecked: i))
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

  // /// Decode the `Unicode.Scalar` starting at `i`. Return it and the start of
  // /// the next scalar.
  // ///
  // /// `i` must be scalar-aligned.
  // internal func _decodeNextScalar(
  //   _ i: Int
  // ) -> (Unicode.Scalar, nextScalarStart: Int) {
  //   precondition(_boundsCheck(i))
  //   return _decodeNextScalar(unchecked: i)
  // }

  // /// Decode the `Unicode.Scalar` starting at `i`. Return it and the start of
  // /// the next scalar.
  // ///
  // /// `i` must be scalar-aligned.
  // ///
  // /// This function does not validate that `i` is within the span's bounds;
  // /// this is an unsafe operation.
  // internal func _decodeNextScalar(
  //   unchecked i: Int
  // ) -> (Unicode.Scalar, nextScalarStart: Int) {
  //   _internalInvariant(_boundsCheck(i))
  //   precondition(_isScalarAligned(i))
  //   return _decodeNextScalar(uncheckedAssumingAligned: i)
  // }

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
    precondition(_boundsCheck(i &- 1))
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
    precondition(_isScalarAligned(unchecked: i))
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

    precondition(_boundsCheck(i))
    return unsafe _start()._scalarAlign(i)
  }

  // /// Find the nearest scalar-aligned position `<= i`.
  // ///
  // /// This function does not validate that `i` is within the span's bounds;
  // /// this is an unsafe operation.
  // internal func _scalarAlignBackwards(unchecked i: Int) -> Int {
  //   _internalInvariant(_boundsCheck(i))
  // }

  /// Find the nearest scalar-aligned position `>= i`.
  internal func _scalarAlignForwards(_ i: Int) -> Int {
    // FIXME: do the bounds check
    // FIXME: stop at end of code units
    //   - this should be an invariant, but checking it lets us avoid ever
    //     reading off the end
    // FIXME: implement directly
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
  // TODO: Single-scalar fast paths

  // /// Whether `i` is on a boundary between `Character`s (i.e. grapheme
  // /// clusters).
  // internal func _isCharacterAligned(_ i: Int) -> Bool {
  //   if i == count || i == 0 { return true }
  //   precondition(_boundsCheck(i))
  //   return _isCharacterAligned(unchecked: i)
  // }

  // /// Whether `i` is on a boundary between `Character`s (i.e. grapheme
  // /// clusters).
  // ///
  // /// This function does not validate that `i` is within the span's bounds;
  // /// this is an unsafe operation.
  // internal func _isCharacterAligned(unchecked i: Int) -> Bool {
  //   if i == count || i == 0 { return true }
  //   _internalInvariant(_boundsCheck(i))
  //   return _start()._isCharacterAligned(i, limitedBy: count)
  // }

  /// Returns the start of the next `Character` (i.e. grapheme cluster) after
  /// the one  starting at `i`, or the end of the span if `i` denotes the final
  /// `Character`.
  ///
  /// `i` must be `Character`-aligned.
  internal func _nextCharacterStart(_ i: Int) -> Int {
    precondition(_boundsCheck(i))
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
    precondition(_isScalarAligned(unchecked: i))
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
    precondition(_boundsCheck(i&-1))
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
    precondition(_isScalarAligned(unchecked: i))
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
    precondition(_boundsCheck(i))
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
    precondition(_isScalarAligned(unchecked: i))
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
    precondition(_boundsCheck(i &- 1))
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
    precondition(_isScalarAligned(unchecked: i))
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

// Derived Character API
// @available(SwiftStdlib 6.2, *)
// extension UTF8Span {
  // /// Find the nearest `Character` (i.e. grapheme cluster)-aligned position
  // /// that is `<= i`.
  // internal func _characterAlignBackwards(_ i: Int) -> Int {
  //   precondition(i == count || _boundsCheck(i))
  //   return _characterAlignBackwards(unchecked: i)
  // }

  // /// Find the nearest `Character` (i.e. grapheme cluster)-aligned position
  // /// that is `<= i`.
  // ///
  // /// This function does not validate that `i` is within the span's bounds;
  // /// this is an unsafe operation.
  // internal func _characterAlignBackwards(unchecked i: Int) -> Int {
  //   _internalInvariant(i == count || _boundsCheck(i))
  //   var i = i
  //   while _slowPath(!_isCharacterAligned(unchecked: i)) {
  //     i &-= 1
  //   }
  //   return i
  // }

  // /// Find the nearest `Character` (i.e. grapheme cluster)-aligned position
  // /// that is `>= i`.
  // internal func _characterAlignForwards(_ i: Int) -> Int {
  //   precondition(i == count || _boundsCheck(i))
  //   return _characterAlignForwards(unchecked: i)
  // }

  // /// Find the nearest `Character` (i.e. grapheme cluster)-aligned position
  // /// that is `>= i`.
  // ///
  // /// This function does not validate that `i` is within the span's bounds;
  // /// this is an unsafe operation.
  // @_alwaysEmitIntoClient
  // internal func _characterAlignForwards(unchecked i: Int) -> Int {
  //   _internalInvariant(i == count || _boundsCheck(i))
  //   var i = i
  //   while _slowPath(!_isCharacterAligned(unchecked: i)) {
  //     i &+= 1
  //   }
  //   return i
  // }
// }

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



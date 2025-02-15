@available(SwiftStdlib 6.1, *)
extension UTF8Span {

  public func _makeScalarIterator() -> ScalarIterator {
    .init(self)
  }

  /// Iterate the `Unicode.Scalar`s  contents of a `UTF8Span`.
  ///
  /// **TODO**: Examples
  public struct ScalarIterator: ~Escapable {
    public var codeUnits: UTF8Span

    /// The byte offset of the start of the next scalar. This is
    /// always scalar-aligned.
    ///
    /// **TODO**: private(set)?
    fileprivate(set)
    public var currentCodeUnitOffset: Int

    // TODO: underscored init?
    public init(_ codeUnits: UTF8Span) {
      self.codeUnits = codeUnits
      self.currentCodeUnitOffset = 0
    }

    /// Decode and return the scalar starting at `currentCodeUnitOffset`.
    /// After the function returns, `currentCodeUnitOffset` holds the
    /// position at the end of the returned scalar, which is also the start
    /// of the next scalar.
    ///
    /// Returns `nil` if at the end of the `UTF8Span`.
    public mutating func next() -> Unicode.Scalar? {
      guard currentCodeUnitOffset < codeUnits.count else { return nil }

      _internalInvariant(codeUnits.isScalarAligned(currentCodeUnitOffset))
      let (result, newPos) = codeUnits._start()._decodeScalar(startingAt: currentCodeUnitOffset)
      self.currentCodeUnitOffset = newPos
      return result
    }

    /// Decode and return the scalar ending at `currentCodeUnitOffset`. After
    /// the function returns, `currentCodeUnitOffset` holds the position at
    /// the start of the returned scalar, which is also the end of the
    /// previous scalar.
    ///
    /// Returns `nil` if at the start of the `UTF8Span`.
    public mutating func previous() -> Unicode.Scalar? {
      guard currentCodeUnitOffset > 0 else { return nil }

      _internalInvariant(codeUnits.isScalarAligned(currentCodeUnitOffset))
      let (result, newPos) = codeUnits._start()._decodeScalar(endingAt: currentCodeUnitOffset)
      self.currentCodeUnitOffset = newPos
      return result
    }


    /// Advance `codeUnitOffset` to the end of the current scalar, without
    /// decoding it.
    ///
    /// Returns the number of `Unicode.Scalar`s skipped over, which can be 0
    /// if at the end of the UTF8Span.
    public mutating func skipForward() -> Int {
      fatalError()
    }

    /// Advance `codeUnitOffset` to the end of `n` scalars, without decoding
    /// them.
    ///
    /// Returns the number of `Unicode.Scalar`s skipped over, which can be
    /// fewer than `n` if at the end of the UTF8Span.
    public mutating func skipForward(by n: Int) -> Int {
      fatalError()
    }

    /// Move `codeUnitOffset` to the start of the previous scalar, without
    /// decoding it.
    ///
    /// Returns the number of `Unicode.Scalar`s skipped over, which can be 0
    /// if at the start of the UTF8Span.
    public mutating func skipBack() -> Bool {
      fatalError()
    }

    /// Move `codeUnitOffset` to the start of the previous `n` scalars,
    /// without decoding them.
    ///
    /// Returns the number of `Unicode.Scalar`s skipped over, which can be
    /// fewer than `n` if at the start of the UTF8Span.
    public mutating func skipBack(by n: Int) -> Bool {
      fatalError()
    }

    /// Reset to the nearest scalar-aligned code unit offset `<= i`.
    ///
    /// **TODO**: Example
    public mutating func reset(roundingBackwardsFrom i: Int)  {
      // TODO: what about out of bounds (and beyond-count) values of i?
      var pos = i
      while !codeUnits.isScalarAligned(pos) { pos -= 1 }
      self.currentCodeUnitOffset = pos
    }

    /// Reset to the nearest scalar-aligned code unit offset `>= i`.
    ///
    /// **TODO**: Example
    public mutating func reset(roundingForwardsFrom i: Int)  {
      // TODO: what about out of bounds (and beyond-count) values of i?
      var pos = i
      while !codeUnits.isScalarAligned(pos) { pos += 1 }
      self.currentCodeUnitOffset = pos
    }

    /// Reset this iterator to code unit offset `i`, skipping _all_ safety
    /// checks.
    ///
    /// Note: This is only for very specific, low-level use cases. If
    /// `codeUnitOffset` is not properly scalar-aligned, this function can
    /// result in undefined behavior when, e.g., `next()` is called.
    ///
    /// For example, this could be used by a regex engine to backtrack to a
    /// known-valid previous position.
    ///
    public mutating func reset(uncheckedAssumingAlignedTo i: Int) {
      _internalInvariant(codeUnits.isScalarAligned(i))
      self.currentCodeUnitOffset = i
    }

    /// Returns the UTF8Span containing all the content up to the iterator's
    /// current position.
    public func _prefix() -> UTF8Span {
      fatalError()
    }

    /// Returns the UTF8Span containing all the content after the iterator's
    /// current position.
    public func _suffix() -> UTF8Span {
      fatalError()
    }
  }
}

@available(SwiftStdlib 6.1, *)
@_unavailableInEmbedded
extension UTF8Span {
  public func _makeCharacterIterator() -> CharacterIterator {
    .init(self)
  }

  /// Iterate the `Character` contents of a `UTF8Span`.
  ///
  /// **TODO**: Examples
  public struct CharacterIterator: ~Escapable {
    public var codeUnits: UTF8Span

    /// **QUESTION**: The notion of `Character` aligned is complex. It can be
    ///   defined as the same as scalar-aligned, as you can run the algorithm
    ///   from any scalar position and get the emergent behavior, even if
    ///   that position would split a `Character` as processed from another
    ///   position. For example, when starting grapheme breaking in the
    ///   middle of a long run of paired regional indicators, you can start
    ///   from odd or even offsets and get different flag emoji out. That can
    ///   occasionally be useful, but can yield counter intuitive results.
    ///
    ///   While we talk about code unit offsets always being scalar-aligned,
    ///   we could go further to talk about `Character` aligned indices
    ///   (where `Character`-alignment is relative to the start of the
    ///   `UTF8Span`) and have API for those.

    /// The byte offset of the start of the next `Character`. This is
    /// always scalar-aligned and `Character`-aligned.
    ///
    /// **TODO**: How to talk about the
    ///   assuming-the-UTF8Span-is-the-entire-content interpretation of
    ///   `Character`-aligned?
    fileprivate(set)
    public var currentCodeUnitOffset: Int

    // TODO: underscored init?
    public init(_ span: UTF8Span) {
      self.codeUnits = span
      self.currentCodeUnitOffset = 0
    }

    /// Return the `Character` starting at `currentCodeUnitOffset`. After the
    /// function returns, `currentCodeUnitOffset` holds the position at the
    /// end of the `Character`, which is also the start of the next
    /// `Character`.
    ///
    /// Returns `nil` if at the end of the `UTF8Span`.
    public mutating func next() -> Character? {
      guard currentCodeUnitOffset < codeUnits.count else { return nil }

      _internalInvariant(codeUnits.isScalarAligned(currentCodeUnitOffset))
      let (result, newPos) = codeUnits._start()._decodeCharacter(
        startingAt: currentCodeUnitOffset,
        limitedBy: codeUnits.count
      )
      self.currentCodeUnitOffset = newPos
      return result
    }

    /// Return the `Character` ending at `currentCodeUnitOffset`. After the
    /// function returns, `currentCodeUnitOffset` holds the position at the
    /// start of the returned `Character`, which is also the end of the
    /// previous `Character`.
    ///
    /// Returns `nil` if at the start of the `UTF8Span`.
    public mutating func previous() -> Character? {
      guard currentCodeUnitOffset > 0 else { return nil }

      _internalInvariant(codeUnits.isScalarAligned(currentCodeUnitOffset))
      let (result, newPos) = codeUnits._start()._decodeCharacter(
        endingAt: currentCodeUnitOffset,
        limitedBy: codeUnits.count)
      self.currentCodeUnitOffset = newPos
      return result
    }

    /// Advance `codeUnitOffset` to the end of the current `Character`,
    /// without constructing it.
    ///
    /// Returns the number of `Character`s skipped over, which can be 0
    /// if at the end of the UTF8Span.
    public mutating func skipForward() {
      fatalError()
    }

    /// Advance `codeUnitOffset` to the end of `n` `Characters`, without
    /// constructing them.
    ///
    /// Returns the number of `Character`s skipped over, which can be
    /// fewer than `n` if at the end of the UTF8Span.
    public mutating func skipForward(by n: Int) {
      fatalError()
    }

    /// Move `codeUnitOffset` to the start of the previous `Character`,
    /// without constructing it.
    ///
    /// Returns the number of `Character`s skipped over, which can be 0
    /// if at the start of the UTF8Span.
    public mutating func skipBack() {
      fatalError()
    }

    /// Move `codeUnitOffset` to the start of the previous `n` `Character`s,
    /// without constructing them.
    ///
    /// Returns the number of `Character`s skipped over, which can be
    /// fewer than `n` if at the start of the UTF8Span.
    public mutating func skipBack(by n: Int) {
      fatalError()
    }

    /// Reset to the nearest character-aligned position `<= i`.
    public mutating func reset(roundingBackwardsFrom i: Int) {
      fatalError()
    }

    /// Reset to the nearest character-aligned position `>= i`.
    public mutating func reset(roundingForwardsFrom i: Int) {
      fatalError()
    }

    /// Reset this iterator to code unit offset `i`, skipping _all_ safety
    /// checks.
    ///
    /// Note: This is only for very specific, low-level use cases. If
    /// `codeUnitOffset` is not properly scalar-aligned, this function can
    /// result in undefined behavior when, e.g., `next()` is called.
    ///
    /// If `i` is scalar-aligned, but not `Character`-aligned, you may get
    /// different results from running `Character` iteration.
    ///
    /// For example, this could be used by a regex engine to backtrack to a
    /// known-valid previous position.
    ///
    public mutating func reset(uncheckedAssumingAlignedTo i: Int) {
      fatalError()
    }

    /// Returns the UTF8Span containing all the content up to the iterator's
    /// current position.
    public func prefix() -> UTF8Span {
      fatalError()
    }

    /// Returns the UTF8Span containing all the content after the iterator's
    /// current position.
    public func suffix() -> UTF8Span {
      fatalError()
    }
  }

}

@available(SwiftStdlib 6.1, *)
extension UTF8Span {

  public func _makeGraphemeBreakIterator() -> GraphemeBreakIterator {
    .init(self)
  }

  /// **QUESTION**: There are many ways we could expose this functionality and
  ///   I'd like some help here. As written, the caller would be looking at
  ///   `currentCodeUnitOffset` and `state` while `next()` / `previous
  ///   ()` would return `false` when they get to the end of the current
  ///   span (which may or may not be a grapheme break, depending on whether
  ///   there's more spans available or not).

  /// **TODO**: Doc comments
  public struct GraphemeBreakIterator: ~Escapable {
    public var codeUnits: UTF8Span
    public var currentCodeUnitOffset: Int
    public var state: Unicode.GraphemeBreakingState

    // TODO: underscored init?
    public init(_ span: UTF8Span) {
      self.codeUnits = span
      self.currentCodeUnitOffset = 0
      self.state = .init()
    }

    // TODO: underscored init?
    public init(_ span: UTF8Span, using state: Unicode.GraphemeBreakingState) {
      self.codeUnits = span
      self.currentCodeUnitOffset = 0
      self.state = state
    }

    public mutating func next() -> Bool {
      fatalError()
    }

    public mutating func previous() -> Bool {
      fatalError()
    }


    public mutating func skipForward() {
    }

    public mutating func skipForward(by n: Int) {}

    public mutating func skipBack() {
    }

    public mutating func skipBack(by n: Int) {
    }

    public mutating func reset(
      roundingBackwardsFrom i: Int, using: Unicode.GraphemeBreakingState
    ) {
    }

    public mutating func reset(
      roundingForwardsFrom i: Int, using: Unicode.GraphemeBreakingState
    ) {
    }

    public mutating func reset(
      uncheckedAssumingAlignedTo i: Int, using: Unicode.GraphemeBreakingState
    ) {
    }

    public func prefix() -> UTF8Span {
      fatalError()
    }
    public func suffix() -> UTF8Span {
      fatalError()
    }
  }
}





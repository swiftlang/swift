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
      let (result, newPos) = codeUnits.unsafeBaseAddress._decodeScalar(startingAt: currentCodeUnitOffset)
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
      let (result, newPos) = codeUnits.unsafeBaseAddress._decodeScalar(endingAt: currentCodeUnitOffset)
      self.currentCodeUnitOffset = newPos
      return result
    }

    // **QUESTION**: How should skip(by: Int) APIs be defined? Should they
    //   implicitly clamp to start/end? Should they return the number of code
    //   units skipped? number of scalars skipped? 
    //  
    //   Code units skipped can be calculated by the caller, but scalars
    //   skipped (if < n) is harder to figure out. For now, I just return a
    //   Bool signaling if there weren't enough scalars.

    /// Advance `codeUnitOffset` to the end of the current scalar, without
    /// decoding it.
    public mutating func skipForward() -> Bool {
      fatalError()
    }

    /// Advance `codeUnitOffset` to the end of `n` scalars, without decoding
    /// them.
    public mutating func skipForward(by n: Int) -> Bool {
      fatalError()
    }

    /// Move `codeUnitOffset` to the start of the previous scalar, without
    /// decoding it.
    public mutating func skipBack() -> Bool {
      fatalError()
    }

    /// Move `codeUnitOffset` to the start of the previous `n` scalars,
    /// without decoding them.
    public mutating func skipBack(by n: Int) -> Bool {
      fatalError()
    }

    // **QUESTION**: For reset rounding, should we return the rounded position as
    //   a discardable result? That would make checking if rounding occurred 
    //   easier, though that might be better served by a isScalarAligned API.

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

    // **QUESTION**: Since UTF8Span can only be sliced on scalar-aligned
    //   positions, and there are multiple levels of semantics to positions
    //   (e.g. scalar-aligned, `Character`-aligned, `Grapheme-breaking
    //   aligned`, I'm proposing having slicing be API on iterators rather
    //   than `_extracting` on UTF8Span. But, is this the most useful
    //   formulation?

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
    ///   should this type claim them to also be `Character` aligned as
    ///   defined by the behavior of the iterator itself (i.e. the span is
    ///   the entirety of the content)? 
    ///
    ///   You can get split-the-character behavior by getting the UTF8Span
    ///   formed by `prefix/suffix` on the scalar iterator if you really want
    ///   to, so I'm going with this is always `Character`-aligned under the
    ///   intrepretation of `UTF8Span` as holding the entirety of the
    ///   content.

    /// The byte offset of the start of the next `Character`. This is 
    /// always scalar-aligned and `Character`-aligned.
    ///
    /// **TODO**: How to talk about the
    ///   assuming-the-UTF8Span-is-the-entire-content interpretation of
    ///   `Character`-aligned?
    ///
    /// **TODO**: private(set)?
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
      let (result, newPos) = codeUnits.unsafeBaseAddress._decodeCharacter(
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
      let (result, newPos) = codeUnits.unsafeBaseAddress._decodeCharacter(
        endingAt: currentCodeUnitOffset,
        limitedBy: codeUnits.count)
      self.currentCodeUnitOffset = newPos
      return result
    }

    /// Advance `codeUnitOffset` to the end of the current `Character`,
    /// without constructing it.
    public mutating func skipForward() {
      fatalError()
    }

    /// Advance `codeUnitOffset` to the end of `n` `Characters`, without
    /// constructing them.
    public mutating func skipForward(by n: Int) {}

    /// Move `codeUnitOffset` to the start of the previous `Character`,
    /// without constructing it.
    public mutating func skipBack() {
      fatalError()
    }

    /// Move `codeUnitOffset` to the start of the previous `n` `Character`s,
    /// without constructing them.
    public mutating func skipBack(by n: Int) {
    }

    /// Reset to the nearest character-aligned position `<= i`.
    public mutating func reset(roundingBackwardsFrom i: Int) {
    }

    /// Reset to the nearest character-aligned position `>= i`.
    public mutating func reset(roundingForwardsFrom i: Int) {
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





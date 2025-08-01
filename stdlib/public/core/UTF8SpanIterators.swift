//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension UTF8Span {
  /// Returns an iterator that will decode the code units into
  /// `Unicode.Scalar`s.
  ///
  /// The resulting iterator has the same lifetime constraints as `self`.
  @lifetime(copy self)
  public func makeUnicodeScalarIterator() -> UnicodeScalarIterator {
    .init(self)
  }

  // **TODO**: Examples in below doc

  /// Iterate the `Unicode.Scalar`s  contents of a `UTF8Span`.
  @frozen
  public struct UnicodeScalarIterator: ~Escapable {
    public let codeUnits: UTF8Span

    /// The byte offset of the start of the next scalar. This is
    /// always scalar-aligned.
    fileprivate(set)
    public var currentCodeUnitOffset: Int

    @lifetime(copy codeUnits)
    public init(_ codeUnits: UTF8Span) {
      self.codeUnits = codeUnits
      self.currentCodeUnitOffset = 0
    }

    private var _start: UnsafeRawPointer {
      codeUnits._start()
    }

    /// Decode and return the scalar starting at `currentCodeUnitOffset`.
    /// After the function returns, `currentCodeUnitOffset` holds the
    /// position at the end of the returned scalar, which is also the start
    /// of the next scalar.
    ///
    /// Returns `nil` if at the end of the `UTF8Span`.
    ///
    /// - Complexity: O(1)
    @lifetime(self: copy self)
    public mutating func next() -> Unicode.Scalar? {
      guard currentCodeUnitOffset < codeUnits.count else {
        return nil
      }

      _internalInvariant(codeUnits._isScalarAligned(unchecked: currentCodeUnitOffset))
      let (result, newPos) = unsafe _start._decodeScalar(startingAt: currentCodeUnitOffset)
      self.currentCodeUnitOffset = newPos
      return result
    }

    /// Decode and return the scalar ending at `currentCodeUnitOffset`. After
    /// the function returns, `currentCodeUnitOffset` holds the position at
    /// the start of the returned scalar, which is also the end of the
    /// previous scalar.
    ///
    /// Returns `nil` if at the start of the `UTF8Span`.
    ///
    /// - Complexity: O(1)
    @lifetime(self: copy self)
    public mutating func previous() -> Unicode.Scalar? {
      guard currentCodeUnitOffset > 0 else {
        return nil
      }

      _internalInvariant(codeUnits._isScalarAligned(unchecked: currentCodeUnitOffset))
      let (result, newPos) = unsafe _start._decodeScalar(endingAt: currentCodeUnitOffset)
      self.currentCodeUnitOffset = newPos
      return result
    }


    /// Advance `codeUnitOffset` to the end of the current scalar, without
    /// decoding it.
    ///
    /// Returns the number of `Unicode.Scalar`s skipped over, which can be 0
    /// if at the end of the UTF8Span.
    ///
    /// - Complexity: O(1)
    @lifetime(self: copy self)
    public mutating func skipForward() -> Int {
      guard currentCodeUnitOffset < codeUnits.count else {
        return 0
      }

      _internalInvariant(codeUnits._isScalarAligned(unchecked: currentCodeUnitOffset))

      currentCodeUnitOffset &+= unsafe _start._scalarLength(startingAt: currentCodeUnitOffset)
      return 1
    }

    /// Advance `codeUnitOffset` to the end of `n` scalars, without decoding
    /// them.
    ///
    /// Returns the number of `Unicode.Scalar`s skipped over, which can be
    /// fewer than `n` if at the end of the UTF8Span.
    ///
    /// - Complexity: O(n)
    @lifetime(self: copy self)
    public mutating func skipForward(by n: Int) -> Int {
      var numSkipped = 0
      while numSkipped < n && skipForward() != 0 {
        numSkipped += 1
      }

      return numSkipped
    }

    /// Move `codeUnitOffset` to the start of the previous scalar, without
    /// decoding it.
    ///
    /// Returns the number of `Unicode.Scalar`s skipped over, which can be 0
    /// if at the start of the UTF8Span.
    ///
    /// - Complexity: O(1)
    @lifetime(self: copy self)
    public mutating func skipBack() -> Int {
      guard currentCodeUnitOffset > 0 else {
        return 0
      }

      _internalInvariant(codeUnits._isScalarAligned(unchecked: currentCodeUnitOffset))

      currentCodeUnitOffset = unsafe _start._previousScalarStart(currentCodeUnitOffset)
      return 1
    }

    /// Move `codeUnitOffset` to the start of the previous `n` scalars,
    /// without decoding them.
    ///
    /// Returns the number of `Unicode.Scalar`s skipped over, which can be
    /// fewer than `n` if at the start of the UTF8Span.
    ///
    /// - Complexity: O(n)
    @lifetime(self: copy self)
    public mutating func skipBack(by n: Int) -> Int {
      var numSkipped = 0
      while numSkipped < n && skipBack() != 0 {
        numSkipped += 1
      }

      return numSkipped
    }

    // TODO: Example for reset docs

    /// Reset to the nearest scalar-aligned code unit offset `<= i`.
    ///
    /// - Complexity: O(1)
    @lifetime(self: copy self)
    public mutating func reset(roundingBackwardsFrom i: Int)  {
      self.currentCodeUnitOffset = codeUnits._scalarAlignBackwards(i)
    }

    /// Reset to the nearest scalar-aligned code unit offset `>= i`.
    ///
    /// - Complexity: O(1)
    @lifetime(self: copy self)
    public mutating func reset(roundingForwardsFrom i: Int)  {
      self.currentCodeUnitOffset = codeUnits._scalarAlignForwards(i)
    }

    // TODO: for below, verify that there is no path to UB, just garabage-data or guaranteed
    // trap!

    /// Reset this iterator to `codeUnitOffset`, skipping _all_ safety
    /// checks (including bounds checks).
    ///
    /// Note: This is only for very specific, low-level use cases. If
    /// `codeUnitOffset` is not properly scalar-aligned, this function can
    /// result in undefined behavior when, e.g., `next()` is called.
    ///
    /// For example, this could be used by a regex engine to backtrack to a
    /// known-valid previous position.
    ///
    ///
    /// - Complexity: O(1)
    @unsafe
    @lifetime(self: copy self)
    public mutating func reset(toUnchecked codeUnitOffset: Int) {
      _internalInvariant(codeUnits._isScalarAligned(unchecked: codeUnitOffset))
      self.currentCodeUnitOffset = codeUnitOffset
    }

    /// Returns the UTF8Span containing all the content up to the iterator's
    /// current position.
    ///
    /// The resultant `UTF8Span` has the same lifetime constraints as `self`.
    ///
    /// - Complexity: O(1)
    @lifetime(copy self)
    public func prefix() -> UTF8Span {
      let slice = codeUnits.span.extracting(0..<currentCodeUnitOffset)
      return UTF8Span(
        _uncheckedAssumingValidUTF8: slice,
        isKnownASCII: codeUnits.isKnownASCII,
        isKnownNFC: codeUnits.isKnownNFC)
    }

    /// Returns the UTF8Span containing all the content after the iterator's
    /// current position.
    ///
    /// The resultant `UTF8Span` has the same lifetime constraints as `self`.
    ///
    /// - Complexity: O(1)
    @lifetime(copy self)
    public func suffix() -> UTF8Span {
      let slice = codeUnits.span.extracting(currentCodeUnitOffset..<codeUnits.count)
      return UTF8Span(
        _uncheckedAssumingValidUTF8: slice,
        isKnownASCII: codeUnits.isKnownASCII,
        isKnownNFC: codeUnits.isKnownNFC)
    }
  }
}

@available(SwiftStdlib 6.2, *)
@_unavailableInEmbedded
extension UTF8Span {
  /// Returns an iterator that will construct `Character`s from the underlying
  /// UTF-8 content.
  ///
  /// The resulting iterator has the same lifetime constraints as `self`.
  @lifetime(copy self)
  public func makeCharacterIterator() -> CharacterIterator {
    .init(self)
  }

  // **TODO**: Examples in below doc

  /// Iterate the `Character` contents of a `UTF8Span`.
  public struct CharacterIterator: ~Escapable {
    public let codeUnits: UTF8Span

    /// The byte offset of the start of the next `Character`. This is always
    /// scalar-aligned. It is always `Character`-aligned relative to the last
    /// call to `reset` (or the start of the span if not called).
    fileprivate(set)
    public var currentCodeUnitOffset: Int

    @lifetime(copy codeUnits)
    public init(_ codeUnits: UTF8Span) {
      self.codeUnits = codeUnits
      self.currentCodeUnitOffset = 0
    }

    private var _start: UnsafeRawPointer {
      codeUnits._start()
    }

    /// Return the `Character` starting at `currentCodeUnitOffset`. After the
    /// function returns, `currentCodeUnitOffset` holds the position at the
    /// end of the `Character`, which is also the start of the next
    /// `Character`.
    ///
    /// Returns `nil` if at the end of the `UTF8Span`.
    @lifetime(self: copy self)
    public mutating func next() -> Character? {
      guard currentCodeUnitOffset < codeUnits.count else { return nil }

      _internalInvariant(codeUnits._isScalarAligned(unchecked: currentCodeUnitOffset))
      let (result, newPos) = unsafe _start._decodeCharacter(
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
    @lifetime(self: copy self)
    public mutating func previous() -> Character? {
      guard currentCodeUnitOffset > 0 else { return nil }

      _internalInvariant(codeUnits._isScalarAligned(unchecked: currentCodeUnitOffset))
      let (result, newPos) = unsafe _start._decodeCharacter(
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
    @lifetime(self: copy self)
    public mutating func skipForward() -> Int {
      guard currentCodeUnitOffset < codeUnits.count else {
        return 0
      }

      _internalInvariant(codeUnits._isScalarAligned(unchecked: currentCodeUnitOffset))

      self.currentCodeUnitOffset = unsafe _start._nextCharacterStart(currentCodeUnitOffset, limitedBy: codeUnits.count)
      return 1
    }

    /// Advance `codeUnitOffset` to the end of `n` `Characters`, without
    /// constructing them.
    ///
    /// Returns the number of `Character`s skipped over, which can be
    /// fewer than `n` if at the end of the UTF8Span.
    @lifetime(self: copy self)
    public mutating func skipForward(by n: Int) -> Int {
      var numSkipped = 0
      while numSkipped < n && skipForward() != 0 {
        numSkipped += 1
      }

      return numSkipped
    }

    /// Move `codeUnitOffset` to the start of the previous `Character`,
    /// without constructing it.
    ///
    /// Returns the number of `Character`s skipped over, which can be 0
    /// if at the start of the UTF8Span.
    @lifetime(self: copy self)
    public mutating func skipBack() -> Int {
      guard currentCodeUnitOffset > 0 else {
        return 0
      }

      _internalInvariant(codeUnits._isScalarAligned(unchecked: currentCodeUnitOffset))

      currentCodeUnitOffset = unsafe _start._previousCharacterStart(currentCodeUnitOffset, limitedBy: codeUnits.count)
      return 1

    }

    /// Move `codeUnitOffset` to the start of the previous `n` `Character`s,
    /// without constructing them.
    ///
    /// Returns the number of `Character`s skipped over, which can be
    /// fewer than `n` if at the start of the UTF8Span.
    @lifetime(self: copy self)
    public mutating func skipBack(by n: Int) -> Int {
      var numSkipped = 0
      while numSkipped < n && skipBack() != 0 {
        numSkipped += 1
      }

      return numSkipped
    }

    /// Reset to the nearest character-aligned position `<= i`.
    @lifetime(self: copy self)
    public mutating func reset(roundingBackwardsFrom i: Int) {
      self.currentCodeUnitOffset = codeUnits._scalarAlignBackwards(i)
    }

    /// Reset to the nearest character-aligned position `>= i`.
    @lifetime(self: copy self)
    public mutating func reset(roundingForwardsFrom i: Int) {
      self.currentCodeUnitOffset = codeUnits._scalarAlignForwards(i)
    }

    /// Reset this iterator to `codeUnitOffset`, skipping _all_ safety
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
    @unsafe
    @lifetime(self: copy self)
    public mutating func reset(toUnchecked codeUnitOffset: Int) {
      _internalInvariant(codeUnits._isScalarAligned(unchecked: codeUnitOffset))
      self.currentCodeUnitOffset = codeUnitOffset
    }

    /// Returns the UTF8Span containing all the content up to the iterator's
    /// current position.
    @lifetime(copy self)
    public func prefix() -> UTF8Span {
      let slice = codeUnits.span.extracting(0..<currentCodeUnitOffset)
      return UTF8Span(
        _uncheckedAssumingValidUTF8: slice,
        isKnownASCII: codeUnits.isKnownASCII,
        isKnownNFC: codeUnits.isKnownNFC)
    }

    /// Returns the UTF8Span containing all the content after the iterator's
    /// current position.
    @lifetime(copy self)
    public func suffix() -> UTF8Span {
      let slice = codeUnits.span.extracting(currentCodeUnitOffset..<codeUnits.count)
      return UTF8Span(
        _uncheckedAssumingValidUTF8: slice,
        isKnownASCII: codeUnits.isKnownASCII,
        isKnownNFC: codeUnits.isKnownNFC)
    }
  }
}



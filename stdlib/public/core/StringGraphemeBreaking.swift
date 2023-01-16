//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

extension Unicode.Scalar {
  /// CR and LF are common special cases in grapheme breaking logic
  fileprivate static var _cr: Self { .init(0x0d as UInt8) }
  fileprivate static var _lf: Self { .init(0x0a as UInt8) }
}

extension _StringGuts {
  @inline(__always)
  internal func roundDownToNearestCharacter(
    _ i: String.Index
  ) -> String.Index {
    _internalInvariant(i._isScalarAligned)
    _internalInvariant(hasMatchingEncoding(i))
    _internalInvariant(i._encodedOffset <= count)

    let offset = i._encodedOffset
    if _fastPath(i._isCharacterAligned) { return i }
    if offset == 0 || offset == count { return i._characterAligned }
    return _slowRoundDownToNearestCharacter(i)
  }

  @inline(never)
  internal func _slowRoundDownToNearestCharacter(
    _ i: String.Index
  ) -> String.Index {
    let offset = i._encodedOffset
    let start = offset - _opaqueCharacterStride(endingAt: offset)
    let stride = _opaqueCharacterStride(startingAt: start)
    _internalInvariant(offset <= start + stride,
      "Grapheme breaking inconsistency")
    if offset >= start + stride {
      // Already aligned, or grapheme breaking returned an unexpected result.
      return i._characterAligned
    }
    let r = String.Index(encodedOffset: start, characterStride: stride)
    return markEncoding(r._characterAligned)
  }

  @inline(__always)
  internal func roundDownToNearestCharacter(
    _ i: String.Index,
    in bounds: Range<String.Index>
  ) -> String.Index {
    _internalInvariant(
      bounds.lowerBound._isScalarAligned && bounds.upperBound._isScalarAligned)
    _internalInvariant(
      hasMatchingEncoding(bounds.lowerBound)
      && hasMatchingEncoding(bounds.upperBound))
    _internalInvariant(bounds.upperBound <= endIndex)

    _internalInvariant(i._isScalarAligned)
    _internalInvariant(hasMatchingEncoding(i))
    _internalInvariant(i >= bounds.lowerBound && i <= bounds.upperBound)

    // We can only use the `_isCharacterAligned` bit if the start index is also
    // character-aligned.
    if _fastPath(
      bounds.lowerBound._isCharacterAligned && i._isCharacterAligned
    ) {
      return i
    }
    if i == bounds.lowerBound || i == bounds.upperBound { return i }
    return _slowRoundDownToNearestCharacter(i, in: bounds)
  }

  @inline(never)
  internal func _slowRoundDownToNearestCharacter(
    _ i: String.Index,
    in bounds: Range<String.Index>
  ) -> String.Index {
    let offset = i._encodedOffset

    let quickBreak = isFastUTF8 && withFastUTF8 { utf8 in
      let prev = _decodeScalar(utf8, endingAt: offset).0
      let next = _decodeScalar(utf8, startingAt: offset).0
      return _GraphemeBreakingState.hasKnownBreak(
        between: prev, and: next) == true
    }
    if quickBreak {
      return i
    }

    let offsetBounds = bounds._encodedOffsetRange
    let prior =
      offset - _opaqueCharacterStride(endingAt: offset, in: offsetBounds)
    let stride = _opaqueCharacterStride(startingAt: prior)
    _internalInvariant(offset <= prior + stride,
      "Grapheme breaking inconsistency")
    if offset >= prior + stride {
      // Already aligned, or grapheme breaking returned an unexpected result.
      return i
    }
    var r = String.Index(encodedOffset: prior, characterStride: stride)
    if bounds.lowerBound._isCharacterAligned {
      r = r._characterAligned
    } else {
      r = r._scalarAligned
    }
    return markEncoding(r)
  }
}

extension _StringGuts {
  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func isOnGraphemeClusterBoundary(_ i: String.Index) -> Bool {
    if i._isCharacterAligned { return true }

    guard i.transcodedOffset == 0 else { return false }

    let offset = i._encodedOffset
    if offset == 0 || offset == self.count { return true }

    guard isOnUnicodeScalarBoundary(i) else { return false }

    let nearest = roundDownToNearestCharacter(i._scalarAligned)
    return i == nearest
  }
}

extension _StringGuts {
  internal struct _ForwardGraphemeBreakingState {
    internal var offset: Int
    internal var _length: UInt8
    internal var state: _GraphemeBreakingState

    internal init(offset: Int, length: Int, state: _GraphemeBreakingState) {
      _internalInvariant(length >= 0 && length <= 4)
      self.offset = offset
      self._length = UInt8(truncatingIfNeeded: length)
      self.state = state
    }

    @inline(__always)
    internal var length: Int {
      get { Int(_length) }
      set {
        _internalInvariant(newValue >= 0 && newValue <= 4)
        _length = UInt8(truncatingIfNeeded: newValue)
      }
    }
  }

  internal func forwardGraphemeBreakingState(
    at i: Int,
    with gbp: Unicode._GraphemeBreakProperty?
  ) -> _ForwardGraphemeBreakingState {
    guard i < count else {
      return .init(offset: i, length: 0, state: _GraphemeBreakingState())
    }
    if _slowPath(isForeign) {
      return _foreignForwardGraphemeBreakingState(at: i, with: gbp)
    }
    let (scalar, len) = withFastUTF8 { utf8 in
      _decodeScalar(utf8, startingAt: i)
    }
    let state = _GraphemeBreakingState(scalar, gbp)
    return .init(offset: i, length: len, state: state)
  }

  @inline(never)
  internal func _foreignForwardGraphemeBreakingState(
    at i: Int,
    with gbp: Unicode._GraphemeBreakProperty?
  ) -> _ForwardGraphemeBreakingState {
    _internalInvariant(isForeign)
    _internalInvariant(i < count)
    let scalars = String.UnicodeScalarView(self)
    let idx = String.Index(_encodedOffset: i)
    let state = _GraphemeBreakingState(scalars[idx], gbp)
    let next = scalars.index(after: idx)
    let length = next._encodedOffset - i
    return .init(offset: i, length: length, state: state)
  }

  internal func formNextGraphemeBreak(
    after p: inout _ForwardGraphemeBreakingState
  ) {
    if _slowPath(isForeign) {
      return _foreignFormNextGraphemeBreak(after: &p)
    }
    withFastUTF8 { utf8 in
      var i = p.offset &+ p.length
      while i < utf8.count {
        let (scalar, len) = _decodeScalar(utf8, startingAt: i)
        if p.state.hasBreak(before: scalar) {
          p.offset = i
          p.length = len
          return
        }
        i &+= len
      }
      p.offset = i
      p.length = 0
    }
  }

  internal func _foreignFormNextGraphemeBreak(
    after p: inout _ForwardGraphemeBreakingState
  ) {
    let scalars = String.UnicodeScalarView(self)
    var i = String.Index(_encodedOffset: p.offset + p.length)
    while i < scalars.endIndex {
      let scalar = scalars[i]
      let j = scalars.index(after: i)
      if p.state.hasBreak(before: scalar) {
        p.offset = i._encodedOffset
        p.length = j._encodedOffset - i._encodedOffset
        return
      }
      i = j
    }
    p.offset = i._encodedOffset
    p.length = 0
  }
}

extension _StringGuts {
  /// Return the length of the extended grapheme cluster starting at offset `i`,
  /// assuming it falls on a grapheme cluster boundary.
  ///
  /// Note: This does not look behind at data preceding `i`, so if `i` is not on
  /// a grapheme cluster boundary, then it may return results that are
  /// inconsistent with `_opaqueCharacterStride(endingAt:)`. On the other hand,
  /// this behavior makes this suitable for use in substrings whose start index
  /// itself does not fall on a cluster boundary.
  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _opaqueCharacterStride(startingAt i: Int) -> Int {
    if _slowPath(isForeign) {
      return _foreignOpaqueCharacterStride(startingAt: i)
    }

    let nextIdx = withFastUTF8 { utf8 in
      _GraphemeBreakingState.nextBoundary(startingAt: i) { j in
        _internalInvariant(j >= 0)
        guard j < utf8.count else { return nil }
        let (scalar, len) = _decodeScalar(utf8, startingAt: j)
        return (scalar, j &+ len)
      }
    }

    return nextIdx &- i
  }

  /// Return the length of the extended grapheme cluster ending at offset `i`,
  /// or if `i` happens to be in the middle of a grapheme cluster, find and
  /// return the distance to its start.
  ///
  /// Note: unlike `_opaqueCharacterStride(startingAt:)`, this method always
  /// finds a correct grapheme cluster boundary.
  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _opaqueCharacterStride(endingAt i: Int) -> Int {
    if _slowPath(isForeign) {
      return _foreignOpaqueCharacterStride(endingAt: i)
    }

    let previousIdx = withFastUTF8 { utf8 in
      _GraphemeBreakingState.previousBoundary(endingAt: i) { j in
        _internalInvariant(j <= utf8.count)
        guard j > 0 else { return nil }
        let (scalar, len) = _decodeScalar(utf8, endingAt: j)
        return (scalar, j &- len)
      }
    }

    return i &- previousIdx
  }

  /// Return the length of the extended grapheme cluster ending at offset `i` in
  /// bounds, or if `i` happens to be in the middle of a grapheme cluster, find
  /// and return the distance to its start.
  ///
  /// Note: unlike `_opaqueCharacterStride(startingAt:)`, this method always
  /// finds a correct grapheme cluster boundary within the substring defined by
  /// the specified bounds.
  @_effects(releasenone)
  internal func _opaqueCharacterStride(
    endingAt i: Int,
    in bounds: Range<Int>
  ) -> Int {
    _internalInvariant(i > bounds.lowerBound && i <= bounds.upperBound)
    if _slowPath(isForeign) {
      return _foreignOpaqueCharacterStride(endingAt: i, in: bounds)
    }

    let previousIdx = withFastUTF8 { utf8 in
      _GraphemeBreakingState.previousBoundary(endingAt: i) { j in
        _internalInvariant(j <= bounds.upperBound)
        guard j > bounds.lowerBound else { return nil }
        let (scalar, len) = _decodeScalar(utf8, endingAt: j)
        return (scalar, j &- len)
      }
    }

    _internalInvariant(bounds.contains(previousIdx))
    return i &- previousIdx
  }

  @inline(never)
  @_effects(releasenone)
  private func _foreignOpaqueCharacterStride(startingAt i: Int) -> Int {
#if _runtime(_ObjC)
    _internalInvariant(isForeign)

    let nextIdx = _GraphemeBreakingState.nextBoundary(startingAt: i) { j in
      _internalInvariant(j >= 0)
      guard j < count else { return nil }
      let scalars = String.UnicodeScalarView(self)
      let idx = String.Index(_encodedOffset: j)

      let scalar = scalars[idx]
      let nextIdx = scalars.index(after: idx)

      return (scalar, nextIdx._encodedOffset)
    }

    return nextIdx &- i
#else
  fatalError("No foreign strings on Linux in this version of Swift")
#endif
  }

  @inline(never)
  @_effects(releasenone)
  private func _foreignOpaqueCharacterStride(
    startingAt i: Int,
    in bounds: Range<Int>
  ) -> Int {
#if _runtime(_ObjC)
    _internalInvariant(isForeign)
    _internalInvariant(bounds.contains(i))

    let nextIdx = _GraphemeBreakingState.nextBoundary(startingAt: i) { j in
      _internalInvariant(j >= bounds.lowerBound)
      guard j < bounds.upperBound else { return nil }
      let scalars = String.UnicodeScalarView(self)
      let idx = String.Index(_encodedOffset: j)

      let scalar = scalars[idx]
      let nextIdx = scalars.index(after: idx)

      return (scalar, nextIdx._encodedOffset)
    }

    return nextIdx &- i
#else
  fatalError("No foreign strings on Linux in this version of Swift")
#endif
  }

  @inline(never)
  @_effects(releasenone)
  private func _foreignOpaqueCharacterStride(endingAt i: Int) -> Int {
#if _runtime(_ObjC)
    _internalInvariant(isForeign)

    let previousIdx = _GraphemeBreakingState.previousBoundary(endingAt: i) { j in
      _internalInvariant(j <= self.count)
      guard j > 0 else { return nil }
      let scalars = String.UnicodeScalarView(self)
      let idx = String.Index(_encodedOffset: j)

      let previousIdx = scalars.index(before: idx)
      let scalar = scalars[previousIdx]

      return (scalar, previousIdx._encodedOffset)
    }

    return i &- previousIdx
#else
  fatalError("No foreign strings on Linux in this version of Swift")
#endif
  }

  @inline(never)
  @_effects(releasenone)
  private func _foreignOpaqueCharacterStride(
    endingAt i: Int,
    in bounds: Range<Int>
  ) -> Int {
#if _runtime(_ObjC)
    _internalInvariant(isForeign)
    _internalInvariant(i > bounds.lowerBound && i <= bounds.upperBound)

    let previousIdx = _GraphemeBreakingState.previousBoundary(endingAt: i) { j in
      _internalInvariant(j <= bounds.upperBound)
      guard j > bounds.lowerBound else { return nil }
      let scalars = String.UnicodeScalarView(self)
      let idx = String.Index(_encodedOffset: j)

      let previousIdx = scalars.index(before: idx)

      let scalar = scalars[previousIdx]
      return (scalar, previousIdx._encodedOffset)
    }

    return i &- previousIdx
#else
  fatalError("No foreign strings on Linux in this version of Swift")
#endif
  }
}

extension Unicode.Scalar {
  fileprivate var _isLinkingConsonant: Bool {
    _swift_stdlib_isLinkingConsonant(value)
  }

  fileprivate var _isVirama: Bool {
    switch value {
    // Devanagari
    case 0x94D:
      return true
    // Bengali
    case 0x9CD:
      return true
    // Gujarati
    case 0xACD:
      return true
    // Oriya
    case 0xB4D:
      return true
    // Telugu
    case 0xC4D:
      return true
    // Malayalam
    case 0xD4D:
      return true

    default:
      return false
    }
  }
}

internal struct _GraphemeBreakingState: Sendable {
  /// The last scalar we have seen.
  internal var lastScalar: Unicode.Scalar

  /// The grapheme break property of the last scalar.
  internal var lastBreakProperty: Unicode._GraphemeBreakProperty

  /// Information about the scalars we have seen so far.
  internal var _flags: UInt8

  internal init() {
    // We don't have a "start of text" value in _GraphemeBreakProperty, so we
    // cannot implement rule GB1 directly. However, NUL is a reasonable
    // substitution, because of rule GB5.
    lastScalar = Unicode.Scalar(0 as UInt8)
    lastBreakProperty = .control
    _flags = 0
  }

  internal init(
    _ firstScalar: Unicode.Scalar,
    _ gbp: Unicode._GraphemeBreakProperty? = nil
  ) {
    lastScalar = firstScalar
    lastBreakProperty = gbp ?? .init(from: firstScalar)
    _flags = 0
  }

  @inline(__always) internal static var _viramaBit: UInt8 { 1 }
  @inline(__always) internal static var _emojiBit: UInt8 { 2 }
  @inline(__always) internal static var _indicBit: UInt8 { 4 }
  @inline(__always) internal static var _regionalIndicatorBit: UInt8 { 8 }

  // When we're looking through an indic sequence, one of the requirements is
  // that there is at LEAST 1 Virama present between two linking consonants.
  // This value helps ensure that when we ultimately need to decide whether or
  // not to break that we've at least seen 1 when walking.
  @inline(__always)
  internal var hasSeenVirama: Bool {
    get { _flags & Self._viramaBit != 0 }
    set {
      if newValue { _flags |= Self._viramaBit }
      else { _flags &= ~Self._viramaBit }
    }
  }

  // When walking forwards in a string, we need to know whether or not we've
  // entered an emoji sequence to be able to eventually break after all of the
  // emoji's various extenders and zero width joiners. This bit allows us to
  // keep track of whether or not we're still in an emoji sequence when deciding
  // to break.
  @inline(__always)
  internal var isInEmojiSequence: Bool {
    get { _flags & Self._emojiBit != 0 }
    set {
      if newValue { _flags |= Self._emojiBit }
      else { _flags &= ~Self._emojiBit }
    }
  }

  // Similar to emoji sequences, we need to know not to break an Indic grapheme
  // sequence. This sequence is (potentially) composed of many scalars and isn't
  // as trivial as comparing two grapheme properties.
  @inline(__always)
  internal var isInIndicSequence: Bool {
    get { _flags & Self._indicBit != 0 }
    set {
      if newValue { _flags |= Self._indicBit }
      else { _flags &= ~Self._indicBit }
    }
  }

  // When walking forward in a string, we need to not break on emoji flag
  // sequences. Emoji flag sequences are composed of 2 regional indicators, so
  // when we see our first (.regionalIndicator, .regionalIndicator) decision,
  // we need to know to return false in this case. However, if the next scalar
  // is another regional indicator, we reach the same decision rule, but in this
  // case we actually need to break there's a boundary between emoji flag
  // sequences.
  @inline(__always)
  internal  var shouldBreakRI: Bool {
    get { _flags & Self._regionalIndicatorBit != 0 }
    set {
      if newValue { _flags |= Self._regionalIndicatorBit }
      else { _flags &= ~Self._regionalIndicatorBit }
    }
  }
}

extension _GraphemeBreakingState: Equatable {
  internal static func ==(left: Self, right: Self) -> Bool {
    left.lastScalar == right.lastScalar
    && left._flags == right._flags
  }
}

extension _GraphemeBreakingState: CustomStringConvertible {
  internal var description: String {
    var f = ""
    if hasSeenVirama { f += "V" }
    if isInEmojiSequence { f += "E" }
    if isInIndicSequence { f += "I" }
    if shouldBreakRI { f += "R" }
    let us = String(lastScalar.value, radix: 16, uppercase: true)
    var r = "[U+\(us):\(lastBreakProperty)"
    if !f.isEmpty { r += ":\(f)" }
    r += "]"
    return r
  }
}

extension Unicode {
  /// A state machine for recognizing character (i.e., extended grapheme
  /// cluster) boundaries in an arbitrary series of Unicode scalars.
  ///
  /// To detect grapheme breaks in a sequence of Unicode scalars, feed each of
  /// them to the `hasBreak(before:)` method. The method returns true if the
  /// sequence has a grapheme break preceding the given value.
  ///
  /// The results produced by this state machine are guaranteed to match the way
  /// `String` splits its contents into `Character` values.
  @available(SwiftStdlib 5.8, *)
  public // SPI(Foundation) FIXME: We need API for this
  struct _CharacterRecognizer: Sendable {
    internal var _state: _GraphemeBreakingState

    /// Returns a non-nil value if it can be determined whether there is a
    /// grapheme break between `scalar1` and `scalar2` without knowing anything
    /// about the scalars that precede `scalar1`. This can optionally be used as
    /// a fast (but incomplete) test before spinning up a full state machine
    /// session.
    @_effects(releasenone)
    public static func quickBreak(
      between scalar1: Unicode.Scalar,
      and scalar2: Unicode.Scalar
    ) -> Bool? {
      _GraphemeBreakingState.hasKnownBreak(between: scalar1, and: scalar2)
    }

    /// Initialize a new character recognizer at the _start of text_ (sot)
    /// position.
    ///
    /// The resulting state machine will report a grapheme break on the
    /// first scalar that is fed to it.
    public init() {
      _state = _GraphemeBreakingState()
    }

    /// Feeds the next scalar to the state machine, returning a Boolean value
    /// indicating whether it starts a new extended grapheme cluster.
    ///
    /// This method will always report a break the first time it is called
    /// on a newly initialized recognizer.
    ///
    /// The state machine does not carry information across character
    /// boundaries. I.e., if this method returns true, then `self` after the
    /// call is equivalent to feeding the same scalar to a newly initialized
    /// recognizer instance.
    @_effects(releasenone)
    public mutating func hasBreak(
      before nextScalar: Unicode.Scalar
    ) -> Bool {
      _state.hasBreak(before: nextScalar)
    }

    /// Decode the scalars in the given UTF-8 buffer and feed them to the
    /// recognizer up to and including the scalar following the first grapheme
    /// break. If the buffer contains a grapheme break, then this function
    /// returns the index range of the scalar that follows the first one;
    /// otherwise it returns `nil`.
    ///
    /// On return, the state of the recognizer is updated to reflect the scalars
    /// up to and including the returned one. You can detect additional grapheme
    /// breaks by feeding the recognizer subsequent data.
    ///
    /// - Parameter buffer: A buffer containing valid UTF-8 data, starting and
    ///    ending on Unicode scalar boundaries.
    ///
    /// - Parameter start: A valid index into `buffer`, addressing the first
    ///    code unit of a UTF-8 scalar in the buffer, or the end.
    ///
    /// - Returns: The index range of the scalar that follows the first grapheme
    ///    break in the buffer, if there is one. If the buffer contains no
    ///    grapheme breaks, then this function returns `nil`.
    ///
    /// - Warning: This function does not validate that the buffer contains
    ///    valid UTF-8 data; its behavior is undefined if given invalid input.
    @_effects(releasenone)
    public mutating func _firstBreak(
      inUncheckedUnsafeUTF8Buffer buffer: UnsafeBufferPointer<UInt8>,
      startingAt start: Int = 0
    ) -> Range<Int>? {
      var i = start
      while i < buffer.endIndex {
        let (next, n) = _decodeScalar(buffer, startingAt: i)
        if hasBreak(before: next) {
          return Range(_uncheckedBounds: (i, i &+ n))
        }
        i &+= n
      }
      return nil
    }
  }
}

@available(SwiftStdlib 5.8, *)
extension Unicode._CharacterRecognizer: Equatable {
  public static func ==(left: Self, right: Self) -> Bool {
    left._state == right._state
  }
}

@available(SwiftStdlib 5.8, *)
extension Unicode._CharacterRecognizer: CustomStringConvertible {
  public var description: String {
    "\(_state)"
  }
}


extension _GraphemeBreakingState {
  // Returns the stride of the grapheme cluster starting at offset `index`,
  // assuming it is on a grapheme cluster boundary.
  //
  // This method never looks at data below `index`. If `index` isn't on a
  // grapheme cluster boundary, then the result may not be consistent with the
  // actual breaks in the string. `Substring` relies on this to generate the
  // right breaks if its start index isn't aligned on one -- in this case, the
  // substring's breaks may not match the ones in its base string.
  internal static func nextBoundary(
    startingAt index: Int,
    nextScalar: (Int) -> (scalar: Unicode.Scalar, end: Int)?
  ) -> Int {
    // Note: If `index` isn't already on a boundary, then starting with an empty
    // state here sometimes leads to this method returning results that diverge
    // from the true breaks in the string.
    let next = nextScalar(index)!
    var state = _GraphemeBreakingState(next.scalar)
    return state.nextBoundary(startingAt: next.end, nextScalar: nextScalar)
  }

  internal mutating func nextBoundary(
    startingAt index: Int,
    nextScalar: (Int) -> (scalar: Unicode.Scalar, end: Int)?
  ) -> Int {
    // Note: If `index` isn't already on a boundary, then starting with an empty
    // state here sometimes leads to this method returning results that diverge
    // from the true breaks in the string.
    var index = index
    while true {
      guard let next = nextScalar(index) else { break }
      if self.hasBreak(before: next.scalar) { break }
      index = next.end
    }
    return index
  }
}

extension _GraphemeBreakingState {
  // Returns the stride of the grapheme cluster ending at offset `index`.
  //
  // This method uses `previousScalar` to looks back in the string as far as
  // necessary to find a correct grapheme cluster boundary, whether or not
  // `index` happens to be on a boundary itself.
  internal static func previousBoundary(
    endingAt index: Int,
    previousScalar: (Int) -> (scalar: Unicode.Scalar, start: Int)?
  ) -> Int {
    // FIXME: This requires potentially arbitrary lookback in each iteration,
    // leading to quadratic behavior in some edge cases. Ideally lookback should
    // only be done once per cluster (or in the case of RI sequences, once per
    // flag sequence). One way to avoid most quadratic behavior is to replace
    // this implementation with a scheme that first searches backwards for a
    // safe point then iterates forward using the regular `shouldBreak` until we
    // reach `index`, as recommended in section 6.4 of TR#29.
    //
    // https://www.unicode.org/reports/tr29/#Random_Access

    var (scalar2, index) = previousScalar(index)!

    while true {
      guard let (scalar1, previousIndex) = previousScalar(index) else { break }
      if shouldBreakWithLookback(
        between: scalar1, and: scalar2, at: index, with: previousScalar
      ) {
        break
      }
      index = previousIndex
      scalar2 = scalar1
    }

    return index
  }
}

extension _GraphemeBreakingState {
  static func hasKnownBreak(
    between scalar1: Unicode.Scalar, and scalar2: Unicode.Scalar
  ) -> Bool? {
    if scalar1 == ._cr && scalar2 == ._lf {
      return false
    }
    let x = Unicode._GraphemeBreakProperty(from: scalar1)
    let y = Unicode._GraphemeBreakProperty(from: scalar2)

    switch (x, y) {
    // Fast path: If we know our scalars have no properties the decision is
    //            trivial and we don't need to crawl to the default statement.
    case (.any, .any):
      return true

    // GB4
    case (.control, _):
      return true

    // GB5
    case (_, .control):
      return true

    // GB6
    case (.l, .l),
         (.l, .v),
         (.l, .lv),
         (.l, .lvt):
      return false

    // GB7
    case (.lv, .v),
         (.v, .v),
         (.lv, .t),
         (.v, .t):
      return false

    // GB8
    case (.lvt, .t),
         (.t, .t):
      return false

    // GB9
    case (_, .extend),
         (_, .zwj):
      return false

    // GB9a
    case (_, .spacingMark):
      return false

    // GB9b
    case (.prepend, _):
      return false

    // GB11
    case (.zwj, .extendedPictographic):
      return nil

    // GB12 & GB13
    case (.regionalIndicator, .regionalIndicator):
      return nil

    // GB9c
    case (.extend, _),
         (.zwj, _):
      if scalar2._isLinkingConsonant {
        return nil
      }
      return true

    // GB999
    default:
      return true
    }
  }
}

extension _GraphemeBreakingState {
  // Return true if there is an extended grapheme cluster boundary between two
  // scalars, based on state information previously collected about preceding
  // scalars.
  //
  // This method never looks at scalars other than the two that are explicitly
  // passed to it. The `state` parameter is assumed to hold all contextual
  // information necessary to make a correct decision; it gets updated with more
  // data as needed.
  //
  // This is based on the Unicode Annex #29 for [Grapheme Cluster Boundary
  // Rules](https://unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules).
  internal mutating func hasBreak(before nextScalar: Unicode.Scalar) -> Bool {
    let last = self
    self.lastScalar = nextScalar

    // We need to toggle each flag to false after every scalar, unless we happen
    // to fall into one of the narrow cases that indicate otherwise below.
    self._flags = 0

    // GB3 (CR/LF)
    if last.lastScalar == ._cr && nextScalar == ._lf {
      self.lastBreakProperty = .control
      return false
    }

    let x = last.lastBreakProperty
    let y = Unicode._GraphemeBreakProperty(from: nextScalar)
    self.lastBreakProperty = y

    switch (x, y) {

    // Fast path: If we know our scalars have no properties the decision is
    //            trivial and we don't need to crawl to the default statement.
    case (.any, .any):
      return true

    // GB4
    case (.control, _):
      return true

    // GB5
    case (_, .control):
      return true

    // GB6
    case (.l, .l),
         (.l, .v),
         (.l, .lv),
         (.l, .lvt):
      return false

    // GB7
    case (.lv, .v),
         (.v, .v),
         (.lv, .t),
         (.v, .t):
      return false

    // GB8
    case (.lvt, .t),
         (.t, .t):
      return false

    // GB9 (partial GB11)
    case (_, .extend),
         (_, .zwj):

      // Prepare for recognizing GB11, by remembering if we're in an emoji
      // sequence.
      //
      //   GB11: Extended_Pictographic Extend* ZWJ × Extended_Pictographic
      //
      // If our left-side scalar is a pictograph, then it starts a new emoji
      // sequence; the sequence continues through subsequent extend/extend and
      // extend/zwj pairs.
      if (
        x == .extendedPictographic || (self.isInEmojiSequence && x == .extend)
      ) {
        self.isInEmojiSequence = true
      }

      // If we're currently in an indic sequence (or if our lhs is a linking
      // consonant), then this check and everything underneath ensures that
      // we continue being in one and may check if this extend is a Virama.
      if last.isInIndicSequence || last.lastScalar._isLinkingConsonant {
        if y == .extend {
          let extendNormData = Unicode._NormData(nextScalar, fastUpperbound: 0x300)

          // If our extend's CCC is 0, then this rule does not apply.
          guard extendNormData.ccc != 0 else {
            // Note: last.hasSeenVirama may be true, but it's cleared now
            return false
          }
        }

        self.isInIndicSequence = true

        if last.hasSeenVirama || nextScalar._isVirama {
          self.hasSeenVirama = true
        }
      }

      return false

    // GB9a
    case (_, .spacingMark):
      return false

    // GB9b
    case (.prepend, _):
      return false

    // GB11
    case (.zwj, .extendedPictographic):
      return !last.isInEmojiSequence

    // GB12 & GB13
    case (.regionalIndicator, .regionalIndicator):
      if last.shouldBreakRI {
        return true
      }
      self.shouldBreakRI = true
      return false

    // GB999
    default:
      // GB9c
      if
        last.isInIndicSequence,
        last.hasSeenVirama,
        nextScalar._isLinkingConsonant
      {
        self.hasSeenVirama = false
        return false
      }

      return true
    }
  }
}

extension _GraphemeBreakingState {
  // Return true if there is an extended grapheme cluster boundary between two
  // scalars, with no previous knowledge about preceding scalars.
  //
  // This method looks back as far as it needs to determine the correct
  // placement of boundaries.
  //
  // This is based off of the Unicode Annex #29 for [Grapheme Cluster Boundary
  // Rules](https://unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules).
  internal static func shouldBreakWithLookback(
    between scalar1: Unicode.Scalar,
    and scalar2: Unicode.Scalar,
    at index: Int,
    with previousScalar: (Int) -> (scalar: Unicode.Scalar, start: Int)?
  ) -> Bool {
    // GB3
    if scalar1 == ._cr, scalar2 == ._lf {
      return false
    }

    let x = Unicode._GraphemeBreakProperty(from: scalar1)
    let y = Unicode._GraphemeBreakProperty(from: scalar2)

    switch (x, y) {

    // Fast path: If we know our scalars have no properties the decision is
    //            trivial and we don't need to crawl to the default statement.
    case (.any, .any):
      return true

    // GB4
    case (.control, _):
      return true

    // GB5
    case (_, .control):
      return true

    // GB6
    case (.l, .l),
         (.l, .v),
         (.l, .lv),
         (.l, .lvt):
      return false

    // GB7
    case (.lv, .v),
         (.v, .v),
         (.lv, .t),
         (.v, .t):
      return false

    // GB8
    case (.lvt, .t),
         (.t, .t):
      return false

    // GB9
    case (_, .extend),
         (_, .zwj):
      return false

    // GB9a
    case (_, .spacingMark):
      return false

    // GB9b
    case (.prepend, _):
      return false

    // GB11
    case (.zwj, .extendedPictographic):
      return !checkIfInEmojiSequence(at: index, with: previousScalar)

    // GB12 & GB13
    case (.regionalIndicator, .regionalIndicator):
      return countRIs(at: index, with: previousScalar)

    // GB9c
    case (.extend, _):
      guard scalar2._isLinkingConsonant else { return true }
      let extendNormData = Unicode._NormData(scalar1, fastUpperbound: 0x300)
      guard extendNormData.ccc != 0 else { return true }
      return !checkIfInIndicSequence(at: index, with: previousScalar)
    case (.zwj, _):
      guard scalar2._isLinkingConsonant else { return true }
      return !checkIfInIndicSequence(at: index, with: previousScalar)

    // GB999
    default:
      return true
    }
  }

  // When walking backwards, it's impossible to know whether we were in an emoji
  // sequence without walking further backwards. This walks the string backwards
  // enough until we figure out whether or not to break our
  // (.zwj, .extendedPictographic) question. For example:
  //
  // Scalar view #1:
  //
  //     [.control, .zwj, .extendedPictographic]
  //                     ^
  //                     | = To determine whether or not we break here, we need
  //                         to see the previous scalar's grapheme property.
  //          ^
  //          | = This is neither .extendedPictographic nor .extend, thus we
  //              were never in an emoji sequence, so break between the .zwj
  //              and .extendedPictographic.
  //
  // Scalar view #2:
  //
  //     [.extendedPictographic, .zwj, .extendedPictographic]
  //                                  ^
  //                                  | = Same as above, move backwards one to
  //                                      view the previous scalar's property.
  //                ^
  //                | = This is an .extendedPictographic, so this indicates that
  //                    we are in an emoji sequence, so we should NOT break
  //                    between the .zwj and .extendedPictographic.
  //
  // Scalar view #3:
  //
  //     [.extendedPictographic, .extend, .extend, .zwj, .extendedPictographic]
  //                                                    ^
  //                                                    | = Same as above
  //                                         ^
  //                                         | = This is an .extend which means
  //                                             there is a potential emoji
  //                                             sequence, walk further backwards
  //                                             to find an .extendedPictographic.
  //
  //                               <-- = Another extend, go backwards more.
  //                ^
  //                | = We found our starting .extendedPictographic letting us
  //                    know that we are in an emoji sequence so our initial
  //                    break question is answered as NO.
  internal static func checkIfInEmojiSequence(
    at index: Int,
    with previousScalar: (Int) -> (scalar: Unicode.Scalar, start: Int)?
  ) -> Bool {
    guard var i = previousScalar(index)?.start else { return false }
    while let prev = previousScalar(i) {
      i = prev.start
      let gbp = Unicode._GraphemeBreakProperty(from: prev.scalar)

      switch gbp {
      case .extend:
        continue
      case .extendedPictographic:
        return true
      default:
        return false
      }
    }
    return false
  }

  // When walking backwards, it's impossible to know whether we break when we
  // see our first ((.extend|.zwj), .linkingConsonant) without walking
  // further backwards. This walks the string backwards enough until we figure
  // out whether or not to break this indic sequence. For example:
  //
  // Scalar view #1:
  //
  //     [.virama, .extend, .linkingConsonant]
  //                       ^
  //                       | = To be able to know whether or not to break these
  //                           two, we need to walk backwards to determine if
  //                           this is a legitimate indic sequence.
  //      ^
  //      | = The scalar sequence ends without a starting linking consonant,
  //          so this is in fact not an indic sequence, so we can break the two.
  //
  // Scalar view #2:
  //
  //     [.linkingConsonant, .virama, .extend, .linkingConsonant]
  //                                          ^
  //                                          | = Same as above
  //                            ^
  //                            | = This is a virama, so we at least have seen
  //                                1 to be able to return true if we see a
  //                                linking consonant later.
  //         ^
  //         | = Is a linking consonant and we've seen a virama, so this is a
  //             legitimate indic sequence, so do NOT break the initial question.
  internal static func checkIfInIndicSequence(
    at index: Int,
    with previousScalar: (Int) -> (scalar: Unicode.Scalar, start: Int)?
  ) -> Bool {
    guard let p = previousScalar(index) else { return false }

    var hasSeenVirama = p.scalar._isVirama
    var i = p.start

    while let (scalar, prev) = previousScalar(i) {
      i = prev
      let gbp = Unicode._GraphemeBreakProperty(from: scalar)

      switch (gbp, scalar._isLinkingConsonant) {
      case (.extend, false):
        let extendNormData = Unicode._NormData(scalar, fastUpperbound: 0x300)

        guard extendNormData.ccc != 0 else {
          return false
        }

        if scalar._isVirama {
          hasSeenVirama = true
        }

      case (.zwj, false):
        continue

      // LinkingConsonant
      case (_, true):
        return hasSeenVirama

      default:
        return false
      }
    }
    return false
  }

  // When walking backwards, it's impossible to know whether we break when we
  // see our first (.regionalIndicator, .regionalIndicator) without walking
  // further backwards. This walks the string backwards enough until we figure
  // out whether or not to break these RIs. For example:
  //
  // Scalar view #1:
  //
  //     [.control, .regionalIndicator, .regionalIndicator]
  //                                   ^
  //                                   | = To be able to know whether or not to
  //                                       break these two, we need to walk
  //                                       backwards to determine if there were
  //                                       any previous .regionalIndicators in
  //                                       a row.
  //         ^
  //         | = Not a .regionalIndicator, so our total riCount is 0 and 0 is
  //             even thus we do not break.
  //
  // Scalar view #2:
  //
  //     [.control, .regionalIndicator, .regionalIndicator, .regionalIndicator]
  //                                                       ^
  //                                                       | = Same as above
  //                         ^
  //                         | = This is a .regionalIndicator, so continue
  //                             walking backwards for more of them. riCount is
  //                             now equal to 1.
  //         ^
  //         | = Not a .regionalIndicator. riCount = 1 which is odd, so break
  //             the last two .regionalIndicators.
  internal static func countRIs(
    at index: Int,
    with previousScalar: (Int) -> (scalar: Unicode.Scalar, start: Int)?
  ) -> Bool {
    guard let p = previousScalar(index) else { return false }
    var i = p.start
    var riCount = 0
    while let p = previousScalar(i) {
      i = p.start

      let gbp = Unicode._GraphemeBreakProperty(from: p.scalar)
      guard gbp == .regionalIndicator else {
        break
      }

      riCount += 1
    }
    return riCount & 1 != 0
  }
}

//===--- StringCharacterView.swift - String's Collection of Characters ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  String is-not-a Sequence or Collection, but it exposes a
//  collection of characters.
//
//===----------------------------------------------------------------------===//

// FIXME(ABI)#70 : The character string view should have a custom iterator type to
// allow performance optimizations of linear traversals.

/// CR and LF are common special cases in grapheme breaking logic
@_versioned internal var _CR: UInt8 { return 0x0d }
@_versioned internal var _LF: UInt8 { return 0x0a }

import SwiftShims

extension String {
  /// A view of a string's contents as a collection of characters.
  ///
  /// In Swift, every string provides a view of its contents as characters. In
  /// this view, many individual characters---for example, "é", "김", and
  /// "🇮🇳"---can be made up of multiple Unicode code points. These code points
  /// are combined by Unicode's boundary algorithms into *extended grapheme
  /// clusters*, represented by the `Character` type. Each element of a
  /// `CharacterView` collection is a `Character` instance.
  ///
  ///     let flowers = "Flowers 💐"
  ///     for c in flowers.characters {
  ///         print(c)
  ///     }
  ///     // F
  ///     // l
  ///     // o
  ///     // w
  ///     // e
  ///     // r
  ///     // s
  ///     //
  ///     // 💐
  ///
  /// You can convert a `String.CharacterView` instance back into a string
  /// using the `String` type's `init(_:)` initializer.
  ///
  ///     let name = "Marie Curie"
  ///     if let firstSpace = name.characters.index(of: " ") {
  ///         let firstName = String(name.characters[..<firstSpace])
  ///         print(firstName)
  ///     }
  ///     // Prints "Marie"
  public struct CharacterView {
    @_versioned
    internal var _core: _StringCore

    /// The offset of this view's `_core` from an original core. This works
    /// around the fact that `_StringCore` is always zero-indexed.
    /// `_coreOffset` should be subtracted from `UnicodeScalarIndex.encodedOffset`
    /// before that value is used as a `_core` index.
    @_versioned
    internal var _coreOffset: Int

    /// Creates a view of the given string.
    public init(_ text: String) {
      self._core = text._core
      self._coreOffset = 0
    }
    
    public // @testable
    init(_ _core: _StringCore, coreOffset: Int = 0) {
      self._core = _core
      self._coreOffset = coreOffset
    }
  }

  /// A view of the string's contents as a collection of characters.
  public var characters: CharacterView {
    get {
      return CharacterView(self)
    }
    set {
      self = String(newValue)
    }
  }

  /// Applies the given closure to a mutable view of the string's characters.
  ///
  /// Do not use the string that is the target of this method inside the
  /// closure passed to `body`, as it may not have its correct value. Instead,
  /// use the closure's `CharacterView` argument.
  ///
  /// This example below uses the `withMutableCharacters(_:)` method to
  /// truncate the string `str` at the first space and to return the remainder
  /// of the string.
  ///
  ///     var str = "All this happened, more or less."
  ///     let afterSpace = str.withMutableCharacters { chars -> String.CharacterView in
  ///         if let i = chars.index(of: " ") {
  ///             let result = chars[chars.index(after: i)...]
  ///             chars.removeSubrange(i...)
  ///             return result
  ///         }
  ///         return String.CharacterView()
  ///     }
  ///
  ///     print(str)
  ///     // Prints "All"
  ///     print(String(afterSpace))
  ///     // Prints "this happened, more or less."
  ///
  /// - Parameter body: A closure that takes a character view as its argument.
  ///   If `body` has a return value, that value is also used as the return
  ///   value for the `withMutableCharacters(_:)` method. The `CharacterView`
  ///   argument is valid only for the duration of the closure's execution.
  /// - Returns: The return value, if any, of the `body` closure parameter.
  public mutating func withMutableCharacters<R>(
    _ body: (inout CharacterView) -> R
  ) -> R {
    // Naively mutating self.characters forces multiple references to
    // exist at the point of mutation. Instead, temporarily move the
    // core of this string into a CharacterView.
    var tmp = CharacterView("")
    (_core, tmp._core) = (tmp._core, _core)
    let r = body(&tmp)
    (_core, tmp._core) = (tmp._core, _core)
    return r
  }

  /// Creates a string from the given character view.
  ///
  /// Use this initializer to recover a string after performing a collection
  /// slicing operation on a string's character view.
  ///
  ///     let poem = """
  ///           'Twas brillig, and the slithy toves /
  ///           Did gyre and gimbal in the wabe: /
  ///           All mimsy were the borogoves /
  ///           And the mome raths outgrabe.
  ///           """
  ///     let excerpt = String(poem.characters.prefix(22)) + "..."
  ///     print(excerpt)
  ///     // Prints "'Twas brillig, and the..."
  ///
  /// - Parameter characters: A character view to convert to a string.
  public init(_ characters: CharacterView) {
    self.init(characters._core)
  }
}

extension String.CharacterView : _SwiftStringView {
  var _persistentContent : String {
    // FIXME: we might want to make sure our _StringCore isn't somehow a slice
    // of some larger storage before blindly wrapping/returning it as
    // persistent.  That said, if current benchmarks are measuring these cases,
    // we might end up regressing something by copying the storage.  For now,
    // assume we are not a slice; we can come back and measure the effects of
    // this fix later.  If we make the fix we should use the line below as an
    // implementation of _ephemeralContent
    return String(self._core)
  }
}


/// `String.CharacterView` is a collection of `Character`.
extension String.CharacterView : BidirectionalCollection {
  internal typealias UnicodeScalarView = String.UnicodeScalarView
  @_versioned
  internal var unicodeScalars: UnicodeScalarView {
    return UnicodeScalarView(_core, coreOffset: _coreOffset)
  }
  
  public typealias Index = String.Index
  public typealias IndexDistance = Int

  /// The position of the first character in a nonempty character view.
  /// 
  /// In an empty character view, `startIndex` is equal to `endIndex`.
  public var startIndex: Index {
    return unicodeScalars.startIndex
  }

  /// A character view's "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// In an empty character view, `endIndex` is equal to `startIndex`.
  public var endIndex: Index {
    return unicodeScalars.endIndex
  }

  internal func _index(atEncodedOffset n: Int) -> Index {
    let stride = _measureExtendedGraphemeClusterForward(
      from: Index(encodedOffset: n))
    return Index(encodedOffset: n, .character(stride: UInt16(stride)))
  }
  
  /// Returns the next consecutive position after `i`.
  ///
  /// - Precondition: The next position is valid.
  public func index(after i: Index) -> Index {
    _precondition(
      i < unicodeScalars.endIndex,
      "cannot increment beyond endIndex")
    
    _precondition(
      i >= unicodeScalars.startIndex,
      "cannot increment invalid index")

    var j = i
    while true {
      if case .character(let oldStride) = j._cache {
        return _index(atEncodedOffset: j.encodedOffset + Int(oldStride))
      }
      j = _index(atEncodedOffset: j.encodedOffset)
    }
  }

  /// Returns the previous consecutive position before `i`.
  ///
  /// - Precondition: The previous position is valid.
  public func index(before i: Index) -> Index {
    _precondition(i > unicodeScalars.startIndex,
      "cannot decrement before startIndex")
    _precondition(i <= unicodeScalars.endIndex,
      "cannot decrement invalid index")
    
    let stride = _measureExtendedGraphemeClusterBackward(
      from: Index(encodedOffset: i.encodedOffset))
    
    return Index(
      encodedOffset: i.encodedOffset &- stride,
      .character(stride: numericCast(stride))
    )
  }

  /// Fast check for a (stable) grapheme break between two UInt16 code units
  @_inlineable
  @_versioned
  internal static func _quickCheckGraphemeBreakBetween(
    _ lhs: UInt16, _ rhs: UInt16
  ) -> Bool {
    // With the exception of CR-LF, there is always a grapheme break between two
    // sub-0x300 code units
    if lhs < 0x300 && rhs < 0x300 {
      return lhs != UInt16(_CR) && rhs != UInt16(_LF)
    }

    return _internalExtraCheckGraphemeBreakBetween(lhs, rhs)
  }

  // A quick check helper to quickly perform extra grapheme-break-between
  // checks that tightly integrate Unicode version-specific assumptions. Should
  // never be inlined into user code, as it is version- specific.
  //
  // TODO: this is actually fine to inline into non-inlinable code
  //
  @inline(never) // @inline(resilient_only)
  @_versioned
  internal static func _internalExtraCheckGraphemeBreakBetween(
    _ lhs: UInt16, _ rhs: UInt16
  ) -> Bool {
    _sanityCheck(
      lhs != _CR || rhs != _LF,
      "CR-LF special case handled by _quickCheckGraphemeBreakBetween")

    // Whether the given scalar, when it appears paired with another scalar
    // satisfying this property, has a grapheme break between it and the other
    // scalar.
    func hasBreakWhenPaired(_ x: UInt16) -> Bool {
      // TODO: This doesn't generate optimal code, tune/re-write at a lower
      // level.
      //
      // NOTE: Order of case ranges affects codegen, and thus performance. All
      // things being equal, keep existing order below.
      switch x {
      // Unified CJK Han ideographs, common and some supplemental, amongst
      // others:
      //   0x3400-0xA4CF
      case 0x3400...0xa4cf: return true
      // TODO: CJK punctuation

      // Repeat sub-300 check, this is beneficial for common cases of Latin
      // characters embedded within non-Latin script (e.g. newlines, spaces,
      // proper nouns and/or jargon, punctuation).
      //
      // NOTE: CR-LF special case has already been checked.
      case 0x0000...0x02ff: return true

      // TODO: general punctuation

      // Non-combining kana:
      //   0x3041-0x3096
      //   0x30A1-0x30FA
      case 0x3041...0x3096: return true
      case 0x30a1...0x30fa: return true

      // Non-combining modern (and some archaic) Cyrillic:
      //   0x0400-0x0482 (first half of Cyrillic block)
      case 0x0400...0x0482: return true

      // Modern Arabic, excluding extenders and prependers:
      //   0x061D-0x064A
      case 0x061d...0x064a: return true

      // Precomposed Hangul syllables:
      //   0xAC00–0xD7AF
      case 0xac00...0xd7af: return true

      default: return false
      }
    }
    return hasBreakWhenPaired(lhs) && hasBreakWhenPaired(rhs)
  }

  // NOTE: Because this function is inlineable, it should contain only the fast
  // paths of grapheme breaking that we have high confidence won't change.
  /// Returns the length of the first extended grapheme cluster in UTF-16
  /// code units.
  @_inlineable
  @_versioned
  internal func _measureExtendedGraphemeClusterForward(
    from start: UnicodeScalarView.Index
  ) -> Int {
    let end = unicodeScalars.endIndex
    if start == end {
      return 0
    }

    // Our relative position (offset). If our _core is not a substring, this is
    // the same as start.encodedOffset.
    let relativeOffset = start.encodedOffset - _coreOffset

    // Grapheme breaking is much simpler if known ASCII
    if _core.isASCII {
      _onFastPath() // Please aggressively inline
      let asciiBuffer = _core.asciiBuffer._unsafelyUnwrappedUnchecked

      // With the exception of CR-LF, ASCII graphemes are single-scalar. Check
      // for that one exception.
      if _slowPath(
        asciiBuffer[relativeOffset] == _CR &&
        relativeOffset+1 < asciiBuffer.endIndex &&
        asciiBuffer[relativeOffset+1] == _LF
      ) {
        return 2
      }

      return 1
    } else {
      // TODO: Check for (potentially non-contiguous) ASCII NSStrings,
      // especially small tagged pointers.
    }
    
    let startIndexUTF16 = start.encodedOffset

    // Last scalar is its own grapheme
    if (startIndexUTF16+1 == end.encodedOffset) {
      return 1
    }

    // Perform a quick single-code-unit grapheme check
    if _core._baseAddress != nil {
      if String.CharacterView._quickCheckGraphemeBreakBetween(
        _core._nthContiguous(relativeOffset),
        _core._nthContiguous(relativeOffset+1)
      ) {
        return 1
      }
    } else {
      // TODO: Check for (potentially non-contiguous) UTF16 NSStrings,
      // especially small tagged pointers
    }
    return _measureExtendedGraphemeClusterForwardSlow(
      relativeOffset: relativeOffset,
      start: start,
      end: end,
      startIndexUTF16: startIndexUTF16
    )
  }
  
  @inline(never)
  @_versioned
  func _measureExtendedGraphemeClusterForwardSlow(
    relativeOffset: Int,
    start: String.UnicodeScalarView.Index,
    end: String.UnicodeScalarView.Index,
    startIndexUTF16: Int
  ) -> Int {
    if _core._baseAddress != nil {
      _onFastPath() // Please aggressively inline
      let breakIterator = _ThreadLocalStorage.getUBreakIterator(for: _core)
      let ubrkFollowing = __swift_stdlib_ubrk_following(
        breakIterator, Int32(relativeOffset)
      )
      // ubrk_following may return UBRK_DONE (-1). Treat that as the rest of the
      // string.
      let nextPosition =
        ubrkFollowing == -1 ? end.encodedOffset : Int(ubrkFollowing)
      return nextPosition - relativeOffset
    } else {
      // TODO: See if we can get fast character contents.
    }

    // FIXME: Need to handle the general case correctly with Unicode 9+
    // semantics, as opposed to this legacy Unicode 8 path. This gets hit for
    // e.g. non-contiguous NSStrings. In such cases, there may be an alternative
    // CFString API available, or worst case we can map over it via UTextFuncs.

    return legacyGraphemeForward(
      start: start, end: end, startIndexUTF16: startIndexUTF16
    )
  }

  @inline(never)
  func legacyGraphemeForward(
    start: UnicodeScalarView.Index,
    end: UnicodeScalarView.Index,
    startIndexUTF16: Int
  ) -> Int {
    var start = start
    let graphemeClusterBreakProperty =
      _UnicodeGraphemeClusterBreakPropertyTrie()
    let segmenter = _UnicodeExtendedGraphemeClusterSegmenter()
    
    var gcb0 = graphemeClusterBreakProperty.getPropertyRawValue(
      unicodeScalars[start].value)
    unicodeScalars.formIndex(after: &start)
    
    while start != end {
      // FIXME(performance): consider removing this "fast path".  A branch
      // that is hard to predict could be worse for performance than a few
      // loads from cache to fetch the property 'gcb1'.
      if segmenter.isBoundaryAfter(gcb0) {
        break
      }
      let gcb1 = graphemeClusterBreakProperty.getPropertyRawValue(
        unicodeScalars[start].value)
      if segmenter.isBoundary(gcb0, gcb1) {
        break
      }
      gcb0 = gcb1
      unicodeScalars.formIndex(after: &start)
    }
    
    return start.encodedOffset - startIndexUTF16
  }

  // NOTE: Because this function is inlineable, it should contain only the fast
  // paths of grapheme breaking that we have high confidence won't change.
  //
  /// Returns the length of the previous extended grapheme cluster in UTF-16
  /// code units.
  @_inlineable
  @_versioned
  internal func _measureExtendedGraphemeClusterBackward(
    from end: UnicodeScalarView.Index
  ) -> Int {
    let start = unicodeScalars.startIndex
    if start == end {
      return 0
    }

    // The relative position (offset) to the last code unit.
    let lastOffset = end.encodedOffset - _coreOffset - 1
    // The relative position (offset) that is one-past-the-last
    let endOffset = lastOffset + 1

    // Grapheme breaking is much simpler if known ASCII
    if _core.isASCII {
      _onFastPath() // Please aggressively inline
      let asciiBuffer = _core.asciiBuffer._unsafelyUnwrappedUnchecked
      _sanityCheck(
        lastOffset >= asciiBuffer.startIndex,
        "should of been caught in earlier start-of-scalars check")

      // With the exception of CR-LF, ASCII graphemes are single-scalar. Check
      // for that one exception.
      if _slowPath(
        asciiBuffer[lastOffset] == _LF &&
        lastOffset-1 >= asciiBuffer.startIndex &&
        asciiBuffer[lastOffset-1] == _CR
      ) {
        return 2
      }

      return 1
    }
    
    let endIndexUTF16 = end.encodedOffset

    // First scalar is its own grapheme
    if (endIndexUTF16-1 == start.encodedOffset) {
      return 1
    }

    // Perform a quick single-code-unit grapheme check
    if _core._baseAddress != nil {
      if String.CharacterView._quickCheckGraphemeBreakBetween(
        _core._nthContiguous(lastOffset-1),
        _core._nthContiguous(lastOffset)
      ) {
        return 1
      }
    }
    return _measureExtendedGraphemeClusterBackwardSlow(
      endOffset: endOffset, start: start, end: end, endIndexUTF16: endIndexUTF16
    )
  }
  
  @inline(never)
  @_versioned
  func _measureExtendedGraphemeClusterBackwardSlow(
    endOffset: Int,
    start: String.UnicodeScalarView.Index,
    end: String.UnicodeScalarView.Index,
    endIndexUTF16: Int
  ) -> Int {
    if _core._baseAddress != nil {
      _onFastPath() // Please aggressively inline
      let breakIterator = _ThreadLocalStorage.getUBreakIterator(for: _core)
      let ubrkPreceding = __swift_stdlib_ubrk_preceding(
        breakIterator, Int32(endOffset)
      )
      // ubrk_following may return UBRK_DONE (-1). Treat that as the rest of the
      // string.
      let priorPosition =
        ubrkPreceding == -1 ? start.encodedOffset : Int(ubrkPreceding)
      return endOffset - priorPosition
    } else {
      // TODO: See if we can get fast character contents.
    }

    // FIXME: Need to handle the general case correctly with Unicode 9+
    // semantics, as opposed to this legacy Unicode 8 path. This gets hit for
    // e.g. non-contiguous NSStrings. In such cases, there may be an alternative
    // CFString API available, or worst case we can map over it via UTextFuncs.

    return legacyGraphemeBackward(
      start: start, end: end, endIndexUTF16: endIndexUTF16
    )
  }

  @inline(never)
  func legacyGraphemeBackward(
    start: UnicodeScalarView.Index,
    end: UnicodeScalarView.Index,
    endIndexUTF16: Int
  ) -> Int {
    let graphemeClusterBreakProperty =
      _UnicodeGraphemeClusterBreakPropertyTrie()
    let segmenter = _UnicodeExtendedGraphemeClusterSegmenter()
    
    var graphemeClusterStart = end
    
    unicodeScalars.formIndex(before: &graphemeClusterStart)
    var gcb0 = graphemeClusterBreakProperty.getPropertyRawValue(
      unicodeScalars[graphemeClusterStart].value)
    
    var graphemeClusterStartUTF16 = graphemeClusterStart.encodedOffset
    
    while graphemeClusterStart != start {
      unicodeScalars.formIndex(before: &graphemeClusterStart)
      let gcb1 = graphemeClusterBreakProperty.getPropertyRawValue(
        unicodeScalars[graphemeClusterStart].value)
      if segmenter.isBoundary(gcb1, gcb0) {
        break
      }
      gcb0 = gcb1
      graphemeClusterStartUTF16 = graphemeClusterStart.encodedOffset
    }
    
    return endIndexUTF16 - graphemeClusterStartUTF16
  }
  
  /// Accesses the character at the given position.
  ///
  /// The following example searches a string's character view for a capital
  /// letter and then prints the character at the found index:
  ///
  ///     let greeting = "Hello, friend!"
  ///     if let i = greeting.characters.index(where: { "A"..."Z" ~= $0 }) {
  ///         print("First capital letter: \(greeting.characters[i])")
  ///     }
  ///     // Prints "First capital letter: H"
  ///
  /// - Parameter position: A valid index of the character view. `position`
  ///   must be less than the view's end index.
  public subscript(i_: Index) -> Character {
    var i = i_
    while true {
      if case .character(let stride) = i._cache {
        if _fastPath(stride == 1) {
          // For single-code-unit graphemes, we can construct a Character directly
          // from a single unicode scalar (if sub-surrogate).
          let relativeOffset = i.encodedOffset - _coreOffset
          if _core.isASCII {
            let asciiBuffer = _core.asciiBuffer._unsafelyUnwrappedUnchecked
            // Bounds checks in an UnsafeBufferPointer (asciiBuffer) are only
            // performed in Debug mode, so they need to be duplicated here.
            // Falling back to the non-optimal behavior in the case they don't
            // pass.
            if relativeOffset >= asciiBuffer.startIndex &&
            relativeOffset < asciiBuffer.endIndex {
              return Character(Unicode.Scalar(asciiBuffer[relativeOffset]))
            }
          } else if _core._baseAddress != nil {
            let cu = _core._nthContiguous(relativeOffset)
            // Only constructible if sub-surrogate
            if (cu < 0xd800) {
              return Character(Unicode.Scalar(cu)._unsafelyUnwrappedUnchecked)
            }
          }
        }
        
        let s = self[i..<Index(encodedOffset: i.encodedOffset + Int(stride))]
        return Character(s._ephemeralContent)
      }
      i = _index(atEncodedOffset: i.encodedOffset)
    }
  }
}

extension String.CharacterView : RangeReplaceableCollection {
  /// Creates an empty character view.
  public init() {
    self.init("")
  }

  /// Replaces the characters within the specified bounds with the given
  /// characters.
  ///
  /// Invalidates all indices with respect to the string.
  ///
  /// - Parameters:
  ///   - bounds: The range of characters to replace. The bounds of the range
  ///     must be valid indices of the character view.
  ///   - newElements: The new characters to add to the view.
  ///
  /// - Complexity: O(*m*), where *m* is the combined length of the character
  ///   view and `newElements`. If the call to `replaceSubrange(_:with:)`
  ///   simply removes characters at the end of the view, the complexity is
  ///   O(*n*), where *n* is equal to `bounds.count`.
  public mutating func replaceSubrange<C>(
    _ bounds: Range<Index>,
    with newElements: C
  ) where C : Collection, C.Element == Character {
    let rawSubRange: Range<Int> =
      bounds.lowerBound.encodedOffset - _coreOffset
      ..< bounds.upperBound.encodedOffset - _coreOffset
    let lazyUTF16 = newElements.lazy.flatMap { $0.utf16 }
    _core.replaceSubrange(rawSubRange, with: lazyUTF16)
  }

  /// Reserves enough space in the character view's underlying storage to store
  /// the specified number of ASCII characters.
  ///
  /// Because each element of a character view can require more than a single
  /// ASCII character's worth of storage, additional allocation may be
  /// necessary when adding characters to the character view after a call to
  /// `reserveCapacity(_:)`.
  ///
  /// - Parameter n: The minimum number of ASCII character's worth of storage
  ///   to allocate.
  ///
  /// - Complexity: O(*n*), where *n* is the capacity being reserved.
  public mutating func reserveCapacity(_ n: Int) {
    _core.reserveCapacity(n)
  }

  /// Appends the given character to the character view.
  ///
  /// - Parameter c: The character to append to the character view.
  public mutating func append(_ c: Character) {
    if let c0 = c._smallUTF16 {
      _core.append(contentsOf: c0)
      return
    }
    _core.append(c._largeUTF16!)
  }

  /// Appends the characters in the given sequence to the character view.
  /// 
  /// - Parameter newElements: A sequence of characters.
  public mutating func append<S : Sequence>(contentsOf newElements: S)
  where S.Element == Character {
    if _fastPath(newElements is _SwiftStringView) {
      let v = newElements as! _SwiftStringView
      if _fastPath(_core.count == 0) {
        _core = v._persistentContent._core
        return
      }
      _core.append(v._ephemeralContent._core)
      return
    }
    reserveCapacity(_core.count + newElements.underestimatedCount)
    for c in newElements { self.append(c) }
  }
}

// Algorithms
extension String.CharacterView {
  /// Accesses the characters in the given range.
  ///
  /// The example below uses this subscript to access the characters up to, but
  /// not including, the first comma (`","`) in the string.
  ///
  ///     let str = "All this happened, more or less."
  ///     let i = str.characters.index(of: ",")!
  ///     let substring = str.characters[str.characters.startIndex ..< i]
  ///     print(String(substring))
  ///     // Prints "All this happened"
  ///
  /// - Complexity: O(*n*) if the underlying string is bridged from
  ///   Objective-C, where *n* is the length of the string; otherwise, O(1).
  public subscript(bounds: Range<Index>) -> String.CharacterView {
    return String.CharacterView(
      unicodeScalars[bounds]._core,
      coreOffset: bounds.lowerBound.encodedOffset)
  }
}

extension String.CharacterView {
  @available(*, unavailable, renamed: "replaceSubrange")
  public mutating func replaceRange<C>(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C : Collection, C.Element == Character {
    Builtin.unreachable()
  }
    
  @available(*, unavailable, renamed: "append(contentsOf:)")
  public mutating func appendContentsOf<S : Sequence>(_ newElements: S)
    where S.Element == Character {
    Builtin.unreachable()
  }
}

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
  ///         let firstName = String(name.characters.prefix(upTo: firstSpace))
  ///         print(firstName)
  ///     }
  ///     // Prints "Marie"
  public struct CharacterView {
    internal var _core: _StringCore

    /// The offset of this view's `_core` from an original core. This works
    /// around the fact that `_StringCore` is always zero-indexed.
    /// `_coreOffset` should be subtracted from `UnicodeScalarIndex._position`
    /// before that value is used as a `_core` index.
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
  ///             let result = chars.suffix(from: chars.index(after: i))
  ///             chars.removeSubrange(i..<chars.endIndex)
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
  ///   The `CharacterView` argument is valid only for the duration of the
  ///   closure's execution.
  /// - Returns: The return value of the `body` closure, if any, is the return
  ///   value of this method.
  public mutating func withMutableCharacters<R>(
    _ body: (inout CharacterView) -> R
  ) -> R {
    // Naively mutating self.characters forces multiple references to
    // exist at the point of mutation. Instead, temporarily move the
    // core of this string into a CharacterView.
    var tmp = CharacterView("")
    swap(&_core, &tmp._core)
    let r = body(&tmp)
    swap(&_core, &tmp._core)
    return r
  }

  /// Creates a string from the given character view.
  ///
  /// Use this initializer to recover a string after performing a collection
  /// slicing operation on a string's character view.
  ///
  ///     let poem = "'Twas brillig, and the slithy toves / " +
  ///                "Did gyre and gimbal in the wabe: / " +
  ///                "All mimsy were the borogoves / " +
  ///                "And the mome raths outgrabe."
  ///     let excerpt = String(poem.characters.prefix(22)) + "..."
  ///     print(excerpt)
  ///     // Prints "'Twas brillig, and the..."
  ///
  /// - Parameter characters: A character view to convert to a string.
  public init(_ characters: CharacterView) {
    self.init(characters._core)
  }
}

/// `String.CharacterView` is a collection of `Character`.
extension String.CharacterView : BidirectionalCollection {
  internal typealias UnicodeScalarView = String.UnicodeScalarView
  internal var unicodeScalars: UnicodeScalarView {
    return UnicodeScalarView(_core, coreOffset: _coreOffset)
  }

  /// A position in a string's `CharacterView` instance.
  ///
  /// You can convert between indices of the different string views by using
  /// conversion initializers and the `samePosition(in:)` method overloads.
  /// The following example finds the index of the first space in the string's
  /// character view and then converts that to the same position in the UTF-8
  /// view:
  ///
  ///     let hearts = "Hearts <3 ♥︎ 💘"
  ///     if let i = hearts.characters.index(of: " ") {
  ///         let j = i.samePosition(in: hearts.utf8)
  ///         print(Array(hearts.utf8.prefix(upTo: j)))
  ///     }
  ///     // Prints "[72, 101, 97, 114, 116, 115]"
  public struct Index : Comparable, CustomPlaygroundQuickLookable {
    public // SPI(Foundation)
    init(_base: String.UnicodeScalarView.Index, in c: String.CharacterView) {
      self._base = _base
      self._countUTF16 = c._measureExtendedGraphemeClusterForward(from: _base)
    }

    internal init(_base: UnicodeScalarView.Index, _countUTF16: Int) {
      self._base = _base
      self._countUTF16 = _countUTF16
    }

    internal let _base: UnicodeScalarView.Index

    /// The count of this extended grapheme cluster in UTF-16 code units.
    internal let _countUTF16: Int

    /// The integer offset of this index in UTF-16 code units.
    public // SPI(Foundation)
    var _utf16Index: Int {
      return _base._position
    }

    /// The one past end index for this extended grapheme cluster in Unicode
    /// scalars.
    internal var _endBase: UnicodeScalarView.Index {
      return UnicodeScalarView.Index(_position: _utf16Index + _countUTF16)
    }

    public var customPlaygroundQuickLook: PlaygroundQuickLook {
      return .int(Int64(_utf16Index))
    }
  }

  public typealias IndexDistance = Int

  /// The position of the first character in a nonempty character view.
  ///
  /// In an empty character view, `startIndex` is equal to `endIndex`.
  public var startIndex: Index {
    return Index(_base: unicodeScalars.startIndex, in: self)
  }

  /// A character view's "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// In an empty character view, `endIndex` is equal to `startIndex`.
  public var endIndex: Index {
    return Index(_base: unicodeScalars.endIndex, in: self)
  }

  /// Returns the next consecutive position after `i`.
  ///
  /// - Precondition: The next position is valid.
  public func index(after i: Index) -> Index {
    _precondition(i._base < unicodeScalars.endIndex,
      "cannot increment beyond endIndex")
    _precondition(i._base >= unicodeScalars.startIndex,
      "cannot increment invalid index")
    return Index(_base: i._endBase, in: self)
  }

  /// Returns the previous consecutive position before `i`.
  ///
  /// - Precondition: The previous position is valid.
  public func index(before i: Index) -> Index {
    _precondition(i._base > unicodeScalars.startIndex,
      "cannot decrement before startIndex")
    _precondition(i._base <= unicodeScalars.endIndex,
      "cannot decrement invalid index")
    let predecessorLengthUTF16 =
      _measureExtendedGraphemeClusterBackward(from: i._base)
    return Index(
      _base: UnicodeScalarView.Index(
        _position: i._utf16Index - predecessorLengthUTF16
      ),
      in: self
    )
  }

  // NOTE: don't make this function inlineable.  Grapheme cluster
  // segmentation uses a completely different algorithm in Unicode 9.0.
  //
  /// Returns the length of the first extended grapheme cluster in UTF-16
  /// code units.
  @inline(never) // Don't remove, see above.
  internal func _measureExtendedGraphemeClusterForward(
    from start: UnicodeScalarView.Index
  ) -> Int {
    var start = start
    let end = unicodeScalars.endIndex
    if start == end {
      return 0
    }

    let startIndexUTF16 = start._position
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

    return start._position - startIndexUTF16
  }

  // NOTE: don't make this function inlineable.  Grapheme cluster
  // segmentation uses a completely different algorithm in Unicode 9.0.
  //
  /// Returns the length of the previous extended grapheme cluster in UTF-16
  /// code units.
  @inline(never) // Don't remove, see above.
  internal func _measureExtendedGraphemeClusterBackward(
    from end: UnicodeScalarView.Index
  ) -> Int {
    let start = unicodeScalars.startIndex
    if start == end {
      return 0
    }

    let endIndexUTF16 = end._position
    let graphemeClusterBreakProperty =
      _UnicodeGraphemeClusterBreakPropertyTrie()
    let segmenter = _UnicodeExtendedGraphemeClusterSegmenter()

    var graphemeClusterStart = end

    unicodeScalars.formIndex(before: &graphemeClusterStart)
    var gcb0 = graphemeClusterBreakProperty.getPropertyRawValue(
      unicodeScalars[graphemeClusterStart].value)

    var graphemeClusterStartUTF16 = graphemeClusterStart._position

    while graphemeClusterStart != start {
      unicodeScalars.formIndex(before: &graphemeClusterStart)
      let gcb1 = graphemeClusterBreakProperty.getPropertyRawValue(
        unicodeScalars[graphemeClusterStart].value)
      if segmenter.isBoundary(gcb1, gcb0) {
        break
      }
      gcb0 = gcb1
      graphemeClusterStartUTF16 = graphemeClusterStart._position
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
  public subscript(i: Index) -> Character {
    return Character(String(unicodeScalars[i._base..<i._endBase]))
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
  ) where C : Collection, C.Iterator.Element == Character {
    let rawSubRange: Range<Int> =
      bounds.lowerBound._base._position - _coreOffset
      ..< bounds.upperBound._base._position - _coreOffset
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
    switch c._representation {
    case .small(let _63bits):
      let bytes = Character._smallValue(_63bits)
      _core.append(contentsOf: Character._SmallUTF16(bytes))
    case .large(_):
      _core.append(String(c)._core)
    }
  }

  /// Appends the characters in the given sequence to the character view.
  ///
  /// - Parameter newElements: A sequence of characters.
  public mutating func append<S : Sequence>(contentsOf newElements: S)
    where S.Iterator.Element == Character {
    reserveCapacity(_core.count + newElements.underestimatedCount)
    for c in newElements {
      self.append(c)
    }
  }

  /// Creates a new character view containing the characters in the given
  /// sequence.
  ///
  /// - Parameter characters: A sequence of characters.
  public init<S : Sequence>(_ characters: S)
    where S.Iterator.Element == Character {
    self = String.CharacterView()
    self.append(contentsOf: characters)
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
    let unicodeScalarRange = bounds.lowerBound._base..<bounds.upperBound._base
    return String.CharacterView(unicodeScalars[unicodeScalarRange]._core,
      coreOffset: unicodeScalarRange.lowerBound._position)
  }
}

extension String.CharacterView {
  @available(*, unavailable, renamed: "replaceSubrange")
  public mutating func replaceRange<C>(
    _ subRange: Range<Index>,
    with newElements: C
  ) where C : Collection, C.Iterator.Element == Character {
    Builtin.unreachable()
  }

  @available(*, unavailable, renamed: "append(contentsOf:)")
  public mutating func appendContentsOf<S : Sequence>(_ newElements: S)
    where S.Iterator.Element == Character {
    Builtin.unreachable()
  }
}

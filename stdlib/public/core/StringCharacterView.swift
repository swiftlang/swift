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

// FIXME(ABI)#70 : The character string view should have a custom iterator type
// to allow performance optimizations of linear traversals.

import SwiftShims

extension String {
  @available(swift, deprecated: 3.2, message:
    "Please use String or Substring directly")
  public typealias CharacterView = _CharacterView
  
  /// A view of a string's contents as a collection of characters.
  ///
  /// In Swift, every string provides a view of its contents as characters. In
  /// this view, many individual characters---for example, "é", "김", and
  /// "🇮🇳"---can be made up of multiple Unicode scalar values. These scalar
  /// values are combined by Unicode's boundary algorithms into *extended
  /// grapheme clusters*, represented by the `Character` type. Each element of
  /// a `CharacterView` collection is a `Character` instance.
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
  ///     if let firstSpace = name.characters.firstIndex(of: " ") {
  ///         let firstName = String(name.characters[..<firstSpace])
  ///         print(firstName)
  ///     }
  ///     // Prints "Marie"
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct _CharacterView {
    @usableFromInline
    internal var _base: String

    /// The offset of this view's `_guts` from an original guts. This works
    /// around the fact that `_StringGuts` is always zero-indexed.
    /// `_baseOffset` should be subtracted from `Index.encodedOffset` before
    /// that value is used as a `_guts` index.
    @usableFromInline
    internal var _baseOffset: Int

    /// Creates a view of the given string.
    @inlinable // FIXME(sil-serialize-all)
    public init(_ text: String) {
      self._base = text
      self._baseOffset = 0
    }

    @inlinable // FIXME(sil-serialize-all)
    public // @testable
    init(_ _base: String, baseOffset: Int = 0) {
      self._base = _base
      self._baseOffset = baseOffset
    }
  }
  
  /// A view of the string's contents as a collection of characters.
  @_transparent // FIXME(sil-serialize-all)
  public var _characters: _CharacterView {
    get {
      return _CharacterView(self)
    }
    set {
      self = newValue._base
    }
  }

  /// A view of the string's contents as a collection of characters.
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, deprecated: 3.2, message:
    "Please use String or Substring directly")
  public var characters: CharacterView {
    get {
      return _characters
    }
    set {
      _characters = newValue
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
  ///     let afterSpace = str.withMutableCharacters {
  ///         chars -> String.CharacterView in
  ///         if let i = chars.firstIndex(of: " ") {
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
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, deprecated: 3.2, message:
    "Please mutate String or Substring directly")
  public mutating func withMutableCharacters<R>(
    _ body: (inout CharacterView) -> R
  ) -> R {
    // Naively mutating self.characters forces multiple references to
    // exist at the point of mutation. Instead, temporarily move the
    // guts of this string into a CharacterView.
    var tmp = _CharacterView("")
    (_guts, tmp._base._guts) = (tmp._base._guts, _guts)
    let r = body(&tmp)
    (_guts, tmp._base._guts) = (tmp._base._guts, _guts)
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
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, deprecated: 3.2, message:
    "Please use String or Substring directly")
  public init(_ characters: CharacterView) {
    self = characters._base
  }
}

extension String._CharacterView : _SwiftStringView {
  @inlinable // FIXME(sil-serialize-all)
  internal var _persistentContent : String {
    return _base
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var _wholeString : String {
    return _base
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var _encodedOffsetRange : Range<Int> {
    return _base._encodedOffsetRange
  }
}

extension String._CharacterView {
  @inlinable // FIXME(sil-serialize-all)
  internal var _guts: _StringGuts {
    return _base._guts
  }
}

extension String._CharacterView {
  internal typealias UnicodeScalarView = String.UnicodeScalarView

  @inlinable // FIXME(sil-serialize-all)
  internal var unicodeScalars: UnicodeScalarView {
    return UnicodeScalarView(_base._guts, coreOffset: _baseOffset)
  }
}

/// `String.CharacterView` is a collection of `Character`.
extension String._CharacterView : BidirectionalCollection {
  public typealias Index = String.Index
  public typealias IndexDistance = String.IndexDistance

  /// Translates a view index into an index in the underlying base string using
  /// this view's `_baseOffset`.
  @inlinable // FIXME(sil-serialize-all)
  internal func _toBaseIndex(_ index: Index) -> Index {
    return Index(
      encodedOffset: index.encodedOffset - _baseOffset,
      index._cache)
  }

  /// Translates an index in the underlying base string into a view index using
  /// this view's `_baseOffset`.
  @inlinable // FIXME(sil-serialize-all)
  internal func _toViewIndex(_ index: Index) -> Index {
    return Index(
      encodedOffset: index.encodedOffset + _baseOffset,
      index._cache)
  }

  /// The position of the first character in a nonempty character view.
  /// 
  /// In an empty character view, `startIndex` is equal to `endIndex`.
  @inlinable // FIXME(sil-serialize-all)
  public var startIndex: Index {
    return _toViewIndex(_base.startIndex)
  }

  /// A character view's "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// In an empty character view, `endIndex` is equal to `startIndex`.
  @inlinable // FIXME(sil-serialize-all)
  public var endIndex: Index {
    return _toViewIndex(_base.endIndex)
  }

  /// Returns the next consecutive position after `i`.
  ///
  /// - Precondition: The next position is valid.
  @inlinable // FIXME(sil-serialize-all)
  public func index(after i: Index) -> Index {
    return _toViewIndex(_base.index(after: _toBaseIndex(i)))
  }

  /// Returns the previous consecutive position before `i`.
  ///
  /// - Precondition: The previous position is valid.
  @inlinable // FIXME(sil-serialize-all)
  public func index(before i: Index) -> Index {
    return _toViewIndex(_base.index(before: _toBaseIndex(i)))
  }

  /// Accesses the character at the given position.
  ///
  /// The following example searches a string's character view for a capital
  /// letter and then prints the character at the found index:
  ///
  ///     let greeting = "Hello, friend!"
  ///     if let i = greeting.characters.firstIndex(where: { "A"..."Z" ~= $0 }) {
  ///         print("First capital letter: \(greeting.characters[i])")
  ///     }
  ///     // Prints "First capital letter: H"
  ///
  /// - Parameter position: A valid index of the character view. `position`
  ///   must be less than the view's end index.
  @inlinable // FIXME(sil-serialize-all)
  public subscript(i: Index) -> Character {
    return _base[_toBaseIndex(i)]
  }
}

extension String._CharacterView : RangeReplaceableCollection {
  /// Creates an empty character view.
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
  public mutating func replaceSubrange<C>(
    _ bounds: Range<Index>,
    with newElements: C
  ) where C : Collection, C.Element == Character {
    _base.replaceSubrange(
      _toBaseIndex(bounds.lowerBound) ..< _toBaseIndex(bounds.upperBound),
      with: newElements)
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
  @inlinable // FIXME(sil-serialize-all)
  public mutating func reserveCapacity(_ n: Int) {
    _base.reserveCapacity(n)
  }

  /// Appends the given character to the character view.
  ///
  /// - Parameter c: The character to append to the character view.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func append(_ c: Character) {
    _base.append(c)
  }

  /// Appends the characters in the given sequence to the character view.
  /// 
  /// - Parameter newElements: A sequence of characters.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func append<S : Sequence>(contentsOf newElements: S)
  where S.Element == Character {
    _base.append(contentsOf: newElements)
  }
}

// Algorithms
extension String._CharacterView {
  /// Accesses the characters in the given range.
  ///
  /// The example below uses this subscript to access the characters up to, but
  /// not including, the first comma (`","`) in the string.
  ///
  ///     let str = "All this happened, more or less."
  ///     let i = str.characters.firstIndex(of: ",")!
  ///     let substring = str.characters[str.characters.startIndex ..< i]
  ///     print(String(substring))
  ///     // Prints "All this happened"
  ///
  /// - Complexity: O(*n*) if the underlying string is bridged from
  ///   Objective-C, where *n* is the length of the string; otherwise, O(1).
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, deprecated: 3.2, message:
    "Please use String or Substring directly")
  public subscript(bounds: Range<Index>) -> String.CharacterView {
    let offsetRange: Range<Int> =
      _toBaseIndex(bounds.lowerBound).encodedOffset ..<
      _toBaseIndex(bounds.upperBound).encodedOffset
    return String.CharacterView(
      String(_base._guts._extractSlice(offsetRange)),
      baseOffset: bounds.lowerBound.encodedOffset)
  }
}

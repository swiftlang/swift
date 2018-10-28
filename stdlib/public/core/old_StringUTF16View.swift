//===--- StringUTF16.swift ------------------------------------------------===//
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

// FIXME(ABI)#71 : The UTF-16 string view should have a custom iterator type to
// allow performance optimizations of linear traversals.

extension String {
  /// A view of a string's contents as a collection of UTF-16 code units.
  ///
  /// You can access a string's view of UTF-16 code units by using its `utf16`
  /// property. A string's UTF-16 view encodes the string's Unicode scalar
  /// values as 16-bit integers.
  ///
  ///     let flowers = "Flowers 💐"
  ///     for v in flowers.utf16 {
  ///         print(v)
  ///     }
  ///     // 70
  ///     // 108
  ///     // 111
  ///     // 119
  ///     // 101
  ///     // 114
  ///     // 115
  ///     // 32
  ///     // 55357
  ///     // 56464
  ///
  /// Unicode scalar values that make up a string's contents can be up to 21
  /// bits long. The longer scalar values may need two `UInt16` values for
  /// storage. Those "pairs" of code units are called *surrogate pairs*.
  ///
  ///     let flowermoji = "💐"
  ///     for v in flowermoji.unicodeScalars {
  ///         print(v, v.value)
  ///     }
  ///     // 💐 128144
  ///
  ///     for v in flowermoji.utf16 {
  ///         print(v)
  ///     }
  ///     // 55357
  ///     // 56464
  ///
  /// To convert a `String.UTF16View` instance back into a string, use the
  /// `String` type's `init(_:)` initializer.
  ///
  ///     let favemoji = "My favorite emoji is 🎉"
  ///     if let i = favemoji.utf16.firstIndex(where: { $0 >= 128 }) {
  ///         let asciiPrefix = String(favemoji.utf16[..<i])
  ///         print(asciiPrefix)
  ///     }
  ///     // Prints "My favorite emoji is "
  ///
  /// UTF16View Elements Match NSString Characters
  /// ============================================
  ///
  /// The UTF-16 code units of a string's `utf16` view match the elements
  /// accessed through indexed `NSString` APIs.
  ///
  ///     print(flowers.utf16.count)
  ///     // Prints "10"
  ///
  ///     let nsflowers = flowers as NSString
  ///     print(nsflowers.length)
  ///     // Prints "10"
  ///
  /// Unlike `NSString`, however, `String.UTF16View` does not use integer
  /// indices. If you need to access a specific position in a UTF-16 view, use
  /// Swift's index manipulation methods. The following example accesses the
  /// fourth code unit in both the `flowers` and `nsflowers` strings:
  ///
  ///     print(nsflowers.character(at: 3))
  ///     // Prints "119"
  ///
  ///     let i = flowers.utf16.index(flowers.utf16.startIndex, offsetBy: 3)
  ///     print(flowers.utf16[i])
  ///     // Prints "119"
  ///
  /// Although the Swift overlay updates many Objective-C methods to return
  /// native Swift indices and index ranges, some still return instances of
  /// `NSRange`. To convert an `NSRange` instance to a range of
  /// `String.Index`, use the `Range(_:in:)` initializer, which takes an
  /// `NSRange` and a string as arguments.
  ///
  ///     let snowy = "❄️ Let it snow! ☃️"
  ///     let nsrange = NSRange(location: 3, length: 12)
  ///     if let range = Range(nsrange, in: snowy) {
  ///         print(snowy[range])
  ///     }
  ///     // Prints "Let it snow!"
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct UTF16View
    : BidirectionalCollection,
    CustomStringConvertible,
    CustomDebugStringConvertible {

    public typealias Index = String.Index

    /// The position of the first code unit if the `String` is
    /// nonempty; identical to `endIndex` otherwise.
    @inlinable // FIXME(sil-serialize-all)
    public var startIndex: Index {
      return Index(encodedOffset: _offset)
    }

    /// The "past the end" position---that is, the position one greater than
    /// the last valid subscript argument.
    ///
    /// In an empty UTF-16 view, `endIndex` is equal to `startIndex`.
    @inlinable // FIXME(sil-serialize-all)
    public var endIndex: Index {
      return Index(encodedOffset: _offset + _length)
    }

    @_fixed_layout // FIXME(sil-serialize-all)
    public struct Indices {
      @inlinable // FIXME(sil-serialize-all)
      internal init(
        _elements: String.UTF16View, _startIndex: Index, _endIndex: Index
      ) {
        self._elements = _elements
        self._startIndex = _startIndex
        self._endIndex = _endIndex
      }
      @usableFromInline // FIXME(sil-serialize-all)
      internal var _elements: String.UTF16View
      @usableFromInline // FIXME(sil-serialize-all)
      internal var _startIndex: Index
      @usableFromInline // FIXME(sil-serialize-all)
      internal var _endIndex: Index
    }

    @inlinable // FIXME(sil-serialize-all)
    public var indices: Indices {
      return Indices(
        _elements: self, startIndex: startIndex, endIndex: endIndex)
    }

    // TODO: swift-3-indexing-model - add docs
    @inlinable // FIXME(sil-serialize-all)
    public func index(after i: Index) -> Index {
      // FIXME: swift-3-indexing-model: range check i?
      return Index(encodedOffset: _unsafePlus(i.encodedOffset, 1))
    }

    // TODO: swift-3-indexing-model - add docs
    @inlinable // FIXME(sil-serialize-all)
    public func index(before i: Index) -> Index {
      // FIXME: swift-3-indexing-model: range check i?
      return Index(encodedOffset: _unsafeMinus(i.encodedOffset, 1))
    }

    // TODO: swift-3-indexing-model - add docs
    @inlinable // FIXME(sil-serialize-all)
    public func index(_ i: Index, offsetBy n: Int) -> Index {
      // FIXME: swift-3-indexing-model: range check i?
      return Index(encodedOffset: i.encodedOffset.advanced(by: n))
    }

    // TODO: swift-3-indexing-model - add docs
    @inlinable // FIXME(sil-serialize-all)
    public func index(
      _ i: Index, offsetBy n: Int, limitedBy limit: Index
    ) -> Index? {
      // FIXME: swift-3-indexing-model: range check i?
      let d = i.encodedOffset.distance(to: limit.encodedOffset)
      if (d >= 0) ? (d < n) : (d > n) {
        return nil
      }
      return Index(encodedOffset: i.encodedOffset.advanced(by: n))
    }

    // TODO: swift-3-indexing-model - add docs
    @inlinable // FIXME(sil-serialize-all)
    public func distance(from start: Index, to end: Index) -> Int {
      // FIXME: swift-3-indexing-model: range check start and end?
      return start.encodedOffset.distance(to: end.encodedOffset)
    }

    @inlinable // FIXME(sil-serialize-all)
    internal func _internalIndex(at i: Int) -> Int {
      return _guts.startIndex + i
    }

    /// Accesses the code unit at the given position.
    ///
    /// The following example uses the subscript to print the value of a
    /// string's first UTF-16 code unit.
    ///
    ///     let greeting = "Hello, friend!"
    ///     let i = greeting.utf16.startIndex
    ///     print("First character's UTF-16 code unit: \(greeting.utf16[i])")
    ///     // Prints "First character's UTF-16 code unit: 72"
    ///
    /// - Parameter position: A valid index of the view. `position` must be
    ///   less than the view's end index.
    @inlinable // FIXME(sil-serialize-all)
    public subscript(i: Index) -> UTF16.CodeUnit {
      _precondition(i >= startIndex && i < endIndex,
          "out-of-range access on a UTF16View")

      let index = _internalIndex(at: i.encodedOffset)
      let u = _guts.codeUnit(atCheckedOffset: index)
      if _fastPath(UTF16._isScalar(u)) {
        // Neither high-surrogate, nor low-surrogate -- well-formed sequence
        // of 1 code unit.
        return u
      }

      if UTF16.isLeadSurrogate(u) {
        // Sequence is well-formed if `u` is followed by a low-surrogate.
        if _fastPath(
          index + 1 < _guts.count &&
          UTF16.isTrailSurrogate(_guts.codeUnit(atCheckedOffset: index + 1)))
        {
          return u
        }
        return UTF16._replacementCodeUnit
      }

      // `u` is a low-surrogate.  Sequence is well-formed if
      // previous code unit is a high-surrogate.
      if _fastPath(
        index != 0 &&
        UTF16.isLeadSurrogate(_guts.codeUnit(atCheckedOffset: index - 1)))
      {
        return u
      }
      return UTF16._replacementCodeUnit
    }

    @inlinable // FIXME(sil-serialize-all)
    internal init(_ _guts: _StringGuts) {
      self.init(_guts, offset: 0, length: _guts.count)
    }

    @inlinable // FIXME(sil-serialize-all)
    internal init(_ _guts: _StringGuts, offset: Int, length: Int) {
      self._offset = offset
      self._length = length
      self._guts = _guts
    }

    public var description: String {
      return String(_guts._extractSlice(_encodedOffsetRange))
    }

    public var debugDescription: String {
      return "StringUTF16(\(self.description.debugDescription))"
    }

    @usableFromInline // FIXME(sil-serialize-all)
    internal var _offset: Int
    @usableFromInline // FIXME(sil-serialize-all)
    internal var _length: Int

    @usableFromInline
    internal var _guts: _StringGuts
  }

  /// A UTF-16 encoding of `self`.
  @inlinable // FIXME(sil-serialize-all)
  public var utf16: UTF16View {
    get {
      return UTF16View(_guts)
    }
    set {
      self = String(describing: newValue)
    }
  }

  /// Creates a string corresponding to the given sequence of UTF-16 code units.
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, introduced: 4.0)
  public init(_ utf16: UTF16View) {
    self = String(utf16._guts)
  }

  /// The index type for subscripting a string.
  public typealias UTF16Index = UTF16View.Index
}

extension String.UTF16View : _SwiftStringView {
  @inlinable // FIXME(sil-serialize-all)
  internal var _ephemeralContent : String { return _persistentContent }
  @inlinable // FIXME(sil-serialize-all)
  internal var _persistentContent : String { return String(self._guts) }

  @inlinable // FIXME(sil-serialize-all)
  var _wholeString : String {
    return String(_guts)
  }

  @inlinable // FIXME(sil-serialize-all)
  var _encodedOffsetRange : Range<Int> {
    return _offset..<_offset+_length
  }
}

// Index conversions
extension String.UTF16View.Index {
  /// Creates an index in the given UTF-16 view that corresponds exactly to the
  /// specified string position.
  ///
  /// If the index passed as `sourcePosition` represents either the start of a
  /// Unicode scalar value or the position of a UTF-16 trailing surrogate,
  /// then the initializer succeeds. If `sourcePosition` does not have an
  /// exact corresponding position in `target`, then the result is `nil`. For
  /// example, an attempt to convert the position of a UTF-8 continuation byte
  /// results in `nil`.
  ///
  /// The following example finds the position of a space in a string and then
  /// converts that position to an index in the string's `utf16` view.
  ///
  ///     let cafe = "Café 🍵"
  ///
  ///     let stringIndex = cafe.firstIndex(of: "é")!
  ///     let utf16Index = String.Index(stringIndex, within: cafe.utf16)!
  ///
  ///     print(cafe.utf16[...utf16Index])
  ///     // Prints "Café"
  ///
  /// - Parameters:
  ///   - sourcePosition: A position in at least one of the views of the string
  ///     shared by `target`.
  ///   - target: The `UTF16View` in which to find the new position.
  @inlinable // FIXME(sil-serialize-all)
  public init?(
    _ sourcePosition: String.Index, within target: String.UTF16View
  ) {
    guard sourcePosition.transcodedOffset == 0 else { return nil }
    self.init(encodedOffset: sourcePosition.encodedOffset)
  }

  /// Returns the position in the given view of Unicode scalars that
  /// corresponds exactly to this index.
  ///
  /// This index must be a valid index of `String(unicodeScalars).utf16`.
  ///
  /// This example first finds the position of a space (UTF-16 code point `32`)
  /// in a string's `utf16` view and then uses this method to find the same
  /// position in the string's `unicodeScalars` view.
  ///
  ///     let cafe = "Café 🍵"
  ///     let i = cafe.utf16.firstIndex(of: 32)!
  ///     let j = i.samePosition(in: cafe.unicodeScalars)!
  ///     print(cafe.unicodeScalars[..<j])
  ///     // Prints "Café"
  ///
  /// - Parameter unicodeScalars: The view to use for the index conversion.
  ///   This index must be a valid index of at least one view of the string
  ///   shared by `unicodeScalars`.
  /// - Returns: The position in `unicodeScalars` that corresponds exactly to
  ///   this index. If this index does not have an exact corresponding
  ///   position in `unicodeScalars`, this method returns `nil`. For example,
  ///   an attempt to convert the position of a UTF-16 trailing surrogate
  ///   returns `nil`.
  @inlinable // FIXME(sil-serialize-all)
  public func samePosition(
    in unicodeScalars: String.UnicodeScalarView
  ) -> String.UnicodeScalarIndex? {
    return String.UnicodeScalarIndex(self, within: unicodeScalars)
  }
}

// Reflection
extension String.UTF16View : CustomReflectable {
  /// Returns a mirror that reflects the UTF-16 view of a string.
  public var customMirror: Mirror {
    return Mirror(self, unlabeledChildren: self)
  }
}

extension String.UTF16View.Indices : BidirectionalCollection {
  public typealias Index = String.UTF16View.Index
  public typealias Indices = String.UTF16View.Indices
  public typealias SubSequence = String.UTF16View.Indices

  @inlinable // FIXME(sil-serialize-all)
  internal init(
    _elements: String.UTF16View,
    startIndex: Index,
    endIndex: Index
  ) {
    self._elements = _elements
    self._startIndex = startIndex
    self._endIndex = endIndex
  }

  @inlinable // FIXME(sil-serialize-all)
  public var startIndex: Index {
    return _startIndex
  }

  @inlinable // FIXME(sil-serialize-all)
  public var endIndex: Index {
    return _endIndex
  }

  @inlinable // FIXME(sil-serialize-all)
  public var indices: Indices {
    return self
  }

  @inlinable // FIXME(sil-serialize-all)
  public subscript(i: Index) -> Index {
    // FIXME: swift-3-indexing-model: range check.
    return i
  }

  @inlinable // FIXME(sil-serialize-all)
  public subscript(bounds: Range<Index>) -> String.UTF16View.Indices {
    // FIXME: swift-3-indexing-model: range check.
    return String.UTF16View.Indices(
      _elements: _elements,
      startIndex: bounds.lowerBound,
      endIndex: bounds.upperBound)
  }

  @inlinable // FIXME(sil-serialize-all)
  public func index(after i: Index) -> Index {
    // FIXME: swift-3-indexing-model: range check.
    return _elements.index(after: i)
  }

  @inlinable // FIXME(sil-serialize-all)
  public func formIndex(after i: inout Index) {
    // FIXME: swift-3-indexing-model: range check.
    _elements.formIndex(after: &i)
  }

  @inlinable // FIXME(sil-serialize-all)
  public func index(before i: Index) -> Index {
    // FIXME: swift-3-indexing-model: range check.
    return _elements.index(before: i)
  }

  @inlinable // FIXME(sil-serialize-all)
  public func formIndex(before i: inout Index) {
    // FIXME: swift-3-indexing-model: range check.
    _elements.formIndex(before: &i)
  }

  @inlinable // FIXME(sil-serialize-all)
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    // FIXME: swift-3-indexing-model: range check i?
    return _elements.index(i, offsetBy: n)
  }

  @inlinable // FIXME(sil-serialize-all)
  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    // FIXME: swift-3-indexing-model: range check i?
    return _elements.index(i, offsetBy: n, limitedBy: limit)
  }

  // TODO: swift-3-indexing-model - add docs
  @inlinable // FIXME(sil-serialize-all)
  public func distance(from start: Index, to end: Index) -> Int {
    // FIXME: swift-3-indexing-model: range check start and end?
    return _elements.distance(from: start, to: end)
  }
}

//===--- Slicing Support --------------------------------------------------===//
/// In Swift 3.2, in the absence of type context,
///
///   someString.utf16[someString.utf16.startIndex..<someString.utf16.endIndex]
///
/// was deduced to be of type `String.UTF16View`.  Provide a more-specific
/// Swift-3-only `subscript` overload that continues to produce
/// `String.UTF16View`.
extension String.UTF16View {
  public typealias SubSequence = Substring.UTF16View

  @inlinable // FIXME(sil-serialize-all)
  @available(swift, introduced: 4)
  public subscript(bounds: Range<Index>) -> String.UTF16View.SubSequence {
    return String.UTF16View.SubSequence(self, _bounds: bounds)
  }
}

//===----------------------------------------------------------------------===//
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

extension String : StringProtocol, RangeReplaceableCollection {
  /// Creates a string representing the given character repeated the specified
  /// number of times.
  ///
  /// For example, use this initializer to create a string with ten `"0"`
  /// characters in a row.
  ///
  ///     let zeroes = String(repeating: "0" as Character, count: 10)
  ///     print(zeroes)
  ///     // Prints "0000000000"
  ///
  /// - Parameters:
  ///   - repeatedValue: The character to repeat.
  ///   - count: The number of times to repeat `repeatedValue` in the
  ///     resulting string.
  @inlinable // FIXME(sil-serialize-all)
  public init(repeating repeatedValue: Character, count: Int) {
    self.init(repeating: String(repeatedValue), count: count)
  }

  // This initializer disambiguates between the following intitializers, now
  // that String conforms to Collection:
  // - init<T>(_ value: T) where T : LosslessStringConvertible
  // - init<S>(_ characters: S) where S : Sequence, S.Element == Character

  /// Creates a new string containing the characters in the given sequence.
  ///
  /// You can use this initializer to create a new string from the result of
  /// one or more collection operations on a string's characters. For example:
  ///
  ///     let str = "The rain in Spain stays mainly in the plain."
  ///
  ///     let vowels: Set<Character> = ["a", "e", "i", "o", "u"]
  ///     let disemvoweled = String(str.lazy.filter { !vowels.contains($0) })
  ///
  ///     print(disemvoweled)
  ///     // Prints "Th rn n Spn stys mnly n th pln."
  ///
  /// - Parameter other: A string instance or another sequence of
  ///   characters.
  @inlinable // FIXME(sil-serialize-all)
  public init<S : Sequence & LosslessStringConvertible>(_ other: S)
  where S.Element == Character {
    self = other.description
  }

  @inlinable
  @inline(__always)
  internal func _boundsCheck(_ index: Index) {
    _precondition(index.encodedOffset >= 0 && index.encodedOffset < _guts.count,
      "String index is out of bounds")
  }

  @inlinable
  @inline(__always)
  internal func _boundsCheck(_ range: Range<Index>) {
    _precondition(
      range.lowerBound.encodedOffset >= 0 &&
      range.upperBound.encodedOffset <= _guts.count,
      "String index range is out of bounds")
  }

  @inlinable
  @inline(__always)
  internal func _boundsCheck(_ range: ClosedRange<Index>) {
    _precondition(
      range.lowerBound.encodedOffset >= 0 &&
      range.upperBound.encodedOffset < _guts.count,
      "String index range is out of bounds")
  }

  internal func _index(atEncodedOffset offset: Int) -> Index {
    return _visitGuts(_guts, args: offset,
      ascii: { ascii, offset in return ascii.characterIndex(atOffset: offset) },
      utf16: { utf16, offset in return utf16.characterIndex(atOffset: offset) },
      opaque: { opaque, offset in
        return opaque.characterIndex(atOffset: offset) })
  }
}

extension String {
  /// Creates a new string containing the characters in the given sequence.
  ///
  /// You can use this initializer to create a new string from the result of
  /// one or more collection operations on a string's characters. For example:
  ///
  ///     let str = "The rain in Spain stays mainly in the plain."
  ///
  ///     let vowels: Set<Character> = ["a", "e", "i", "o", "u"]
  ///     let disemvoweled = String(str.lazy.filter { !vowels.contains($0) })
  ///
  ///     print(disemvoweled)
  ///     // Prints "Th rn n Spn stys mnly n th pln."
  ///
  /// - Parameter characters: A string instance or another sequence of
  ///   characters.
  @inlinable // FIXME(sil-serialize-all)
  public init<S : Sequence>(_ characters: S)
    where S.Iterator.Element == Character {
    self = ""
    self.append(contentsOf: characters)
  }

  /// Reserves enough space in the string's underlying storage to store the
  /// specified number of ASCII characters.
  ///
  /// Because each character in a string can require more than a single ASCII
  /// character's worth of storage, additional allocation may be necessary
  /// when adding characters to a string after a call to
  /// `reserveCapacity(_:)`.
  ///
  /// - Parameter n: The minimum number of ASCII character's worth of storage
  ///   to allocate.
  ///
  /// - Complexity: O(*n*)
  public mutating func reserveCapacity(_ n: Int) {
    _guts.reserveCapacity(n)
  }

  /// Appends the given character to the string.
  ///
  /// The following example adds an emoji globe to the end of a string.
  ///
  ///     var globe = "Globe "
  ///     globe.append("🌍")
  ///     print(globe)
  ///     // Prints "Globe 🌍"
  ///
  /// - Parameter c: The character to append to the string.
  public mutating func append(_ c: Character) {
    if let small = c._smallUTF16 {
      _guts.append(contentsOf: small)
    } else {
      _guts.append(c._largeUTF16!.unmanagedView)
      _fixLifetime(c)
    }
  }

  public mutating func append(contentsOf newElements: String) {
    append(newElements)
  }

  public mutating func append(contentsOf newElements: Substring) {
    _guts.append(
      newElements._wholeString._guts,
      range: newElements._encodedOffsetRange)
  }

  /// Appends the characters in the given sequence to the string.
  ///
  /// - Parameter newElements: A sequence of characters.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func append<S : Sequence>(contentsOf newElements: S)
    where S.Iterator.Element == Character {
    if _fastPath(newElements is _SwiftStringView) {
      let v = newElements as! _SwiftStringView
      _guts.append(v._wholeString._guts, range: v._encodedOffsetRange)
      return
    }
    _guts.reserveUnusedCapacity(
      newElements.underestimatedCount,
      ascii: _guts.isASCII)
    for c in newElements { self.append(c) }
  }

  /// Replaces the text within the specified bounds with the given characters.
  ///
  /// Calling this method invalidates any existing indices for use with this
  /// string.
  ///
  /// - Parameters:
  ///   - bounds: The range of text to replace. The bounds of the range must be
  ///     valid indices of the string.
  ///   - newElements: The new characters to add to the string.
  ///
  /// - Complexity: O(*m*), where *m* is the combined length of the string and
  ///   `newElements`. If the call to `replaceSubrange(_:with:)` simply
  ///   removes text at the end of the string, the complexity is O(*n*), where
  ///   *n* is equal to `bounds.count`.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func replaceSubrange<C>(
    _ bounds: Range<Index>,
    with newElements: C
  ) where C : Collection, C.Iterator.Element == Character {
    let offsetRange: Range<Int> =
      bounds.lowerBound.encodedOffset ..< bounds.upperBound.encodedOffset
    let lazyUTF16 = newElements.lazy.flatMap { $0.utf16 }
    _guts.replaceSubrange(offsetRange, with: lazyUTF16)
  }

  /// Inserts a new character at the specified position.
  ///
  /// Calling this method invalidates any existing indices for use with this
  /// string.
  ///
  /// - Parameters:
  ///   - newElement: The new character to insert into the string.
  ///   - i: A valid index of the string. If `i` is equal to the string's end
  ///     index, this methods appends `newElement` to the string.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the string.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func insert(_ newElement: Character, at i: Index) {
    let offset = i.encodedOffset
    _guts.replaceSubrange(offset..<offset, with: newElement.utf16)
  }

  /// Inserts a collection of characters at the specified position.
  ///
  /// Calling this method invalidates any existing indices for use with this
  /// string.
  ///
  /// - Parameters:
  ///   - newElements: A collection of `Character` elements to insert into the
  ///     string.
  ///   - i: A valid index of the string. If `i` is equal to the string's end
  ///     index, this methods appends the contents of `newElements` to the
  ///     string.
  ///
  /// - Complexity: O(*n*), where *n* is the combined length of the string and
  ///   `newElements`.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func insert<S : Collection>(
    contentsOf newElements: S, at i: Index
  ) where S.Iterator.Element == Character {
    let offset = i.encodedOffset
    let utf16 = newElements.lazy.flatMap { $0.utf16 }
    _guts.replaceSubrange(offset..<offset, with: utf16)
  }

  /// Removes and returns the character at the specified position.
  ///
  /// All the elements following `i` are moved to close the gap. This example
  /// removes the hyphen from the middle of a string.
  ///
  ///     var nonempty = "non-empty"
  ///     if let i = nonempty.firstIndex(of: "-") {
  ///         nonempty.remove(at: i)
  ///     }
  ///     print(nonempty)
  ///     // Prints "nonempty"
  ///
  /// Calling this method invalidates any existing indices for use with this
  /// string.
  ///
  /// - Parameter i: The position of the character to remove. `i` must be a
  ///   valid index of the string that is not equal to the string's end index.
  /// - Returns: The character that was removed.
  @inlinable // FIXME(sil-serialize-all)
  @discardableResult
  public mutating func remove(at i: Index) -> Character {
    let offset = i.encodedOffset
    let stride = _stride(of: i)
    let range: Range<Int> = offset ..< offset + stride
    let old = Character(_unverified: _guts, range: range)
    _guts.replaceSubrange(range, with: EmptyCollection())
    return old
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _stride(of i: Index) -> Int {
    if let stride = i.characterStride { return stride }

    let offset = i.encodedOffset
    return _visitGuts(_guts, args: offset,
      ascii: { ascii, offset in
        return ascii.characterStride(atOffset: offset) },
      utf16: { utf16, offset in
        return utf16.characterStride(atOffset: offset) },
      opaque: { opaque, offset in
        return opaque.characterStride(atOffset: offset) })
  }

  /// Removes the characters in the given range.
  ///
  /// Calling this method invalidates any existing indices for use with this
  /// string.
  ///
  /// - Parameter bounds: The range of the elements to remove. The upper and
  ///   lower bounds of `bounds` must be valid indices of the string and not
  ///   equal to the string's end index.
  /// - Parameter bounds: The range of the elements to remove. The upper and
  ///   lower bounds of `bounds` must be valid indices of the string.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func removeSubrange(_ bounds: Range<Index>) {
    let start = bounds.lowerBound.encodedOffset
    let end = bounds.upperBound.encodedOffset
    _guts.replaceSubrange(start..<end, with: EmptyCollection())
  }

  /// Replaces this string with the empty string.
  ///
  /// Calling this method invalidates any existing indices for use with this
  /// string.
  ///
  /// - Parameter keepCapacity: Pass `true` to prevent the release of the
  ///   string's allocated storage. Retaining the storage can be a useful
  ///   optimization when you're planning to grow the string again. The
  ///   default value is `false`.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) {
    if keepCapacity {
      _guts.replaceSubrange(0..<_guts.count, with: EmptyCollection())
    } else {
      _guts = _StringGuts()
    }
  }
}

extension String {
  // This is needed because of the issue described in SR-4660 which causes
  // source compatibility issues when String becomes a collection
  @_transparent
  public func max<T : Comparable>(_ x: T, _ y: T) -> T {
    return Swift.max(x,y)
  }

  // This is needed because of the issue described in SR-4660 which causes
  // source compatibility issues when String becomes a collection
  @_transparent
  public func min<T : Comparable>(_ x: T, _ y: T) -> T {
    return Swift.min(x,y)
  }
}

//===----------------------------------------------------------------------===//

extension Sequence where Element == String {
  @available(*, unavailable, message: "Operator '+' cannot be used to append a String to a sequence of strings")
  public static func + (lhs: Self, rhs: String) -> Never {
    fatalError()
  }

  @available(*, unavailable, message: "Operator '+' cannot be used to append a String to a sequence of strings")
  public static func + (lhs: String, rhs: Self) -> Never {
    fatalError()
  }
}

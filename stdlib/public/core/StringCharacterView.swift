//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// String is a bidirectional collection of `Character`s, aka graphemes
extension String: BidirectionalCollection {
  /// A type that represents the number of steps between two `String.Index`
  /// values, where one value is reachable from the other.
  ///
  /// In Swift, *reachability* refers to the ability to produce one value from
  /// the other through zero or more applications of `index(after:)`.
  public typealias IndexDistance = Int

  public typealias SubSequence = Substring

  /// The position of the first character in a nonempty string.
  ///
  /// In an empty string, `startIndex` is equal to `endIndex`.
  @inlinable // FIXME(sil-serialize-all)
  public var startIndex: Index { return Index(encodedOffset: 0) }

  /// A string's "past the end" position---that is, the position one greater
  /// than the last valid subscript argument.
  ///
  /// In an empty string, `endIndex` is equal to `startIndex`.
  @inlinable // FIXME(sil-serialize-all)
  public var endIndex: Index { return Index(encodedOffset: _guts.count) }

  /// The number of characters in a string.
  public var count: Int {
    return distance(from: startIndex, to: endIndex)
  }

  /// Returns the position immediately after the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be less than
  ///   `endIndex`.
  /// - Returns: The index value immediately after `i`.
  public func index(after i: Index) -> Index {
    return _visitGuts(_guts, args: i,
      ascii: { ascii, i in ascii.characterIndex(after: i) },
      utf16: { utf16, i in utf16.characterIndex(after: i) },
      opaque: { opaque, i in opaque.characterIndex(after: i) })
  }

  /// Returns the position immediately before the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be greater than
  ///   `startIndex`.
  /// - Returns: The index value immediately before `i`.
  public func index(before i: Index) -> Index {
    return _visitGuts(_guts, args: i,
      ascii: { ascii, i in ascii.characterIndex(before: i) },
      utf16: { utf16, i in utf16.characterIndex(before: i) },
      opaque: { opaque, i in opaque.characterIndex(before: i) })
  }

  /// Returns an index that is the specified distance from the given index.
  ///
  /// The following example obtains an index advanced four positions from a
  /// string's starting index and then prints the character at that position.
  ///
  ///     let s = "Swift"
  ///     let i = s.index(s.startIndex, offsetBy: 4)
  ///     print(s[i])
  ///     // Prints "t"
  ///
  /// The value passed as `distance` must not offset `i` beyond the bounds of
  /// the collection.
  ///
  /// - Parameters:
  ///   - i: A valid index of the collection.
  ///   - distance: The distance to offset `i`.
  /// - Returns: An index offset by `distance` from the index `i`. If
  ///   `distance` is positive, this is the same value as the result of
  ///   `distance` calls to `index(after:)`. If `distance` is negative, this
  ///   is the same value as the result of `abs(distance)` calls to
  ///   `index(before:)`.
  ///
  /// - Complexity: O(*k*), where *k* is the absolute value of `distance`.
  public func index(_ i: Index, offsetBy distance: IndexDistance) -> Index {
    return _visitGuts(_guts, args: (i, distance),
      ascii: { ascii, args in let (i, n) = args
        return ascii.characterIndex(i, offsetBy: n) },
      utf16: { utf16, args in let (i, n) = args
        return utf16.characterIndex(i, offsetBy: n) },
      opaque: { opaque, args in let (i, n) = args
        return opaque.characterIndex(i, offsetBy: n) })
  }

  /// Returns an index that is the specified distance from the given index,
  /// unless that distance is beyond a given limiting index.
  ///
  /// The following example obtains an index advanced four positions from a
  /// string's starting index and then prints the character at that position.
  /// The operation doesn't require going beyond the limiting `s.endIndex`
  /// value, so it succeeds.
  ///
  ///     let s = "Swift"
  ///     if let i = s.index(s.startIndex, offsetBy: 4, limitedBy: s.endIndex) {
  ///         print(s[i])
  ///     }
  ///     // Prints "t"
  ///
  /// The next example attempts to retrieve an index six positions from
  /// `s.startIndex` but fails, because that distance is beyond the index
  /// passed as `limit`.
  ///
  ///     let j = s.index(s.startIndex, offsetBy: 6, limitedBy: s.endIndex)
  ///     print(j)
  ///     // Prints "nil"
  ///
  /// The value passed as `distance` must not offset `i` beyond the bounds of the
  /// collection, unless the index passed as `limit` prevents offsetting
  /// beyond those bounds.
  ///
  /// - Parameters:
  ///   - i: A valid index of the collection.
  ///   - distance: The distance to offset `i`.
  ///   - limit: A valid index of the collection to use as a limit. If `distance > 0`,
  ///     a limit that is less than `i` has no effect. Likewise, if `distance < 0`, a
  ///     limit that is greater than `i` has no effect.
  /// - Returns: An index offset by `distance` from the index `i`, unless that index
  ///   would be beyond `limit` in the direction of movement. In that case,
  ///   the method returns `nil`.
  ///
  /// - Complexity: O(*k*), where *k* is the absolute value of `distance`.
  public func index(
    _ i: Index, offsetBy distance: IndexDistance, limitedBy limit: Index
  ) -> Index? {
    return _visitGuts(_guts, args: (i, distance, limit),
      ascii: { ascii, args in let (i, n, limit) = args
        return ascii.characterIndex(i, offsetBy: n, limitedBy: limit) },
      utf16: { utf16, args in let (i, n, limit) = args
        return utf16.characterIndex(i, offsetBy: n, limitedBy: limit) },
      opaque: { opaque, args in let (i, n, limit) = args
        return opaque.characterIndex(i, offsetBy: n, limitedBy: limit) })
  }

  /// Returns the distance between two indices.
  ///
  /// - Parameters:
  ///   - start: A valid index of the collection.
  ///   - end: Another valid index of the collection. If `end` is equal to
  ///     `start`, the result is zero.
  /// - Returns: The distance between `start` and `end`.
  ///
  /// - Complexity: O(*k*), where *k* is the resulting distance.
  public func distance(from start: Index, to end: Index) -> IndexDistance {
    return _visitGuts(_guts, args: (start, end),
      ascii: { ascii, args in let (start, end) = args
        return ascii.characterDistance(from: start, to: end) },
      utf16: { utf16, args in let (start, end) = args
        return utf16.characterDistance(from: start, to: end) },
      opaque: { opaque, args in let (start, end) = args
        return opaque.characterDistance(from: start, to: end) })
  }

  /// Accesses the character at the given position.
  ///
  /// You can use the same indices for subscripting a string and its substring.
  /// For example, this code finds the first letter after the first space:
  ///
  ///     let str = "Greetings, friend! How are you?"
  ///     let firstSpace = str.firstIndex(of: " ") ?? str.endIndex
  ///     let substr = str[firstSpace...]
  ///     if let nextCapital = substr.firstIndex(where: { $0 >= "A" && $0 <= "Z" }) {
  ///         print("Capital after a space: \(str[nextCapital])")
  ///     }
  ///     // Prints "Capital after a space: H"
  ///
  /// - Parameter i: A valid index of the string. `i` must be less than the
  ///   string's end index.
  public subscript(i: Index) -> Character {
    return _visitGuts(_guts, args: i,
      ascii: { ascii, i in return ascii.character(at: i) },
      utf16: { utf16, i in return utf16.character(at: i) },
      opaque: { opaque, i in return opaque.character(at: i) })
  }
}
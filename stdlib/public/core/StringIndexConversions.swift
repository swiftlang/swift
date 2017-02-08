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

extension String.Index {
  /// Creates an index in the given string that corresponds exactly to the
  /// specified `UnicodeScalarView` position.
  ///
  /// The following example converts the position of the Unicode scalar `"e"`
  /// into its corresponding position in the string's character view. The
  /// character at that position is the composed `"é"` character.
  ///
  ///     let cafe = "Cafe\u{0301}"
  ///     print(cafe)
  ///     // Prints "Café"
  ///
  ///     let scalarsIndex = cafe.unicodeScalars.index(of: "e")!
  ///     let charactersIndex = String.Index(scalarsIndex, within: cafe)!
  ///
  ///     print(String(cafe.characters.prefix(through: charactersIndex)))
  ///     // Prints "Café"
  ///
  /// If the position passed in `unicodeScalarIndex` doesn't have an exact
  /// corresponding position in `other.characters`, the result of the
  /// initializer is `nil`. For example, an attempt to convert the position of
  /// the combining acute accent (`"\u{0301}"`) fails. Combining Unicode
  /// scalars do not have their own position in a character view.
  ///
  ///     let nextIndex = String.Index(cafe.unicodeScalars.index(after: scalarsIndex),
  ///                                  within: cafe)
  ///     print(nextIndex)
  ///     // Prints "nil"
  ///
  /// - Parameters:
  ///   - unicodeScalarIndex: A position in the `unicodeScalars` view of the
  ///     `other` parameter.
  ///   - other: The string referenced by both `unicodeScalarIndex` and the
  ///     resulting index.
  public init?(
    _ unicodeScalarIndex: String.UnicodeScalarIndex,
    within other: String
  ) {
    if !other.unicodeScalars._isOnGraphemeClusterBoundary(unicodeScalarIndex) {
      return nil
    }
    self.init(_base: unicodeScalarIndex, in: other.characters)
  }

  /// Creates an index in the given string that corresponds exactly to the
  /// specified `UTF16View` position.
  ///
  /// The following example finds the position of a space in a string's `utf16`
  /// view and then converts that position to an index in the string's
  /// `characters` view. The value `32` is the UTF-16 encoded value of a space
  /// character.
  ///
  ///     let cafe = "Café 🍵"
  ///
  ///     let utf16Index = cafe.utf16.index(of: 32)!
  ///     let charactersIndex = String.Index(utf16Index, within: cafe)!
  ///
  ///     print(String(cafe.characters.prefix(upTo: charactersIndex)))
  ///     // Prints "Café"
  ///
  /// If the position passed in `utf16Index` doesn't have an exact
  /// corresponding position in `other.characters`, the result of the
  /// initializer is `nil`. For example, an attempt to convert the position of
  /// the trailing surrogate of a UTF-16 surrogate pair fails.
  ///
  /// The next example attempts to convert the indices of the two UTF-16 code
  /// points that represent the teacup emoji (`"🍵"`). The index of the lead
  /// surrogate is successfully converted to a position in `other.characters`,
  /// but the index of the trailing surrogate is not.
  ///
  ///     let emojiHigh = cafe.utf16.index(after: utf16Index)
  ///     print(String.Index(emojiHigh, within: cafe))
  ///     // Prints "Optional(String.Index(...))"
  ///
  ///     let emojiLow = cafe.utf16.index(after: emojiHigh)
  ///     print(String.Index(emojiLow, within: cafe))
  ///     // Prints "nil"
  ///
  /// - Parameters:
  ///   - utf16Index: A position in the `utf16` view of the `other` parameter.
  ///   - other: The string referenced by both `utf16Index` and the resulting
  ///     index.
  public init?(
    _ utf16Index: String.UTF16Index,
    within other: String
  ) {
    if let me = utf16Index.samePosition(
      in: other.unicodeScalars
    )?.samePosition(in: other) {
      self = me
    }
    else {
      return nil
    }
  }

  /// Creates an index in the given string that corresponds exactly to the
  /// specified `UTF8View` position.
  ///
  /// If the position passed in `utf8Index` doesn't have an exact corresponding
  /// position in `other.characters`, the result of the initializer is `nil`.
  /// For example, an attempt to convert the position of a UTF-8 continuation
  /// byte returns `nil`.
  ///
  /// - Parameters:
  ///   - utf8Index: A position in the `utf8` view of the `other` parameter.
  ///   - other: The string referenced by both `utf8Index` and the resulting
  ///     index.
  public init?(
    _ utf8Index: String.UTF8Index,
    within other: String
  ) {
    if let me = utf8Index.samePosition(
      in: other.unicodeScalars
    )?.samePosition(in: other) {
      self = me
    }
    else {
      return nil
    }
  }

  /// Returns the position in the given UTF-8 view that corresponds exactly to
  /// this index.
  ///
  /// The index must be a valid index of `String(utf8).characters`.
  ///
  /// This example first finds the position of the character `"é"` and then uses
  /// this method find the same position in the string's `utf8` view.
  ///
  ///     let cafe = "Café"
  ///     if let i = cafe.characters.index(of: "é") {
  ///         let j = i.samePosition(in: cafe.utf8)
  ///         print(Array(cafe.utf8.suffix(from: j)))
  ///     }
  ///     // Prints "[195, 169]"
  ///
  /// - Parameter utf8: The view to use for the index conversion.
  /// - Returns: The position in `utf8` that corresponds exactly to this index.
  public func samePosition(
    in utf8: String.UTF8View
  ) -> String.UTF8View.Index {
    return String.UTF8View.Index(self, within: utf8)
  }

  /// Returns the position in the given UTF-16 view that corresponds exactly to
  /// this index.
  ///
  /// The index must be a valid index of `String(utf16).characters`.
  ///
  /// This example first finds the position of the character `"é"` and then uses
  /// this method find the same position in the string's `utf16` view.
  ///
  ///     let cafe = "Café"
  ///     if let i = cafe.characters.index(of: "é") {
  ///         let j = i.samePosition(in: cafe.utf16)
  ///         print(cafe.utf16[j])
  ///     }
  ///     // Prints "233"
  ///
  /// - Parameter utf16: The view to use for the index conversion.
  /// - Returns: The position in `utf16` that corresponds exactly to this index.
  public func samePosition(
    in utf16: String.UTF16View
  ) -> String.UTF16View.Index {
    return String.UTF16View.Index(self, within: utf16)
  }

  /// Returns the position in the given view of Unicode scalars that
  /// corresponds exactly to this index.
  ///
  /// The index must be a valid index of `String(unicodeScalars).characters`.
  ///
  /// This example first finds the position of the character `"é"` and then uses
  /// this method find the same position in the string's `unicodeScalars`
  /// view.
  ///
  ///     let cafe = "Café"
  ///     if let i = cafe.characters.index(of: "é") {
  ///         let j = i.samePosition(in: cafe.unicodeScalars)
  ///         print(cafe.unicodeScalars[j])
  ///     }
  ///     // Prints "é"
  ///
  /// - Parameter unicodeScalars: The view to use for the index conversion.
  /// - Returns: The position in `unicodeScalars` that corresponds exactly to
  ///   this index.
  public func samePosition(
    in unicodeScalars: String.UnicodeScalarView
  ) -> String.UnicodeScalarView.Index {
    return String.UnicodeScalarView.Index(self, within: unicodeScalars)
  }
}


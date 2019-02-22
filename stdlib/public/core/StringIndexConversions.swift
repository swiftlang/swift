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
  /// specified position.
  ///
  /// If the index passed as `sourcePosition` represents the start of an
  /// extended grapheme cluster---the element type of a string---then the
  /// initializer succeeds.
  ///
  /// The following example converts the position of the Unicode scalar `"e"`
  /// into its corresponding position in the string. The character at that
  /// position is the composed `"é"` character.
  ///
  ///     let cafe = "Cafe\u{0301}"
  ///     print(cafe)
  ///     // Prints "Café"
  ///
  ///     let scalarsIndex = cafe.unicodeScalars.firstIndex(of: "e")!
  ///     let stringIndex = String.Index(scalarsIndex, within: cafe)!
  ///
  ///     print(cafe[...stringIndex])
  ///     // Prints "Café"
  ///
  /// If the index passed as `sourcePosition` doesn't have an exact
  /// corresponding position in `target`, the result of the initializer is
  /// `nil`. For example, an attempt to convert the position of the combining
  /// acute accent (`"\u{0301}"`) fails. Combining Unicode scalars do not have
  /// their own position in a string.
  ///
  ///     let nextScalarsIndex = cafe.unicodeScalars.index(after: scalarsIndex)
  ///     let nextStringIndex = String.Index(nextScalarsIndex, within: cafe)
  ///
  ///     print(nextStringIndex)
  ///     // Prints "nil"
  ///
  /// - Parameters:
  ///   - sourcePosition: A position in a view of the `target` parameter.
  ///     `sourcePosition` must be a valid index of at least one of the views
  ///     of `target`.
  ///   - target: The string referenced by the resulting index.
  public init?(
    _ sourcePosition: String.Index,
    within target: String
  ) {
    guard target._guts.isOnGraphemeClusterBoundary(sourcePosition) else {
      return nil
    }
    self = sourcePosition
  }

  /// Returns the position in the given UTF-8 view that corresponds exactly to
  /// this index.
  ///
  /// This example first finds the position of the character `"é"`, and then
  /// uses this method find the same position in the string's `utf8` view.
  ///
  ///     let cafe = "Café"
  ///     if let i = cafe.firstIndex(of: "é") {
  ///         let j = i.samePosition(in: cafe.utf8)!
  ///         print(Array(cafe.utf8[j...]))
  ///     }
  ///     // Prints "[195, 169]"
  ///
  /// - Parameter utf8: The view to use for the index conversion. This index
  ///   must be a valid index of at least one view of the string shared by
  ///   `utf8`.
  /// - Returns: The position in `utf8` that corresponds exactly to this index.
  ///   If this index does not have an exact corresponding position in `utf8`,
  ///   this method returns `nil`. For example, an attempt to convert the
  ///   position of a UTF-16 trailing surrogate returns `nil`.
  public func samePosition(
    in utf8: String.UTF8View
    ) -> String.UTF8View.Index? {
    return String.UTF8View.Index(self, within: utf8)
  }

  /// Returns the position in the given UTF-16 view that corresponds exactly to
  /// this index.
  ///
  /// The index must be a valid index of `String(utf16)`.
  ///
  /// This example first finds the position of the character `"é"` and then
  /// uses this method find the same position in the string's `utf16` view.
  ///
  ///     let cafe = "Café"
  ///     if let i = cafe.firstIndex(of: "é") {
  ///         let j = i.samePosition(in: cafe.utf16)!
  ///         print(cafe.utf16[j])
  ///     }
  ///     // Prints "233"
  ///
  /// - Parameter utf16: The view to use for the index conversion. This index
  ///   must be a valid index of at least one view of the string shared by
  ///   `utf16`.
  /// - Returns: The position in `utf16` that corresponds exactly to this
  ///   index. If this index does not have an exact corresponding position in
  ///   `utf16`, this method returns `nil`. For example, an attempt to convert
  ///   the position of a UTF-8 continuation byte returns `nil`.
  public func samePosition(
    in utf16: String.UTF16View
  ) -> String.UTF16View.Index? {
    return String.UTF16View.Index(self, within: utf16)
  }
}


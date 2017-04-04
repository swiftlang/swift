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

  public typealias CharacterView = AnyCharacterUnicodeView
  // public struct CharacterView {
  //   internal var _core: _StringCore

  //   /// The offset of this view's `_core` from an original core. This works
  //   /// around the fact that `_StringCore` is always zero-indexed.
  //   /// `_coreOffset` should be subtracted from `UnicodeScalarIndex._position`
  //   /// before that value is used as a `_core` index.
  //   internal var _coreOffset: Int

  //   /// Creates a view of the given string.
  //   public init(_ text: String) {
  //     self._core = text._core
  //     self._coreOffset = 0
  //   }
    
  //   public // @testable
  //   init(_ _core: _StringCore, coreOffset: Int = 0) {
  //     self._core = _core
  //     self._coreOffset = coreOffset
  //   }
  // }

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

    return body(&self.characters)

    // // Naively mutating self.characters forces multiple references to
    // // exist at the point of mutation. Instead, temporarily move the
    // // core of this string into a CharacterView.
    // var tmp = CharacterView("")
    // swap(&_core, &tmp._core)
    // let r = body(&tmp)
    // swap(&_core, &tmp._core)
    // return r
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
    self.init(characters)
  }
}

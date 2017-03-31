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
  ///     if let i = favemoji.utf16.index(where: { $0 >= 128 }) {
  ///         let asciiPrefix = String(favemoji.utf16.prefix(upTo: i))
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
  /// `String.UTF16View.Index`, follow these steps:
  ///
  /// 1. Use the `NSRange` type's `toRange` method to convert the instance to
  ///    an optional range of `Int` values.
  /// 2. Use your string's `utf16` view's index manipulation methods to convert
  ///    the integer bounds to `String.UTF16View.Index` values.
  /// 3. Create a new `Range` instance from the new index values.
  ///
  /// Here's an implementation of those steps, showing how to retrieve a
  /// substring described by an `NSRange` instance from the middle of a
  /// string.
  ///
  ///     let snowy = "❄️ Let it snow! ☃️"
  ///     let nsrange = NSRange(location: 3, length: 12)
  ///     if let r = nsrange.toRange() {
  ///         let start = snowy.utf16.index(snowy.utf16.startIndex, offsetBy: r.lowerBound)
  ///         let end = snowy.utf16.index(snowy.utf16.startIndex, offsetBy: r.upperBound)
  ///         let substringRange = start..<end
  ///         print(snowy.utf16[substringRange])
  ///     }
  ///     // Prints "Let it snow!"
  public typealias UTF16View = AnyUInt16UnicodeView

  /// A UTF-16 encoding of `self`.
  public var utf16: UTF16View {
    get {
      return UTF16View(_core)
    }
    set {
      self = String(describing: newValue)
    }
  }

  /// Creates a string corresponding to the given sequence of UTF-16 code units.
  ///
  /// If `utf16` contains unpaired UTF-16 surrogates, the result is `nil`.
  ///
  /// You can use this initializer to create a new string from a slice of
  /// another string's `utf16` view.
  ///
  ///     let picnicGuest = "Deserving porcupine"
  ///     if let i = picnicGuest.utf16.index(of: 32) {
  ///         let adjective = String(picnicGuest.utf16.prefix(upTo: i))
  ///         print(adjective)
  ///     }
  ///     // Prints "Optional(Deserving)"
  ///
  /// The `adjective` constant is created by calling this initializer with a
  /// slice of the `picnicGuest.utf16` view.
  ///
  /// - Parameter utf16: A UTF-16 code sequence.
  // public init?(_ utf16: UTF16View) {
  //   let wholeString = String(utf16._core)

  //   guard
  //     let start = UTF16Index(_offset: utf16._offset)
  //       .samePosition(in: wholeString),
  //     let end = UTF16Index(_offset: utf16._offset + utf16._length)
  //       .samePosition(in: wholeString)
  //     else
  //   {
  //       return nil
  //   }
  //   self = wholeString[start..<end]
  // }

  public init(_ utf16: UTF16View) {
    self.init()
    self.content.utf16 = utf16
  }

  /// The index type for subscripting a string's `utf16` view.
  public typealias UTF16Index = UTF16View.Index
}

extension String.UTF16View : CustomStringConvertible, CustomDebugStringConvertible {
  public var description: String {
    return String(self)
  }

  public var debugDescription: String {
    return "StringUnicodeScalarView(\(self.description.debugDescription))"
  }
}

// extension String.UTF16View.Index : Comparable {
//   // FIXME: swift-3-indexing-model: add complete set of forwards for Comparable 
//   //        assuming String.UTF8View.Index continues to exist
//   public static func == (
//     lhs: String.UTF16View.Index,
//     rhs: String.UTF16View.Index
//   ) -> Bool {
//     return lhs._offset == rhs._offset
//   }

//   public static func < (
//     lhs: String.UTF16View.Index,
//     rhs: String.UTF16View.Index
//   ) -> Bool {
//     return lhs._offset < rhs._offset
//   }
// }

// // Index conversions
// extension String.UTF16View.Index {
//   /// Creates an index in the given UTF-16 view that corresponds exactly to the
//   /// specified `UTF8View` position.
//   ///
//   /// The following example finds the position of a space in a string's `utf8`
//   /// view and then converts that position to an index in the string's
//   /// `utf16` view.
//   ///
//   ///     let cafe = "Café 🍵"
//   ///
//   ///     let utf8Index = cafe.utf8.index(of: 32)!
//   ///     let utf16Index = String.UTF16View.Index(utf8Index, within: cafe.utf16)!
//   ///
//   ///     print(cafe.utf16.prefix(upTo: utf16Index))
//   ///     // Prints "Café"
//   ///
//   /// If the position passed as `utf8Index` doesn't have an exact corresponding
//   /// position in `utf16`, the result of the initializer is `nil`. For
//   /// example, because UTF-8 and UTF-16 represent high Unicode code points
//   /// differently, an attempt to convert the position of a UTF-8 continuation
//   /// byte fails.
//   ///
//   /// - Parameters:
//   ///   - utf8Index: A position in a `UTF8View` instance. `utf8Index` must be
//   ///     an element in `String(utf16).utf8.indices`.
//   ///   - utf16: The `UTF16View` in which to find the new position.
//   public init?(
//     _ utf8Index: String.UTF8Index, within utf16: String.UTF16View
//   ) {
//     let core = utf16._core

//     _precondition(
//       utf8Index._coreIndex >= 0 && utf8Index._coreIndex <= core.endIndex,
//       "Invalid String.UTF8Index for this UTF-16 view")

//     // Detect positions that have no corresponding index.
//     if !utf8Index._isOnUnicodeScalarBoundary(in: core) {
//       return nil
//     }
//     _offset = utf8Index._coreIndex
//   }

//   /// Creates an index in the given UTF-16 view that corresponds exactly to the
//   /// specified `UnicodeScalarView` position.
//   ///
//   /// The following example finds the position of a space in a string's `utf8`
//   /// view and then converts that position to an index in the string's
//   /// `utf16` view.
//   ///
//   ///     let cafe = "Café 🍵"
//   ///
//   ///     let scalarIndex = cafe.unicodeScalars.index(of: "é")!
//   ///     let utf16Index = String.UTF16View.Index(scalarIndex, within: cafe.utf16)
//   ///
//   ///     print(cafe.utf16.prefix(through: utf16Index))
//   ///     // Prints "Café"
//   ///
//   /// - Parameters:
//   ///   - unicodeScalarIndex: A position in a `UnicodeScalarView` instance.
//   ///     `unicodeScalarIndex` must be an element in
//   ///     `String(utf16).unicodeScalarIndex.indices`.
//   ///   - utf16: The `UTF16View` in which to find the new position.
//   // public init(
//   //   _ unicodeScalarIndex: String.UnicodeScalarIndex,
//   //   within utf16: String.UTF16View) {
//   //   _offset = unicodeScalarIndex._position
//   // }

//   /// Creates an index in the given UTF-16 view that corresponds exactly to the
//   /// specified `CharacterView` position.
//   ///
//   /// The following example finds the position of a space in a string's `characters`
//   /// view and then converts that position to an index in the string's
//   /// `utf16` view.
//   ///
//   ///     let cafe = "Café 🍵"
//   ///
//   ///     let characterIndex = cafe.characters.index(of: "é")!
//   ///     let utf16Index = String.UTF16View.Index(characterIndex, within: cafe.utf16)
//   ///
//   ///     print(cafe.utf16.prefix(through: utf16Index))
//   ///     // Prints "Café"
//   ///
//   /// - Parameters:
//   ///   - characterIndex: A position in a `CharacterView` instance.
//   ///     `characterIndex` must be an element in
//   ///     `String(utf16).characters.indices`.
//   ///   - utf16: The `UTF16View` in which to find the new position.
//   // public init(_ characterIndex: String.Index, within utf16: String.UTF16View) {
//   //   _offset = characterIndex._utf16Index
//   // }

//   /// Returns the position in the given UTF-8 view that corresponds exactly to
//   /// this index.
//   ///
//   /// The index must be a valid index of `String(utf8).utf16`.
//   ///
//   /// This example first finds the position of a space (UTF-16 code point `32`)
//   /// in a string's `utf16` view and then uses this method to find the same
//   /// position in the string's `utf8` view.
//   ///
//   ///     let cafe = "Café 🍵"
//   ///     let i = cafe.utf16.index(of: 32)!
//   ///     let j = i.samePosition(in: cafe.utf8)!
//   ///     print(Array(cafe.utf8.prefix(upTo: j)))
//   ///     // Prints "[67, 97, 102, 195, 169]"
//   ///
//   /// - Parameter utf8: The view to use for the index conversion.
//   /// - Returns: The position in `utf8` that corresponds exactly to this index.
//   ///   If this index does not have an exact corresponding position in `utf8`,
//   ///   this method returns `nil`. For example, an attempt to convert the
//   ///   position of a UTF-16 trailing surrogate returns `nil`.
//   public func samePosition(
//     in utf8: String.UTF8View
//   ) -> String.UTF8View.Index? {
//     return String.UTF8View.Index(self, within: utf8)
//   }

//   /// Returns the position in the given view of Unicode scalars that
//   /// corresponds exactly to this index.
//   ///
//   /// This index must be a valid index of `String(unicodeScalars).utf16`.
//   ///
//   /// This example first finds the position of a space (UTF-16 code point `32`)
//   /// in a string's `utf16` view and then uses this method to find the same
//   /// position in the string's `unicodeScalars` view.
//   ///
//   ///     let cafe = "Café 🍵"
//   ///     let i = cafe.utf16.index(of: 32)!
//   ///     let j = i.samePosition(in: cafe.unicodeScalars)!
//   ///     print(cafe.unicodeScalars.prefix(upTo: j))
//   ///     // Prints "Café"
//   ///
//   /// - Parameter unicodeScalars: The view to use for the index conversion.
//   /// - Returns: The position in `unicodeScalars` that corresponds exactly to
//   ///   this index. If this index does not have an exact corresponding
//   ///   position in `unicodeScalars`, this method returns `nil`. For example,
//   ///   an attempt to convert the position of a UTF-16 trailing surrogate
//   ///   returns `nil`.
//   public func samePosition(
//     in unicodeScalars: String.UnicodeScalarView
//   ) -> String.UnicodeScalarIndex? {
//     return String.UnicodeScalarIndex(self, within: unicodeScalars)
//   }

//   /// Returns the position in the given string that corresponds exactly to this
//   /// index.
//   ///
//   /// This index must be a valid index of `characters.utf16`.
//   ///
//   /// This example first finds the position of a space (UTF-16 code point `32`)
//   /// in a string's `utf16` view and then uses this method find the same position
//   /// in the string.
//   ///
//   ///     let cafe = "Café 🍵"
//   ///     let i = cafe.utf16.index(of: 32)!
//   ///     let j = i.samePosition(in: cafe)!
//   ///     print(cafe[cafe.startIndex ..< j])
//   ///     // Prints "Café"
//   ///
//   /// - Parameter characters: The string to use for the index conversion.
//   /// - Returns: The position in `characters` that corresponds exactly to this
//   ///   index. If this index does not have an exact corresponding position in
//   ///   `characters`, this method returns `nil`. For example, an attempt to
//   ///   convert the position of a UTF-16 trailing surrogate returns `nil`.
//   public func samePosition(
//     in characters: String
//   ) -> String.Index? {
//     return String.Index(self, within: characters)
//   }
// }

// Reflection
extension String.UTF16View : CustomReflectable {
  /// Returns a mirror that reflects the UTF-16 view of a string.
  public var customMirror: Mirror {
    return Mirror(self, unlabeledChildren: self)
  }
}

extension String.UTF16View : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(description)
  }
}

// extension String.UTF16View.Indices : BidirectionalCollection {
//   public typealias Index = String.UTF16View.Index
//   public typealias IndexDistance = String.UTF16View.IndexDistance
//   public typealias Indices = String.UTF16View.Indices
//   public typealias SubSequence = String.UTF16View.Indices

//   internal init(
//     _elements: String.UTF16View,
//     startIndex: Index,
//     endIndex: Index
//   ) {
//     self._elements = _elements
//     self._startIndex = startIndex
//     self._endIndex = endIndex
//   }

//   public var startIndex: Index {
//     return _startIndex
//   }

//   public var endIndex: Index {
//     return _endIndex
//   }

//   public var indices: Indices {
//     return self
//   }

//   public subscript(i: Index) -> Index {
//     // FIXME: swift-3-indexing-model: range check.
//     return i
//   }

//   public subscript(bounds: Range<Index>) -> String.UTF16View.Indices {
//     // FIXME: swift-3-indexing-model: range check.
//     return String.UTF16View.Indices(
//       _elements: _elements,
//       startIndex: bounds.lowerBound,
//       endIndex: bounds.upperBound)
//   }

//   public func index(after i: Index) -> Index {
//     // FIXME: swift-3-indexing-model: range check.
//     return _elements.index(after: i)
//   }

//   public func formIndex(after i: inout Index) {
//     // FIXME: swift-3-indexing-model: range check.
//     _elements.formIndex(after: &i)
//   }

//   public func index(before i: Index) -> Index {
//     // FIXME: swift-3-indexing-model: range check.
//     return _elements.index(before: i)
//   }

//   public func formIndex(before i: inout Index) {
//     // FIXME: swift-3-indexing-model: range check.
//     _elements.formIndex(before: &i)
//   }

//   public func index(_ i: Index, offsetBy n: IndexDistance) -> Index {
//     // FIXME: swift-3-indexing-model: range check i?
//     return _elements.index(i, offsetBy: n)
//   }

//   public func index(
//     _ i: Index, offsetBy n: IndexDistance, limitedBy limit: Index
//   ) -> Index? {
//     // FIXME: swift-3-indexing-model: range check i?
//     return _elements.index(i, offsetBy: n, limitedBy: limit)
//   }

//   // TODO: swift-3-indexing-model - add docs
//   public func distance(from start: Index, to end: Index) -> IndexDistance {
//     // FIXME: swift-3-indexing-model: range check start and end?
//     return _elements.distance(from: start, to: end)
//   }
// }


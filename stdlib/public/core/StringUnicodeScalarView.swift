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

extension String {
  /// A view of a string's contents as a collection of Unicode scalar values.
  ///
  /// You can access a string's view of Unicode scalar values by using its
  /// `unicodeScalars` property. Unicode scalar values are the 21-bit codes
  /// that are the basic unit of Unicode. Each scalar value is represented by
  /// a `UnicodeScalar` instance and is equivalent to a UTF-32 code unit.
  ///
  ///     let flowers = "Flowers 💐"
  ///     for v in flowers.unicodeScalars {
  ///         print(v.value)
  ///     }
  ///     // 70
  ///     // 108
  ///     // 111
  ///     // 119
  ///     // 101
  ///     // 114
  ///     // 115
  ///     // 32
  ///     // 128144
  ///
  /// Some characters that are visible in a string are made up of more than one
  /// Unicode scalar value. In that case, a string's `unicodeScalars` view
  /// contains more values than its `characters` view.
  ///
  ///     let flag = "🇵🇷"
  ///     for c in flag.characters {
  ///         print(c)
  ///     }
  ///     // 🇵🇷
  ///
  ///     for v in flag.unicodeScalars {
  ///         print(v.value)
  ///     }
  ///     // 127477
  ///     // 127479
  ///
  /// You can convert a `String.UnicodeScalarView` instance back into a string
  /// using the `String` type's `init(_:)` initializer.
  ///
  ///     let favemoji = "My favorite emoji is 🎉"
  ///     if let i = favemoji.unicodeScalars.index(where: { $0.value >= 128 }) {
  ///         let asciiPrefix = String(favemoji.unicodeScalars.prefix(upTo: i))
  ///         print(asciiPrefix)
  ///     }
  ///     // Prints "My favorite emoji is "

  public typealias UnicodeScalarView = AnyUnicodeScalarUnicodeView

  /// Creates a string corresponding to the given collection of Unicode
  /// scalars.
  ///
  /// You can use this initializer to create a new string from a slice of
  /// another string's `unicodeScalars` view.
  ///
  ///     let picnicGuest = "Deserving porcupine"
  ///     if let i = picnicGuest.unicodeScalars.index(of: " ") {
  ///         let adjective = String(picnicGuest.unicodeScalars.prefix(upTo: i))
  ///         print(adjective)
  ///     }
  ///     // Prints "Deserving"
  ///
  /// The `adjective` constant is created by calling this initializer with a
  /// slice of the `picnicGuest.unicodeScalars` view.
  ///
  /// - Parameter unicodeScalars: A collection of Unicode scalar values.
  public init(_ unicodeScalars: UnicodeScalarView) {
    self.init()
    self.content.unicodeScalars = unicodeScalars
  }

  public init(_ unicodeScalars: UnicodeScalarView.SubSequence) {
    // Michael FIXME: This should go away after we get Substring sorted out
    self.init(UnicodeScalarView(unicodeScalars))
  }

  // public init(_ unicodeScalar: UnicodeScalar) {
  //   self.init(unicodeScalar)
  // }

  /// The index type for a string's `unicodeScalars` view.
  public typealias UnicodeScalarIndex = UnicodeScalarView.Index
}

extension String.UnicodeScalarView : CustomStringConvertible, CustomDebugStringConvertible {
  public var description: String {
    return String(self)
  }

  public var debugDescription: String {
    return "StringUnicodeScalarView(\(self.description.debugDescription))"
  }
}


extension String {
  /// The string's value represented as a collection of Unicode scalar values.
  public var unicodeScalars: UnicodeScalarView {
    get {
      return self.content.unicodeScalars
    }
    set {
      self.content.unicodeScalars = newValue
    }
  }
}

// extension String.UnicodeScalarView.Index : Comparable {
//   public static func == (
//     lhs: String.UnicodeScalarView.Index,
//     rhs: String.UnicodeScalarView.Index
//   ) -> Bool {
//     return lhs._position == rhs._position
//   }

//   public static func < (
//     lhs: String.UnicodeScalarView.Index,
//     rhs: String.UnicodeScalarView.Index
//   ) -> Bool {
//     return lhs._position < rhs._position
//   }
// }

// extension String.UnicodeScalarView : RangeReplaceableCollection {
//   /// Creates an empty view instance.
//   // public init() {
//   //   self = String.UnicodeScalarView(_StringCore())
//   // }
  
//   /// Reserves enough space in the view's underlying storage to store the
//   /// specified number of ASCII characters.
//   ///
//   /// Because a Unicode scalar value can require more than a single ASCII
//   /// character's worth of storage, additional allocation may be necessary
//   /// when adding to a Unicode scalar view after a call to
//   /// `reserveCapacity(_:)`.
//   ///
//   /// - Parameter n: The minimum number of ASCII character's worth of storage
//   ///   to allocate.
//   ///
//   /// - Complexity: O(*n*), where *n* is the capacity being reserved.
//   public mutating func reserveCapacity(_ n: Int) {
//     _core.reserveCapacity(n)
//   }
  
//   /// Appends the given Unicode scalar to the view.
//   ///
//   /// - Parameter c: The character to append to the string.
//   public mutating func append(_ x: UnicodeScalar) {
//     _core.append(x)
//   }

//   /// Appends the Unicode scalar values in the given sequence to the view.
//   ///
//   /// - Parameter newElements: A sequence of Unicode scalar values.
//   ///
//   /// - Complexity: O(*n*), where *n* is the length of the resulting view.
//   public mutating func append<S : Sequence>(contentsOf newElements: S)
//     where S.Iterator.Element == UnicodeScalar {
//     _core.append(contentsOf: newElements.lazy.flatMap { $0.utf16 })
//   }
  
//   /// Replaces the elements within the specified bounds with the given Unicode
//   /// scalar values.
//   ///
//   /// Calling this method invalidates any existing indices for use with this
//   /// string.
//   ///
//   /// - Parameters:
//   ///   - bounds: The range of elements to replace. The bounds of the range
//   ///     must be valid indices of the view.
//   ///   - newElements: The new Unicode scalar values to add to the string.
//   ///
//   /// - Complexity: O(*m*), where *m* is the combined length of the view and
//   ///   `newElements`. If the call to `replaceSubrange(_:with:)` simply
//   ///   removes elements at the end of the string, the complexity is O(*n*),
//   ///   where *n* is equal to `bounds.count`.
//   // public mutating func replaceSubrange<C>(
//   //   _ bounds: Range<Index>,
//   //   with newElements: C
//   // ) where C : Collection, C.Iterator.Element == UnicodeScalar {
//   //   let rawSubRange: Range<Int> = _toCoreIndex(bounds.lowerBound) ..<
//   //     _toCoreIndex(bounds.upperBound)
//   //   let lazyUTF16 = newElements.lazy.flatMap { $0.utf16 }
//   //   _core.replaceSubrange(rawSubRange, with: lazyUTF16)
//   // }
// }

// Index conversions
// extension String.UnicodeScalarIndex {
//   /// Creates an index in the given Unicode scalars view that corresponds
//   /// exactly to the specified `UTF16View` position.
//   ///
//   /// The following example finds the position of a space in a string's `utf16`
//   /// view and then converts that position to an index in the string's
//   /// `unicodeScalars` view:
//   ///
//   ///     let cafe = "Café 🍵"
//   ///
//   ///     let utf16Index = cafe.utf16.index(of: 32)!
//   ///     let scalarIndex = String.UnicodeScalarView.Index(utf16Index, within: cafe.unicodeScalars)!
//   ///
//   ///     print(String(cafe.unicodeScalars.prefix(upTo: scalarIndex)))
//   ///     // Prints "Café"
//   ///
//   /// If the position passed in `utf16Index` doesn't have an exact
//   /// corresponding position in `unicodeScalars`, the result of the
//   /// initializer is `nil`. For example, an attempt to convert the position of
//   /// the trailing surrogate of a UTF-16 surrogate pair fails.
//   ///
//   /// - Parameters:
//   ///   - utf16Index: A position in the `utf16` view of the `characters`
//   ///     parameter.
//   ///   - unicodeScalars: The `UnicodeScalarView` instance referenced by both
//   ///     `utf16Index` and the resulting index.
//   public init?(
//     _ utf16Index: String.UTF16Index,
//     within unicodeScalars: String.UnicodeScalarView
//   ) {
//     let utf16 = String.UTF16View(unicodeScalars._core)

//     if utf16Index != utf16.startIndex
//     && utf16Index != utf16.endIndex {
//       _precondition(
//         utf16Index >= utf16.startIndex
//         && utf16Index <= utf16.endIndex,
//         "Invalid String.UTF16Index for this UnicodeScalar view")

//       // Detect positions that have no corresponding index.  Note that
//       // we have to check before and after, because an unpaired
//       // surrogate will be decoded as a single replacement character,
//       // thus making the corresponding position valid.
//       if UTF16.isTrailSurrogate(utf16[utf16Index])
//         && UTF16.isLeadSurrogate(utf16[utf16.index(before: utf16Index)]) {
//         return nil
//       }
//     }
//     self.init(_position: utf16Index._offset)
//   }

//   /// Creates an index in the given Unicode scalars view that corresponds
//   /// exactly to the specified `UTF8View` position.
//   ///
//   /// If the position passed as `utf8Index` doesn't have an exact corresponding
//   /// position in `unicodeScalars`, the result of the initializer is `nil`.
//   /// For example, an attempt to convert the position of a UTF-8 continuation
//   /// byte returns `nil`.
//   ///
//   /// - Parameters:
//   ///   - utf8Index: A position in the `utf8` view of the `characters`
//   ///     parameter.
//   ///   - unicodeScalars: The `UnicodeScalarView` instance referenced by both
//   ///     `utf8Index` and the resulting index.
//   // public init?(
//   //   _ utf8Index: String.UTF8Index,
//   //   within unicodeScalars: String.UnicodeScalarView
//   // ) {
//   //   let core = unicodeScalars._core

//   //   _precondition(
//   //     utf8Index._coreIndex >= 0 && utf8Index._coreIndex <= core.endIndex,
//   //     "Invalid String.UTF8Index for this UnicodeScalar view")

//   //   // Detect positions that have no corresponding index.
//   //   if !utf8Index._isOnUnicodeScalarBoundary(in: core) {
//   //     return nil
//   //   }
//   //   self.init(_position: utf8Index._coreIndex)
//   // }

//   /// Creates an index in the given Unicode scalars view that corresponds
//   /// exactly to the specified string position.
//   ///
//   /// The following example converts the position of the teacup emoji (`"🍵"`)
//   /// into its corresponding position in the string's `unicodeScalars` view.
//   ///
//   ///     let cafe = "Café 🍵"
//   ///     let characterIndex = cafe.characters.index(of: "🍵")!
//   ///     let scalarIndex = String.UnicodeScalarView.Index(characterIndex, within: cafe.unicodeScalars)
//   ///
//   ///     print(cafe.unicodeScalars.suffix(from: scalarIndex))
//   ///     // Prints "🍵"
//   ///
//   /// - Parameters:
//   ///   - characterIndex: A position in a `CharacterView` instance.
//   ///     `characterIndex` must be an element of
//   ///     `String(utf8).characters.indices`.
//   ///   - utf8: The `UTF8View` in which to find the new position.
//   // public init(
//   //   _ characterIndex: String.Index,
//   //   within unicodeScalars: String.UnicodeScalarView
//   // ) {
//   //   self.init(_position: characterIndex._base._position)
//   // }

//   /// Returns the position in the given UTF-8 view that corresponds exactly to
//   /// this index.
//   ///
//   /// The index must be a valid index of `String(utf8).unicodeScalars`.
//   ///
//   /// This example first finds the position of the character `"é"` and then uses
//   /// this method find the same position in the string's `utf8` view.
//   ///
//   ///     let cafe = "Café"
//   ///     if let i = cafe.unicodeScalars.index(of: "é") {
//   ///         let j = i.samePosition(in: cafe.utf8)
//   ///         print(Array(cafe.utf8.suffix(from: j)))
//   ///     }
//   ///     // Prints "[195, 169]"
//   ///
//   /// - Parameter utf8: The view to use for the index conversion.
//   /// - Returns: The position in `utf8` that corresponds exactly to this index.
//   public func samePosition(in utf8: String.UTF8View) -> String.UTF8View.Index {
//     return String.UTF8View.Index(self, within: utf8)
//   }

//   /// Returns the position in the given UTF-16 view that corresponds exactly to
//   /// this index.
//   ///
//   /// The index must be a valid index of `String(utf16).unicodeScalars`.
//   ///
//   /// This example first finds the position of the character `"é"` and then uses
//   /// this method find the same position in the string's `utf16` view.
//   ///
//   ///     let cafe = "Café"
//   ///     if let i = cafe.characters.index(of: "é") {
//   ///         let j = i.samePosition(in: cafe.utf16)
//   ///         print(cafe.utf16[j])
//   ///     }
//   ///     // Prints "233"
//   ///
//   /// - Parameter utf16: The view to use for the index conversion.
//   /// - Returns: The position in `utf16` that corresponds exactly to this index.
//   public func samePosition(
//     in utf16: String.UTF16View
//   ) -> String.UTF16View.Index {
//     return String.UTF16View.Index(self, within: utf16)
//   }

//   /// Returns the position in the given string that corresponds exactly to this
//   /// index.
//   ///
//   /// This index must be a valid index of `characters.unicodeScalars`.
//   ///
//   /// This example first finds the position of a space (UTF-8 code point `32`)
//   /// in a string's `utf8` view and then uses this method find the same position
//   /// in the string.
//   ///
//   ///     let cafe = "Café 🍵"
//   ///     let i = cafe.unicodeScalars.index(of: "🍵")
//   ///     let j = i.samePosition(in: cafe)!
//   ///     print(cafe.suffix(from: j))
//   ///     // Prints "🍵"
//   ///
//   /// - Parameter characters: The string to use for the index conversion.
//   /// - Returns: The position in `characters` that corresponds exactly to
//   ///   this index. If this index does not have an exact corresponding
//   ///   position in `characters`, this method returns `nil`. For example,
//   ///   an attempt to convert the position of a UTF-8 continuation byte
//   ///   returns `nil`.
//   public func samePosition(in characters: String) -> String.Index? {
//     return String.Index(self, within: characters)
//   }
// }

extension String.UnicodeScalarView {
  // NOTE: Don't make this function inlineable.  Grapheme cluster
  // segmentation uses a completely different algorithm in Unicode 9.0.
  internal func _isOnGraphemeClusterBoundary(_ i: Index) -> Bool {
    if i == startIndex || i == endIndex {
      return true
    }
    let precedingScalar = self[index(before: i)]

    let graphemeClusterBreakProperty =
      _UnicodeGraphemeClusterBreakPropertyTrie()
    let segmenter = _UnicodeExtendedGraphemeClusterSegmenter()

    let gcb0 = graphemeClusterBreakProperty.getPropertyRawValue(
      precedingScalar.value)

    if segmenter.isBoundaryAfter(gcb0) {
      return true
    }

    let gcb1 = graphemeClusterBreakProperty.getPropertyRawValue(self[i].value)

    return segmenter.isBoundary(gcb0, gcb1)
  }
}

// Reflection
extension String.UnicodeScalarView : CustomReflectable {
  /// Returns a mirror that reflects the Unicode scalars view of a string.
  public var customMirror: Mirror {
    return Mirror(self, unlabeledChildren: self)
  }
}

extension String.UnicodeScalarView : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(description)
  }
}

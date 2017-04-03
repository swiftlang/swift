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

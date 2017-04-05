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
      return self.content.utf16
    }
    set {
      self.content.utf16 = newValue
    }
  }


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

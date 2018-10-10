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

import SwiftShims

@inlinable @_transparent
internal func unimplemented_utf8(
  _ message: String = "",
  file: StaticString = #file, line: UInt = #line
) -> Never {
  fatalError("Unimplemented for UTF-8 support", file: file, line: line)
}
@inlinable @_transparent
internal func unimplemented_utf8_32bit(
  _ message: String = "",
  file: StaticString = #file, line: UInt = #line
) -> Never {
  fatalError("32-bit: Unimplemented for UTF-8 support", file: file, line: line)
}

// TODO(UTF8): Find a better place to stick these...
extension UnsafePointer where Pointee == UInt8 {
  @inlinable
  internal var _asCChar: UnsafePointer<CChar> {
    @inline(__always) get {
      return UnsafeRawPointer(self).assumingMemoryBound(to: CChar.self)
    }
  }
}
extension UnsafeBufferPointer where Element == UInt8 {
  @inlinable
  internal var _asCChar: UnsafeBufferPointer<CChar> {
    @inline(__always) get {
      return UnsafeBufferPointer<CChar>(
        start: self.baseAddress._unsafelyUnwrappedUnchecked._asCChar,
        count: self.count)
    }
  }
}
extension UnsafeRawPointer {
  @inlinable
  internal var _asCChar: UnsafePointer<CChar> {
    @inline(__always) get {
      return self.assumingMemoryBound(to: CChar.self)
    }
  }
}
extension UnsafeRawBufferPointer {
  @inlinable
  internal var _asCChar: UnsafeBufferPointer<CChar> {
    @inline(__always) get {
      return UnsafeBufferPointer<CChar>(
        start: self.baseAddress._unsafelyUnwrappedUnchecked._asCChar,
        count: self.count)
    }
  }
}
extension UnsafePointer where Pointee == CChar {
  @inlinable
  internal var _asUInt8: UnsafePointer<UInt8> {
    @inline(__always) get {
      return UnsafeRawPointer(self).assumingMemoryBound(to: UInt8.self)
    }
  }
}
extension UnsafeBufferPointer where Element == CChar {
  @inlinable
  internal var _asUInt8: UnsafeBufferPointer<UInt8> {
    @inline(__always) get {
      return UnsafeBufferPointer<UInt8>(
        start: self.baseAddress._unsafelyUnwrappedUnchecked._asUInt8,
        count: self.count)
    }
  }
}
extension UnsafeRawPointer {
  @inlinable
  internal var _asUInt8: UnsafePointer<UInt8> {
    @inline(__always) get {
      return self.assumingMemoryBound(to: UInt8.self)
    }
  }
}
extension UnsafeRawBufferPointer {
  @inlinable
  internal var _asUInt8: UnsafeBufferPointer<UInt8> {
    @inline(__always) get {
      return UnsafeBufferPointer<UInt8>(
        start: self.baseAddress._unsafelyUnwrappedUnchecked._asUInt8,
        count: self.count)
    }
  }
}

extension Slice where Base == UnsafeBufferPointer<UInt8> {
  @inlinable
  internal var _rebased: UnsafeBufferPointer<UInt8> {
    @inline(__always) get {
      return UnsafeBufferPointer<UInt8>(rebasing: self)
    }
  }
}
extension Slice where Base == UnsafeBufferPointer<CChar> {
  @inlinable
  internal var _rebased: UnsafeBufferPointer<CChar> {
    @inline(__always) get {
      return UnsafeBufferPointer<CChar>(rebasing: self)
    }
  }
}

/// A Unicode string value that is a collection of characters.
///
/// A string is a series of characters, such as `"Swift"`, that forms a
/// collection. Strings in Swift are Unicode correct and locale insensitive,
/// and are designed to be efficient. The `String` type bridges with the
/// Objective-C class `NSString` and offers interoperability with C functions
/// that works with strings.
///
/// You can create new strings using string literals or string interpolations.
/// A *string literal* is a series of characters enclosed in quotes.
///
///     let greeting = "Welcome!"
///
/// *String interpolations* are string literals that evaluate any included
/// expressions and convert the results to string form. String interpolations
/// give you an easy way to build a string from multiple pieces. Wrap each
/// expression in a string interpolation in parentheses, prefixed by a
/// backslash.
///
///     let name = "Rosa"
///     let personalizedGreeting = "Welcome, \(name)!"
///     // personalizedGreeting == "Welcome, Rosa!"
///
///     let price = 2
///     let number = 3
///     let cookiePrice = "\(number) cookies: $\(price * number)."
///     // cookiePrice == "3 cookies: $6."
///
/// Combine strings using the concatenation operator (`+`).
///
///     let longerGreeting = greeting + " We're glad you're here!"
///     // longerGreeting == "Welcome! We're glad you're here!"
///
/// Multiline string literals are enclosed in three double quotation marks
/// (`"""`), with each delimiter on its own line. Indentation is stripped from
/// each line of a multiline string literal to match the indentation of the
/// closing delimiter.
///
///     let banner = """
///               __,
///              (           o  /) _/_
///               `.  , , , ,  //  /
///             (___)(_(_/_(_ //_ (__
///                          /)
///                         (/
///             """
///
/// Modifying and Comparing Strings
/// ===============================
///
/// Strings always have value semantics. Modifying a copy of a string leaves
/// the original unaffected.
///
///     var otherGreeting = greeting
///     otherGreeting += " Have a nice time!"
///     // otherGreeting == "Welcome! Have a nice time!"
///
///     print(greeting)
///     // Prints "Welcome!"
///
/// Comparing strings for equality using the equal-to operator (`==`) or a
/// relational operator (like `<` or `>=`) is always performed using Unicode
/// canonical representation. As a result, different representations of a
/// string compare as being equal.
///
///     let cafe1 = "Cafe\u{301}"
///     let cafe2 = "Café"
///     print(cafe1 == cafe2)
///     // Prints "true"
///
/// The Unicode scalar value `"\u{301}"` modifies the preceding character to
/// include an accent, so `"e\u{301}"` has the same canonical representation
/// as the single Unicode scalar value `"é"`.
///
/// Basic string operations are not sensitive to locale settings, ensuring that
/// string comparisons and other operations always have a single, stable
/// result, allowing strings to be used as keys in `Dictionary` instances and
/// for other purposes.
///
/// Accessing String Elements
/// =========================
///
/// A string is a collection of *extended grapheme clusters*, which approximate
/// human-readable characters. Many individual characters, such as "é", "김",
/// and "🇮🇳", can be made up of multiple Unicode scalar values. These scalar
/// values are combined by Unicode's boundary algorithms into extended
/// grapheme clusters, represented by the Swift `Character` type. Each element
/// of a string is represented by a `Character` instance.
///
/// For example, to retrieve the first word of a longer string, you can search
/// for a space and then create a substring from a prefix of the string up to
/// that point:
///
///     let name = "Marie Curie"
///     let firstSpace = name.firstIndex(of: " ") ?? name.endIndex
///     let firstName = name[..<firstSpace]
///     // firstName == "Marie"
///
/// The `firstName` constant is an instance of the `Substring` type---a type
/// that represents substrings of a string while sharing the original string's
/// storage. Substrings present the same interface as strings.
///
///     print("\(name)'s first name has \(firstName.count) letters.")
///     // Prints "Marie Curie's first name has 5 letters."
///
/// Accessing a String's Unicode Representation
/// ===========================================
///
/// If you need to access the contents of a string as encoded in different
/// Unicode encodings, use one of the string's `unicodeScalars`, `utf16`, or
/// `utf8` properties. Each property provides access to a view of the string
/// as a series of code units, each encoded in a different Unicode encoding.
///
/// To demonstrate the different views available for every string, the
/// following examples use this `String` instance:
///
///     let cafe = "Cafe\u{301} du 🌍"
///     print(cafe)
///     // Prints "Café du 🌍"
///
/// The `cafe` string is a collection of the nine characters that are visible
/// when the string is displayed.
///
///     print(cafe.count)
///     // Prints "9"
///     print(Array(cafe))
///     // Prints "["C", "a", "f", "é", " ", "d", "u", " ", "🌍"]"
///
/// Unicode Scalar View
/// -------------------
///
/// A string's `unicodeScalars` property is a collection of Unicode scalar
/// values, the 21-bit codes that are the basic unit of Unicode. Each scalar
/// value is represented by a `Unicode.Scalar` instance and is equivalent to a
/// UTF-32 code unit.
///
///     print(cafe.unicodeScalars.count)
///     // Prints "10"
///     print(Array(cafe.unicodeScalars))
///     // Prints "["C", "a", "f", "e", "\u{0301}", " ", "d", "u", " ", "\u{0001F30D}"]"
///     print(cafe.unicodeScalars.map { $0.value })
///     // Prints "[67, 97, 102, 101, 769, 32, 100, 117, 32, 127757]"
///
/// The `unicodeScalars` view's elements comprise each Unicode scalar value in
/// the `cafe` string. In particular, because `cafe` was declared using the
/// decomposed form of the `"é"` character, `unicodeScalars` contains the
/// scalar values for both the letter `"e"` (101) and the accent character
/// `"´"` (769).
///
/// UTF-16 View
/// -----------
///
/// A string's `utf16` property is a collection of UTF-16 code units, the
/// 16-bit encoding form of the string's Unicode scalar values. Each code unit
/// is stored as a `UInt16` instance.
///
///     print(cafe.utf16.count)
///     // Prints "11"
///     print(Array(cafe.utf16))
///     // Prints "[67, 97, 102, 101, 769, 32, 100, 117, 32, 55356, 57101]"
///
/// The elements of the `utf16` view are the code units for the string when
/// encoded in UTF-16. These elements match those accessed through indexed
/// `NSString` APIs.
///
///     let nscafe = cafe as NSString
///     print(nscafe.length)
///     // Prints "11"
///     print(nscafe.character(at: 3))
///     // Prints "101"
///
/// UTF-8 View
/// ----------
///
/// A string's `utf8` property is a collection of UTF-8 code units, the 8-bit
/// encoding form of the string's Unicode scalar values. Each code unit is
/// stored as a `UInt8` instance.
///
///     print(cafe.utf8.count)
///     // Prints "14"
///     print(Array(cafe.utf8))
///     // Prints "[67, 97, 102, 101, 204, 129, 32, 100, 117, 32, 240, 159, 140, 141]"
///
/// The elements of the `utf8` view are the code units for the string when
/// encoded in UTF-8. This representation matches the one used when `String`
/// instances are passed to C APIs.
///
///     let cLength = strlen(cafe)
///     print(cLength)
///     // Prints "14"
///
/// Measuring the Length of a String
/// ================================
///
/// When you need to know the length of a string, you must first consider what
/// you'll use the length for. Are you measuring the number of characters that
/// will be displayed on the screen, or are you measuring the amount of
/// storage needed for the string in a particular encoding? A single string
/// can have greatly differing lengths when measured by its different views.
///
/// For example, an ASCII character like the capital letter *A* is represented
/// by a single element in each of its four views. The Unicode scalar value of
/// *A* is `65`, which is small enough to fit in a single code unit in both
/// UTF-16 and UTF-8.
///
///     let capitalA = "A"
///     print(capitalA.count)
///     // Prints "1"
///     print(capitalA.unicodeScalars.count)
///     // Prints "1"
///     print(capitalA.utf16.count)
///     // Prints "1"
///     print(capitalA.utf8.count)
///     // Prints "1"
///
/// On the other hand, an emoji flag character is constructed from a pair of
/// Unicode scalar values, like `"\u{1F1F5}"` and `"\u{1F1F7}"`. Each of these
/// scalar values, in turn, is too large to fit into a single UTF-16 or UTF-8
/// code unit. As a result, each view of the string `"🇵🇷"` reports a different
/// length.
///
///     let flag = "🇵🇷"
///     print(flag.count)
///     // Prints "1"
///     print(flag.unicodeScalars.count)
///     // Prints "2"
///     print(flag.utf16.count)
///     // Prints "4"
///     print(flag.utf8.count)
///     // Prints "8"
///
/// To check whether a string is empty, use its `isEmpty` property instead of
/// comparing the length of one of the views to `0`. Unlike with `isEmpty`,
/// calculating a view's `count` property requires iterating through the
/// elements of the string.
///
/// Accessing String View Elements
/// ==============================
///
/// To find individual elements of a string, use the appropriate view for your
/// task. For example, to retrieve the first word of a longer string, you can
/// search the string for a space and then create a new string from a prefix
/// of the string up to that point.
///
///     let name = "Marie Curie"
///     let firstSpace = name.firstIndex(of: " ") ?? name.endIndex
///     let firstName = name[..<firstSpace]
///     print(firstName)
///     // Prints "Marie"
///
/// Strings and their views share indices, so you can access the UTF-8 view of
/// the `name` string using the same `firstSpace` index.
///
///     print(Array(name.utf8[..<firstSpace]))
///     // Prints "[77, 97, 114, 105, 101]"
///
/// Note that an index into one view may not have an exact corresponding
/// position in another view. For example, the `flag` string declared above
/// comprises a single character, but is composed of eight code units when
/// encoded as UTF-8. The following code creates constants for the first and
/// second positions in the `flag.utf8` view. Accessing the `utf8` view with
/// these indices yields the first and second code UTF-8 units.
///
///     let firstCodeUnit = flag.startIndex
///     let secondCodeUnit = flag.utf8.index(after: firstCodeUnit)
///     // flag.utf8[firstCodeUnit] == 240
///     // flag.utf8[secondCodeUnit] == 159
///
/// When used to access the elements of the `flag` string itself, however, the
/// `secondCodeUnit` index does not correspond to the position of a specific
/// character. Instead of only accessing the specific UTF-8 code unit, that
/// index is treated as the position of the character at the index's encoded
/// offset. In the case of `secondCodeUnit`, that character is still the flag
/// itself.
///
///     // flag[firstCodeUnit] == "🇵🇷"
///     // flag[secondCodeUnit] == "🇵🇷"
///
/// If you need to validate that an index from one string's view corresponds
/// with an exact position in another view, use the index's
/// `samePosition(in:)` method or the `init(_:within:)` initializer.
///
///     if let exactIndex = secondCodeUnit.samePosition(in: flag) {
///         print(flag[exactIndex])
///     } else {
///         print("No exact match for this position.")
///     }
///     // Prints "No exact match for this position."
///
/// Performance Optimizations
/// =========================
///
/// Although strings in Swift have value semantics, strings use a copy-on-write
/// strategy to store their data in a buffer. This buffer can then be shared
/// by different copies of a string. A string's data is only copied lazily,
/// upon mutation, when more than one string instance is using the same
/// buffer. Therefore, the first in any sequence of mutating operations may
/// cost O(*n*) time and space.
///
/// When a string's contiguous storage fills up, a new buffer must be allocated
/// and data must be moved to the new storage. String buffers use an
/// exponential growth strategy that makes appending to a string a constant
/// time operation when averaged over many append operations.
///
/// Bridging Between String and NSString
/// ====================================
///
/// Any `String` instance can be bridged to `NSString` using the type-cast
/// operator (`as`), and any `String` instance that originates in Objective-C
/// may use an `NSString` instance as its storage. Because any arbitrary
/// subclass of `NSString` can become a `String` instance, there are no
/// guarantees about representation or efficiency when a `String` instance is
/// backed by `NSString` storage. Because `NSString` is immutable, it is just
/// as though the storage was shared by a copy. The first in any sequence of
/// mutating operations causes elements to be copied into unique, contiguous
/// storage which may cost O(*n*) time and space, where *n* is the length of
/// the string's encoded representation (or more, if the underlying `NSString`
/// has unusual performance characteristics).
///
/// For more information about the Unicode terms used in this discussion, see
/// the [Unicode.org glossary][glossary]. In particular, this discussion
/// mentions [extended grapheme clusters][clusters], [Unicode scalar
/// values][scalars], and [canonical equivalence][equivalence].
///
/// [glossary]: http://www.unicode.org/glossary/
/// [clusters]: http://www.unicode.org/glossary/#extended_grapheme_cluster
/// [scalars]: http://www.unicode.org/glossary/#unicode_scalar_value
/// [equivalence]: http://www.unicode.org/glossary/#canonical_equivalent
@_fixed_layout
public struct String {
  @usableFromInline
  internal var _guts: _StringGuts

  @inlinable @inline(__always)
  internal init(_ _guts: _StringGuts) {
    self._guts = _guts
    _invariantCheck()
  }

  /// Creates an empty string.
  ///
  /// Using this initializer is equivalent to initializing a string with an
  /// empty string literal.
  ///
  ///     let empty = ""
  ///     let alsoEmpty = String()
  @inlinable @inline(__always)
  public init() { self.init(_StringGuts()) }

  // TODO(UTF8 merge): Move these declarations back to their appropriate files.
  // Currently, they can crash the compiler in some build configurations.

  /// A view of a string's contents as a collection of Unicode scalar values.
  ///
  /// You can access a string's view of Unicode scalar values by using its
  /// `unicodeScalars` property. Unicode scalar values are the 21-bit codes
  /// that are the basic unit of Unicode. Each scalar value is represented by
  /// a `Unicode.Scalar` instance and is equivalent to a UTF-32 code unit.
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
  /// contains more elements than the string itself.
  ///
  ///     let flag = "🇵🇷"
  ///     for c in flag {
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
  ///     if let i = favemoji.unicodeScalars.firstIndex(where: { $0.value >= 128 }) {
  ///         let asciiPrefix = String(favemoji.unicodeScalars[..<i])
  ///         print(asciiPrefix)
  ///     }
  ///     // Prints "My favorite emoji is "
  @_fixed_layout
  public struct UnicodeScalarView {
    @usableFromInline
    internal var _guts: _StringGuts

    @inlinable @inline(__always)
    internal init(_ _guts: _StringGuts) {
      self._guts = _guts
      _invariantCheck()
    }
  }

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
  public struct UTF16View {
    @usableFromInline
    internal var _guts: _StringGuts

    @inlinable // FIXME(sil-serialize-all)
    internal init(_ guts: _StringGuts) {
      self._guts = guts
      _invariantCheck()
    }
  }

  /// A view of a string's contents as a collection of UTF-8 code units.
  ///
  /// You can access a string's view of UTF-8 code units by using its `utf8`
  /// property. A string's UTF-8 view encodes the string's Unicode scalar
  /// values as 8-bit integers.
  ///
  ///     let flowers = "Flowers 💐"
  ///     for v in flowers.utf8 {
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
  ///     // 240
  ///     // 159
  ///     // 146
  ///     // 144
  ///
  /// A string's Unicode scalar values can be up to 21 bits in length. To
  /// represent those scalar values using 8-bit integers, more than one UTF-8
  /// code unit is often required.
  ///
  ///     let flowermoji = "💐"
  ///     for v in flowermoji.unicodeScalars {
  ///         print(v, v.value)
  ///     }
  ///     // 💐 128144
  ///
  ///     for v in flowermoji.utf8 {
  ///         print(v)
  ///     }
  ///     // 240
  ///     // 159
  ///     // 146
  ///     // 144
  ///
  /// In the encoded representation of a Unicode scalar value, each UTF-8 code
  /// unit after the first is called a *continuation byte*.
  ///
  /// UTF8View Elements Match Encoded C Strings
  /// =========================================
  ///
  /// Swift streamlines interoperation with C string APIs by letting you pass a
  /// `String` instance to a function as an `Int8` or `UInt8` pointer. When you
  /// call a C function using a `String`, Swift automatically creates a buffer
  /// of UTF-8 code units and passes a pointer to that buffer. The code units
  /// of that buffer match the code units in the string's `utf8` view.
  ///
  /// The following example uses the C `strncmp` function to compare the
  /// beginning of two Swift strings. The `strncmp` function takes two
  /// `const char*` pointers and an integer specifying the number of characters
  /// to compare. Because the strings are identical up to the 14th character,
  /// comparing only those characters results in a return value of `0`.
  ///
  ///     let s1 = "They call me 'Bell'"
  ///     let s2 = "They call me 'Stacey'"
  ///
  ///     print(strncmp(s1, s2, 14))
  ///     // Prints "0"
  ///     print(String(s1.utf8.prefix(14)))
  ///     // Prints "They call me '"
  ///
  /// Extending the compared character count to 15 includes the differing
  /// characters, so a nonzero result is returned.
  ///
  ///     print(strncmp(s1, s2, 15))
  ///     // Prints "-17"
  ///     print(String(s1.utf8.prefix(15)))
  ///     // Prints "They call me 'B"
  @_fixed_layout
  public struct UTF8View {
    @usableFromInline
    internal var _guts: _StringGuts

    @inlinable @inline(__always)
    internal init(_ guts: _StringGuts) {
      self._guts = guts
      _invariantCheck()
    }
  }
}

extension String {
  #if !INTERNAL_CHECKS_ENABLED
  @inlinable @inline(__always) internal func _invariantCheck() {}
  #else
  @usableFromInline @inline(never) @_effects(releasenone)
  internal func _invariantCheck() {
    _guts._invariantCheck()
  }
  #endif // INTERNAL_CHECKS_ENABLED

  public func _dump() {
    #if INTERNAL_CHECKS_ENABLED
    _guts._dump()
    #endif // INTERNAL_CHECKS_ENABLED
  }
}

extension String {
  /// Creates a string from the given Unicode code units in the specified
  /// encoding.
  ///
  /// - Parameters:
  ///   - codeUnits: A collection of code units encoded in the encoding
  ///     specified in `sourceEncoding`.
  ///   - sourceEncoding: The encoding in which `codeUnits` should be
  ///     interpreted.
  @inlinable
  @inline(__always) // Eliminate dynamic type check when possible
  public init<C: Collection, Encoding: Unicode.Encoding>(
    decoding codeUnits: C, as sourceEncoding: Encoding.Type
  ) where C.Iterator.Element == Encoding.CodeUnit {
    if let contigBytes = codeUnits as? _HasContiguousBytes,
       sourceEncoding == UTF8.self,
       contigBytes._providesContiguousBytesNoCopy
    {
      self = contigBytes.withUnsafeBytes { rawBufPtr in
        let ptr = rawBufPtr.baseAddress._unsafelyUnwrappedUnchecked
        return String._fromUTF8Repairing(
          UnsafeBufferPointer(
            start: ptr.assumingMemoryBound(to: UInt8.self),
            count: rawBufPtr.count)).0
      }
      return
    }

    self = String._fromCodeUnits(
      codeUnits, encoding: sourceEncoding, repair: true)!.0
  }

  /// Calls the given closure with a pointer to the contents of the string,
  /// represented as a null-terminated sequence of code units.
  ///
  /// The pointer passed as an argument to `body` is valid only during the
  /// execution of `withCString(encodedAs:_:)`. Do not store or return the
  /// pointer for later use.
  ///
  /// - Parameters:
  ///   - body: A closure with a pointer parameter that points to a
  ///     null-terminated sequence of code units. If `body` has a return
  ///     value, that value is also used as the return value for the
  ///     `withCString(encodedAs:_:)` method. The pointer argument is valid
  ///     only for the duration of the method's execution.
  ///   - targetEncoding: The encoding in which the code units should be
  ///     interpreted.
  /// - Returns: The return value, if any, of the `body` closure parameter.
  @inlinable
  @inline(__always) // Eliminate dynamic type check when possible
  public func withCString<Result, TargetEncoding: Unicode.Encoding>(
    encodedAs targetEncoding: TargetEncoding.Type,
    _ body: (UnsafePointer<TargetEncoding.CodeUnit>) throws -> Result
  ) rethrows -> Result {
    if targetEncoding == UTF8.self {
      return try self.withCString {
        (cPtr: UnsafePointer<CChar>) -> Result  in
        _sanityCheck(UInt8.self == TargetEncoding.CodeUnit.self)
        let ptr = UnsafeRawPointer(cPtr).assumingMemoryBound(
          to: TargetEncoding.CodeUnit.self)
        return try body(ptr)
      }
    }
    return try _slowWithCString(encodedAs: targetEncoding, body)
  }

  @usableFromInline @inline(never) // slow-path
  @_effects(releasenone)
  internal func _slowWithCString<Result, TargetEncoding: Unicode.Encoding>(
    encodedAs targetEncoding: TargetEncoding.Type,
    _ body: (UnsafePointer<TargetEncoding.CodeUnit>) throws -> Result
  ) rethrows -> Result {
    // TODO(UTF8 perf): Transcode from guts directly
    let codeUnits = Array(self.utf8)
    var arg = Array<TargetEncoding.CodeUnit>()
    arg.reserveCapacity(1 &+ self._guts.count / 4)
    let repaired = transcode(
      codeUnits.makeIterator(),
      from: UTF8.self,
      to: targetEncoding,
      stoppingOnError: false,
      into: { arg.append($0) })
    arg.append(TargetEncoding.CodeUnit(0))
    _sanityCheck(!repaired)
    return try body(arg)
  }
}

extension String: _ExpressibleByBuiltinUnicodeScalarLiteral {
  @_effects(readonly)
  @inlinable @inline(__always)
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self.init(Unicode.Scalar(_unchecked: UInt32(value)))
  }

  @inlinable @inline(__always)
  public init(_ scalar: Unicode.Scalar) {
    self = scalar.withUTF8CodeUnits { String._uncheckedFromUTF8($0) }
  }
}

extension String: _ExpressibleByBuiltinExtendedGraphemeClusterLiteral {
  @inlinable @inline(__always)
  @_effects(readonly) @_semantics("string.makeUTF8")
  public init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
  ) {
    self.init(
      _builtinStringLiteral: start,
      utf8CodeUnitCount: utf8CodeUnitCount,
      isASCII: isASCII)
  }
}

extension String: _ExpressibleByBuiltinStringLiteral {
  @inlinable @inline(__always)
  @_effects(readonly) @_semantics("string.makeUTF8")
  public init(
    _builtinStringLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
    ) {
    let bufPtr = UnsafeBufferPointer(
      start: UnsafeRawPointer(start).assumingMemoryBound(to: UInt8.self),
      count: Int(utf8CodeUnitCount))
    if let smol = _SmallString(bufPtr) {
      self = String(_StringGuts(smol))
      return
    }
    self.init(_StringGuts(bufPtr, isASCII: Bool(isASCII)))
  }
}

extension String: ExpressibleByStringLiteral {
  // TODO(UTF8 merge): drop all of the below
  public typealias StringLiteralType = String
  public typealias UnicodeScalarLiteralType = String
  public typealias ExtendedGraphemeClusterLiteralType = String

  /// Creates an instance initialized to the given string value.
  ///
  /// Do not call this initializer directly. It is used by the compiler when you
  /// initialize a string using a string literal. For example:
  ///
  ///     let nextStop = "Clark & Lake"
  ///
  /// This assignment to the `nextStop` constant calls this string literal
  /// initializer behind the scenes.
  @inlinable @inline(__always)
  public init(stringLiteral value: String) {
    self = value
  }
}

extension String: CustomDebugStringConvertible {
  /// A representation of the string that is suitable for debugging.
  public var debugDescription: String {
    // TODO(UTF8): Drop some explicig `String` calls; needed for the SPM build
    var result: String = "\""
    for us in self.unicodeScalars {
      result += String(us.escaped(asASCII: false))
    }
    result += "\""
    return result
  }
}

extension String {
  @inlinable // Forward inlinability to append
  @_effects(readonly) @_semantics("string.concat")
  public static func + (lhs: String, rhs: String) -> String {
    var result = lhs
    result.append(rhs)
    return result
  }

  // String append
  @inlinable // Forward inlinability to append
  public static func += (lhs: inout String, rhs: String) {
    lhs.append(rhs)
  }
}

extension String {
  /// Constructs a `String` in `resultStorage` containing the given UTF-8.
  ///
  /// Low-level construction interface used by introspection
  /// implementation in the runtime library.
  @inlinable @inline(__always)
  @_silgen_name("swift_stringFromUTF8InRawMemory")
  public // COMPILER_INTRINSIC
  static func _fromUTF8InRawMemory(
    _ resultStorage: UnsafeMutablePointer<String>,
    start: UnsafeMutablePointer<UTF8.CodeUnit>,
    utf8CodeUnitCount: Int
    ) {
    unimplemented_utf8()
  }
}

extension Sequence where Element: StringProtocol {
  /// Returns a new string by concatenating the elements of the sequence,
  /// adding the given separator between each element.
  ///
  /// The following example shows how an array of strings can be joined to a
  /// single, comma-separated string:
  ///
  ///     let cast = ["Vivien", "Marlon", "Kim", "Karl"]
  ///     let list = cast.joined(separator: ", ")
  ///     print(list)
  ///     // Prints "Vivien, Marlon, Kim, Karl"
  ///
  /// - Parameter separator: A string to insert between each of the elements
  ///   in this sequence. The default separator is an empty string.
  /// - Returns: A single, concatenated string.
  @_specialize(where Self == Array<Substring>)
  @_specialize(where Self == Array<String>)
  public func joined(separator: String = "") -> String {
    return _joined(separator: separator)
  }

  @inline(__always) // Pick up @_specialize and devirtualize from two callers
  internal func _joined(separator: String) -> String {
    // A likely-under-estimate, but lets us skip some of the growth curve
    // for large Sequences.
    let understimatedCap =
      (1 &+ separator._guts.count) &* self.underestimatedCount
    var result = String() // TODO(UTF8 merge): replace String() with ""
    result.reserveCapacity(understimatedCap)
    if separator.isEmpty {
      for x in self {
        result.append(x._ephemeralString)
      }
      return result
    }

    var iter = makeIterator()
    if let first = iter.next() {
      result.append(first._ephemeralString)
      while let next = iter.next() {
        result.append(separator)
        result.append(next._ephemeralString)
      }
    }
    return result
  }
}

// This overload is necessary because String now conforms to
// BidirectionalCollection, and there are other `joined` overloads that are
// considered more specific. See Flatten.swift.gyb.
extension BidirectionalCollection where Element == String {
  /// Returns a new string by concatenating the elements of the sequence,
  /// adding the given separator between each element.
  ///
  /// The following example shows how an array of strings can be joined to a
  /// single, comma-separated string:
  ///
  ///     let cast = ["Vivien", "Marlon", "Kim", "Karl"]
  ///     let list = cast.joined(separator: ", ")
  ///     print(list)
  ///     // Prints "Vivien, Marlon, Kim, Karl"
  ///
  /// - Parameter separator: A string to insert between each of the elements
  ///   in this sequence. The default separator is an empty string.
  /// - Returns: A single, concatenated string.
  @_specialize(where Self == Array<String>)
  public func joined(separator: String = "") -> String {
    return _joined(separator: separator)
  }
}

// TODO(UTF8): Can we change the test and remove this? This is only here for
// test/RuntimeObjC.swift
#if _runtime(_ObjC)
@usableFromInline // FIXME(sil-serialize-all)
@_silgen_name("swift_stdlib_NSStringLowercaseString")
internal func _stdlib_NSStringLowercaseString(_ str: AnyObject) -> _CocoaString

@usableFromInline // FIXME(sil-serialize-all)
@_silgen_name("swift_stdlib_NSStringUppercaseString")
internal func _stdlib_NSStringUppercaseString(_ str: AnyObject) -> _CocoaString
#endif

// Unicode algorithms
extension String {
  @inline(__always)
  internal func _uppercaseASCII(_ x: UInt8) -> UInt8 {
    /// A "table" for which ASCII characters need to be upper cased.
    /// To determine which bit corresponds to which ASCII character, subtract 1
    /// from the ASCII value of that character and divide by 2. The bit is set iff
    /// that character is a lower case character.
    let _lowercaseTable: UInt64 =
      0b0001_1111_1111_1111_0000_0000_0000_0000 &<< 32

    // Lookup if it should be shifted in our ascii table, then we subtract 0x20 if
    // it should, 0x0 if not.
    // This code is equivalent to:
    // This code is equivalent to:
    // switch sourcex {
    // case let x where (x >= 0x41 && x <= 0x5a):
    //   return x &- 0x20
    // case let x:
    //   return x
    // }
    let isLower = _lowercaseTable &>> UInt64(((x &- 1) & 0b0111_1111) &>> 1)
    let toSubtract = (isLower & 0x1) &<< 5
    return x &- UInt8(truncatingIfNeeded: toSubtract)
  }

  @inline(__always)
  internal func _lowercaseASCII(_ x: UInt8) -> UInt8 {
    /// A "table" for which ASCII characters need to be lower cased.
    /// To determine which bit corresponds to which ASCII character, subtract 1
    /// from the ASCII value of that character and divide by 2. The bit is set iff
    /// that character is a upper case character.
    let _uppercaseTable: UInt64 =
      0b0000_0000_0000_0000_0001_1111_1111_1111 &<< 32

    // Lookup if it should be shifted in our ascii table, then we add 0x20 if
    // it should, 0x0 if not.
    // This code is equivalent to:
    // This code is equivalent to:
    // switch sourcex {
    // case let x where (x >= 0x41 && x <= 0x5a):
    //   return x &- 0x20
    // case let x:
    //   return x
    // }
    let isUpper = _uppercaseTable &>> UInt64(((x &- 1) & 0b0111_1111) &>> 1)
    let toAdd = (isUpper & 0x1) &<< 5
    return x &+ UInt8(truncatingIfNeeded: toAdd)
  }


  /// Returns a lowercase version of the string.
  ///
  /// Here's an example of transforming a string to all lowercase letters.
  ///
  ///     let cafe = "BBQ Café 🍵"
  ///     print(cafe.lowercased())
  ///     // Prints "bbq café 🍵"
  ///
  /// - Returns: A lowercase copy of the string.
  ///
  /// - Complexity: O(*n*)
  @_effects(releasenone)
  public func lowercased() -> String {
    if _fastPath(_guts.isFastASCII) {
      return _guts.withFastUTF8 { utf8 in
        // TODO(UTF8 perf): code-unit appendInPlace on guts
        var result = String()
        result.reserveCapacity(utf8.count)
        for u8 in utf8 {
          result._guts.append(String(Unicode.Scalar(_lowercaseASCII(u8)))._guts)
        }
        return result
      }
    }

    // TODO(UTF8 perf): This is a horribly slow means...
    let codeUnits = Array(self.utf16).withUnsafeBufferPointer {
      (uChars: UnsafeBufferPointer<UInt16>) -> Array<UInt16> in
      var result = Array<UInt16>(repeating: 0, count: uChars.count)
      let len = result.withUnsafeMutableBufferPointer {
        (output) -> Int in
        var err = __swift_stdlib_U_ZERO_ERROR
        return Int(truncatingIfNeeded:
          __swift_stdlib_u_strToLower(
            output.baseAddress._unsafelyUnwrappedUnchecked,
            Int32(output.count),
            uChars.baseAddress._unsafelyUnwrappedUnchecked,
            Int32(uChars.count),
            "", // TODO(UTF8): with new root, nil
            &err))
      }
      if len > uChars.count {
        var err = __swift_stdlib_U_ZERO_ERROR
        result = Array<UInt16>(repeating: 0, count: len)
        result.withUnsafeMutableBufferPointer {
          output -> Void in
          __swift_stdlib_u_strToLower(
            output.baseAddress._unsafelyUnwrappedUnchecked,
            Int32(output.count),
            uChars.baseAddress._unsafelyUnwrappedUnchecked,
            Int32(uChars.count),
            "", // TODO(UTF8): with new root, nil
            &err)
        }
      }
      return result
    }
    return codeUnits.withUnsafeBufferPointer { String._uncheckedFromUTF16($0) }
  }

  /// Returns an uppercase version of the string.
  ///
  /// The following example transforms a string to uppercase letters:
  ///
  ///     let cafe = "Café 🍵"
  ///     print(cafe.uppercased())
  ///     // Prints "CAFÉ 🍵"
  ///
  /// - Returns: An uppercase copy of the string.
  ///
  /// - Complexity: O(*n*)
  @_effects(releasenone)
  public func uppercased() -> String {
    if _fastPath(_guts.isFastASCII) {
      return _guts.withFastUTF8 { utf8 in
        // TODO(UTF8 perf): code-unit appendInPlace on guts
        var result = String()
        result.reserveCapacity(utf8.count)
        for u8 in utf8 {
          result._guts.append(String(Unicode.Scalar(_uppercaseASCII(u8)))._guts)
        }
        return result
      }
    }

    // TODO(UTF8 perf): This is a horribly slow means...
    let codeUnits = Array(self.utf16).withUnsafeBufferPointer {
      (uChars: UnsafeBufferPointer<UInt16>) -> Array<UInt16> in
      var result = Array<UInt16>(repeating: 0, count: uChars.count)
      let len = result.withUnsafeMutableBufferPointer {
        (output) -> Int in
        var err = __swift_stdlib_U_ZERO_ERROR
        return Int(truncatingIfNeeded:
          __swift_stdlib_u_strToUpper(
            output.baseAddress._unsafelyUnwrappedUnchecked,
            Int32(output.count),
            uChars.baseAddress._unsafelyUnwrappedUnchecked,
            Int32(uChars.count),
            "", // TODO(UTF8): with new root, nil
            &err))
      }
      if len > uChars.count {
        var err = __swift_stdlib_U_ZERO_ERROR
        result = Array<UInt16>(repeating: 0, count: len)
        result.withUnsafeMutableBufferPointer {
          output -> Void in
          __swift_stdlib_u_strToUpper(
            output.baseAddress._unsafelyUnwrappedUnchecked,
            Int32(output.count),
            uChars.baseAddress._unsafelyUnwrappedUnchecked,
            Int32(uChars.count),
            "", // TODO(UTF8): with new root, nil
            &err)
        }
      }
      return result
    }
    return codeUnits.withUnsafeBufferPointer { String._uncheckedFromUTF16($0) }
  }

  /// Creates an instance from the description of a given
  /// `LosslessStringConvertible` instance.
  @inlinable @inline(__always)
  public init<T : LosslessStringConvertible>(_ value: T) {
    self = value.description
  }
}

extension String: CustomStringConvertible {
  /// The value of this string.
  ///
  /// Using this property directly is discouraged. Instead, use simple
  /// assignment to create a new constant or variable equal to this string.
  @inlinable
  public var description: String { return self }
}

extension String {
  /// Calls the given closure with a pointer to the contents of the string,
  /// represented as a null-terminated sequence of UTF-8 code units.
  ///
  /// The pointer passed as an argument to `body` is valid only during the
  /// execution of `withCString(_:)`. Do not store or return the pointer for
  /// later use.
  ///
  /// - Parameter body: A closure with a pointer parameter that points to a
  ///   null-terminated sequence of UTF-8 code units. If `body` has a return
  ///   value, that value is also used as the return value for the
  ///   `withCString(_:)` method. The pointer argument is valid only for the
  ///   duration of the method's execution.
  /// - Returns: The return value, if any, of the `body` closure parameter.
  @inlinable // fast-path: already C-string compatible
  public func withCString<Result>(
    _ body: (UnsafePointer<Int8>) throws -> Result
  ) rethrows -> Result {
    return try _guts.withCString(body)
  }
}

// TODO(UTF8): Move this decl back to StringIndex.swift
extension String {
  /// A position of a character or code unit in a string.
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Index {
    @usableFromInline
    internal var _rawBits: UInt64

    @inlinable @inline(__always)
    init(_ raw: UInt64) {
      self._rawBits = raw
      self._invariantCheck()
    }
  }
}

extension String : LosslessStringConvertible {
  @inlinable // FIXME(sil-serialize-all)
  public init(_ content: String) {
    self = content
  }
}

extension String {
  public // @testable
  func _withNFCCodeUnits(_ f: (UInt8) throws -> Void) rethrows {
    try _slicedGuts.withNFCCodeUnitsIterator { 
      for cu in $0 {
        try f(cu)
      }
    }
  }
}

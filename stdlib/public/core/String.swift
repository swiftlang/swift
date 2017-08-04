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

/// A type that can represent a string as a collection of characters.
///
/// Do not declare new conformances to `StringProtocol`. Only the `String` and
/// `Substring` types in the standard library are valid conforming types.
public protocol StringProtocol
  : BidirectionalCollection,
  TextOutputStream, TextOutputStreamable,
  LosslessStringConvertible, ExpressibleByStringLiteral,
  Hashable, Comparable
  where Iterator.Element == Character {

  associatedtype UTF8View : /*Bidirectional*/Collection
  where UTF8View.Element == UInt8 // Unicode.UTF8.CodeUnit
  
  associatedtype UTF16View : BidirectionalCollection
  where UTF16View.Element == UInt16 // Unicode.UTF16.CodeUnit

  associatedtype UnicodeScalarView : BidirectionalCollection
  where UnicodeScalarView.Element == Unicode.Scalar
  
  var utf8: UTF8View { get }
  var utf16: UTF16View { get }
  var unicodeScalars: UnicodeScalarView { get }
  
#if _runtime(_ObjC)
  func hasPrefix(_ prefix: String) -> Bool
  func hasSuffix(_ prefix: String) -> Bool
#endif

  func lowercased() -> String
  func uppercased() -> String

  /// Creates a string from the given Unicode code units in the specified
  /// encoding.
  ///
  /// - Parameters:
  ///   - codeUnits: A collection of code units encoded in the ecoding
  ///     specified in `sourceEncoding`.
  ///   - sourceEncoding: The encoding in which `codeUnits` should be
  ///     interpreted.
  init<C: Collection, Encoding: Unicode.Encoding>(
    decoding codeUnits: C, as sourceEncoding: Encoding.Type
  )
    where C.Iterator.Element == Encoding.CodeUnit

  /// Creates a string from the null-terminated, UTF-8 encoded sequence of
  /// bytes at the given pointer.
  ///
  /// - Parameter nullTerminatedUTF8: A pointer to a sequence of contiguous,
  ///   UTF-8 encoded bytes ending just before the first zero byte.
  init(cString nullTerminatedUTF8: UnsafePointer<CChar>)
  
  /// Creates a string from the null-terminated sequence of bytes at the given
  /// pointer.
  ///
  /// - Parameters:
  ///   - nullTerminatedCodeUnits: A pointer to a sequence of contiguous code
  ///     units in the encoding specified in `sourceEncoding`, ending just
  ///     before the first zero code unit.
  ///   - sourceEncoding: The encoding in which the code units should be
  ///     interpreted.
  init<Encoding: Unicode.Encoding>(
    decodingCString nullTerminatedCodeUnits: UnsafePointer<Encoding.CodeUnit>,
    as sourceEncoding: Encoding.Type)
  
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
  func withCString<Result>(
    _ body: (UnsafePointer<CChar>) throws -> Result) rethrows -> Result

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
  func withCString<Result, Encoding: Unicode.Encoding>(
    encodedAs targetEncoding: Encoding.Type,
    _ body: (UnsafePointer<Encoding.CodeUnit>) throws -> Result
  ) rethrows -> Result
}

extension StringProtocol {
  //@available(swift, deprecated: 3.2, obsoleted: 4.0, message: "Please use the StringProtocol itself")
  //public var characters: Self { return self }

  @available(swift, deprecated: 3.2, obsoleted: 4.0, renamed: "UTF8View.Index")
  public typealias UTF8Index = UTF8View.Index
  @available(swift, deprecated: 3.2, obsoleted: 4.0, renamed: "UTF16View.Index")
  public typealias UTF16Index = UTF16View.Index
  @available(swift, deprecated: 3.2, obsoleted: 4.0, renamed: "UnicodeScalarView.Index")
  public typealias UnicodeScalarIndex = UnicodeScalarView.Index
}

/// A protocol that provides fast access to a known representation of String.
///
/// Can be used to specialize generic functions that would otherwise end up
/// doing grapheme breaking to vend individual characters.
internal protocol _SwiftStringView {
  /// A `String`, having the same contents as `self`, that may be unsuitable for
  /// long-term storage.
  var _ephemeralContent : String { get }
  
  /// A `String`, having the same contents as `self`, that is suitable for
  /// long-term storage.
  var _persistentContent : String { get }
}

extension _SwiftStringView {
  var _ephemeralContent : String { return _persistentContent }
}

extension StringProtocol {
  public // Used in the Foundation overlay
  var _ephemeralString : String {
    if _fastPath(self is _SwiftStringView) {
      return (self as! _SwiftStringView)._ephemeralContent
    }
    return String(String.CharacterView(self))
  }
}

extension String : _SwiftStringView {
  var _persistentContent : String { return characters._persistentContent }
}

/// Call body with a pointer to zero-terminated sequence of
/// `TargetEncoding.CodeUnit` representing the same string as `source`, when
/// `source` is interpreted as being encoded with `SourceEncoding`.
internal func _withCString<
  Source : Collection,
  SourceEncoding : Unicode.Encoding, 
  TargetEncoding : Unicode.Encoding, 
  Result
>(
  encodedAs targetEncoding: TargetEncoding.Type,
  from source: Source,
  encodedAs sourceEncoding: SourceEncoding.Type,
  execute body : (UnsafePointer<TargetEncoding.CodeUnit>) throws -> Result
) rethrows -> Result
where Source.Iterator.Element == SourceEncoding.CodeUnit {
  return try _withCStringAndLength(
    encodedAs: targetEncoding,
    from: source,
    encodedAs: sourceEncoding) { p, _ in try body(p) }
}

@_semantics("optimize.sil.specialize.generic.partial.never")
internal func _withCStringAndLength<
  Source : Collection,
  SourceEncoding : Unicode.Encoding, 
  TargetEncoding : Unicode.Encoding, 
  Result
>(
  encodedAs targetEncoding: TargetEncoding.Type,
  from source: Source,
  encodedAs sourceEncoding: SourceEncoding.Type,
  execute body : (UnsafePointer<TargetEncoding.CodeUnit>, Int) throws -> Result
) rethrows -> Result
where Source.Iterator.Element == SourceEncoding.CodeUnit {
  var targetLength = 0 // nul terminator
  var i = source.makeIterator()
  SourceEncoding.ForwardParser._parse(&i) {
    targetLength += numericCast(
      targetEncoding._transcode($0, from: SourceEncoding.self).count)
  }
  var a: [TargetEncoding.CodeUnit] = []
  a.reserveCapacity(targetLength + 1)
  i = source.makeIterator()
  SourceEncoding.ForwardParser._parse(&i) {
    a.append(
      contentsOf: targetEncoding._transcode($0, from: SourceEncoding.self))
  }
  a.append(0)
  return try body(a, targetLength)
}

extension _StringCore {
  /// Invokes `body` on a null-terminated sequence of code units in the given
  /// encoding corresponding to the substring in `bounds`.
  internal func _withCSubstring<Result, TargetEncoding: Unicode.Encoding>(
    in bounds: Range<Index>,
    encoding targetEncoding: TargetEncoding.Type,
    _ body: (UnsafePointer<TargetEncoding.CodeUnit>) throws -> Result
  ) rethrows -> Result {
    return try _withCSubstringAndLength(in: bounds, encoding: targetEncoding) {
      p,_ in try body(p)
    }
  }

  @_semantics("optimize.sil.specialize.generic.partial.never")
  internal func _withCSubstringAndLength<
    Result, TargetEncoding: Unicode.Encoding
  >(
    in bounds: Range<Index>,
    encoding targetEncoding: TargetEncoding.Type,
    _ body: (UnsafePointer<TargetEncoding.CodeUnit>, Int) throws -> Result
  ) rethrows -> Result {
    if _fastPath(hasContiguousStorage) {
      defer { _fixLifetime(self) }
      if isASCII {
        return try Swift._withCStringAndLength(
          encodedAs: targetEncoding,
          from: UnsafeBufferPointer(start: startASCII, count: count)[bounds],
          encodedAs: Unicode.ASCII.self,
          execute: body
        )
      }
      else {
        return try Swift._withCStringAndLength(
          encodedAs: targetEncoding,
          from: UnsafeBufferPointer(start: startUTF16, count: count)[bounds],
          encodedAs: Unicode.UTF16.self,
          execute: body
        )
      }
    }
    return try Swift._withCStringAndLength(
      encodedAs: targetEncoding,
      from: self[bounds],
      encodedAs: Unicode.UTF16.self,
      execute: body
    )
  }
}

extension String {
  /// Creates a string from the given Unicode code units in the specified
  /// encoding.
  ///
  /// - Parameters:
  ///   - codeUnits: A collection of code units encoded in the ecoding
  ///     specified in `sourceEncoding`.
  ///   - sourceEncoding: The encoding in which `codeUnits` should be
  ///     interpreted.
  public init<C: Collection, Encoding: Unicode.Encoding>(
    decoding codeUnits: C, as sourceEncoding: Encoding.Type
  ) where C.Iterator.Element == Encoding.CodeUnit {
    let (b,_) = _StringBuffer.fromCodeUnits(
      codeUnits, encoding: sourceEncoding, repairIllFormedSequences: true)
    self = String(_StringCore(b!))
  }
  
  /// Creates a string from the null-terminated sequence of bytes at the given
  /// pointer.
  ///
  /// - Parameters:
  ///   - nullTerminatedCodeUnits: A pointer to a sequence of contiguous code
  ///     units in the encoding specified in `sourceEncoding`, ending just
  ///     before the first zero code unit.
  ///   - sourceEncoding: The encoding in which the code units should be
  ///     interpreted.
  public init<Encoding: Unicode.Encoding>(
    decodingCString nullTerminatedCodeUnits: UnsafePointer<Encoding.CodeUnit>,
    as sourceEncoding: Encoding.Type) {

    let codeUnits = _SentinelCollection(
      UnsafeBufferPointer(_unboundedStartingAt: nullTerminatedCodeUnits),
      until: _IsZero()
    )
    self.init(decoding: codeUnits, as: sourceEncoding)
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
  public func withCString<Result, TargetEncoding: Unicode.Encoding>(
    encodedAs targetEncoding: TargetEncoding.Type,
    _ body: (UnsafePointer<TargetEncoding.CodeUnit>) throws -> Result
  ) rethrows -> Result {
    return try _core._withCSubstring(
      in: _core.startIndex..<_core.endIndex, encoding: targetEncoding, body)
  }
}
// FIXME: complexity documentation for most of methods on String ought to be
// qualified with "amortized" at least, as Characters are variable-length.

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
///              (          o   /) _/_
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
/// The Unicode code point `"\u{301}"` modifies the preceding character to
/// include an accent, so `"e\u{301}"` has the same canonical representation
/// as the single Unicode code point `"é"`.
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
/// and "🇮🇳", can be made up of multiple Unicode code points. These code points
/// are combined by Unicode's boundary algorithms into extended grapheme
/// clusters, represented by the Swift `Character` type. Each element of a
/// string is represented by a `Character` instance.
///
/// For example, to retrieve the first word of a longer string, you can search
/// for a space and then create a substring from a prefix of the string up to
/// that point:
///
///     let name = "Marie Curie"
///     let firstSpace = name.index(of: " ") ?? name.endIndex
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
/// decomposed form of the `"é"` character, `unicodeScalars` contains the code
/// points for both the letter `"e"` (101) and the accent character `"´"`
/// (769).
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
///     let firstSpace = name.index(of: " ") ?? name.endIndex
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
  /// Creates an empty string.
  public init() {
    _core = _StringCore()
  }

  public // @testable
  init(_ _core: _StringCore) {
    self._core = _core
  }

  public // @testable
  var _core: _StringCore
}

extension String {
  public // @testable
  static func _fromWellFormedCodeUnitSequence<
    Encoding : Unicode.Encoding, Input : Collection
  >(
    _ encoding: Encoding.Type, input: Input
  ) -> String
    where  Input.Element == Encoding.CodeUnit {
    return String._fromCodeUnitSequence(encoding, input: input)!
  }

  public // @testable
  static func _fromCodeUnitSequence<
    Encoding : Unicode.Encoding, Input : Collection
  >(
    _ encoding: Encoding.Type, input: Input
  ) -> String?
  where Input.Element == Encoding.CodeUnit {
    let (stringBufferOptional, _) =
        _StringBuffer.fromCodeUnits(input, encoding: encoding,
            repairIllFormedSequences: false)
    return stringBufferOptional.map { String(_storage: $0) }
  }

  public // @testable
  static func _fromCodeUnitSequenceWithRepair<
    Encoding : Unicode.Encoding, Input : Collection
  >(
    _ encoding: Encoding.Type, input: Input
  ) -> (String, hadError: Bool)
  where Input.Element == Encoding.CodeUnit {
    let (stringBuffer, hadError) =
        _StringBuffer.fromCodeUnits(input, encoding: encoding,
            repairIllFormedSequences: true)
    return (String(_storage: stringBuffer!), hadError)
  }
}

extension String : _ExpressibleByBuiltinUnicodeScalarLiteral {
  @effects(readonly)
  public // @testable
  init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self = String._fromWellFormedCodeUnitSequence(
      UTF32.self, input: CollectionOfOne(UInt32(value)))
  }
}

extension String : _ExpressibleByBuiltinExtendedGraphemeClusterLiteral {
  @_inlineable
  @effects(readonly)
  @_semantics("string.makeUTF8")
  public init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1) {
    self = String._fromWellFormedCodeUnitSequence(
      UTF8.self,
      input: UnsafeBufferPointer(
        start: UnsafeMutablePointer<UTF8.CodeUnit>(start),
        count: Int(utf8CodeUnitCount)))
  }
}

extension String : _ExpressibleByBuiltinUTF16StringLiteral {
  @_inlineable
  @effects(readonly)
  @_semantics("string.makeUTF16")
  public init(
    _builtinUTF16StringLiteral start: Builtin.RawPointer,
    utf16CodeUnitCount: Builtin.Word
  ) {
    self = String(
      _StringCore(
        baseAddress: UnsafeMutableRawPointer(start),
        count: Int(utf16CodeUnitCount),
        elementShift: 1,
        hasCocoaBuffer: false,
        owner: nil))
  }
}

extension String : _ExpressibleByBuiltinStringLiteral {
  @_inlineable
  @effects(readonly)
  @_semantics("string.makeUTF8")
  public init(
    _builtinStringLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1) {
    if Bool(isASCII) {
      self = String(
        _StringCore(
          baseAddress: UnsafeMutableRawPointer(start),
          count: Int(utf8CodeUnitCount),
          elementShift: 0,
          hasCocoaBuffer: false,
          owner: nil))
    }
    else {
      self = String._fromWellFormedCodeUnitSequence(
        UTF8.self,
        input: UnsafeBufferPointer(
          start: UnsafeMutablePointer<UTF8.CodeUnit>(start),
          count: Int(utf8CodeUnitCount)))
    }
  }
}

extension String : ExpressibleByStringLiteral {
  /// Creates an instance initialized to the given string value.
  ///
  /// Do not call this initializer directly. It is used by the compiler when you
  /// initialize a string using a string literal. For example:
  ///
  ///     let nextStop = "Clark & Lake"
  ///
  /// This assignment to the `nextStop` constant calls this string literal
  /// initializer behind the scenes.
  public init(stringLiteral value: String) {
     self = value
  }
}

extension String : CustomDebugStringConvertible {
  /// A representation of the string that is suitable for debugging.
  public var debugDescription: String {
    var result = "\""
    for us in self.unicodeScalars {
      result += us.escaped(asASCII: false)
    }
    result += "\""
    return result
  }
}

extension String {
  /// Returns the number of code units occupied by this string
  /// in the given encoding.
  func _encodedLength<
    Encoding: Unicode.Encoding
  >(_ encoding: Encoding.Type) -> Int {
    var codeUnitCount = 0
    self._encode(encoding, into: { _ in codeUnitCount += 1 })
    return codeUnitCount
  }

  // FIXME: this function may not handle the case when a wrapped NSString
  // contains unpaired surrogates.  Fix this before exposing this function as a
  // public API.  But it is unclear if it is valid to have such an NSString in
  // the first place.  If it is not, we should not be crashing in an obscure
  // way -- add a test for that.
  // Related: <rdar://problem/17340917> Please document how NSString interacts
  // with unpaired surrogates
  func _encode<Encoding: Unicode.Encoding>(
    _ encoding: Encoding.Type,
    into processCodeUnit: (Encoding.CodeUnit) -> Void
  ) {
    return _core.encode(encoding, into: processCodeUnit)
  }
}

// Support for copy-on-write
extension String {

  /// Appends the given string to this string.
  ///
  /// The following example builds a customized greeting by using the
  /// `append(_:)` method:
  ///
  ///     var greeting = "Hello, "
  ///     if let name = getUserName() {
  ///         greeting.append(name)
  ///     } else {
  ///         greeting.append("friend")
  ///     }
  ///     print(greeting)
  ///     // Prints "Hello, friend"
  ///
  /// - Parameter other: Another string.
  public mutating func append(_ other: String) {
    _core.append(other._core)
  }

  /// Appends the given Unicode scalar to the string.
  ///
  /// - Parameter x: A Unicode scalar value.
  ///
  /// - Complexity: Appending a Unicode scalar to a string averages to O(1)
  ///   over many additions.
  @available(*, unavailable, message: "Replaced by append(_: String)")
  public mutating func append(_ x: Unicode.Scalar) {
    Builtin.unreachable()
  }

  public // SPI(Foundation)
  init(_storage: _StringBuffer) {
    _core = _StringCore(_storage)
  }
}

extension String {
  @effects(readonly)
  @_semantics("string.concat")
  public static func + (lhs: String, rhs: String) -> String {
    if lhs.isEmpty {
      return rhs
    }
    var lhs = lhs
    lhs._core.append(rhs._core)
    return lhs
  }

  // String append
  public static func += (lhs: inout String, rhs: String) {
    if lhs.isEmpty {
      lhs = rhs
    }
    else {
      lhs._core.append(rhs._core)
    }
  }

  /// Constructs a `String` in `resultStorage` containing the given UTF-8.
  ///
  /// Low-level construction interface used by introspection
  /// implementation in the runtime library.
  @_inlineable
  @_silgen_name("swift_stringFromUTF8InRawMemory")
  public // COMPILER_INTRINSIC
  static func _fromUTF8InRawMemory(
    _ resultStorage: UnsafeMutablePointer<String>,
    start: UnsafeMutablePointer<UTF8.CodeUnit>,
    utf8CodeUnitCount: Int
  ) {
    resultStorage.initialize(to: 
      String._fromWellFormedCodeUnitSequence(
        UTF8.self,
        input: UnsafeBufferPointer(start: start, count: utf8CodeUnitCount)))
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
  public func joined(separator: String = "") -> String {
    return _joined(separator: separator)
  }

  @inline(__always)
  internal func _joined(separator: String = "") -> String {
    var result = ""

    // FIXME(performance): this code assumes UTF-16 in-memory representation.
    // It should be switched to low-level APIs.
    let separatorSize = separator.utf16.count

    let reservation = self._preprocessingPass {
      () -> Int in
      var r = 0
      for chunk in self {
        // FIXME(performance): this code assumes UTF-16 in-memory representation.
        // It should be switched to low-level APIs.
        r += separatorSize + chunk._ephemeralString.utf16.count
      }
      return r - separatorSize
    }

    if let n = reservation {
      result.reserveCapacity(n)
    }

    if separatorSize == 0 {
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
extension BidirectionalCollection where Iterator.Element == String {
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
  public func joined(separator: String = "") -> String {
    return _joined(separator: separator)
  }
}

#if _runtime(_ObjC)
@_silgen_name("swift_stdlib_NSStringLowercaseString")
func _stdlib_NSStringLowercaseString(_ str: AnyObject) -> _CocoaString

@_silgen_name("swift_stdlib_NSStringUppercaseString")
func _stdlib_NSStringUppercaseString(_ str: AnyObject) -> _CocoaString
#else
internal func _nativeUnicodeLowercaseString(_ str: String) -> String {
  var buffer = _StringBuffer(
    capacity: str._core.count, initialSize: str._core.count, elementWidth: 2)

  // Allocation of a StringBuffer requires binding the memory to the correct
  // encoding type.
  let dest = buffer.start.bindMemory(
    to: UTF16.CodeUnit.self, capacity: str._core.count)

  // Try to write it out to the same length.
  let z = _swift_stdlib_unicode_strToLower(
    dest, Int32(str._core.count),
    str._core.startUTF16, Int32(str._core.count))
  let correctSize = Int(z)

  // If more space is needed, do it again with the correct buffer size.
  if correctSize != str._core.count {
    buffer = _StringBuffer(
      capacity: correctSize, initialSize: correctSize, elementWidth: 2)
    let dest = buffer.start.bindMemory(
      to: UTF16.CodeUnit.self, capacity: str._core.count)
    _swift_stdlib_unicode_strToLower(
      dest, Int32(correctSize), str._core.startUTF16, Int32(str._core.count))
  }

  return String(_storage: buffer)
}

internal func _nativeUnicodeUppercaseString(_ str: String) -> String {
  var buffer = _StringBuffer(
    capacity: str._core.count, initialSize: str._core.count, elementWidth: 2)

  // Allocation of a StringBuffer requires binding the memory to the correct
  // encoding type.
  let dest = buffer.start.bindMemory(
    to: UTF16.CodeUnit.self, capacity: str._core.count)

  // Try to write it out to the same length.
  let z = _swift_stdlib_unicode_strToUpper(
    dest, Int32(str._core.count),
    str._core.startUTF16, Int32(str._core.count))
  let correctSize = Int(z)

  // If more space is needed, do it again with the correct buffer size.
  if correctSize != str._core.count {
    buffer = _StringBuffer(
      capacity: correctSize, initialSize: correctSize, elementWidth: 2)
    let dest = buffer.start.bindMemory(
      to: UTF16.CodeUnit.self, capacity: str._core.count)
    _swift_stdlib_unicode_strToUpper(
      dest, Int32(correctSize), str._core.startUTF16, Int32(str._core.count))
  }

  return String(_storage: buffer)
}
#endif

// Unicode algorithms
extension String {
  // FIXME: implement case folding without relying on Foundation.
  // <rdar://problem/17550602> [unicode] Implement case folding

  /// A "table" for which ASCII characters need to be upper cased.
  /// To determine which bit corresponds to which ASCII character, subtract 1
  /// from the ASCII value of that character and divide by 2. The bit is set iff
  /// that character is a lower case character.
  internal var _asciiLowerCaseTable: UInt64 {
    @inline(__always)
    get {
      return 0b0001_1111_1111_1111_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
    }
  }

  /// The same table for upper case characters.
  internal var _asciiUpperCaseTable: UInt64 {
    @inline(__always)
    get {
      return 0b0000_0000_0000_0000_0001_1111_1111_1111_0000_0000_0000_0000_0000_0000_0000_0000
    }
  }

  /// Returns a lowercase version of the string.
  ///
  /// Here's an example of transforming a string to all lowercase letters.
  ///
  ///     let cafe = "Café 🍵"
  ///     print(cafe.lowercased())
  ///     // Prints "café 🍵"
  ///
  /// - Returns: A lowercase copy of the string.
  ///
  /// - Complexity: O(*n*)
  public func lowercased() -> String {
    if let asciiBuffer = self._core.asciiBuffer {
      let count = asciiBuffer.count
      let source = asciiBuffer.baseAddress!
      let buffer = _StringBuffer(
        capacity: count, initialSize: count, elementWidth: 1)
      let dest = buffer.start
      for i in 0..<count {
        // For each character in the string, we lookup if it should be shifted
        // in our ascii table, then we return 0x20 if it should, 0x0 if not.
        // This code is equivalent to:
        // switch source[i] {
        // case let x where (x >= 0x41 && x <= 0x5a):
        //   dest[i] = x &+ 0x20
        // case let x:
        //   dest[i] = x
        // }
        let value = source[i]
        let isUpper =
          _asciiUpperCaseTable &>>
          UInt64(((value &- 1) & 0b0111_1111) &>> 1)
        let add = (isUpper & 0x1) &<< 5
        // Since we are left with either 0x0 or 0x20, we can safely truncate to
        // a UInt8 and add to our ASCII value (this will not overflow numbers in
        // the ASCII range).
        dest.storeBytes(of: value &+ UInt8(truncatingIfNeeded: add),
          toByteOffset: i, as: UInt8.self)
      }
      return String(_storage: buffer)
    }

#if _runtime(_ObjC)
    return _cocoaStringToSwiftString_NonASCII(
      _stdlib_NSStringLowercaseString(self._bridgeToObjectiveCImpl()))
#else
    return _nativeUnicodeLowercaseString(self)
#endif
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
  public func uppercased() -> String {
    if let asciiBuffer = self._core.asciiBuffer {
      let count = asciiBuffer.count
      let source = asciiBuffer.baseAddress!
      let buffer = _StringBuffer(
        capacity: count, initialSize: count, elementWidth: 1)
      let dest = buffer.start
      for i in 0..<count {
        // See the comment above in lowercaseString.
        let value = source[i]
        let isLower =
          _asciiLowerCaseTable &>>
          UInt64(((value &- 1) & 0b0111_1111) &>> 1)
        let add = (isLower & 0x1) &<< 5
        dest.storeBytes(of: value &- UInt8(truncatingIfNeeded: add),
          toByteOffset: i, as: UInt8.self)
      }
      return String(_storage: buffer)
    }

#if _runtime(_ObjC)
    return _cocoaStringToSwiftString_NonASCII(
      _stdlib_NSStringUppercaseString(self._bridgeToObjectiveCImpl()))
#else
    return _nativeUnicodeUppercaseString(self)
#endif
  }
  
  /// Creates an instance from the description of a given
  /// `LosslessStringConvertible` instance.
  public init<T : LosslessStringConvertible>(_ value: T) {
    self = value.description
  }
}

extension String : CustomStringConvertible {
  public var description: String {
    return self
  }
}

extension String {
  @available(*, unavailable, renamed: "append(_:)")
  public mutating func appendContentsOf(_ other: String) {
    Builtin.unreachable()
  }

  @available(*, unavailable, renamed: "append(contentsOf:)")
  public mutating func appendContentsOf<S : Sequence>(_ newElements: S)
    where S.Element == Character {
    Builtin.unreachable()
  }

  @available(*, unavailable, renamed: "insert(contentsOf:at:)")
  public mutating func insertContentsOf<S : Collection>(
    _ newElements: S, at i: Index
  ) where S.Element == Character {
    Builtin.unreachable()
  }

  @available(*, unavailable, renamed: "replaceSubrange")
  public mutating func replaceRange<C : Collection>(
    _ subRange: Range<Index>, with newElements: C
  ) where C.Element == Character {
    Builtin.unreachable()
  }
    
  @available(*, unavailable, renamed: "replaceSubrange")
  public mutating func replaceRange(
    _ subRange: Range<Index>, with newElements: String
  ) {
    Builtin.unreachable()
  }
  
  @available(*, unavailable, renamed: "remove(at:)")
  public mutating func removeAtIndex(_ i: Index) -> Character {
    Builtin.unreachable()
  }

  @available(*, unavailable, renamed: "removeSubrange")
  public mutating func removeRange(_ subRange: Range<Index>) {
    Builtin.unreachable()
  }

  @available(*, unavailable, renamed: "lowercased()")
  public var lowercaseString: String {
    Builtin.unreachable()
  }

  @available(*, unavailable, renamed: "uppercased()")
  public var uppercaseString: String {
    Builtin.unreachable()
  }

  @available(*, unavailable, renamed: "init(describing:)")
  public init<T>(_: T) {
    Builtin.unreachable()
  }
}

extension Sequence where Element == String {
  @available(*, unavailable, renamed: "joined(separator:)")
  public func joinWithSeparator(_ separator: String) -> String {
    Builtin.unreachable()
  }
}

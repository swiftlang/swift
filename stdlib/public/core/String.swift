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

// FIXME: complexity documentation for most of methods on String is ought to be
// qualified with "amortized" at least, as Characters are variable-length.

/// A Unicode string value.
///
/// A string is a series of characters, such as `"Swift"`. Strings in Swift are
/// Unicode correct, locale insensitive, and designed to be efficient. The
/// `String` type bridges with the Objective-C class `NSString` and offers
/// interoperability with C functions that works with strings.
///
/// You can create new strings using string literals or string interpolations.
/// A string literal is a series of characters enclosed in quotes.
///
///     let greeting = "Welcome!"
///
/// String interpolations are string literals that evaluate any included
/// expressions and convert the results to string form. String interpolations
/// are an easy way to build a string from multiple pieces. Wrap each
/// expression in a string interpolation in parentheses, prefixed by a
/// backslash.
///
///     let name = "Rosa"
///     let personalizedGreeting = "Welcome, \(name)!"
///
///     let price = 2
///     let number = 3
///     let cookiePrice = "\(number) cookies: $\(price * number)."
///
/// Combine strings using the concatenation operator (`+`).
///
///     let longerGreeting = greeting + " We're glad you're here!"
///     print(longerGreeting)
///     // Prints "Welcome! We're glad you're here!"
///
/// Modifying and Comparing Strings
/// ===============================
///
/// Strings always have value semantics. Modifying a copy of a string leaves
/// the original unaffected.
///
///     var otherGreeting = greeting
///     otherGreeting += " Have a nice time!"
///     print(otherGreeting)
///     // Prints "Welcome! Have a nice time!"
///
///     print(greeting)
///     // Prints "Welcome!"
///
/// Comparing strings for equality using the equal-to operator (`==`) or a
/// relational operator (like `<` and `>=`) is always performed using the
/// Unicode canonical representation. This means that different
/// representations of a string compare as being equal.
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
/// Basic string operations are not sensitive to locale settings. This ensures
/// that string comparisons and other operations always have a single, stable
/// result, allowing strings to be used as keys in `Dictionary` instances and
/// for other purposes.
///
/// Representing Strings: Views
/// ===========================
///
/// A string is not itself a collection. Instead, it has properties that
/// present its contents as meaningful collections. Each of these collections
/// is a particular type of *view* of the string's visible and data
/// representation.
///
/// To demonstrate the different views available for every string, the
/// following examples use this `String` instance:
///
///     let cafe = "Cafe\u{301} du 🌍"
///     print(cafe)
///     // Prints "Café du 🌍"
///
/// Character View
/// --------------
///
/// A string's `characters` property is a collection of *extended grapheme
/// clusters*, which approximate human-readable characters. Many individual
/// characters, such as "é", "김", and "🇮🇳", can be made up of multiple Unicode
/// code points. These code points are combined by Unicode's boundary
/// algorithms into extended grapheme clusters, represented by Swift's
/// `Character` type. Each element of the `characters` view is represented by
/// a `Character` instance.
///
///     print(cafe.characters.count)
///     // Prints "9"
///     print(Array(cafe.characters))
///     // Prints "["C", "a", "f", "é", " ", "d", "u", " ", "🌍"]"
///
/// Each visible character in the `cafe` string is a separate element of the
/// `characters` view.
///
/// Unicode Scalar View
/// -------------------
///
/// A string's `unicodeScalars` property is a collection of Unicode scalar
/// values, the 21-bit codes that are the basic unit of Unicode. Each scalar
/// value is represented by a `UnicodeScalar` instance and is equivalent to a
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
/// encoded in UTF-16.
///
/// The elements of this collection match those accessed through indexed
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
/// Counting the Length of a String
/// ===============================
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
///     print(capitalA.characters.count)
///     // Prints "1"
///     print(capitalA.unicodeScalars.count)
///     // Prints "1"
///     print(capitalA.utf16.count)
///     // Prints "1"
///     print(capitalA.utf8.count)
///     // Prints "1"
///
/// On the other hand, an emoji flag character is constructed from a pair of
/// Unicode scalars values, like `"\u{1F1F5}"` and `"\u{1F1F7}"`. Each of
/// these scalar values, in turn, is too large to fit into a single UTF-16 or
/// UTF-8 code unit. As a result, each view of the string `"🇵🇷"` reports a
/// different length.
///
///     let flag = "🇵🇷"
///     print(flag.characters.count)
///     // Prints "1"
///     print(flag.unicodeScalars.count)
///     // Prints "2"
///     print(flag.utf16.count)
///     // Prints "4"
///     print(flag.utf8.count)
///     // Prints "8"
///
/// To check whether a string is empty, use its `isEmpty` property instead
/// of comparing the length of one of the views to `0`. Unlike `isEmpty`,
/// calculating a view's `count` property requires iterating through the
/// elements of the string.
///
/// Accessing String View Elements
/// ==============================
///
/// To find individual elements of a string, use the appropriate view for your
/// task. For example, to retrieve the first word of a longer string, you can
/// search the `characters` view for a space and then create a new string from
/// a prefix of the `characters` view up to that point.
///
///     let name = "Marie Curie"
///     let firstSpace = name.characters.index(of: " ")!
///     let firstName = String(name.characters.prefix(upTo: firstSpace))
///     print(firstName)
///     // Prints "Marie"
///
/// You can convert an index into one of a string's views to an index into
/// another view.
///
///     let firstSpaceUTF8 = firstSpace.samePosition(in: name.utf8)
///     print(Array(name.utf8.prefix(upTo: firstSpaceUTF8)))
///     // Prints "[77, 97, 114, 105, 101]"
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
/// Bridging between String and NSString
/// ====================================
///
/// Any `String` instance can be bridged to `NSString` using the type-cast
/// operator (`as`), and any `String` instance that originates in Objective-C
/// may use an `NSString` instance as its storage. Because any arbitrary
/// subclass of `NSString` can become a `String` instance, there are no
/// guarantees about representation or efficiency when a `String` instance is
/// backed by `NSString` storage. Because `NSString` is immutable, it is just
/// as though the storage was shared by a copy: The first in any sequence of
/// mutating operations causes elements to be copied into unique, contiguous
/// storage which may cost O(*n*) time and space, where *n* is the length of
/// the string's encoded representation (or more, if the underlying `NSString`
/// has unusual performance characteristics).
///
/// For more information about the Unicode terms used in this discussion, see
/// the [Unicode.org glossary][glossary]. In particular, this discussion
/// mentions [extended grapheme clusters][clusters],
/// [Unicode scalar values][scalars], and [canonical equivalence][equivalence].
///
/// [glossary]: http://www.unicode.org/glossary/
/// [clusters]: http://www.unicode.org/glossary/#extended_grapheme_cluster
/// [scalars]: http://www.unicode.org/glossary/#unicode_scalar_value
/// [equivalence]: http://www.unicode.org/glossary/#canonical_equivalent
///
/// - SeeAlso: `String.CharacterView`, `String.UnicodeScalarView`,
///   `String.UTF16View`, `String.UTF8View`

extension String {
  public // @testable
  static func _fromWellFormedCodeUnitSequence<Encoding, Input>(
    _ encoding: Encoding.Type, input: Input
  ) -> String
    where
    Encoding: UnicodeCodec,
    Input: Collection,
    Input.Iterator.Element == Encoding.CodeUnit {
    return String._fromCodeUnitSequence(encoding, input: input)!
  }

  public // @testable
  static func _fromCodeUnitSequence<Encoding, Input>(
    _ encoding: Encoding.Type, input: Input
  ) -> String?
    where
    Encoding: UnicodeCodec,
    Input: Collection,
    Input.Iterator.Element == Encoding.CodeUnit {
    let (stringBufferOptional, _) =
        _StringBuffer.fromCodeUnits(input, encoding: encoding,
            repairIllFormedSequences: false)
    return stringBufferOptional.map { String(_storage: $0) }
  }

  public // @testable
  static func _fromCodeUnitSequenceWithRepair<Encoding, Input>(
    _ encoding: Encoding.Type, input: Input
  ) -> (String, hadError: Bool)
    where
    Encoding: UnicodeCodec,
    Input: Collection,
    Input.Iterator.Element == Encoding.CodeUnit {

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

extension String {
  /// Returns the number of code units occupied by this string
  /// in the given encoding.
  func _encodedLength<
    Encoding: UnicodeCodec
  >(_ encoding: Encoding.Type) -> Int {
    var codeUnitCount = 0
    self._encode(encoding, into: { _ in codeUnitCount += 1 })
    return codeUnitCount
  }

  // FIXME: this function does not handle the case when a wrapped NSString
  // contains unpaired surrogates.  Fix this before exposing this function as a
  // public API.  But it is unclear if it is valid to have such an NSString in
  // the first place.  If it is not, we should not be crashing in an obscure
  // way -- add a test for that.
  // Related: <rdar://problem/17340917> Please document how NSString interacts
  // with unpaired surrogates
  func _encode<
    Encoding: UnicodeCodec
  >(
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

  public // SPI(Foundation)
  init(_storage: _StringBuffer) {
    self.init(_StringCore(_storage))
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

extension Sequence where Iterator.Element == String {

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
        r += numericCast(separatorSize + chunk.utf16.count)
      }
      return r - numericCast(separatorSize)
    }

    if let n = reservation {
      result.reserveCapacity(numericCast(n))
    }

    if separatorSize == 0 {
      for x in self {
        result.append(x)
      }
      return result
    }

    var iter = makeIterator()
    if let first = iter.next() {
      result.append(first)
      while let next = iter.next() {
        result.append(separator)
        result.append(next)
      }
    }

    return result
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
      let source = asciiBuffer.baseAddress
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
          _asciiUpperCaseTable >>
          UInt64(((value &- 1) & 0b0111_1111) >> 1)
        let add = (isUpper & 0x1) << 5
        // Since we are left with either 0x0 or 0x20, we can safely truncate to
        // a UInt8 and add to our ASCII value (this will not overflow numbers in
        // the ASCII range).
        dest.storeBytes(of: value &+ UInt8(truncatingBitPattern: add),
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
      let source = asciiBuffer.baseAddress
      let buffer = _StringBuffer(
        capacity: count, initialSize: count, elementWidth: 1)
      let dest = buffer.start
      for i in 0..<count {
        // See the comment above in lowercaseString.
        let value = source[i]
        let isLower =
          _asciiLowerCaseTable >>
          UInt64(((value &- 1) & 0b0111_1111) >> 1)
        let add = (isLower & 0x1) << 5
        dest.storeBytes(of: value &- UInt8(truncatingBitPattern: add),
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

// extension String : CustomStringConvertible {
//   public var description: String {
//     return self
//   }
// }

extension String : LosslessStringConvertible {
  public init?(_ description: String) {
    self = description
  }
}

extension String {
  // Michael NOTE: temporarily disabled, as these can affect overload resolution
  // when we're just missing a key API.

  // @available(*, unavailable, renamed: "append(_:)")
  // public mutating func appendContentsOf(_ other: String) {
  //   Builtin.unreachable()
  // }

  // @available(*, unavailable, renamed: "append(contentsOf:)")
  // public mutating func appendContentsOf<S : Sequence>(_ newElements: S)
  //   where S.Iterator.Element == Character {
  //   Builtin.unreachable()
  // }

  // @available(*, unavailable, renamed: "insert(contentsOf:at:)")
  // public mutating func insertContentsOf<S : Collection>(
  //   _ newElements: S, at i: Index
  // ) where S.Iterator.Element == Character {
  //   Builtin.unreachable()
  // }

  // @available(*, unavailable, renamed: "replaceSubrange")
  // public mutating func replaceRange<C : Collection>(
  //   _ subRange: Range<Index>, with newElements: C
  // ) where C.Iterator.Element == Character {
  //   Builtin.unreachable()
  // }
    
  // @available(*, unavailable, renamed: "replaceSubrange")
  // public mutating func replaceRange(
  //   _ subRange: Range<Index>, with newElements: String
  // ) {
  //   Builtin.unreachable()
  // }
  
  // @available(*, unavailable, renamed: "remove(at:)")
  // public mutating func removeAtIndex(_ i: Index) -> Character {
  //   Builtin.unreachable()
  // }

  // @available(*, unavailable, renamed: "removeSubrange")
  // public mutating func removeRange(_ subRange: Range<Index>) {
  //   Builtin.unreachable()
  // }

  // @available(*, unavailable, renamed: "lowercased()")
  // public var lowercaseString: String {
  //   Builtin.unreachable()
  // }

  // @available(*, unavailable, renamed: "uppercased()")
  // public var uppercaseString: String {
  //   Builtin.unreachable()
  // }

  // @available(*, unavailable, renamed: "init(describing:)")
  // public init<T>(_: T) {
  //   Builtin.unreachable()
  // }
}

extension Sequence where Iterator.Element == String {
  // Michael NOTE: temporarily disabled, as these can affect overload resolution
  // when we're just missing a key API.

  // @available(*, unavailable, renamed: "joined(separator:)")
  // public func joinWithSeparator(_ separator: String) -> String {
  //   Builtin.unreachable()
  // }
}

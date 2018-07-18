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

@inlinable // FIXME(sil-serialize-all)
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

extension _StringGuts {
  //
  // TODO:(TODO: JIRA) This is all very bloated code; needs a rewrite given
  // StringGuts' new design and the potential to run directly on internal
  // storage. For now, follow a hand-coded opaque pattern.
  //

  /// Invokes `body` on a null-terminated sequence of code units in the given
  /// encoding corresponding to the substring in `bounds`.
  @inlinable // FIXME(sil-serialize-all)
  internal func _withCSubstring<Result, TargetEncoding: Unicode.Encoding>(
    in bounds: Range<Int>,
    encoding targetEncoding: TargetEncoding.Type,
    _ body: (UnsafePointer<TargetEncoding.CodeUnit>) throws -> Result
  ) rethrows -> Result {
    return try _withCSubstringAndLength(in: bounds, encoding: targetEncoding) {
      p,_ in try body(p)
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  @_semantics("optimize.sil.specialize.generic.partial.never")
  internal func _withCSubstringAndLength<
    Result, TargetEncoding: Unicode.Encoding
  >(
    in bounds: Range<Int>,
    encoding targetEncoding: TargetEncoding.Type,
    _ body: (UnsafePointer<TargetEncoding.CodeUnit>, Int) throws -> Result
  ) rethrows -> Result {
    if _slowPath(_isOpaque) {
      return try _opaqueWithCStringAndLength(
        in: bounds, encoding: targetEncoding, body)
    }

    defer { _fixLifetime(self) }
    if isASCII {
      let ascii = _unmanagedASCIIView[bounds]
      return try Swift._withCStringAndLength(
        encodedAs: targetEncoding,
        from: ascii.buffer,
        encodedAs: Unicode.ASCII.self,
        execute: body)
    }
    let utf16 = _unmanagedUTF16View[bounds]
    return try Swift._withCStringAndLength(
      encodedAs: targetEncoding,
      from: utf16.buffer,
      encodedAs: Unicode.UTF16.self,
      execute: body)
  }

  @usableFromInline // @opaque
  func _opaqueWithCStringAndLength<
    Result, TargetEncoding: Unicode.Encoding
  >(
    in bounds: Range<Int>,
    encoding targetEncoding: TargetEncoding.Type,
    _ body: (UnsafePointer<TargetEncoding.CodeUnit>, Int) throws -> Result
  ) rethrows -> Result {
    _sanityCheck(_isOpaque)

    if self._isSmall {
      let small = self._smallUTF8String[bounds]
      if small.isASCII {
        return try small.withUnmanagedASCII {
          (ascii: _UnmanagedString<Unicode.UTF8.CodeUnit>) throws -> Result in
          return try Swift._withCStringAndLength(
            encodedAs: targetEncoding,
            from: ascii.buffer,
            encodedAs: Unicode.UTF8.self,
            execute: body)
          }
      } else {
        fatalError("TODO: UTF-8 support in small strings")
      }
    }

    defer { _fixLifetime(self) }
    let opaque = _asOpaque()[bounds]
    return try Swift._withCStringAndLength(
      encodedAs: targetEncoding,
      from: opaque,
      encodedAs: Unicode.UTF16.self,
      execute: body)
  }
}

extension String {
  @inlinable
  internal static func _fromCodeUnits<
    Input: Collection,
    Encoding: Unicode.Encoding
  >(
    _ input: Input,
    encoding: Encoding.Type,
    repairIllFormedSequences: Bool
  ) -> String?
  where Input.Element == Encoding.CodeUnit {

    // TODO(SSO): small check

    // Determine how many UTF-16 code units we'll need
    let inputStream = input.makeIterator()
    guard let (utf16Count, isASCII) = UTF16.transcodedLength(
        of: inputStream,
        decodedAs: encoding,
        repairingIllFormedSequences: repairIllFormedSequences) else {
      return nil
    }

    let capacity = utf16Count
    if isASCII {
      if let small = _SmallUTF8String(
        _fromCodeUnits: input,
        utf16Length: utf16Count,
        isASCII: true,
        Encoding.self
      ) {
        return String(_StringGuts(small))
      }

      let storage = _SwiftStringStorage<UInt8>.create(
        capacity: capacity,
        count: utf16Count)
      var p = storage.start
      let sink: (UTF32.CodeUnit) -> Void = {
        p.pointee = UTF8.CodeUnit($0)
        p += 1
      }
      let hadError = transcode(
        input.makeIterator(),
        from: encoding, to: UTF32.self,
        stoppingOnError: true,
        into: sink)
      _sanityCheck(!hadError,
        "string cannot be ASCII if there were decoding errors")
      return String(_largeStorage: storage)
    } else {
      // TODO(SSO): Small transcoded string

      let storage = _SwiftStringStorage<UTF16.CodeUnit>.create(
        capacity: capacity,
        count: utf16Count)
      var p = storage.start
      let sink: (UTF16.CodeUnit) -> Void = {
        p.pointee = $0
        p += 1
      }
      _ = transcode(
        input.makeIterator(),
        from: encoding, to: UTF16.self,
        stoppingOnError: !repairIllFormedSequences,
        into: sink)
      return String(_largeStorage: storage)
    }
  }

  internal static func _fromNonASCIIUTF8(
    _ input: UnsafeBufferPointer<UInt8>, repair: Bool
  ) -> String? {
    if let smol = _SmallUTF8String(input) {
      return String(_StringGuts(smol))
    }

    // Determine how many UTF-16 code units we'll need
    let inputStream = input.makeIterator()

    // TODO: Replace with much, much faster length check
    guard let (utf16Count, isASCII) = UTF16.transcodedLength(
        of: inputStream,
        decodedAs: UTF8.self,
        repairingIllFormedSequences: repair) else {
      return nil
    }

    let capacity = utf16Count
    _sanityCheck(!isASCII, "was given ASCII UTF-8")
    let storage = _SwiftStringStorage<UTF16.CodeUnit>.create(
      capacity: capacity,
      count: utf16Count)
    var p = storage.start
    let sink: (UTF16.CodeUnit) -> Void = {
      p.pointee = $0
      p += 1
    }
    // TODO: Replace with much, much faster transcoding
    _ = transcode(
      input.makeIterator(),
      from: UTF8.self, to: UTF16.self,
      stoppingOnError: !repair,
      into: sink)
    return String(_largeStorage: storage)
  }

  /// Creates a string from the given Unicode code units in the specified
  /// encoding.
  ///
  /// - Parameters:
  ///   - codeUnits: A collection of code units encoded in the encoding
  ///     specified in `sourceEncoding`.
  ///   - sourceEncoding: The encoding in which `codeUnits` should be
  ///     interpreted.
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always) // Eliminate dynamic type check when possible
  public init<C: Collection, Encoding: Unicode.Encoding>(
    decoding codeUnits: C, as sourceEncoding: Encoding.Type
  ) where C.Iterator.Element == Encoding.CodeUnit {
    if let contigBytes = codeUnits as? _HasContiguousBytes,
       sourceEncoding == UTF8.self
    {
      self = contigBytes.withUnsafeBytes { rawBufPtr in
        let ptr = rawBufPtr.baseAddress._unsafelyUnwrappedUnchecked
        return String._fromUTF8(
          UnsafeBufferPointer(
            start: ptr.assumingMemoryBound(to: UInt8.self),
            count: rawBufPtr.count),
          repair: true).unsafelyUnwrapped
      }
      return
    }

    self = String._fromCodeUnits(
      codeUnits, encoding: sourceEncoding, repairIllFormedSequences: true)!
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
  @inlinable // FIXME(sil-serialize-all)
  public init<Encoding: Unicode.Encoding>(
    decodingCString nullTerminatedCodeUnits: UnsafePointer<Encoding.CodeUnit>,
    as sourceEncoding: Encoding.Type) {

    self = String.decodeCString(
      nullTerminatedCodeUnits, as: sourceEncoding)!.result
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
  @inlinable // FIXME(sil-serialize-all)
  public func withCString<Result, TargetEncoding: Unicode.Encoding>(
    encodedAs targetEncoding: TargetEncoding.Type,
    _ body: (UnsafePointer<TargetEncoding.CodeUnit>) throws -> Result
  ) rethrows -> Result {
    return try _guts._withCSubstring(
      in: 0..<_guts.count,
      encoding: TargetEncoding.self,
      body)
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
  public var _guts: _StringGuts

  /// Creates an empty string.
  ///
  /// Using this initializer is equivalent to initializing a string with an
  /// empty string literal.
  ///
  ///     let empty = ""
  ///     let alsoEmpty = String()
  @inlinable // FIXME(sil-serialize-all)
  public init() {
    self._guts = _StringGuts()
  }

  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  init(_ _guts: _StringGuts) {
    self._guts = _guts
  }
}

extension String {
  public func _dump() { // FIXME: remove
    self._guts._dump()
  }
}

internal func _isAllASCII(_ input: UnsafeBufferPointer<UInt8>) -> Bool {
  for byte in input {
    guard byte <= 0x7F else { return false }
  }
  return true
}

// TODO: re-organize a bit before merging...

@usableFromInline
internal protocol _HasContiguousBytes {
  func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R
}
extension Array: _HasContiguousBytes {}
extension UnsafeBufferPointer: _HasContiguousBytes {
  @inlinable
  @inline(__always)
  func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    let ptr = UnsafeRawPointer(self.baseAddress._unsafelyUnwrappedUnchecked)
    let len = self.count &* MemoryLayout<Element>.stride
    return try body(UnsafeRawBufferPointer(start: ptr, count: len))
  }
}
extension UnsafeMutableBufferPointer: _HasContiguousBytes {
  @inlinable
  @inline(__always)
  func withUnsafeBytes<R>(
    _ body: (UnsafeRawBufferPointer) throws -> R
  ) rethrows -> R {
    let ptr = UnsafeRawPointer(self.baseAddress._unsafelyUnwrappedUnchecked)
    let len = self.count &* MemoryLayout<Element>.stride
    return try body(UnsafeRawBufferPointer(start: ptr, count: len))
  }
}

extension String {
  @usableFromInline
  static func _fromUTF8(
    _ input: UnsafeBufferPointer<UInt8>, repair: Bool
  ) -> String? {
    if _isAllASCII(input) {
      return _fromASCII(input)
    }
    return _fromNonASCIIUTF8(input, repair: repair)
  }

  @usableFromInline
  static func _fromASCII(_ input: UnsafeBufferPointer<UInt8>) -> String {
    if let smol = _SmallUTF8String(input) {
      return String(_StringGuts(smol))
    }
    let storage = _SwiftStringStorage<UInt8>.create(
      capacity: input.count, count: input.count)
    _sanityCheck(storage.count == input.count)
    storage.start.initialize(
      from: input.baseAddress._unsafelyUnwrappedUnchecked, count: input.count)
    return String(_StringGuts(_large: storage))
  }

  @usableFromInline
  static func _fromWellFormedUTF8(
    _ input: UnsafeBufferPointer<UInt8>, repair: Bool = false
  ) -> String {
    return String._fromUTF8(input, repair: repair)!
  }

  @inlinable
  @usableFromInline
  static func _fromWellFormedUTF16CodeUnits<C : RandomAccessCollection>(
    _ input: C, repair: Bool = false
  ) -> String where C.Element == UTF16.CodeUnit {
    if let smol = _SmallUTF8String(input) {
      return String(_StringGuts(smol))
    }
    return String._fromCodeUnits(
      input, encoding: UTF16.self, repairIllFormedSequences: repair)!
  }
}

extension String : _ExpressibleByBuiltinUnicodeScalarLiteral {
  @inlinable // FIXME(sil-serialize-all)
  @_effects(readonly)
  public // @testable
  init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self.init(Unicode.Scalar(_value: UInt32(value)))
  }
  @inlinable // FIXME(sil-serialize-all)
  public init(_ scalar: Unicode.Scalar) {
    // Until we have UTF-8 support in small string, need to be large
    //
    // TODO: All scalars are small
    if scalar.value <= 0x7f {
      if let small = _SmallUTF8String(scalar) {
        self = String(_StringGuts(small))
        return
      } else {
#if arch(i386) || arch(arm)
#else
      _sanityCheckFailure("Couldn't fit ASCII scalar into small string?")
#endif
      }
    }
    self = String._fromCodeUnits(
      CollectionOfOne(scalar.value),
      encoding: UTF32.self,
      repairIllFormedSequences: false
    )._unsafelyUnwrappedUnchecked
  }
}

extension String : _ExpressibleByBuiltinExtendedGraphemeClusterLiteral {
  @inlinable
  @_effects(readonly)
  @_semantics("string.makeUTF8")
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

extension String : _ExpressibleByBuiltinUTF16StringLiteral {
  @inlinable
  @_effects(readonly)
  @_semantics("string.makeUTF16")
  public init(
    _builtinUTF16StringLiteral start: Builtin.RawPointer,
    utf16CodeUnitCount: Builtin.Word
  ) {
    let bufPtr = UnsafeBufferPointer(
      start: UnsafeRawPointer(start).assumingMemoryBound(to: UInt16.self),
      count: Int(utf16CodeUnitCount))
    if let small = _SmallUTF8String(bufPtr) {
      self = String(_StringGuts(small))
      return
    }

    self = String(_StringGuts(_large: _UnmanagedString(bufPtr)))
  }
}

extension String : _ExpressibleByBuiltinStringLiteral {
  @inline(__always)
  @inlinable
  @_effects(readonly)
  @_semantics("string.makeUTF8")
  public init(
    _builtinStringLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
  ) {
    let bufPtr = UnsafeBufferPointer(
      start: UnsafeRawPointer(start).assumingMemoryBound(to: UInt8.self),
      count: Int(utf8CodeUnitCount))
    if bufPtr.isEmpty {
      self.init()
      return
    }

    if let small = _SmallUTF8String(bufPtr) {
      self = String(_StringGuts(small))
      return
    }
    if _fastPath(Bool(isASCII)) {
      self = String(_StringGuts(_large: _UnmanagedString(bufPtr)))
      return
    }
    self = String._fromWellFormedUTF8(bufPtr)
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
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
  internal func _encodedLength<
    Encoding: Unicode.Encoding
  >(_ encoding: Encoding.Type) -> Int {
    var codeUnitCount = 0
    self._encode(encoding, into: { _ in codeUnitCount += 1 })
    return codeUnitCount
  }

  //
  // TODO (TODO: JIRA): This needs to be completely rewritten. It's about 12KB
  // of code, most of which are MOV instructions. Keeping the by-hand opaque
  // visitation pattern for now.
  //

  // FIXME: this function may not handle the case when a wrapped NSString
  // contains unpaired surrogates.  Fix this before exposing this function as a
  // public API.  But it is unclear if it is valid to have such an NSString in
  // the first place.  If it is not, we should not be crashing in an obscure
  // way -- add a test for that.
  // Related: <rdar://problem/17340917> Please document how NSString interacts
  // with unpaired surrogates
  @inlinable // FIXME(sil-serialize-all)
  internal func _encode<Encoding: Unicode.Encoding>(
    _ encoding: Encoding.Type,
    into processCodeUnit: (Encoding.CodeUnit) -> Void
  ) {
    if _slowPath(_guts._isOpaque) {
      _opaqueEncode(encoding, into: processCodeUnit)
      return
    }

    defer { _fixLifetime(self) }
    if _guts.isASCII {
      let ascii = _guts._unmanagedASCIIView
      if encoding == Unicode.ASCII.self
      || encoding == Unicode.UTF8.self
      || encoding == Unicode.UTF16.self
      || encoding == Unicode.UTF32.self {
        ascii.forEach {
          processCodeUnit(Encoding.CodeUnit(truncatingIfNeeded: $0))
        }
      } else {
        // TODO: be sure tests exercise this code path.
        for b in ascii {
          Encoding._encode(
            Unicode.Scalar(_unchecked: UInt32(b))).forEach(processCodeUnit)
        }
      }
      return
    }
    let utf16 = _guts._unmanagedUTF16View
    var i = utf16.makeIterator()
    Unicode.UTF16.ForwardParser._parse(&i) {
      Encoding._transcode($0, from: UTF16.self).forEach(processCodeUnit)
    }
  }

  @usableFromInline // @opaque
  internal func _opaqueEncode<Encoding: Unicode.Encoding>(
    _ encoding: Encoding.Type,
    into processCodeUnit: (Encoding.CodeUnit) -> Void
  ) {
    // TODO: ASCII fast path, and probably adjust this interface too.
    if _guts._isSmall {
      _guts._smallUTF8String.withUnmanagedUTF16 { utf16 in
        var i = utf16.makeIterator()
        Unicode.UTF16.ForwardParser._parse(&i) {
          Encoding._transcode($0, from: UTF16.self).forEach(processCodeUnit)
        }
      }
      return
    }

    _sanityCheck(_guts._isOpaque)
    defer { _fixLifetime(self) }
    let opaque = _guts._asOpaque()
    var i = opaque.makeIterator()
    Unicode.UTF16.ForwardParser._parse(&i) {
      Encoding._transcode($0, from: UTF16.self).forEach(processCodeUnit)
    }
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
    self._guts.append(other._guts)
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

  // TODO(SSO): Consider small-checking version
  @inlinable // FIXME(sil-serialize-all)
  init<CodeUnit>(_largeStorage storage: _SwiftStringStorage<CodeUnit>)
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
    _guts = _StringGuts(_large: storage)
  }
}

extension String {
  @inlinable // FIXME(sil-serialize-all)
  @_effects(readonly)
  @_semantics("string.concat")
  public static func + (lhs: String, rhs: String) -> String {
    var lhs = lhs
    lhs.append(rhs)
    return lhs
  }

  // String append
  @inlinable // FIXME(sil-serialize-all)
  public static func += (lhs: inout String, rhs: String) {
    lhs.append(rhs)
  }
}

extension String {
  /// Constructs a `String` in `resultStorage` containing the given UTF-8.
  ///
  /// Low-level construction interface used by introspection
  /// implementation in the runtime library.
  @inlinable
  @_silgen_name("swift_stringFromUTF8InRawMemory")
  public // COMPILER_INTRINSIC
  static func _fromUTF8InRawMemory(
    _ resultStorage: UnsafeMutablePointer<String>,
    start: UnsafeMutablePointer<UTF8.CodeUnit>,
    utf8CodeUnitCount: Int
  ) {
    resultStorage.initialize(to:
      String._fromWellFormedUTF8(
        UnsafeBufferPointer(start: start, count: utf8CodeUnitCount)))
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

  internal func _joined(separator: String = "") -> String {
    let separatorSize = separator._guts.count
    var width = separator._guts.byteWidth

    let reservation = self._preprocessingPass {
      () -> Int in
      var r = 0
      for chunk in self {
        r += separatorSize + chunk._encodedOffsetRange.count
        width = Swift.max(width, chunk._wholeString._guts.byteWidth)
      }
      return r > 0 ? r - separatorSize : 0
    }

    let capacity = reservation ?? separatorSize
    var result = ""
    result.reserveCapacity(capacity)
    if separator.isEmpty {
      for x in self {
        result._guts.append(x)
      }
      return result
    }

    var iter = makeIterator()
    if let first = iter.next() {
      result._guts.append(first)
      while let next = iter.next() {
        result.append(separator)
        result._guts.append(next)
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
  @_specialize(where Self == Array<String>)
  public func joined(separator: String = "") -> String {
    return _joined(separator: separator)
  }
}

#if _runtime(_ObjC)
@usableFromInline // FIXME(sil-serialize-all)
@_silgen_name("swift_stdlib_NSStringLowercaseString")
internal func _stdlib_NSStringLowercaseString(_ str: AnyObject) -> _CocoaString

@usableFromInline // FIXME(sil-serialize-all)
@_silgen_name("swift_stdlib_NSStringUppercaseString")
internal func _stdlib_NSStringUppercaseString(_ str: AnyObject) -> _CocoaString
#else
internal func _nativeUnicodeLowercaseString(_ str: String) -> String {

  // TODO (TODO: JIRA): check for small

  let guts = str._guts._extractContiguousUTF16()
  defer { _fixLifetime(guts) }
  let utf16 = guts._unmanagedUTF16View
  var storage = _SwiftStringStorage<UTF16.CodeUnit>.create(
    capacity: utf16.count,
    count: utf16.count)

  // Try to write it out to the same length.
  let z = _swift_stdlib_unicode_strToLower(
    storage.start, Int32(storage.capacity), // FIXME: handle overflow case
    utf16.start, Int32(utf16.count))
  let correctSize = Int(z)

  // If more space is needed, do it again with the correct buffer size.
  if correctSize > storage.capacity {
    storage = _SwiftStringStorage<UTF16.CodeUnit>.create(
      capacity: correctSize,
      count: correctSize)
    _swift_stdlib_unicode_strToLower(
      storage.start, Int32(storage.capacity), // FIXME: handle overflow case
      utf16.start, Int32(utf16.count))
  }
  storage.count = correctSize
  return String(_largeStorage: storage)
}

@usableFromInline // FIXME(sil-serialize-all)
internal func _nativeUnicodeUppercaseString(_ str: String) -> String {

  // TODO (TODO: JIRA): check for small

  let guts = str._guts._extractContiguousUTF16()
  defer { _fixLifetime(guts) }
  let utf16 = guts._unmanagedUTF16View
  var storage = _SwiftStringStorage<UTF16.CodeUnit>.create(
    capacity: utf16.count,
    count: utf16.count)

  // Try to write it out to the same length.
  let z = _swift_stdlib_unicode_strToUpper(
    storage.start, Int32(storage.capacity), // FIXME: handle overflow case
    utf16.start, Int32(utf16.count))
  let correctSize = Int(z)

  // If more space is needed, do it again with the correct buffer size.
  if correctSize > storage.capacity {
    storage = _SwiftStringStorage<UTF16.CodeUnit>.create(
      capacity: correctSize,
      count: correctSize)
    _swift_stdlib_unicode_strToUpper(
      storage.start, Int32(storage.capacity), // FIXME: handle overflow case
      utf16.start, Int32(utf16.count))
  }
  storage.count = correctSize
  return String(_largeStorage: storage)
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
  @inlinable // FIXME(sil-serialize-all)
  internal var _asciiLowerCaseTable: UInt64 {
    @inline(__always)
    get {
      return 0b0001_1111_1111_1111_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
    }
  }

  /// The same table for upper case characters.
  @inlinable // FIXME(sil-serialize-all)
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
  ///     let cafe = "BBQ Café 🍵"
  ///     print(cafe.lowercased())
  ///     // Prints "bbq café 🍵"
  ///
  /// - Returns: A lowercase copy of the string.
  ///
  /// - Complexity: O(*n*)
  public func lowercased() -> String {
    if _guts.isASCII {
      var guts = _guts
      guts.withMutableASCIIStorage(unusedCapacity: 0) { storage in
        for i in 0..<storage._value.count {
          // For each character in the string, we lookup if it should be shifted
          // in our ascii table, then we return 0x20 if it should, 0x0 if not.
          // This code is equivalent to:
          // switch source[i] {
          // case let x where (x >= 0x41 && x <= 0x5a):
          //   dest[i] = x &+ 0x20
          // case let x:
          //   dest[i] = x
          // }
          let value = storage._value.start[i]
          let isUpper =
            _asciiUpperCaseTable &>>
            UInt64(((value &- 1) & 0b0111_1111) &>> 1)
          let add = (isUpper & 0x1) &<< 5
          // Since we are left with either 0x0 or 0x20, we can safely truncate
          // to a UInt8 and add to our ASCII value (this will not overflow
          // numbers in the ASCII range).
          storage._value.start[i] = value &+ UInt8(truncatingIfNeeded: add)
        }
      }
      return String(guts)
    }

#if _runtime(_ObjC)
    return String(_cocoaString:
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
    if _guts.isASCII {
      var guts = _guts
      guts.withMutableASCIIStorage(unusedCapacity: 0) { storage in
        for i in 0..<storage._value.count {
          // See the comment above in lowercased.
          let value = storage._value.start[i]
          let isLower =
            _asciiLowerCaseTable &>>
            UInt64(((value &- 1) & 0b0111_1111) &>> 1)
          let add = (isLower & 0x1) &<< 5
          storage._value.start[i] = value &- UInt8(truncatingIfNeeded: add)
        }
      }
      return String(guts)
    }

#if _runtime(_ObjC)
    return String(_cocoaString:
      _stdlib_NSStringUppercaseString(self._bridgeToObjectiveCImpl()))
#else
    return _nativeUnicodeUppercaseString(self)
#endif
  }

  /// Creates an instance from the description of a given
  /// `LosslessStringConvertible` instance.
  @inlinable // FIXME(sil-serialize-all)
  public init<T : LosslessStringConvertible>(_ value: T) {
    self = value.description
  }
}

extension String : CustomStringConvertible {
  /// The value of this string.
  ///
  /// Using this property directly is discouraged. Instead, use simple
  /// assignment to create a new constant or variable equal to this string.
  @inlinable // FIXME(sil-serialize-all)
  public var description: String {
    return self
  }
}

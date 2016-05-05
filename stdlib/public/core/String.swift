//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

// FIXME: complexity documentation for most of methods on String is ought to be
// qualified with "amortized" at least, as Characters are variable-length.

/// An arbitrary Unicode string value.
///
/// Unicode-Correct
/// ===============
///
/// Swift strings are designed to be Unicode-correct.  In particular,
/// the APIs make it easy to write code that works correctly, and does
/// not surprise end-users, regardless of where you venture in the
/// Unicode character space.  For example, the `==` operator checks
/// for [Unicode canonical
/// equivalence](http://www.unicode.org/glossary/#deterministic_comparison),
/// so two different representations of the same string will always
/// compare equal.
///
/// Locale-Insensitive
/// ==================
///
/// The fundamental operations on Swift strings are not sensitive to
/// locale settings.  That's because, for example, the validity of a
/// `Dictionary<String, T>` in a running program depends on a given
/// string comparison having a single, stable result.  Therefore,
/// Swift always uses the default,
/// un-[tailored](http://www.unicode.org/glossary/#tailorable) Unicode
/// algorithms for basic string operations.
///
/// Importing `Foundation` endows swift strings with the full power of
/// the `NSString` API, which allows you to choose more complex
/// locale-sensitive operations explicitly.
///
/// Value Semantics
/// ===============
///
/// Each string variable, `let` binding, or stored property has an
/// independent value, so mutations to the string are not observable
/// through its copies:
///
///     var a = "foo"
///     var b = a
///     b.append("bar")
///     print("a=\(a), b=\(b)")     // a=foo, b=foobar
///
/// Strings use Copy-on-Write so that their data is only copied
/// lazily, upon mutation, when more than one string instance is using
/// the same buffer.  Therefore, the first in any sequence of mutating
/// operations may cost `O(N)` time and space, where `N` is the length
/// of the string's (unspecified) underlying representation.
///
/// Views
/// =====
///
/// `String` is not itself a collection of anything.  Instead, it has
/// properties that present the string's contents as meaningful
/// collections:
///
///   - `characters`: a collection of `Character` ([extended grapheme
///     cluster](http://www.unicode.org/glossary/#extended_grapheme_cluster))
///     elements, a unit of text that is meaningful to most humans.
///
///   - `unicodeScalars`: a collection of `UnicodeScalar` ([Unicode
///     scalar
///     values](http://www.unicode.org/glossary/#unicode_scalar_value))
///     the 21-bit codes that are the basic unit of Unicode.  These
///     values are equivalent to UTF-32 code units.
///
///   - `utf16`: a collection of `UTF16.CodeUnit`, the 16-bit
///     elements of the string's UTF-16 encoding.
///
///   - `utf8`: a collection of `UTF8.CodeUnit`, the 8-bit
///     elements of the string's UTF-8 encoding.
///
/// Growth and Capacity
/// ===================
///
/// When a string's contiguous storage fills up, new storage must be
/// allocated and characters must be moved to the new storage.
/// `String` uses an exponential growth strategy that makes `append` a
/// constant time operation *when amortized over many invocations*.
///
/// Objective-C Bridge
/// ==================
///
/// `String` is bridged to Objective-C as `NSString`, and a `String`
/// that originated in Objective-C may store its characters in an
/// `NSString`.  Since any arbitrary subclass of `NSString` can
/// become a `String`, there are no guarantees about representation or
/// efficiency in this case.  Since `NSString` is immutable, it is
/// just as though the storage was shared by some copy: the first in
/// any sequence of mutating operations causes elements to be copied
/// into unique, contiguous storage which may cost `O(N)` time and
/// space, where `N` is the length of the string representation (or
/// more, if the underlying `NSString` has unusual performance
/// characteristics).
@_fixed_layout
public struct String {
  /// An empty `String`.
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
  @warn_unused_result
  public // @testable
  static func _fromWellFormedCodeUnitSequence<
    Encoding: UnicodeCodec, Input: Collection
    where Input.Iterator.Element == Encoding.CodeUnit
  >(
    _ encoding: Encoding.Type, input: Input
  ) -> String {
    return String._fromCodeUnitSequence(encoding, input: input)!
  }

  @warn_unused_result
  public // @testable
  static func _fromCodeUnitSequence<
    Encoding: UnicodeCodec, Input: Collection
    where Input.Iterator.Element == Encoding.CodeUnit
  >(
    _ encoding: Encoding.Type, input: Input
  ) -> String? {
    let (stringBufferOptional, _) =
        _StringBuffer.fromCodeUnits(input, encoding: encoding,
            repairIllFormedSequences: false)
    if let stringBuffer = stringBufferOptional {
      return String(_storage: stringBuffer)
    } else {
      return nil
    }
  }

  @warn_unused_result
  public // @testable
  static func _fromCodeUnitSequenceWithRepair<
    Encoding: UnicodeCodec, Input: Collection
    where Input.Iterator.Element == Encoding.CodeUnit
  >(
    _ encoding: Encoding.Type, input: Input
  ) -> (String, hadError: Bool) {
    let (stringBuffer, hadError) =
        _StringBuffer.fromCodeUnits(input, encoding: encoding,
            repairIllFormedSequences: true)
    return (String(_storage: stringBuffer!), hadError)
  }
}

extension String : _BuiltinUnicodeScalarLiteralConvertible {
  @effects(readonly)
  public // @testable
  init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self = String._fromWellFormedCodeUnitSequence(
      UTF32.self, input: CollectionOfOne(UInt32(value)))
  }
}

extension String : UnicodeScalarLiteralConvertible {
  /// Create an instance initialized to `value`.
  public init(unicodeScalarLiteral value: String) {
    self = value
  }
}

extension String : _BuiltinExtendedGraphemeClusterLiteralConvertible {
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

extension String : ExtendedGraphemeClusterLiteralConvertible {
  /// Create an instance initialized to `value`.
  public init(extendedGraphemeClusterLiteral value: String) {
    self = value
  }
}

extension String : _BuiltinUTF16StringLiteralConvertible {
  @effects(readonly)
  @_semantics("string.makeUTF16")
  public init(
    _builtinUTF16StringLiteral start: Builtin.RawPointer,
    utf16CodeUnitCount: Builtin.Word
  ) {
    self = String(
      _StringCore(
        baseAddress: OpaquePointer(start),
        count: Int(utf16CodeUnitCount),
        elementShift: 1,
        hasCocoaBuffer: false,
        owner: nil))
  }
}

extension String : _BuiltinStringLiteralConvertible {
  @effects(readonly)
  @_semantics("string.makeUTF8")
  public init(
    _builtinStringLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1) {
    if Bool(isASCII) {
      self = String(
        _StringCore(
          baseAddress: OpaquePointer(start),
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

extension String : StringLiteralConvertible {
  /// Create an instance initialized to `value`.
  public init(stringLiteral value: String) {
     self = value
  }
}

extension String : CustomDebugStringConvertible {
  /// A textual representation of `self`, suitable for debugging.
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
  @warn_unused_result
  func _encodedLength<
    Encoding: UnicodeCodec
  >(_ encoding: Encoding.Type) -> Int {
    var codeUnitCount = 0
    let output: (Encoding.CodeUnit) -> Void = { _ in codeUnitCount += 1 }
    self._encode(encoding, output: output)
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
  >(_ encoding: Encoding.Type, output: @noescape (Encoding.CodeUnit) -> Void)
  {
    return _core.encode(encoding, output: output)
  }
}

#if _runtime(_ObjC)
/// Compare two strings using the Unicode collation algorithm in the
/// deterministic comparison mode.  (The strings which are equivalent according
/// to their NFD form are considered equal.  Strings which are equivalent
/// according to the plain Unicode collation algorithm are additionally ordered
/// based on their NFD.)
///
/// See Unicode Technical Standard #10.
///
/// The behavior is equivalent to `NSString.compare()` with default options.
///
/// - returns:
///   * an unspecified value less than zero if `lhs < rhs`,
///   * zero if `lhs == rhs`,
///   * an unspecified value greater than zero if `lhs > rhs`.
@_silgen_name("swift_stdlib_compareNSStringDeterministicUnicodeCollation")
public func _stdlib_compareNSStringDeterministicUnicodeCollation(
  _ lhs: AnyObject, _ rhs: AnyObject
) -> Int32

@_silgen_name("swift_stdlib_compareNSStringDeterministicUnicodeCollationPtr")
public func _stdlib_compareNSStringDeterministicUnicodeCollationPointer(
  _ lhs: OpaquePointer, _ rhs: OpaquePointer
) -> Int32
#endif

extension String : Equatable {
}

@warn_unused_result
public func ==(lhs: String, rhs: String) -> Bool {
  if lhs._core.isASCII && rhs._core.isASCII {
    if lhs._core.count != rhs._core.count {
      return false
    }
    return _swift_stdlib_memcmp(
      lhs._core.startASCII, rhs._core.startASCII,
      rhs._core.count) == 0
  }
  return lhs._compareString(rhs) == 0
}

extension String : Comparable {
}

extension String {
#if _runtime(_ObjC)
  /// This is consistent with Foundation, but incorrect as defined by Unicode.
  /// Unicode weights some ASCII punctuation in a different order than ASCII
  /// value. Such as:
  ///
  ///   0022  ; [*02FF.0020.0002] # QUOTATION MARK
  ///   0023  ; [*038B.0020.0002] # NUMBER SIGN
  ///   0025  ; [*038C.0020.0002] # PERCENT SIGN
  ///   0026  ; [*0389.0020.0002] # AMPERSAND
  ///   0027  ; [*02F8.0020.0002] # APOSTROPHE
  ///
  /// - Precondition: Both `self` and `rhs` are ASCII strings.
  @warn_unused_result
  public // @testable
  func _compareASCII(_ rhs: String) -> Int {
    var compare = Int(_swift_stdlib_memcmp(
      self._core.startASCII, rhs._core.startASCII,
      min(self._core.count, rhs._core.count)))
    if compare == 0 {
      compare = self._core.count - rhs._core.count
    }
    // This efficiently normalizes the result to -1, 0, or 1 to match the
    // behavior of NSString's compare function.
    return (compare > 0 ? 1 : 0) - (compare < 0 ? 1 : 0)
  }
#endif

  /// Compares two strings with the Unicode Collation Algorithm.
  @warn_unused_result
  @inline(never)
  @_semantics("stdlib_binary_only") // Hide the CF/ICU dependency
  public  // @testable
  func _compareDeterministicUnicodeCollation(_ rhs: String) -> Int {
    // Note: this operation should be consistent with equality comparison of
    // Character.
#if _runtime(_ObjC)
    if self._core.hasContiguousStorage && rhs._core.hasContiguousStorage {
      let lhsStr = _NSContiguousString(self._core)
      let rhsStr = _NSContiguousString(rhs._core)
      let res = lhsStr._unsafeWithNotEscapedSelfPointerPair(rhsStr) {
        return Int(
            _stdlib_compareNSStringDeterministicUnicodeCollationPointer($0, $1))
      }
      return res
    }
    return Int(_stdlib_compareNSStringDeterministicUnicodeCollation(
      _bridgeToObjectiveCImpl(), rhs._bridgeToObjectiveCImpl()))
#else
    switch (_core.isASCII, rhs._core.isASCII) {
    case (true, false):
      let lhsPtr = UnsafePointer<Int8>(_core.startASCII)
      let rhsPtr = UnsafePointer<UTF16.CodeUnit>(rhs._core.startUTF16)

      return Int(_swift_stdlib_unicode_compare_utf8_utf16(
        lhsPtr, Int32(_core.count), rhsPtr, Int32(rhs._core.count)))
    case (false, true):
      // Just invert it and recurse for this case.
      return -rhs._compareDeterministicUnicodeCollation(self)
    case (false, false):
      let lhsPtr = UnsafePointer<UTF16.CodeUnit>(_core.startUTF16)
      let rhsPtr = UnsafePointer<UTF16.CodeUnit>(rhs._core.startUTF16)

      return Int(_swift_stdlib_unicode_compare_utf16_utf16(
        lhsPtr, Int32(_core.count),
        rhsPtr, Int32(rhs._core.count)))
    case (true, true):
      let lhsPtr = UnsafePointer<Int8>(_core.startASCII)
      let rhsPtr = UnsafePointer<Int8>(rhs._core.startASCII)

      return Int(_swift_stdlib_unicode_compare_utf8_utf8(
        lhsPtr, Int32(_core.count),
        rhsPtr, Int32(rhs._core.count)))
    }
#endif
  }

  @warn_unused_result
  public  // @testable
  func _compareString(_ rhs: String) -> Int {
#if _runtime(_ObjC)
    // We only want to perform this optimization on objc runtimes. Elsewhere,
    // we will make it follow the unicode collation algorithm even for ASCII.
    if (_core.isASCII && rhs._core.isASCII) {
      return _compareASCII(rhs)
    }
#endif
    return _compareDeterministicUnicodeCollation(rhs)
  }
}

@warn_unused_result
public func <(lhs: String, rhs: String) -> Bool {
  return lhs._compareString(rhs) < 0
}

// Support for copy-on-write
extension String {

  /// Append the elements of `other` to `self`.
  public mutating func append(_ other: String) {
    _core.append(other._core)
  }

  /// Append `x` to `self`.
  ///
  /// - Complexity: Amortized O(1).
  public mutating func append(_ x: UnicodeScalar) {
    _core.append(x)
  }

  public // SPI(Foundation)
  init(_storage: _StringBuffer) {
    _core = _StringCore(_storage)
  }
}

#if _runtime(_ObjC)
@warn_unused_result
@_silgen_name("swift_stdlib_NSStringHashValue")
func _stdlib_NSStringHashValue(_ str: AnyObject, _ isASCII: Bool) -> Int

@warn_unused_result
@_silgen_name("swift_stdlib_NSStringHashValuePointer")
func _stdlib_NSStringHashValuePointer(_ str: OpaquePointer, _ isASCII: Bool) -> Int
#endif

extension String : Hashable {
  /// The hash value.
  ///
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
  ///
  /// - Note: The hash value is not guaranteed to be stable across
  ///   different invocations of the same program.  Do not persist the
  ///   hash value across program runs.
  public var hashValue: Int {
#if _runtime(_ObjC)
    // Mix random bits into NSString's hash so that clients don't rely on
    // Swift.String.hashValue and NSString.hash being the same.
#if arch(i386) || arch(arm)
    let hashOffset = Int(bitPattern: 0x88dd_cc21)
#else
    let hashOffset = Int(bitPattern: 0x429b_1266_88dd_cc21)
#endif
    // If we have a contiguous string then we can use the stack optimization.
    let core = self._core
    let isASCII = core.isASCII
    if core.hasContiguousStorage {
      let stackAllocated = _NSContiguousString(core)
      return hashOffset ^ stackAllocated._unsafeWithNotEscapedSelfPointer {
        return _stdlib_NSStringHashValuePointer($0, isASCII)
      }
    } else {
      let cocoaString = unsafeBitCast(
        self._bridgeToObjectiveCImpl(), to: _NSStringCore.self)
      return hashOffset ^ _stdlib_NSStringHashValue(cocoaString, isASCII)
    }
#else
    if self._core.isASCII {
      return _swift_stdlib_unicode_hash_ascii(
        UnsafeMutablePointer<Int8>(_core.startASCII),
        Int32(_core.count))
    } else {
      return _swift_stdlib_unicode_hash(
        UnsafeMutablePointer<UInt16>(_core.startUTF16),
        Int32(_core.count))
    }
#endif
  }
}

@warn_unused_result
@effects(readonly)
@_semantics("string.concat")
public func + (lhs: String, rhs: String) -> String {
  var lhs = lhs
  if (lhs.isEmpty) {
    return rhs
  }
  lhs._core.append(rhs._core)
  return lhs
}

// String append
public func += (lhs: inout String, rhs: String) {
  if lhs.isEmpty {
    lhs = rhs
  }
  else {
    lhs._core.append(rhs._core)
  }
}

extension String {
  /// Constructs a `String` in `resultStorage` containing the given UTF-8.
  ///
  /// Low-level construction interface used by introspection
  /// implementation in the runtime library.
  @_silgen_name("swift_stringFromUTF8InRawMemory")
  public // COMPILER_INTRINSIC
  static func _fromUTF8InRawMemory(
    _ resultStorage: UnsafeMutablePointer<String>,
    start: UnsafeMutablePointer<UTF8.CodeUnit>,
    utf8CodeUnitCount: Int
  ) {
    resultStorage.initialize(with: 
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
  ///   in this sequence.
  /// - Returns: A single, concatenated string.
  @warn_unused_result
  public func joined(separator: String) -> String {
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
        r += separatorSize + chunk.utf16.count
      }
      return r - separatorSize
    }

    if let n = reservation {
      result.reserveCapacity(n)
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
@warn_unused_result
@_silgen_name("swift_stdlib_NSStringLowercaseString")
func _stdlib_NSStringLowercaseString(_ str: AnyObject) -> _CocoaString

@warn_unused_result
@_silgen_name("swift_stdlib_NSStringUppercaseString")
func _stdlib_NSStringUppercaseString(_ str: AnyObject) -> _CocoaString
#else
@warn_unused_result
internal func _nativeUnicodeLowercaseString(_ str: String) -> String {
  var buffer = _StringBuffer(
    capacity: str._core.count, initialSize: str._core.count, elementWidth: 2)

  // Try to write it out to the same length.
  let dest = UnsafeMutablePointer<UTF16.CodeUnit>(buffer.start)
  let z = _swift_stdlib_unicode_strToLower(
    dest, Int32(str._core.count),
    str._core.startUTF16, Int32(str._core.count))
  let correctSize = Int(z)

  // If more space is needed, do it again with the correct buffer size.
  if correctSize != str._core.count {
    buffer = _StringBuffer(
      capacity: correctSize, initialSize: correctSize, elementWidth: 2)
    let dest = UnsafeMutablePointer<UTF16.CodeUnit>(buffer.start)
    _swift_stdlib_unicode_strToLower(
      dest, Int32(correctSize), str._core.startUTF16, Int32(str._core.count))
  }

  return String(_storage: buffer)
}

@warn_unused_result
internal func _nativeUnicodeUppercaseString(_ str: String) -> String {
  var buffer = _StringBuffer(
    capacity: str._core.count, initialSize: str._core.count, elementWidth: 2)

  // Try to write it out to the same length.
  let dest = UnsafeMutablePointer<UTF16.CodeUnit>(buffer.start)
  let z = _swift_stdlib_unicode_strToUpper(
    dest, Int32(str._core.count),
    str._core.startUTF16, Int32(str._core.count))
  let correctSize = Int(z)

  // If more space is needed, do it again with the correct buffer size.
  if correctSize != str._core.count {
    buffer = _StringBuffer(
      capacity: correctSize, initialSize: correctSize, elementWidth: 2)
    let dest = UnsafeMutablePointer<UTF16.CodeUnit>(buffer.start)
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

  /// Return `self` converted to lower case.
  ///
  /// - Complexity: O(n)
  public func lowercased() -> String {
    if self._core.isASCII {
      let count = self._core.count
      let source = self._core.startASCII
      let buffer = _StringBuffer(
        capacity: count, initialSize: count, elementWidth: 1)
      let dest = UnsafeMutablePointer<UInt8>(buffer.start)
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
        dest[i] = value &+ UInt8(truncatingBitPattern: add)
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

  /// Return `self` converted to upper case.
  ///
  /// - Complexity: O(n)
  public func uppercased() -> String {
    if self._core.isASCII {
      let count = self._core.count
      let source = self._core.startASCII
      let buffer = _StringBuffer(
        capacity: count, initialSize: count, elementWidth: 1)
      let dest = UnsafeMutablePointer<UInt8>(buffer.start)
      for i in 0..<count {
        // See the comment above in lowercaseString.
        let value = source[i]
        let isLower =
          _asciiLowerCaseTable >>
          UInt64(((value &- 1) & 0b0111_1111) >> 1)
        let add = (isLower & 0x1) << 5
        dest[i] = value &- UInt8(truncatingBitPattern: add)
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
}

extension String {
  @available(*, unavailable, renamed: "append")
  public mutating func appendContentsOf(_ other: String) {
    Builtin.unreachable()
  }

  @available(*, unavailable, renamed: "append(contentsOf:)")
  public mutating func appendContentsOf<
    S : Sequence where S.Iterator.Element == Character
  >(_ newElements: S) {
    Builtin.unreachable()
  }

  @available(*, unavailable, renamed: "insert(contentsOf:at:)")
  public mutating func insertContentsOf<
    S : Collection where S.Iterator.Element == Character
  >(_ newElements: S, at i: Index) {
    Builtin.unreachable()
  }

  @available(*, unavailable, renamed: "replaceSubrange")
  public mutating func replaceRange<
    C : Collection where C.Iterator.Element == Character
  >(
    _ subRange: Range<Index>, with newElements: C
  ) {
    Builtin.unreachable()
  }
    
  @available(*, unavailable, renamed: "replaceSubrange")
  public mutating func replaceRange(
    _ subRange: Range<Index>, with newElements: String
  ) {
    Builtin.unreachable()
  }
  
  @available(*, unavailable, renamed: "removeAt")
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
}

extension Sequence where Iterator.Element == String {
  @available(*, unavailable, renamed: "joined")
  public func joinWithSeparator(_ separator: String) -> String {
    Builtin.unreachable()
  }
}

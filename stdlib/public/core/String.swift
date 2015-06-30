//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

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
///     b.extend("bar")
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
/// `NSString`.  Since any arbitrary subclass of `NSSString` can
/// become a `String`, there are no guarantees about representation or
/// efficiency in this case.  Since `NSString` is immutable, it is
/// just as though the storage was shared by some copy: the first in
/// any sequence of mutating operations causes elements to be copied
/// into unique, contiguous storage which may cost `O(N)` time and
/// space, where `N` is the length of the string representation (or
/// more, if the underlying `NSString` is has unusual performance
/// characteristics).
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
  public // @testable
  static func _fromWellFormedCodeUnitSequence<
    Encoding: UnicodeCodecType, Input: CollectionType
    where Input.Generator.Element == Encoding.CodeUnit
  >(
    encoding: Encoding.Type, input: Input
  ) -> String {
    return String._fromCodeUnitSequence(encoding, input: input)!
  }

  public // @testable
  static func _fromCodeUnitSequence<
    Encoding: UnicodeCodecType, Input: CollectionType
    where Input.Generator.Element == Encoding.CodeUnit
  >(
    encoding: Encoding.Type, input: Input
  ) -> String? {
    let (stringBufferOptional, _) =
        _StringBuffer.fromCodeUnits(encoding, input: input,
            repairIllFormedSequences: false)
    if let stringBuffer = stringBufferOptional {
      return String(_storage: stringBuffer)
    } else {
      return .None
    }
  }

  public // @testable
  static func _fromCodeUnitSequenceWithRepair<
    Encoding: UnicodeCodecType, Input: CollectionType
    where Input.Generator.Element == Encoding.CodeUnit
  >(
    encoding: Encoding.Type, input: Input
  ) -> (String, hadError: Bool) {
    let (stringBuffer, hadError) =
        _StringBuffer.fromCodeUnits(encoding, input: input,
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
  public
  init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1) {
    self = String._fromWellFormedCodeUnitSequence(
        UTF8.self,
        input: UnsafeBufferPointer(
            start: UnsafeMutablePointer<UTF8.CodeUnit>(start),
            count: Int(byteSize)))
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
  public
  init(
    _builtinUTF16StringLiteral start: Builtin.RawPointer,
    numberOfCodeUnits: Builtin.Word
  )  {
    self = String(
      _StringCore(
        baseAddress: COpaquePointer(start),
        count: Int(numberOfCodeUnits),
        elementShift: 1,
        hasCocoaBuffer: false,
        owner: nil))
  }
}

extension String : _BuiltinStringLiteralConvertible {
  @effects(readonly)
  @_semantics("string.makeUTF8")
  public
  init(
    _builtinStringLiteral start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1) {
    if Bool(isASCII) {
      self = String(
        _StringCore(
          baseAddress: COpaquePointer(start),
          count: Int(byteSize),
          elementShift: 0,
          hasCocoaBuffer: false,
          owner: nil))
    }
    else {
      self = String._fromWellFormedCodeUnitSequence(
          UTF8.self,
          input: UnsafeBufferPointer(
              start: UnsafeMutablePointer<UTF8.CodeUnit>(start),
              count: Int(byteSize)))
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
      result += us.escape(asASCII: false)
    }
    result += "\""
    return result
  }
}

extension String {
  /// Return the number of code units occupied by this string
  /// in the given encoding.
  func _encodedLength<
    Encoding: UnicodeCodecType
  >(encoding: Encoding.Type) -> Int {
    var codeUnitCount = 0
    var output = SinkOf<Encoding.CodeUnit> { _ in ++codeUnitCount;() }
    self._encode(encoding, output: &output)
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
    Encoding: UnicodeCodecType,
    Output: SinkType
    where Encoding.CodeUnit == Output.Element
  >(encoding: Encoding.Type, inout output: Output)
  {
    return _core.encode(encoding, output: &output)
  }
}

#if _runtime(_ObjC)
/// Compare two strings using the Unicode collation algorithm in the
/// deterministic comparison mode.  (The strings which are equivalent according
/// to their NFD form are considered equal.  Strings which are equivalent
/// according to the plain Unicode collation algorithm are additionaly ordered
/// based on their NFD.)
///
/// See Unicode Technical Standard #10.
///
/// The behavior is equivalent to `NSString.compare()` with default options.
///
/// - returns:
///   * an unspecified value less than zero if `lhs < rhs`,
///   * zero if `lhs == rhs`,
///   * an unspecified value greater than zero  if `lhs > rhs`.
@asmname("swift_stdlib_compareNSStringDeterministicUnicodeCollation")
public func _stdlib_compareNSStringDeterministicUnicodeCollation(
  lhs: AnyObject, _ rhs: AnyObject
)-> Int32
#endif

extension String : Equatable {
}

public func ==(lhs: String, rhs: String) -> Bool {
  if lhs._core.isASCII && rhs._core.isASCII {
    if lhs._core.count != rhs._core.count {
      return false
    }
    return memcmp(
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
  /// 0022  ; [*02FF.0020.0002] # QUOTATION MARK
  /// 0023  ; [*038B.0020.0002] # NUMBER SIGN
  /// 0025  ; [*038C.0020.0002] # PERCENT SIGN
  /// 0026  ; [*0389.0020.0002] # AMPERSAND
  /// 0027  ; [*02F8.0020.0002] # APOSTROPHE
  /// - Precondition: Both self and rhs are ASCII strings.
  public // @testable
  func _compareASCII(rhs: String) -> Int {
    var compare = Int(memcmp(
      self._core.startASCII, rhs._core.startASCII,
      min(self._core.count, rhs._core.count)))
    if compare == 0 {
      compare = self._core.count - rhs._core.count
    }
    // This efficiently normalizes the result to -1, 0, or 1 to match the
    // behaviour of NSString's compare function.
    return (compare > 0 ? 1 : 0) - (compare < 0 ? 1 : 0)
  }
#endif

  /// Compares two strings with the Unicode Collation Algorithm.
  @inline(never) @_semantics("stdlib_binary_only") // Hide the CF/ICU dependency
  public  // @testable
  func _compareDeterministicUnicodeCollation(rhs: String) -> Int {
    // Note: this operation should be consistent with equality comparison of
    // Character.
#if _runtime(_ObjC)
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

  public  // @testable
  func _compareString(rhs: String) -> Int {
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

public func <(lhs: String, rhs: String) -> Bool {
  return lhs._compareString(rhs) < 0
}

// Support for copy-on-write
extension String {

  /// Append the elements of `other` to `self`.
  public mutating func extend(other: String) {
    _core.append(other._core)
  }

  /// Append `x` to `self`.
  ///
  /// - Complexity: Amortized O(1).
  public mutating func append(x: UnicodeScalar) {
    _core.append(x)
  }

  var _utf16Count: Int {
    return _core.count
  }

  public // SPI(Foundation)
  init(_storage: _StringBuffer) {
    _core = _StringCore(_storage)
  }
}

#if _runtime(_ObjC)
@asmname("swift_stdlib_NSStringNFDHashValue")
func _stdlib_NSStringNFDHashValue(str: AnyObject) -> Int

@asmname("swift_stdlib_NSStringASCIIHashValue")
func _stdlib_NSStringASCIIHashValue(str: AnyObject) -> Int
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
    // FIXME(performance): constructing a temporary NSString is extremely
    // wasteful and inefficient.
    let cocoaString = unsafeBitCast(
      self._bridgeToObjectiveCImpl(), _NSStringCoreType.self)

    // If we have an ASCII string, we do not need to normalize.
    if self._core.isASCII {
      return hashOffset ^ _stdlib_NSStringASCIIHashValue(cocoaString)
    } else {
      return hashOffset ^ _stdlib_NSStringNFDHashValue(cocoaString)
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

@effects(readonly)
@_semantics("string.concat")
public func +(var lhs: String, rhs: String) -> String {
  if (lhs.isEmpty) {
    return rhs
  }
  lhs._core.append(rhs._core)
  return lhs
}

// String append
public func += (inout lhs: String, rhs: String) {
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
  @asmname("swift_stringFromUTF8InRawMemory")
  public // COMPILER_INTRINSIC
  static func _fromUTF8InRawMemory(
    resultStorage: UnsafeMutablePointer<String>,
    start: UnsafeMutablePointer<UTF8.CodeUnit>, utf8Count: Int
  ) {
    resultStorage.initialize(
        String._fromWellFormedCodeUnitSequence(UTF8.self,
            input: UnsafeBufferPointer(start: start, count: utf8Count)))
  }
}

extension String {
  public typealias Index = CharacterView.Index
  
  /// The position of the first `Character` in `self.characters` if
  /// `self` is non-empty; identical to `endIndex` otherwise.
  public var startIndex: Index { return characters.startIndex }
  
  /// The "past the end" position in `self.characters`.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: Index { return characters.endIndex }

  /// Access the `Character` at `position`.
  ///
  /// - Requires: `position` is a valid position in `self.characters`
  ///   and `position != endIndex`.
  public subscript(i: Index) -> Character { return characters[i] }
}

public func == (lhs: String.Index, rhs: String.Index) -> Bool {
  return lhs._base == rhs._base
}

public func < (lhs: String.Index, rhs: String.Index) -> Bool {
  return lhs._base < rhs._base
}

extension String {
  /// Access the characters in the given `subRange`.
  ///
  /// - Complexity: O(1) unless bridging from Objective-C requires an
  ///   O(N) conversion.
  public subscript(subRange: Range<Index>) -> String {
    return String(characters[subRange])
  }
}

extension String {
  public mutating func reserveCapacity(n: Int) {
    withMutableCharacters {
      (inout v: CharacterView) in v.reserveCapacity(n)
    }
  }
  public mutating func append(c: Character) {
    withMutableCharacters {
      (inout v: CharacterView) in v.append(c)
    }
  }
  
  public mutating func extend<
      S : SequenceType
  where S.Generator.Element == Character
  >(newElements: S) {
    withMutableCharacters { (inout v: CharacterView) in v.extend(newElements) }
  }
  
  /// Create an instance containing `characters`.
  public init<
      S : SequenceType
      where S.Generator.Element == Character
  >(_ characters: S) {
    self._core = CharacterView(characters)._core
  }
}

extension String {
  /// Interpose `self` between every pair of consecutive `elements`,
  /// then concatenate the result.  For example:
  ///
  ///     "-|-".join(["foo", "bar", "baz"]) // "foo-|-bar-|-baz"
  public func join<
      S : SequenceType where S.Generator.Element == String
  >(elements: S) -> String {
    return String(
      characters.join(lazy(elements).map { $0.characters })
    )
  }
}

extension String {
  /// Replace the given `subRange` of elements with `newElements`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`subRange.count`) if `subRange.endIndex
  ///   == self.endIndex` and `isEmpty(newElements)`, O(N) otherwise.
  public mutating func replaceRange<
    C: CollectionType where C.Generator.Element == Character
  >(
    subRange: Range<Index>, with newElements: C
  ) {
    withMutableCharacters {
      (inout v: CharacterView) in v.replaceRange(subRange, with: newElements)
    }
  }

  /// Replace the given `subRange` of elements with `newElements`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`subRange.count`) if `subRange.endIndex
  ///   == self.endIndex` and `isEmpty(newElements)`, O(N) otherwise.
  public mutating func replaceRange(
    subRange: Range<Index>, with newElements: String
  ) {
    replaceRange(subRange, with: newElements.characters)
  }

  /// Insert `newElement` at index `i`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count`).
  public mutating func insert(newElement: Character, atIndex i: Index) {
    withMutableCharacters {
      (inout v: CharacterView) in v.insert(newElement, atIndex: i)
    }
  }

  /// Insert `newElements` at index `i`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count + newElements.count`).
  public mutating func splice<
    S : CollectionType where S.Generator.Element == Character
  >(newElements: S, atIndex i: Index) {
    withMutableCharacters {
      (inout v: CharacterView) in v.splice(newElements, atIndex: i)
    }
  }

  /// Remove and return the element at index `i`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count`).
  public mutating func removeAtIndex(i: Index) -> Character {
    return withMutableCharacters {
      (inout v: CharacterView) in v.removeAtIndex(i)
    }
  }

  /// Remove the indicated `subRange` of characters.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - Complexity: O(`self.count`).
  public mutating func removeRange(subRange: Range<Index>) {
    withMutableCharacters {
      (inout v: CharacterView) in v.removeRange(subRange)
    }
  }

  /// Remove all characters.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// - parameter keepCapacity: If `true`, prevents the release of
  ///   allocated storage, which can be a useful optimization
  ///   when `self` is going to be grown again.
  public mutating func removeAll(keepCapacity keepCapacity: Bool = false) {
    withMutableCharacters {
      (inout v: CharacterView) in v.removeAll(keepCapacity: keepCapacity)
    }
  }
}
#if _runtime(_ObjC)
@asmname("swift_stdlib_NSStringLowercaseString")
func _stdlib_NSStringLowercaseString(str: AnyObject) -> _CocoaStringType

@asmname("swift_stdlib_NSStringUppercaseString")
func _stdlib_NSStringUppercaseString(str: AnyObject) -> _CocoaStringType
#else
internal func _nativeUnicodeLowercaseString(str: String) -> String {
  var buffer = _StringBuffer(
    capacity: str._core.count, initialSize: str._core.count, elementWidth: 2)

  // Try to write it out to the same length.
  var dest = UnsafeMutablePointer<UTF16.CodeUnit>(buffer.start)
  let z = _swift_stdlib_unicode_strToLower(
    dest, Int32(str._core.count),
    str._core.startUTF16, Int32(str._core.count))
  let correctSize = Int(z)

  // If more space is needed, do it again with the correct buffer size.
  if correctSize != str._core.count {
    buffer = _StringBuffer(
      capacity: correctSize, initialSize: correctSize, elementWidth: 2)
    _swift_stdlib_unicode_strToLower(
      dest, Int32(correctSize), str._core.startUTF16, Int32(str._core.count))
  }

  return String(_storage: buffer)
}

internal func _nativeUnicodeUppercaseString(str: String) -> String {
  var buffer = _StringBuffer(
    capacity: str._core.count, initialSize: str._core.count, elementWidth: 2)

  // Try to write it out to the same length.
  var dest = UnsafeMutablePointer<UTF16.CodeUnit>(buffer.start)
  let z = _swift_stdlib_unicode_strToUpper(
    dest, Int32(str._core.count),
    str._core.startUTF16, Int32(str._core.count))
  let correctSize = Int(z)

  // If more space is needed, do it again with the correct buffer size.
  if correctSize != str._core.count {
    buffer = _StringBuffer(
      capacity: correctSize, initialSize: correctSize, elementWidth: 2)
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

  public var lowercaseString: String {
    if self._core.isASCII {
      let length = self._core.count
      let source = self._core.startASCII
      let buffer = _StringBuffer(
        capacity: length, initialSize: length, elementWidth: 1)
      let dest = UnsafeMutablePointer<UInt8>(buffer.start)
      for i in 0..<length {
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

  public var uppercaseString: String {
    if self._core.isASCII {
      let length = self._core.count
      let source = self._core.startASCII
      let buffer = _StringBuffer(
        capacity: length, initialSize: length, elementWidth: 1)
      let dest = UnsafeMutablePointer<UInt8>(buffer.start)
      for i in 0..<length {
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

// Index conversions
extension String.Index {
  /// Construct the position in `characters` that corresponds exactly to
  /// `unicodeScalarIndex`. If no such position exists, the result is `nil`.
  ///
  /// - Requires: `unicodeScalarIndex` is an element of
  ///   `characters.unicodeScalars.indices`.
  public init?(
    _ unicodeScalarIndex: String.UnicodeScalarIndex,
    within characters: String
  ) {
    if !unicodeScalarIndex._isOnGraphemeClusterBoundary {
      return nil
    }
    self.init(_base: unicodeScalarIndex)
  }

  /// Construct the position in `characters` that corresponds exactly to
  /// `utf16Index`. If no such position exists, the result is `nil`.
  ///
  /// - Requires: `utf16Index` is an element of
  ///   `characters.utf16.indices`.
  public init?(
    _ utf16Index: String.UTF16Index,
    within characters: String
  ) {
    if let me = utf16Index.samePositionIn(
      characters.unicodeScalars
    )?.samePositionIn(characters) {
      self = me
    }
    else {
      return nil
    }
  }

  /// Construct the position in `characters` that corresponds exactly to
  /// `utf8Index`. If no such position exists, the result is `nil`.
  ///
  /// - Requires: `utf8Index` is an element of
  ///   `characters.utf8.indices`.
  public init?(
    _ utf8Index: String.UTF8Index,
    within characters: String
  ) {
    if let me = utf8Index.samePositionIn(
      characters.unicodeScalars
    )?.samePositionIn(characters) {
      self = me
    }
    else {
      return nil
    }
  }

  /// Return the position in `utf8` that corresponds exactly
  /// to `self`.
  ///
  /// - Requires: `self` is an element of `String(utf8).indices`.
  public func samePositionIn(
    utf8: String.UTF8View
  ) -> String.UTF8View.Index {
    return String.UTF8View.Index(self, within: utf8)
  }

  /// Return the position in `utf16` that corresponds exactly
  /// to `self`.
  ///
  /// - Requires: `self` is an element of `String(utf16).indices`.
  public func samePositionIn(
    utf16: String.UTF16View
  ) -> String.UTF16View.Index {
    return String.UTF16View.Index(self, within: utf16)
  }

  /// Return the position in `unicodeScalars` that corresponds exactly
  /// to `self`.
  ///
  /// - Requires: `self` is an element of `String(unicodeScalars).indices`.
  public func samePositionIn(
    unicodeScalars: String.UnicodeScalarView
  ) -> String.UnicodeScalarView.Index {
    return String.UnicodeScalarView.Index(self, within: unicodeScalars)
  }
}


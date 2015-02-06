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
/// Unicode character space.  For example,
///
/// * The `==` operator checks for `Unicode canonical equivalence
///   <http://www.unicode.org/glossary/#deterministic_comparison>`_,
///   so two different representations of the same string will always
///   compare equal.
///
/// * String elements are `Characters` (`extended grapheme clusters
///   <http://www.unicode.org/glossary/#extended_grapheme_cluster>`_),
///   a unit of text that is meaningful to most humans.
///
/// Locale-Insensitive
/// ==================
///
/// The fundamental operations on Swift strings are not sensitive to
/// locale settings.  That's because, for example, the validity of a
/// `Dictionary<String, T>` in a running program depends on a given
/// string comparison having a single, stable result.  Therefore,
/// Swift always uses the default, un-\ `tailored
/// <http://www.unicode.org/glossary/#tailorable>`_ Unicode algorithms
/// for basic string operations.
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
/// through its copies::
///
///   var a = "foo"
///   var b = a
///   b[b.endIndex.predecessor()] = "x"
///   println("a=\(a), b=\(b)")     // a=foo, b=fox
///
/// Strings use Copy-on-Write so that their data is only copied
/// lazily, upon mutation, when more than one string instance is using
/// the same buffer.  Therefore, the first in any sequence of mutating
/// operations may cost `O(N)` time and space, where `N` is the length
/// of the string's (unspecified) underlying representation,.
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
  public init() {
    _core = _StringCore()
  }

  public init(_ _core: _StringCore) {
    self._core = _core
  }

  public var _core: _StringCore
}

extension String {
  public static func _fromWellFormedCodeUnitSequence<
    Encoding: UnicodeCodecType, Input: CollectionType
    where Input.Generator.Element == Encoding.CodeUnit
  >(
    encoding: Encoding.Type, input: Input
  ) -> String {
    return String._fromCodeUnitSequence(encoding, input: input)!
  }

  public static func _fromCodeUnitSequence<
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

  public static func _fromCodeUnitSequenceWithRepair<
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
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
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
  @semantics("string.makeUTF8")
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
  @semantics("string.makeUTF16")
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
  @semantics("string.makeUTF8")
  public
  init(
    _builtinStringLiteral start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1) {
    if isASCII {
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

extension String : DebugPrintable {
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
/// :returns:
///   * an unspecified value less than zero if `lhs < rhs`,
///   * zero if `lhs == rhs`,
///   * an unspecified value greater than zero  if `lhs > rhs`.
@asmname("swift_stdlib_compareNSStringDeterministicUnicodeCollation")
public func _stdlib_compareNSStringDeterministicUnicodeCollation(
  lhs: AnyObject, rhs: AnyObject
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
      UnsafeMutablePointer(lhs._core.startASCII),
      UnsafeMutablePointer(rhs._core.startASCII),
      UInt(rhs._core.count)) == 0
  }
#if _runtime(_ObjC)
  // Note: this operation should be consistent with equality comparison of
  // Character.
  return _stdlib_compareNSStringDeterministicUnicodeCollation(
    lhs._bridgeToObjectiveCImpl(), rhs._bridgeToObjectiveCImpl()) == 0
#else
  // FIXME: Actually implement. For now, all strings are unequal.
  // rdar://problem/18878343
  return false
#endif
}

extension String : Comparable {
}

extension String {
  @inline(never) @semantics("stdlib_binary_only") // Hide the CF dependency
  public // @testable
  func _lessThanUTF16(rhs: String) -> Bool {
#if _runtime(_ObjC)
    return _stdlib_compareNSStringDeterministicUnicodeCollation(
      self._stdlib_binary_bridgeToObjectiveCImpl(),
      rhs._stdlib_binary_bridgeToObjectiveCImpl()) < 0
#else
    // FIXME: Actually implement. For now, all strings are unequal
    // rdar://problem/18878343
    return false
#endif
  }

  /// This is consistent with Foundation, but incorrect as defined by Unicode.
  /// Unicode weights some ASCII punctuation in a different order than ASCII
  /// value. Such as:
  /// 0022  ; [*02FF.0020.0002] # QUOTATION MARK
  /// 0023  ; [*038B.0020.0002] # NUMBER SIGN
  /// 0025  ; [*038C.0020.0002] # PERCENT SIGN
  /// 0026  ; [*0389.0020.0002] # AMPERSAND
  /// 0027  ; [*02F8.0020.0002] # APOSTROPHE
  /// precondition: both self and rhs are ASCII strings
  public // @testable
  func _lessThanASCII(rhs: String) -> Bool {
    let compare = memcmp(
      UnsafeMutablePointer(self._core.startASCII),
      UnsafeMutablePointer(rhs._core.startASCII),
      min(UInt(self._core.count), UInt(rhs._core.count)))
    if compare == 0 {
      return self._core.count < rhs._core.count
    } else {
      return compare < 0
    }
  }
}

public func <(lhs: String, rhs: String) -> Bool {
  if lhs._core.isASCII && rhs._core.isASCII {
    return lhs._lessThanASCII(rhs)
  }
  return lhs._lessThanUTF16(rhs)
}

// Support for copy-on-write
extension String {

  /// Append the elements of `other` to `self`.
  public mutating func extend(other: String) {
    _core.append(other._core)
  }

  /// Append `x` to `self`.
  ///
  /// Complexity: amortized O(1).
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
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`
  ///
  /// **Note:** the hash value is not guaranteed to be stable across
  /// different invocations of the same program.  Do not persist the
  /// hash value across program runs.
  public var hashValue: Int {
    // Mix random bits into NSString's hash so that clients don't rely on
    // Swift.String.hashValue and NSString.hash being the same.
#if arch(i386) || arch(arm)
    let hashOffset = Int(bitPattern: 0x88dd_cc21)
#else
    let hashOffset = Int(bitPattern: 0x429b_1266_88dd_cc21)
#endif
#if _runtime(_ObjC)
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
    // FIXME: Actually implement. For now, all strings have the same hash.
    // rdar://problem/18878343
    return hashOffset
#endif
  }
}

extension String : StringInterpolationConvertible {
  /// Create an instance by concatenating the elements of `strings`
  @effects(readonly)
  public
  init(stringInterpolation strings: String...) {
    self.init()
    for str in strings {
      self += str
    }
  }

  /// Create an instance containing `expr`\ 's `print` representation
  public
  init<T>(stringInterpolationSegment expr: T) {
    self = toString(expr)
  }
}

@effects(readonly)
@semantics("string.concat")
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

/// String is a CollectionType of Character
extension String : CollectionType {
  /// A character position in a `String`
  public struct Index : BidirectionalIndexType, Comparable, Reflectable {
    public // SPI(Foundation)
    init(_base: UnicodeScalarView.Index) {
      self._base = _base
      self._lengthUTF16 = Index._measureExtendedGraphemeClusterForward(_base)
    }

    internal init(_base: UnicodeScalarView.Index, _lengthUTF16: Int) {
      self._base = _base
      self._lengthUTF16 = _lengthUTF16
    }

    /// Returns the next consecutive value after `self`.
    ///
    /// Requires: the next value is representable.
    public func successor() -> Index {
      _precondition(_base != _base._viewEndIndex, "can not increment endIndex")
      return Index(_base: _endBase)
    }

  /// Returns the previous consecutive value before `self`.
  ///
  /// Requires: the previous value is representable.
    public func predecessor() -> Index {
      _precondition(_base != _base._viewStartIndex,
          "can not decrement startIndex")
      let predecessorLengthUTF16 =
          Index._measureExtendedGraphemeClusterBackward(_base)
      return Index(
        _base: UnicodeScalarView.Index(
          _utf16Index - predecessorLengthUTF16, _base._core))
    }

    let _base: UnicodeScalarView.Index

    /// The length of this extended grapheme cluster in UTF-16 code units.
    let _lengthUTF16: Int

    /// The integer offset of this index in UTF-16 code units.
    public var _utf16Index: Int {
      return _base._position
    }

    /// The one past end index for this extended grapheme cluster in Unicode
    /// scalars.
    var _endBase: UnicodeScalarView.Index {
      return UnicodeScalarView.Index(
          _utf16Index + _lengthUTF16, _base._core)
    }

    /// Returns the length of the first extended grapheme cluster in UTF-16
    /// code units.
    static func _measureExtendedGraphemeClusterForward(
        var start: UnicodeScalarView.Index
    ) -> Int {
      let end = start._viewEndIndex
      if start == end {
        return 0
      }

      let startIndexUTF16 = start._position
      let unicodeScalars = UnicodeScalarView(start._core)
      let graphemeClusterBreakProperty =
          _UnicodeGraphemeClusterBreakPropertyTrie()
      let segmenter = _UnicodeExtendedGraphemeClusterSegmenter()

      var gcb0 = graphemeClusterBreakProperty.getPropertyRawValue(
          unicodeScalars[start].value)
      ++start

      for ; start != end; ++start {
        // FIXME(performance): consider removing this "fast path".  A branch
        // that is hard to predict could be worse for performance than a few
        // loads from cache to fetch the property 'gcb1'.
        if segmenter.isBoundaryAfter(gcb0) {
          break
        }
        let gcb1 = graphemeClusterBreakProperty.getPropertyRawValue(
            unicodeScalars[start].value)
        if segmenter.isBoundary(gcb0, gcb1) {
          break
        }
        gcb0 = gcb1
      }

      return start._position - startIndexUTF16
    }

    /// Returns the length of the previous extended grapheme cluster in UTF-16
    /// code units.
    static func _measureExtendedGraphemeClusterBackward(
        end: UnicodeScalarView.Index
    ) -> Int {
      var start = end._viewStartIndex
      if start == end {
        return 0
      }

      let endIndexUTF16 = end._position
      let unicodeScalars = UnicodeScalarView(start._core)
      let graphemeClusterBreakProperty =
          _UnicodeGraphemeClusterBreakPropertyTrie()
      let segmenter = _UnicodeExtendedGraphemeClusterSegmenter()

      var graphemeClusterStart = end

      --graphemeClusterStart
      var gcb0 = graphemeClusterBreakProperty.getPropertyRawValue(
          unicodeScalars[graphemeClusterStart].value)

      var graphemeClusterStartUTF16 = graphemeClusterStart._position

      while graphemeClusterStart != start {
        --graphemeClusterStart
        let gcb1 = graphemeClusterBreakProperty.getPropertyRawValue(
            unicodeScalars[graphemeClusterStart].value)
        if segmenter.isBoundary(gcb1, gcb0) {
          break
        }
        gcb0 = gcb1
        graphemeClusterStartUTF16 = graphemeClusterStart._position
      }

      return endIndexUTF16 - graphemeClusterStartUTF16
    }
    
    /// Returns a mirror that reflects `self`.
    public func getMirror() -> MirrorType {
      return _IndexMirror(self)
    }
  }

  /// The position of the first `Character` if the `String` is
  /// non-empty; identical to `endIndex` otherwise.
  public var startIndex: Index {
    return Index(_base: unicodeScalars.startIndex)
  }

  /// The `String`\ 's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: Index {
    return Index(_base: unicodeScalars.endIndex)
  }

  /// Access the `Character` at `position`.
  ///
  /// Requires: `position` is a valid position in `self` and
  /// `position != endIndex`.
  public subscript(i: Index) -> Character {
    return Character(String(unicodeScalars[i._base..<i._endBase]))
  }

  @availability(*, unavailable, message="cannot subscript String with an Int")
  public subscript(i: Int) -> Character {
    _fatalErrorMessage(
      "fatal error", 
      "cannot subscript String with an Int",
      __FILE__, 
      __LINE__
    )
  }

  /// Return a *generator* over the `Characters` in this `String`.
  ///
  /// Complexity: O(1)
  public func generate() -> IndexingGenerator<String> {
    return IndexingGenerator(self)
  }
  
  internal struct _IndexMirror : MirrorType {
    var _value: Index

    init(_ x: Index) {
      _value = x
    }

    var value: Any { return _value }

    var valueType: Any.Type { return (_value as Any).dynamicType }

    var objectIdentifier: ObjectIdentifier? { return .None }

    var disposition: MirrorDisposition { return .Aggregate }
    
    var count: Int { return 0 }

    subscript(i: Int) -> (String, MirrorType) {
      _preconditionFailure("MirrorType access out of bounds")
    }

    var summary: String { return "\(_value._utf16Index)" }

    var quickLookObject: QuickLookObject? { return .Some(.Int(Int64(_value._utf16Index))) }
  }
}

public func == (lhs: String.Index, rhs: String.Index) -> Bool {
  return lhs._base == rhs._base
}

public func < (lhs: String.Index, rhs: String.Index) -> Bool {
  return lhs._base < rhs._base
}

extension String : Sliceable {
  /// Access the characters in the given `subRange`
  ///
  /// Complexity: O(1) unless bridging from Objective-C requires an
  /// O(N) conversion.
  public subscript(subRange: Range<Index>) -> String {
    return String(
      unicodeScalars[subRange.startIndex._base..<subRange.endIndex._base]._core)
  }

  @availability(*, unavailable, message="cannot subscript String with a range of Int")
  public subscript(subRange: Range<Int>) -> String {
    return ""
  }
}

extension String : ExtensibleCollectionType {
  /// Reserve enough space to store `n` ASCII characters.
  ///
  /// Complexity: O(`n`)
  public mutating func reserveCapacity(n: Int) {
    _core.reserveCapacity(n)
  }

  /// Append `c` to `self`.
  ///
  /// Complexity: amortized O(1).
  public mutating func append(c: Character) {
    switch c._representation {
    case .Small(let _63bits):
      let bytes = Character._smallValue(_63bits)
      _core.extend(Character._SmallUTF16(bytes))
    case .Large(let storage):
      _core.append(_StringCore(_StringBuffer(storage)))
    }
  }
  
  /// Append the elements of `newElements` to `self`.
  public mutating func extend<
      S : SequenceType
      where S.Generator.Element == Character
  >(newElements: S) {
    reserveCapacity(_core.count + underestimateCount(newElements))
    for c in newElements {
      self.append(c)
    }
  }

  /// Create an instance containing `characters`.
  public init<
      S : SequenceType
      where S.Generator.Element == Character
  >(_ characters: S) {
    self = ""
    self.extend(characters)
  }
}

// Algorithms
extension String {
  /// Interpose `self` between every pair of consecutive `elements`,
  /// then concatenate the result.  For example::
  ///
  ///   "-|-".join(["foo", "bar", "baz"]) // "foo-|-bar-|-baz"
  public func join<
      S : SequenceType where S.Generator.Element == String
  >(elements: S) -> String{
    return Swift.join(self, elements)
  }
}

extension String : RangeReplaceableCollectionType {
  /// Replace the given `subRange` of elements with `newElements`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `count(subRange)`\ ) if `subRange.endIndex
  /// == self.endIndex` and `isEmpty(newElements)`\ , O(N) otherwise.
  public mutating func replaceRange<
    C: CollectionType where C.Generator.Element == Character
  >(
    subRange: Range<Index>, with newElements: C
  ) {
    _core.replaceRange(
      subRange.startIndex._base._position
      ..< subRange.endIndex._base._position,
      with:
        _lazyConcatenate(lazy(newElements).map { $0.utf16 })
    )
  }

  /// Insert `newElement` at index `i`.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `count(self)`\ ).
  public mutating func insert(newElement: Character, atIndex i: Index) {
    Swift.insert(&self, newElement, atIndex: i)
  }
  
  /// Insert `newElements` at index `i`
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `count(self) + count(newElements)`\ ).
  public mutating func splice<
    S : CollectionType where S.Generator.Element == Character
  >(newElements: S, atIndex i: Index) {
    Swift.splice(&self, newElements, atIndex: i)
  }

  /// Remove and return the element at index `i`
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `count(self)`\ ).
  public mutating func removeAtIndex(i: Index) -> Character {
    return Swift.removeAtIndex(&self, i)
  }
  
  /// Remove the indicated `subRange` of characters
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// Complexity: O(\ `count(self)`\ ).
  public mutating func removeRange(subRange: Range<Index>) {
    Swift.removeRange(&self, subRange)
  }

  /// Remove all characters.
  ///
  /// Invalidates all indices with respect to `self`.
  ///
  /// :param: `keepCapacity`, if `true`, prevents the release of
  ////   allocated storage, which can be a useful optimization
  ///    when `self` is going to be grown again.
  public mutating func removeAll(keepCapacity: Bool = false) {
    Swift.removeAll(&self, keepCapacity: keepCapacity)
  }
}

#if _runtime(_ObjC)
@asmname("swift_stdlib_NSStringLowercaseString")
func _stdlib_NSStringLowercaseString(str: AnyObject) -> _CocoaStringType

@asmname("swift_stdlib_NSStringUppercaseString")
func _stdlib_NSStringUppercaseString(str: AnyObject) -> _CocoaStringType
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
      var buffer = _StringBuffer(
        capacity: length, initialSize: length, elementWidth: 1)
      var dest = UnsafeMutablePointer<UInt8>(buffer.start)
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
    // FIXME: Actually implement.  For now, don't change case.
    // rdar://problem/18878343
    return self
#endif
  }

  public var uppercaseString: String {
    if self._core.isASCII {
      let length = self._core.count
      let source = self._core.startASCII
      var buffer = _StringBuffer(
        capacity: length, initialSize: length, elementWidth: 1)
      var dest = UnsafeMutablePointer<UInt8>(buffer.start)
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
    // FIXME: Actually implement.  For now, don't change case.
    // rdar://problem/18878343
    return self
#endif
  }
}

// Index conversions
extension String.Index {
  /// Construct the position in `characters` that corresponds exactly to
  /// `unicodeScalarIndex`. If no such position exists, the result is `nil`.
  ///
  /// Requires: `unicodeScalarIndex` is an element of
  /// `indices(characters.unicodeScalars)`.
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
  /// Requires: `utf16Index` is an element of
  /// `indices(characters.utf16)`.
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
  /// Requires: `utf8Index` is an element of
  /// `indices(characters.utf8)`.
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
  /// Requires: `self` is an element of `indices(String(utf8))`.
  public func samePositionIn(
    utf8: String.UTF8View
  ) -> String.UTF8View.Index {
    return String.UTF8View.Index(self, within: utf8)
  }
  
  /// Return the position in `utf16` that corresponds exactly
  /// to `self`.
  ///
  /// Requires: `self` is an element of `indices(String(utf16))`.
  public func samePositionIn(
    utf16: String.UTF16View
  ) -> String.UTF16View.Index {
    return String.UTF16View.Index(self, within: utf16)
  }
  
  /// Return the position in `unicodeScalars` that corresponds exactly
  /// to `self`.
  ///
  /// Requires: `self` is an element of `indices(String(unicodeScalars))`.
  public func samePositionIn(
    unicodeScalars: String.UnicodeScalarView
  ) -> String.UnicodeScalarView.Index {
    return String.UnicodeScalarView.Index(self, within: unicodeScalars)
  }
}

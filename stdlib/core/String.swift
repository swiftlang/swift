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
    self._encode(
      encoding, output: SinkOf<Encoding.CodeUnit>({ _ in ++codeUnitCount;() }))
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
  >(encoding: Encoding.Type, output: Output)
  {
    return _core.encode(encoding, output: output)
  }
}

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

extension String: Equatable {
}

public func ==(lhs: String, rhs: String) -> Bool {
  // Note: this operation should be consistent with equality comparison of
  // Character.
  return _stdlib_compareNSStringDeterministicUnicodeCollation(
    lhs._bridgeToObjectiveCImpl(), rhs._bridgeToObjectiveCImpl()) == 0
}

extension String : Comparable {
}

public func <(lhs: String, rhs: String) -> Bool {
  return _stdlib_compareNSStringDeterministicUnicodeCollation(
    lhs._bridgeToObjectiveCImpl(), rhs._bridgeToObjectiveCImpl()) < 0
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

  init(_storage: _StringBuffer) {
    _core = _StringCore(_storage)
  }
}

@asmname("swift_stdlib_NSStringNFDHashValue")
func _stdlib_NSStringNFDHashValue(str: AnyObject) -> Int

extension String : Hashable {
  public var hashValue: Int {
    // Mix random bits into NSString's hash so that clients don't rely on
    // Swift.String.hashValue and NSString.hash being the same.
#if arch(i386) || arch(arm)
    let hashOffset = 0x88ddcc21
#else
    let hashOffset = 0x429b126688ddcc21
#endif
    // FIXME(performance): constructing a temporary NSString is extremely
    // wasteful and inefficient.
    let cocoaString =
      unsafeBitCast(self._bridgeToObjectiveCImpl(), _SwiftNSStringType.self)
    return hashOffset ^ _stdlib_NSStringNFDHashValue(cocoaString)
  }
}

extension String : StringInterpolationConvertible {
  @effects(readonly)
  public
  init(stringInterpolation strings: String...) {
    self.init()
    for str in strings {
      self += str
    }
  }

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

// Comparison operators
extension String : Comparable {
}

extension String {
  /// Low-level construction interface used by introspection
  /// implementation in the runtime library.  Constructs a String in
  /// resultStorage containing the given UTF-8.
  @asmname("swift_stringFromUTF8InRawMemory")
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
  // An adapter over UnicodeScalarView that advances by whole Character
  public struct Index : BidirectionalIndexType, Comparable, Reflectable {
    public init(_ _base: UnicodeScalarView.Index) {
      self._base = _base
      self._lengthUTF16 = Index._measureExtendedGraphemeClusterForward(_base)
    }

    init(_ _base: UnicodeScalarView.Index, _ _lengthUTF16: Int) {
      self._base = _base
      self._lengthUTF16 = _lengthUTF16
    }

    /// Returns the next consecutive value after `self`.
    ///
    /// Requires: the next value is representable.
    public func successor() -> Index {
      _precondition(_base != _base._viewEndIndex, "can not increment endIndex")
      return Index(_endBase)
    }

  /// Returns the previous consecutive value before `self`.
  ///
  /// Requires: the previous value is representable.
    public func predecessor() -> Index {
      _precondition(_base != _base._viewStartIndex,
          "can not decrement startIndex")
      let predecessorLengthUTF16 =
          Index._measureExtendedGraphemeClusterBackward(_base)
      return Index(UnicodeScalarView.Index(
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
    
    public func getMirror() -> MirrorType {
      return _IndexMirror(self)
    }
  }

  /// The position of the first `Character` if the `String` is
  /// non-empty; identical to `endIndex` otherwise.
  public var startIndex: Index {
    return Index(unicodeScalars.startIndex)
  }

  /// The `String`\ 's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: Index {
    return Index(unicodeScalars.endIndex)
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
    switch c {
    case .SmallRepresentation(let _63bits):
      let bytes = Character._smallValue(_63bits)
      _core.extend(Character._SmallUTF16(bytes))
    case .LargeRepresentation(let str):
      _core.append(str._value._core)
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
  
  public init<
      S : SequenceType
      where S.Generator.Element == Character
  >(_ seq: S) {
    self = ""
    self.extend(seq)
  }
}

// Algorithms
extension String {
  public func join<
      S : SequenceType where S.Generator.Element == String
  >(elements: S) -> String{
    return Swift.join(self, elements)
  }
}

extension String : RangeReplaceableCollectionType {
  /// Replace the given `subRange` of elements with `newValues`.
  /// Complexity: O(\ `countElements(subRange)`\ ) if `subRange.endIndex
  /// == self.endIndex` and `isEmpty(newValues)`\ , O(N) otherwise.
  public mutating func replaceRange<
    C: CollectionType where C.Generator.Element == Character
  >(
    subRange: Range<Index>, with newValues: C
  ) {
    _core.replaceRange(
      subRange.startIndex._base._position
      ..< subRange.endIndex._base._position,
      with:
        _lazyConcatenate(lazy(newValues).map { $0.utf16 })
    )
  }

  public mutating func insert(newElement: Character, atIndex i: Index) {
    Swift.insert(&self, newElement, atIndex: i)
  }
  
  public mutating func splice<
    S : CollectionType where S.Generator.Element == Character
  >(newValues: S, atIndex i: Index) {
    Swift.splice(&self, newValues, atIndex: i)
  }

  public mutating func removeAtIndex(i: Index) -> Character {
    return Swift.removeAtIndex(&self, i)
  }
  
  public mutating func removeRange(subRange: Range<Index>) {
    Swift.removeRange(&self, subRange)
  }

  public mutating func removeAll(keepCapacity: Bool = false) {
    Swift.removeAll(&self, keepCapacity: keepCapacity)
  }
}

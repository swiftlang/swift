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
    core = _StringCore()
  }

  public init(_ _core: _StringCore) {
    self.core = _core
  }

  public var core: _StringCore
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
      return String(stringBuffer)
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
    return (String(stringBuffer!), hadError)
  }
}

extension String : _BuiltinExtendedGraphemeClusterLiteralConvertible {
  public
  static func _convertFromBuiltinExtendedGraphemeClusterLiteral(
    start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1) -> String {

    return String._fromWellFormedCodeUnitSequence(
        UTF8.self,
        input: UnsafeArray(
            start: UnsafePointer<UTF8.CodeUnit>(start),
            length: Int(byteSize)))
  }
}

extension String : ExtendedGraphemeClusterLiteralConvertible {
  public static func convertFromExtendedGraphemeClusterLiteral(
    value: String
  ) -> String {
    return value
  }
}

extension String : _BuiltinUTF16StringLiteralConvertible {
  @semantics("readonly")
  public
  static func _convertFromBuiltinUTF16StringLiteral(
    start: Builtin.RawPointer, numberOfCodeUnits: Builtin.Word
  ) -> String {

    return String(
      _StringCore(
        baseAddress: COpaquePointer(start),
        count: Int(numberOfCodeUnits),
        elementShift: 1,
        hasCocoaBuffer: false,
        owner: nil))
  }
}

extension String : _BuiltinStringLiteralConvertible {
  public
  static func _convertFromBuiltinStringLiteral(
    start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1) -> String {

    if isASCII {
      return String(
        _StringCore(
          baseAddress: COpaquePointer(start),
          count: Int(byteSize),
          elementShift: 0,
          hasCocoaBuffer: false,
          owner: nil))
    }
    else {
      return String._fromWellFormedCodeUnitSequence(
          UTF8.self,
          input: UnsafeArray(
              start: UnsafePointer<UTF8.CodeUnit>(start),
              length: Int(byteSize)))
    }
  }
}

extension String : StringLiteralConvertible {
  public static func convertFromStringLiteral(value: String) -> String {
    return value
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
    return core.encode(encoding, output: output)
  }
}

extension String: Equatable {
}

public func ==(lhs: String, rhs: String) -> Bool {
  // FIXME: Compares UnicodeScalars, but should eventually do proper
  // Unicode string comparison. This is above the level of the
  // standard equal algorithm because even the largest units
  // (Characters/a.k.a. grapheme clusters) don't have a 1-for-1
  // correspondence.  For example, "SS" == "ß" should be true.
  //
  // NOTE: if this algorithm is changed, consider updating equality comparison
  // of Character.
  return Swift.equal(lhs.unicodeScalars, rhs.unicodeScalars)
}

public func <(lhs: String, rhs: String) -> Bool {
  // FIXME: Does lexicographical ordering on component UnicodeScalars,
  // but should eventually do a proper unicode String collation.  See
  // the comment on == for more information.
  return lexicographicalCompare(lhs.unicodeScalars, rhs.unicodeScalars)
}

// Support for copy-on-write
extension String {

  mutating func _append(rhs: String) {
    core.append(rhs.core)
  }

  mutating func _append(x: UnicodeScalar) {
    core.append(x)
  }

  var _utf16Count: Int {
    return core.count
  }

  init(_ storage: _StringBuffer) {
    core = _StringCore(storage)
  }
}

extension String : Hashable {
  public var hashValue: Int {
    var r : Int = 5381
    _encode(
      UTF8.self,
      output: SinkOf<UTF8.CodeUnit> ({
          r = ((r << 5) &+ r) &+ Int($0)
        }))

    return r
  }
}

extension String : StringInterpolationConvertible {
  public
  static func convertFromStringInterpolation(strings: String...) -> String {
    var result = String()
    for str in strings {
      result += str
    }
    return result
  }

  public
  static func convertFromStringInterpolationSegment<T>(expr: T) -> String {
    return toString(expr)
  }
}

public func +(var lhs: String, rhs: String) -> String {
  if (lhs.isEmpty) {
    return rhs
  }
  lhs._append(rhs)
  return lhs
}

public func +(var lhs: String, rhs: Character) -> String {
  lhs._append(String(rhs))
  return lhs
}
public func +(lhs: Character, rhs: String) -> String {
  var result = String(lhs)
  result._append(rhs)
  return result
}
public func +(lhs: Character, rhs: Character) -> String {
  var result = String(lhs)
  result += String(rhs)
  return result
}


// String append
public func += (inout lhs: String, rhs: String) {
  if (lhs.isEmpty) {
    lhs = rhs
  }
  else {
    lhs._append(rhs)
  }
}

public func += (inout lhs: String, rhs: Character) {
  lhs += String(rhs)
}

// Comparison operators
// FIXME: Compare Characters, not code units
extension String : Comparable {
}

extension String {
  /// Low-level construction interface used by introspection
  /// implementation in the runtime library.  Constructs a String in
  /// resultStorage containing the given UTF-8.
  @asmname("swift_stringFromUTF8InRawMemory")
  static func _fromUTF8InRawMemory(
    resultStorage: UnsafePointer<String>,
    start: UnsafePointer<UTF8.CodeUnit>, utf8Count: Int
  ) {
    resultStorage.initialize(
        String._fromWellFormedCodeUnitSequence(UTF8.self,
            input: UnsafeArray(start: start, length: utf8Count)))
  }
}

/// String is a CollectionType of Character
extension String : CollectionType {
  // An adapter over UnicodeScalarView that advances by whole Character
  public struct Index : BidirectionalIndexType, Reflectable {
    public init(_ _base: UnicodeScalarView.Index) {
      self._base = _base
      self._lengthUTF16 = Index._measureExtendedGraphemeClusterForward(_base)
    }

    init(_ _base: UnicodeScalarView.Index, _ _lengthUTF16: Int) {
      self._base = _base
      self._lengthUTF16 = _lengthUTF16
    }

    public func successor() -> Index {
      _precondition(_base != _base._viewEndIndex, "can not increment endIndex")
      return Index(_endBase)
    }

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

  public var startIndex: Index {
    return Index(unicodeScalars.startIndex)
  }

  public var endIndex: Index {
    return Index(unicodeScalars.endIndex)
  }

  public subscript(i: Index) -> Character {
    return Character(String(unicodeScalars[i._base..<i._endBase]))
  }

  public func generate() -> IndexingGenerator<String> {
    return IndexingGenerator(self)
  }
  
  internal struct _IndexMirror : MirrorType {
    var _value: Index

    init(_ x: Index) {
      _value = x
    }

    public var value: Any { return _value }

    public var valueType: Any.Type { return (_value as Any).dynamicType }

    public var objectIdentifier: ObjectIdentifier? { return .None }

    public var disposition: MirrorDisposition { return .Aggregate }
    
    public var count: Int { return 0 }

    public subscript(i: Int) -> (String,MirrorType) { 
      _fatalError("MirrorType access out of bounds")
    }

    public var summary: String { return "\(_value._utf16Index)" }

    public var quickLookObject: QuickLookObject? { return .Some(.Int(Int64(_value._utf16Index))) }
  }
}

public func == (lhs: String.Index, rhs: String.Index) -> Bool {
  return lhs._base == rhs._base
}

extension String : Sliceable {
  public subscript(subRange: Range<Index>) -> String {
    return String(
      unicodeScalars[subRange.startIndex._base..<subRange.endIndex._base]._core)
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


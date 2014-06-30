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

@public struct String {
  @public init() {
    core = _StringCore()
  }

  init(_ _core: _StringCore) {
    self.core = _core
  }

  var core: _StringCore
}

extension String {
  static func _fromWellFormedCodeUnitSequence<
    Encoding: UnicodeCodec, Input: Collection
    where Input.GeneratorType.Element == Encoding.CodeUnit
  >(
    encoding: Encoding.Type, input: Input
  ) -> String {
    return String._fromCodeUnitSequence(encoding, input: input)!
  }

  static func _fromCodeUnitSequence<
    Encoding: UnicodeCodec, Input: Collection
    where Input.GeneratorType.Element == Encoding.CodeUnit
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

  static func _fromCodeUnitSequenceWithRepair<
    Encoding: UnicodeCodec, Input: Collection
    where Input.GeneratorType.Element == Encoding.CodeUnit
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
  static func _convertFromBuiltinExtendedGraphemeClusterLiteral(
    start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1) -> String {

    return String._fromWellFormedCodeUnitSequence(
      UTF8.self,
      input: UnsafeArray(start: UnsafePointer<UTF8.CodeUnit>(start), 
                         length: Int(byteSize)))
  }
}

extension String : ExtendedGraphemeClusterLiteralConvertible {
  @public static func convertFromExtendedGraphemeClusterLiteral(
    value: String
  ) -> String {
    return value
  }
}

extension String : _BuiltinUTF16StringLiteralConvertible {
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
        input: UnsafeArray(start: UnsafePointer<UTF8.CodeUnit>(start), length: 
                           Int(byteSize)))
    }
  }
}

extension String : StringLiteralConvertible {
  @public static func convertFromStringLiteral(value: String) -> String {
    return value
  }
}

extension String : DebugPrintable {
  @public var debugDescription: String {
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
  func _encodedLength<Encoding: UnicodeCodec>(encoding: Encoding.Type) -> Int {
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
    Encoding: UnicodeCodec,
    Output: Sink
    where Encoding.CodeUnit == Output.Element
  >(encoding: Encoding.Type, output: Output) 
  {
    return core.encode(encoding, output: output)
  }
}

extension String: Equatable {
}

@public func ==(lhs: String, rhs: String) -> Bool {
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

@public func <(lhs: String, rhs: String) -> Bool {
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
  @public var hashValue: Int {
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
  @public
  static func convertFromStringInterpolation(strings: String...) -> String {
    var result = String()
    for str in strings {
      result += str
    }
    return result
  }

  @public
  static func convertFromStringInterpolationSegment<T>(expr: T) -> String {
    return toString(expr)
  }
}

@public func +(var lhs: String, rhs: String) -> String {
  if (lhs.isEmpty) {
    return rhs
  }
  lhs._append(rhs)
  return lhs
}

@public func +(var lhs: String, rhs: Character) -> String {
  lhs._append(String(rhs))
  return lhs
}
@public func +(lhs: Character, rhs: String) -> String {
  var result = String(lhs)
  result._append(rhs)
  return result
}
@public func +(lhs: Character, rhs: Character) -> String {
  var result = String(lhs)
  result += String(rhs)
  return result
}


// String append
@assignment @public func += (inout lhs: String, rhs: String) {
  if (lhs.isEmpty) {
    lhs = rhs
  }
  else {
    lhs._append(rhs)
  }
}

@assignment @public func += (inout lhs: String, rhs: Character) {
  lhs += String(rhs)
}

// Comparison operators
// FIXME: Compare Characters, not code units
extension String : Comparable {  
}

extension String {
  /// Low-level construction interface used by introspection
  /// implementation in the runtime library.  Constructs a String in
  /// resultStorage containing the given UTF8.
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

/// String is a Collection of Character
extension String : Collection {
  // An adapter over UnicodeScalarView that advances by whole Character
  @public struct Index : BidirectionalIndex {
    init(_ _base: UnicodeScalarView.IndexType) {
      self._base = _base
    }

    @public func successor() -> Index {
       // For now, each Character will just be a single UnicodeScalar
      return Index(_base.successor())
    }
    @public func predecessor() -> Index {
       // For now, each Character will just be a single UnicodeScalar
      return Index(_base.predecessor())
    }
    let _base: UnicodeScalarView.IndexType

    /// The integer offset of this index in UTF16 text.  
    var _utf16Index: Int {
      return _base._position
    }
  }

  @public var startIndex: Index {
    return Index(unicodeScalars.startIndex)
  }
  
  @public var endIndex: Index {
    return Index(unicodeScalars.endIndex)
  }
  
  @public subscript(i: Index) -> Character {
    return Character(unicodeScalars[i._base])
  }

  @public func generate() -> IndexingGenerator<String> {
    return IndexingGenerator(self)
  }
}

@public func == (lhs: String.Index, rhs: String.Index) -> Bool {
  return lhs._base == rhs._base
}

extension String : Sliceable {
  @public subscript(subRange: Range<Index>) -> String {
    return String(
      unicodeScalars[subRange.startIndex._base..<subRange.endIndex._base]._core)
  }
}

// Algorithms
extension String {
  @public func join<
      S : Sequence where S.GeneratorType.Element == String
  >(elements: S) -> String{
    return Swift.join(self, elements)
  }
}


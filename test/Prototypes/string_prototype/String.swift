// Prototype code to end up in an eventual String.swift

import Swift

struct String {
  enum Contents {
    // Swift canonical string: UTF-16 in TODO-normal-form
    case canonical(SwiftCanonicalString)

    // 8-bit Latin1
    case latin1(Latin1String)

    #if false
    // Unknown: we are a buffer of bytes representing code units and an
    // associated encoding
    case mystery(UnsafeRawPointer, AnyUnicodeEncoding.Type) // TODO: AnyCodeUnits?

    case nsstring(UnsafeRawPointer) // TODO: what is payload?

    // TODO: small string forms
    case smol1(UInt)
    case smol2(UInt)
    case smol3(UInt)
    case smol4(UInt)
    #endif
  }

  var contents: Contents

  init(canonical str: SwiftCanonicalString) {
    self.contents = .canonical(str)
  }

  init(latin1 str: Latin1String) {
    self.contents = .latin1(str)
  }

  init() {
    self.contents = .canonical(SwiftCanonicalString())
  }
}

// TODO: make AnyUnicode conformance instead, type erase all the things
extension String : Unicode {
  typealias Encoding = SwiftCanonicalString.Encoding

  typealias CodeUnits = SwiftCanonicalString.CodeUnits
  var codeUnits: CodeUnits {
    switch contents {
    case .canonical(let str):
      return str.codeUnits
    default:
      fatalError("TODO")
    }
  }

  typealias ValidUTF8View = SwiftCanonicalString.ValidUTF8View
  var utf8: ValidUTF8View {
    switch contents {
    case .canonical(let str):
      return str.utf8
    default:
      fatalError("TODO")
    }
  }

  typealias ValidUTF16View = SwiftCanonicalString.ValidUTF16View
  var utf16: ValidUTF16View {
    switch contents {
    case .canonical(let str):
      return str.utf16
    default:
      fatalError("TODO")
    }
  }

  typealias ValidUTF32View = SwiftCanonicalString.ValidUTF32View
  var utf32: ValidUTF32View {
    switch contents {
    case .canonical(let str):
      return str.utf32
    default:
      fatalError("TODO")
    }
  }

  // TODO: this properly
  var unicodeScalars: LazyMapBidirectionalCollection<ValidUTF32View, UnicodeScalar> {
    switch contents {
    case .canonical(let str):
      return str.utf32.lazy.map { UnicodeScalar($0)! }
    default:
      fatalError("TODO")
    }
  }

  typealias ExtendedASCII = SwiftCanonicalString.ExtendedASCII
  var extendedASCII: ExtendedASCII {
    switch contents {
    case .canonical(let str):
      return str.extendedASCII
    default:
      fatalError("TODO")
    }
  }

  typealias Characters = SwiftCanonicalString.Characters
  // TODO: deprecate now String is a collection of Characters
  var characters: Characters {
    switch contents {
    case .canonical(let str):
      return str.characters
    default:
      fatalError("TODO")
    }
   }

  func isASCII(scan: Bool/* = true */) -> Bool {
    switch contents {
    case .canonical(let str):
      return str.isASCII(scan: scan)
    case .latin1(let str):
      return str.isASCII(scan: scan)
    default:
      fatalError("TODO")
    }
  }
  func isLatin1(scan: Bool/* = true */) -> Bool {
    switch contents {
    case .canonical(let str):
      return str.isLatin1(scan: scan)
    case .latin1:
      return true
    default:
      fatalError("TODO")
    }
  }
  func isNormalizedNFC(scan: Bool/* = true*/) -> Bool {
    switch contents {
    case .canonical(let str):
      return str.isNormalizedNFC(scan: scan)
    case .latin1:
      return true
    default:
      fatalError("TODO")
    }
  }
  func isNormalizedNFD(scan: Bool/* = true*/) -> Bool {
    switch contents {
    case .canonical(let str):
      return str.isNormalizedNFD(scan: scan)
    default:
      fatalError("TODO")
    }
  }
  func isInFastCOrDForm(scan: Bool/* = true*/) -> Bool {
    switch contents {
    case .canonical(let str):
      return str.isInFastCOrDForm(scan: scan)
    default:
      fatalError("TODO")
    }
  }
}

extension String : Comparable {
  static func ==(
    _ lhs: String, rhs: String
  ) -> Bool {
    switch (lhs.contents, rhs.contents) {
    case (.canonical(let lhsStr), .canonical(let rhsStr)):
      return lhsStr == rhsStr
    default:
      fatalError("TODO")
    }
  }
  static func <(
    _ lhs: String, rhs: String
  ) -> Bool {
    switch (lhs.contents, rhs.contents) {
    case (.canonical(let lhsStr), .canonical(let rhsStr)):
      return lhsStr < rhsStr
    default:
      fatalError("TODO")
    }
  }
}

extension String : Hashable {
  var hashValue : Int {
    switch contents {
    case .canonical(let representation):
      return representation.hashValue
    default:
      fatalError("TODO")
    }
  }
}


extension String : BidirectionalCollection {
  typealias Index = Characters.Index
  var startIndex: Index {
    return characters.startIndex
  }
  var endIndex: Index {
    return characters.endIndex
  }

  subscript(_ idx: Index) -> Character {
    return characters[idx]
  }

  func index(before idx: Index) -> Index {
    return characters.index(before: idx)
  }

  func index(after idx: Index) -> Index {
    return characters.index(after: idx)
  }
}

extension String /* : RangeReplaceableCollection */ {
  mutating func replaceSubrange<C: Collection>(
    _ subrange: Range<Int>, with newValues: C
  )
  where C.Iterator.Element == Character
  {
    switch contents {
    case .canonical(var str):
      // FIXME: the `str` var is a copy, and not a move, forcing CoW :(

      // Squash the stream of characters into utf16 
      str.replaceSubrange(subrange, with: newValues.lazy.flatMap { Swift.String($0).utf16 })
      
      self.contents = .canonical(str)
    default: 
      fatalError("TODO")
    }
  }
}

// TODO: reconsider these protocols
extension String : _ExpressibleByBuiltinUnicodeScalarLiteral {
  @effects(readonly)
  public // @testable
  init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    // self = String._fromWellFormedCodeUnitSequence(
    //   UTF32.self, input: CollectionOfOne(UInt32(value)))
    fatalError("Proto String._ExpressibleByBuiltinUnicodeScalarLiteral.init")
  }
}

extension String : ExpressibleByUnicodeScalarLiteral {
  /// Creates an instance initialized to the given Unicode scalar value.
  ///
  /// Do not call this initializer directly. It may be used by the compiler when
  /// you initialize a string using a string literal that contains a single
  /// Unicode scalar value.
  public init(unicodeScalarLiteral value: String) {
    // self = value
    fatalError("Proto String.ExpressibleByUnicodeScalarLiteral.init")
  }
}

extension String : _ExpressibleByBuiltinExtendedGraphemeClusterLiteral {
  @effects(readonly)
  // @_semantics("string.makeUTF8")
  public init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1) {
    // self = String._fromWellFormedCodeUnitSequence(
    //   UTF8.self,
    //   input: UnsafeBufferPointer(
    //     start: UnsafeMutablePointer<UTF8.CodeUnit>(start),
    //     count: Int(utf8CodeUnitCount)))
    fatalError("Proto String._ExpressibleByBuiltinExtendedGraphemeClusterLiteral.init")
  }
}

extension String : ExpressibleByExtendedGraphemeClusterLiteral {
  /// Creates an instance initialized to the given extended grapheme cluster
  /// literal.
  ///
  /// Do not call this initializer directly. It may be used by the compiler when
  /// you initialize a string using a string literal containing a single
  /// extended grapheme cluster.
  public init(extendedGraphemeClusterLiteral value: String) {
    // self = value
    fatalError("Proto String.ExpressibleByExtendedGraphemeClusterLiteral.init")
  }
}

extension String : _ExpressibleByBuiltinUTF16StringLiteral {
  @effects(readonly)
  @_semantics("string.makeUTF16")
  public init(
    _builtinUTF16StringLiteral start: Builtin.RawPointer,
    utf16CodeUnitCount: Builtin.Word
  ) {
    // TODO: constant literal support instead of copying
    // TODO: guarantee compiler provides properly canonicalized literals
    self = String(canonical: SwiftCanonicalString(
      codeUnits: UnsafeBufferPointer(start: UnsafePointer<UInt16>(start), 
                                     count: Int(utf16CodeUnitCount)),
      encodedWith: UTF16.self
    ))
  }
}

extension String : _ExpressibleByBuiltinStringLiteral {
  @effects(readonly)
  @_semantics("string.makeUTF8")
  public init(
    _builtinStringLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1) {
    if Bool(isASCII) {
      // TODO: replace this with Latin1 version when ready
      self = String(canonical: SwiftCanonicalString(
        codeUnits: UnsafeBufferPointer(start: UnsafePointer<UInt8>(start), 
                                       count: Int(utf8CodeUnitCount)),
        encodedWith: UTF8.self
      ))
    } else {
      // TODO: Does this ever happen today? 
      //       Need to consider where to insert compiler-native Latin1 support
      fatalError("Non-ASCII proto-string _ExpressibleByBuiltinStringLiteral")
      // Swift 3 string code:
  //     self = String._fromWellFormedCodeUnitSequence(
  //       UTF8.self,
  //       input: UnsafeBufferPointer(
  //         start: UnsafeMutablePointer<UTF8.CodeUnit>(start),
  //         count: Int(utf8CodeUnitCount)))
    }
  }
}

extension String : ExpressibleByStringLiteral {
  public init(stringLiteral value: String) {
    // TODO: is this ever called if the _ version is available?
    fatalError("ExpressibleByStringLiteral")
    // self = value
  }
}

extension String : CustomStringConvertible {
  // TODO: once this replaces Swift.String this needs to revert
  // back to the trivial implementation of returnning self
  public var description: Swift.String {
    return Swift.String(self)
  }
}

extension String : CustomDebugStringConvertible {
  public var debugDescription: Swift.String {
    var result = "\""
    for us in self.unicodeScalars {
      result += us.escaped(asASCII: false)
    }
    result += "\""
    return result
  }
}


extension String {
  /// Constructs a `String` having the same contents as `nulTerminatedUTF8`.
  ///
  /// - Parameter nulTerminatedUTF8: a sequence of contiguous UTF-8 encoded 
  ///   bytes ending just before the first zero byte (NUL character).
  init(cString nulTerminatedUTF8: UnsafePointer<CChar>) {
    let len = UTF8._nullCodeUnitOffset(in: nulTerminatedUTF8)
    self = nulTerminatedUTF8.withMemoryRebound(to: UInt8.self, capacity: len) {
      // TODO: use String.init that detects optimal storage
      String(canonical: SwiftCanonicalString(
        codeUnits: UnsafeBufferPointer(start: $0, count: len),
        encodedWith: UTF8.self
      ))
    }
  }
  
  /// Constructs a `String` having the same contents as `nulTerminatedCodeUnits`.
  ///
  /// - Parameter nulTerminatedCodeUnits: a sequence of contiguous code units in
  ///   the given `encoding`, ending just before the first zero code unit.
  /// - Parameter encoding: describes the encoding in which the code units
  ///   should be interpreted.
  init<Encoding: UnicodeEncoding>(
    cString nulTerminatedCodeUnits: UnsafePointer<Encoding.CodeUnit>,
    encoding: Encoding.Type) {
      let len = Encoding._nullCodeUnitOffset(in: nulTerminatedCodeUnits)
      // TODO: use String.init that detects optimal storage
      self = String(canonical: SwiftCanonicalString(
        codeUnits: UnsafeBufferPointer(start: nulTerminatedCodeUnits, count: len),
        encodedWith: Encoding.self
      ))
  }
    
  /// Invokes the given closure on the contents of the string, represented as a
  /// pointer to a null-terminated sequence of UTF-8 code units.
  func withCString<Result>(
    _ body: (UnsafePointer<CChar>) throws -> Result) rethrows -> Result {
        // TODO: improve for Latin1 case
        var result = ContiguousArray<CChar>()
        result.reserveCapacity(utf8.count + 1)
        for c in utf8 {
          result.append(CChar(bitPattern: c))
        }
        result.append(0)
        return try result.withUnsafeBufferPointer {
          try body($0.baseAddress!)
        }
    }
}



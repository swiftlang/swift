// Prototype code to end up in an eventual String.swift

import Swift

// The preferred string format for Swift. In-memory UTF16 encoding in TODO-
// normal-form.
struct SwiftCanonicalString {
  typealias CodeUnits = _StringBuffer<UInt16>
  typealias Encoding = UTF16
  typealias Storage = UnicodeStorage<CodeUnits, Encoding>
  var storage: Storage

  // Store some bits TODO: should be packed into our storage ref?
  //
  // Always set at construction time, conservatively updated at modification
  var isKnownASCII: Bool
  var isKnownLatin1: Bool

  // Perform a copy, transcoding, and normalization of the supplied code units
  init<OtherCodeUnits, OtherEncoding>(
    _ other: UnicodeStorage<OtherCodeUnits, OtherEncoding>
  ) {
    // FIXME: do normalization on the fly, perhaps a normalized view?
    let otherUTF16 = other.transcoded(to: Encoding.self)

    // TODO: more effient to allocate too much space (guessed by encoding
    // sizes), and copy in, rather than linear time count operation
    let newCount = otherUTF16.count

    let newStringStorage = _StringStorage<UInt16>(
      count: newCount, minimumCapacity: newCount
    )

    // Start off as true, we will unset when we find a violation
    self.isKnownASCII = true
    self.isKnownLatin1 = true

    // Copy in
    // FIXME: why can't I use .indices below?
    for (idx, elt) in zip(0..<newCount, otherUTF16) {
      if (elt > 0xff) {
        self.isKnownLatin1 = false
        self.isKnownASCII = false
      } else if (elt > 0x7f) {
        isKnownASCII = false
      }
      newStringStorage[idx] = elt
    }

    self.storage = UnicodeStorage(
      CodeUnits(newStringStorage),
      Encoding.self
    )
  }
}

extension SwiftCanonicalString {
  init<
    OtherCodeUnits: RandomAccessCollection,
    OtherEncoding: UnicodeEncoding
  >
  (
    codeUnits: OtherCodeUnits, encodedWith otherEncoding: OtherEncoding.Type
  )
  where
    OtherEncoding.EncodedScalar.Iterator.Element == OtherCodeUnits.Iterator.Element,
    OtherCodeUnits.SubSequence : RandomAccessCollection,
    OtherCodeUnits.SubSequence.Index == OtherCodeUnits.Index,
    OtherCodeUnits.SubSequence.SubSequence == OtherCodeUnits.SubSequence,
    OtherCodeUnits.SubSequence.Iterator.Element == OtherCodeUnits.Iterator.Element
  {
    self.init(UnicodeStorage(codeUnits, otherEncoding))
  }
}

extension SwiftCanonicalString : Unicode {
  var codeUnits: CodeUnits { return storage.codeUnits }

  typealias ValidUTF8View = Storage.TranscodedView<UTF8>
  var utf8: ValidUTF8View { return ValidUTF8View(codeUnits) }

  typealias ValidUTF16View = Storage.TranscodedView<UTF16>
  var utf16: ValidUTF16View { return ValidUTF16View(codeUnits) }

  typealias ExtendedASCII = LazyMapRandomAccessCollection<CodeUnits, UInt32>
  var extendedASCII: ExtendedASCII {
    return codeUnits.lazy.map { UInt32($0) }
  }

  typealias ValidUTF32View = Storage.TranscodedView<UTF32>
  var utf32: ValidUTF32View { return ValidUTF32View(codeUnits) }

  // typealias EncodedScalars = Storage.EncodedScalars
  // var encodedScalars: EncodedScalars {
  //   return storage.scalars
  // }

  typealias Characters = Storage.Characters
  var characters: Characters {
    return storage.characters
  }

  func isASCII(scan: Bool = true) -> Bool {
    if isKnownASCII {
      return true
    }
    return scan && false // TODO: perform scan?
  }
  func isLatin1(scan: Bool = true) -> Bool {
    if isKnownLatin1 {
      return true
    }
    return scan && false // TODO: perform scan?
  }
  func isNormalizedNFC(scan: Bool = true) -> Bool {
    // TODO: is this the ideal normal form for us?
    return true
  }
  func isNormalizedNFD(scan: Bool = true) -> Bool {
    return false
    // TODO: perform scan perhaps? If every scalar is a whole grapheme, then
    // this would be true
  }
  func isInFastCOrDForm(scan: Bool = true) -> Bool {
    // FIXME: *almost* all NFC is FCC, but not all ...
    return true
  }
}

// Super dumb comparable conformance...
extension SwiftCanonicalString : Comparable {
  static func ==(
    _ lhs: SwiftCanonicalString, rhs: SwiftCanonicalString
  ) -> Bool {
    return lhs.storage.ordered(with: rhs.storage) == .same
  }
  static func <(
    _ lhs: SwiftCanonicalString, rhs: SwiftCanonicalString
  ) -> Bool {
    return lhs.storage.ordered(with: rhs.storage) == .before
  }
}

struct String {
  enum Contents {
    // Swift canonical string: UTF-16 in TODO-normal-form
    case canonical(SwiftCanonicalString)

    // 8-bit Latin1
    case latin1([UInt8]) // TODO: AnyUTF8? UnicodeStorage? Latin1String?

    // Unknown: we are a buffer of bytes representing code units and an
    // associated encoding
    case mystery(UnsafeRawPointer, AnyUnicodeEncoding.Type) // TODO: AnyCodeUnits?

    case nsstring(UnsafeRawPointer) // TODO: what is payload?

    // TODO: small string forms
    case smol1(UInt)
    case smol2(UInt)
    case smol3(UInt)
    case smol4(UInt)
  }

  var contents: Contents

  init(_ str: SwiftCanonicalString) {
    self.contents = .canonical(str)
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
    default:
      fatalError("TODO")
    }
  }
  func isLatin1(scan: Bool/* = true */) -> Bool {
    switch contents {
    case .canonical(let str):
      return str.isLatin1(scan: scan)
    default:
      fatalError("TODO")
    }
  }
  func isNormalizedNFC(scan: Bool/* = true*/) -> Bool {
    switch contents {
    case .canonical(let str):
      return str.isNormalizedNFC(scan: scan)
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
    self = String(SwiftCanonicalString(
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
      self = String(SwiftCanonicalString(
        codeUnits: UnsafeBufferPointer(start: UnsafePointer<UInt8>(start), 
                                       count: Int(utf8CodeUnitCount)),
        encodedWith: UTF8.self
      ))
    } else {
      // TODO: does this ever happen today? Need to consider where to insert compiler-native Latin1 support
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

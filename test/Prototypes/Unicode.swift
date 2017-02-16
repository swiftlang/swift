// RUN: %target-build-swift %s -parse-stdlib -Xfrontend -disable-access-control -Onone -o %t
// RUN: %target-run %t
// REQUIRES: executable_test

import Swift
import SwiftShims
import StdlibUnittest

var testSuite = TestSuite("t")

///
/// Unicode.swift
///

protocol Unicode {
  associatedtype Encoding: UnicodeEncoding
  associatedtype CodeUnits: RandomAccessCollection
  /* where CodeUnits.Iterator.Element == Encoding.CodeUnit */
  var codeUnits: CodeUnits { get }

  associatedtype ValidUTF8View : BidirectionalCollection
  // where ValidUTF8View.Iterator.Element == UTF8.CodeUnit */
  // = TranscodedView<CodeUnits, Encoding, UTF8>
  var utf8: ValidUTF8View { get }

  associatedtype ValidUTF16View : BidirectionalCollection
  // where ValidUTF16View.Iterator.Element == UTF16.CodeUnit
  // = TranscodedView<CodeUnits, Encoding, UTF16>
  var utf16: ValidUTF16View { get }

  associatedtype ValidUTF32View : BidirectionalCollection
  // where ValidUTF32View.Iterator.Element == UTF32.CodeUnit
  // = TranscodedView<CodeUnits, Encoding, UTF32>
  var utf32: ValidUTF32View { get }

  associatedtype ExtendedASCII : BidirectionalCollection // FIXME: Can this be Random Access?
  /* where ExtendedASCII.Iterator.Element == UInt32 */
  var extendedASCII: ExtendedASCII { get }

  associatedtype Characters : BidirectionalCollection
  /* where Characters.Iterator.Element == Character */
  var characters: Characters { get }

  func isASCII(scan: Bool/* = true */) -> Bool
  func isLatin1(scan: Bool/* = true */) -> Bool
  func isNormalizedNFC(scan: Bool/* = true*/) -> Bool
  func isNormalizedNFD(scan: Bool/* = true*/) -> Bool
  func isInFastCOrDForm(scan: Bool/* = true*/) -> Bool
}

///
/// UnicodeStorage.swift
///

extension UnicodeStorage {
  func transcoded<OtherEncoding: UnicodeEncoding>(
    to otherEncoding: OtherEncoding.Type
  ) -> TranscodedView<OtherEncoding> {
    return type(of: self).TranscodedView(self.codeUnits, to: otherEncoding)
  }

  typealias Characters = CharacterView<CodeUnits, Encoding>
  var characters: Characters {
    return Characters(codeUnits, Encoding.self)
  }
}

testSuite.test("CharacterView") {
  // FIXME: precondition checks in Character prevent us from trying this last
  // one.
  let s = "🇸🇸🇬🇱abc🇱🇸🇩🇯🇺🇸\nΣὲ 👥🥓γ͙᷏̃̂᷀νω" // + "👩‍❤️‍👩"
  let a: [Character] = [
    "🇸🇸", "🇬🇱", "a", "b", "c", "🇱🇸", "🇩🇯", "🇺🇸", "\n",
    "Σ", "ὲ", " ", "👥", "🥓", "γ͙᷏̃̂᷀", "ν", "ω"
  ] // + "👩‍❤️‍👩"

  // FIXME: the generic arguments should be deducible, but aren't; <rdar://30323161>
  let v8 = UnicodeStorage<Array<UInt8>, UTF8>.CharacterView(Array(s.utf8), UTF8.self)
  expectEqual(a, Array(v8))

  // FIXME: We need to wrap s.utf16 in Array because of <rdar://30386193> Unaccountable link errors
  // FIXME: the generic arguments should be deducible; <rdar://30323161>
  let v16 = UnicodeStorage<Array<UInt16>, UTF16>.CharacterView(Array(s.utf16), UTF16.self)
  expectEqual(a, Array(v16))

  expectEqual(v8.reversed(), a.reversed())
  expectEqual(v16.reversed(), a.reversed())

  // This one demonstrates that we get grapheme breaking of regional indicators
  // (RI) right, while Swift 3 string does not.
  expectFalse(a.elementsEqual(s.characters))
}

///
/// StringStorage.swift
///

protocol CopyConstructible {  }
extension CopyConstructible {
  init(_ me: Self) {
    self = me
  }
}

/// - Requires: Element is trivial (UInt8/UInt16)
@_versioned
final class _StringStorage<Element: UnsignedInteger>
 : /*_SwiftNativeNSString,*/ _NSStringCore, CopyConstructible {

  var count: Int
  var capacity: Int // Should be a let.

  internal init(DoNotCallMe: ()) { count = 0; capacity = 0 }

  convenience init(count: Int, minimumCapacity: Int) {
    self.init(
      Builtin.allocWithTailElems_1(
      _StringStorage.self,
        Swift.max(count, minimumCapacity)._builtinWordValue, Element.self))

    let storageAddr = UnsafeMutableRawPointer(
      Builtin.projectTailElems(self, Element.self))
    let endAddr = storageAddr + _swift_stdlib_malloc_size(storageAddr)
    let realCapacity = endAddr.assumingMemoryBound(to: Element.self)
      - storageAddr.assumingMemoryBound(to: Element.self)

    // All stored properties are uninitialized, but we can use assignment
    // because they're trivial types.
    self.count = count
    self.capacity = realCapacity
  }

  @_versioned
  internal func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<Element>)->R
  ) -> R {
    defer { _fixLifetime(self) }
    return body(
      UnsafeMutableBufferPointer(
        start: UnsafeMutablePointer(
          Builtin.projectTailElems(self, Element.self)),
        count: count
      )
    )
  }

  @_versioned
  internal func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>)->R
  ) -> R {
    return withUnsafeMutableBufferPointer {
      body(UnsafeBufferPointer(start: UnsafePointer($0.baseAddress), count: count))
    }
  }
/*
}
// TODO: JIRA for error: @objc is not supported within extensions of generic classes
extension _StringStorage : _NSStringCore {
*/

  @objc
  func length() -> Int {
    return count
  }

  @objc
  func characterAtIndex(_ index: Int) -> UInt16 {
    return numericCast(withUnsafeBufferPointer { $0[index] })
  }

  @objc
  func _fastCharacterContents() -> UnsafeMutablePointer<UInt16>? {
    guard Element.self is UInt16.Type else { return nil }
    return UnsafeMutablePointer<UInt16>(
      Builtin.projectTailElems(self, Element.self))
  }

  @objc(copyWithZone:)
  internal func copy(with _: _SwiftNSZone?) -> AnyObject {
    return self
  }
}

extension _StringStorage : RandomAccessCollection, MutableCollection {
  var startIndex : Int { return 0 }
  var endIndex : Int { return count }

  subscript(i: Int) -> Element {
    // FIXME: Add addressors
    get {
      return withUnsafeBufferPointer { $0[i] }
    }
    set {
      withUnsafeMutableBufferPointer { $0[i] = newValue }
    }
  }
}

/// - Requires: Element is trivial (UInt8/UInt16)
struct _StringBuffer<Element: UnsignedInteger> {
  internal var _storage: _StringStorage<Element>
}

extension _StringBuffer : RandomAccessCollection, MutableCollection {
  init(_ storage: _StringStorage<Element>) { _storage = storage }

  var startIndex : Int { return _storage.startIndex }
  var endIndex : Int { return _storage.endIndex }

  subscript(i: Int) -> Element {
    // FIXME: Add addressors
    get {
      return _storage[i]
    }
    set {
      _storage[i] = newValue
    }
  }
}

extension _StringBuffer : RangeReplaceableCollection {
  init() {
    // FIXME: replace with EmptyStringStorage
    self.init(_StringStorage(count: 0, minimumCapacity: 0))
  }

  mutating func replaceSubrange<C: Collection>(
    _ target: Range<Index>, with source: C
  )
  where C.Iterator.Element == Element
  {
    let growth = numericCast(source.count) -
      distance(from: target.lowerBound, to: target.upperBound)

    let newCount = count + growth

    if _fastPath(newCount <= _storage.capacity) {
      _storage.withUnsafeMutableBufferPointer { elements in
        if growth > 0 {
          fatalError("implement me!")
        }
        else {
          fatalError("implement me!")
        }
      }
      return
    }
    else {
      fatalError("replace _storage with a new one that copies the elements")
    }
  }
}

///
/// String.swift
///

// The preferred string format for Swift. In-memory UTF16 encoding in TODO-
// normal-form
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
    return lhs.characters.elementsEqual(rhs.characters)
  }
  static func <(
    _ lhs: SwiftCanonicalString, rhs: SwiftCanonicalString
  ) -> Bool {
    for (lhsChar, rhsChar) in zip(lhs.characters, rhs.characters) {
      if lhsChar != rhsChar {
        return lhsChar < rhsChar
      }
    }
    return lhs.characters.count < rhs.characters.count
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

extension String : Equatable {
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

testSuite.test("SwiftCanonicalString") {
  let s = "abcdefghijklmnopqrstuvwxyz\n"
  + "🇸🇸🇬🇱🇱🇸🇩🇯🇺🇸\n"
  + "Σὲ 👥🥓γνωρίζω ἀπὸ τὴν κόψη χαῖρε, ὦ χαῖρε, ᾿Ελευθεριά!\n"
  + "Οὐχὶ ταὐτὰ παρίσταταί μοι γιγνώσκειν, ὦ ἄνδρες ᾿Αθηναῖοι,\n"
  + "გთხოვთ ახლავე გაიაროთ რეგისტრაცია Unicode-ის მეათე საერთაშორისო\n"
  + "Зарегистрируйтесь сейчас на Десятую Международную Конференцию по\n"
  + "  ๏ แผ่นดินฮั่นเสื่อมโทรมแสนสังเวช  พระปกเกศกองบู๊กู้ขึ้นใหม่\n"
  + "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ"
  let s32 = s.unicodeScalars.lazy.map { $0.value }
  let s16 = Array(s.utf16)
  let s8 = Array(s.utf8)
  let s16to32 = UnicodeStorage.TranscodedView(s16, from: UTF16.self, to: UTF32.self)
  let s16to8 = UnicodeStorage.TranscodedView(s16, from: UTF16.self, to: UTF8.self)
  let s8to16 = UnicodeStorage.TranscodedView(s8, from: UTF8.self, to: UTF16.self)
  let s8Vto16 = UnicodeStorage.TranscodedView(s8, from: ValidUTF8.self, to: UTF16.self)

  let sncFrom32 = String(SwiftCanonicalString(
    codeUnits: s32.map { $0 }, encodedWith: UTF32.self
  ))
  let sncFrom16 = String(SwiftCanonicalString(
    codeUnits: s16, encodedWith: UTF16.self
  ))
  let sncFrom8 = String(SwiftCanonicalString(
    codeUnits: s8.map { $0 }, encodedWith: UTF8.self
  ))
  let sncFrom16to32 = String(SwiftCanonicalString(
    codeUnits: s16to32.map { $0 }, encodedWith: UTF32.self
  ))
  let sncFrom16to8 = String(SwiftCanonicalString(
    codeUnits: s16to8.map { $0 }, encodedWith: UTF8.self
  ))
  let sncFrom8to16 = String(SwiftCanonicalString(
    codeUnits: s8to16.map { $0 }, encodedWith: UTF16.self
  ))

  expectEqual(sncFrom32, sncFrom16)
  expectEqual(sncFrom16, sncFrom8)
  expectEqual(sncFrom8, sncFrom16to32)
  expectEqual(sncFrom16to32, sncFrom16to8)
  expectEqual(sncFrom16to8, sncFrom8to16)
}


///
/// Prototypes/Latin1String.swift
///

struct Latin1String<Base : RandomAccessCollection> : Unicode
where Base.Iterator.Element == UInt8, Base.Index == Base.SubSequence.Index,
Base.SubSequence.SubSequence == Base.SubSequence,
Base.SubSequence : RandomAccessCollection,
Base.Iterator.Element == UInt8,
Base.SubSequence.Iterator.Element == Base.Iterator.Element {
  typealias Encoding = Latin1
  typealias CodeUnits = Base
  typealias Storage = UnicodeStorage<Base, Latin1>
  let storage: Storage
  let _isASCII: Bool?
  var codeUnits: CodeUnits { return storage.codeUnits }

  init(_ codeUnits: CodeUnits, isASCII: Bool? = nil) {
    self.storage = UnicodeStorage(codeUnits)
    self._isASCII = isASCII
  }

  typealias Characters = LazyMapRandomAccessCollection<CodeUnits, Character>
  var utf8: ValidUTF8View { return ValidUTF8View(codeUnits) }

  typealias ExtendedASCII = LazyMapRandomAccessCollection<CodeUnits, UInt32>
  var utf16: ValidUTF16View { return ValidUTF16View(codeUnits) }

  typealias ValidUTF32View = Storage.TranscodedView<UTF32>
  var utf32: ValidUTF32View { return ValidUTF32View(codeUnits) }

  typealias ValidUTF16View = Storage.TranscodedView<UTF16>
  var extendedASCII: ExtendedASCII {
    return codeUnits.lazy.map { UInt32($0) }
  }

  typealias ValidUTF8View = Storage.TranscodedView<UTF8>
  var characters: Characters {
    return codeUnits.lazy.map {
      Character(UnicodeScalar(UInt32($0))!)
    }
  }

  func isASCII(scan: Bool = true) -> Bool {
    if let result = _isASCII { return result }
    return scan && !codeUnits.contains { $0 > 0x7f }
  }
  func isLatin1(scan: Bool = true) -> Bool {
    return true
  }
  func isNormalizedNFC(scan: Bool = true) -> Bool {
    return true
  }
  func isNormalizedNFD(scan: Bool = true) -> Bool {
    return true
  }
  func isInFastCOrDForm(scan: Bool = true) -> Bool {
    return true
  }
}

testSuite.test("basic") {
  let s = "abcdefghijklmnopqrstuvwxyz\n"
  + "🇸🇸🇬🇱🇱🇸🇩🇯🇺🇸\n"
  + "Σὲ 👥🥓γνωρίζω ἀπὸ τὴν κόψη χαῖρε, ὦ χαῖρε, ᾿Ελευθεριά!\n"
  + "Οὐχὶ ταὐτὰ παρίσταταί μοι γιγνώσκειν, ὦ ἄνδρες ᾿Αθηναῖοι,\n"
  + "გთხოვთ ახლავე გაიაროთ რეგისტრაცია Unicode-ის მეათე საერთაშორისო\n"
  + "Зарегистрируйтесь сейчас на Десятую Международную Конференцию по\n"
  + "  ๏ แผ่นดินฮั่นเสื่อมโทรมแสนสังเวช  พระปกเกศกองบู๊กู้ขึ้นใหม่\n"
  + "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ"
  let s32 = s.unicodeScalars.lazy.map { $0.value }
  let s16 = Array(s.utf16)
  let s8 = Array(s.utf8)
  let s16to32 = UnicodeStorage.TranscodedView(s16, from: UTF16.self, to: UTF32.self)
  let s16to8 = UnicodeStorage.TranscodedView(s16, from: UTF16.self, to: UTF8.self)
  let s8to16 = UnicodeStorage.TranscodedView(s8, from: UTF8.self, to: UTF16.self)
  let s8Vto16 = UnicodeStorage.TranscodedView(s8, from: ValidUTF8.self, to: UTF16.self)
  print(Array(s32))
  print(Array(s16to32))
  expectTrue(s32.elementsEqual(s16to32))
  expectTrue(s8.elementsEqual(s16to8))
  expectTrue(s16.elementsEqual(s8to16))
  expectTrue(s16.elementsEqual(s8Vto16))

  expectTrue(s32.reversed().elementsEqual(s16to32.reversed()))
  expectTrue(s8.reversed().elementsEqual(s16to8.reversed()))
  expectTrue(s16.reversed().elementsEqual(s8to16.reversed()))
  expectTrue(s16.reversed().elementsEqual(s8Vto16.reversed()))

  do {
    // We happen to know that alphabet is non-ASCII, but we're not going to say
    // anything about that.
    let alphabet = Latin1String(s8.prefix(27))
    expectTrue(alphabet.isASCII())
    expectFalse(alphabet.isASCII(scan: false))

    // We know that if you interpret s8 as Latin1, it has a lot of non-ASCII
    let nonASCII = Latin1String(s8)
    expectFalse(nonASCII.isASCII(scan: true))
    expectFalse(nonASCII.isASCII(scan: false))
  }

  do {
    let alphabet = Latin1String(s8.prefix(27), isASCII: true)
    let nonASCII = Latin1String(s8, isASCII: false)
    expectTrue(alphabet.isASCII())
    expectTrue(alphabet.isASCII(scan: false))
    expectFalse(nonASCII.isASCII(scan: true))
    expectFalse(nonASCII.isASCII(scan: false))
  }
}

///
/// UnicodeEncoding.swift
///

// TODO: put these on protocol decl, and have impls provide functionality
// instead
//
// The below establishes the concept of a *trivially-decodable* code unit.A
// trivially-decodable code unit can be decoded into its unicode scalar merely
// by zero-extending the value. It represents an entire unicode scalar all on
// its own. Trivially decodable code units can be more efficiently compared.
extension UnicodeEncoding
where
  EncodedScalar.Iterator.Element : UnsignedInteger
{
  // The trivial-decodable encodings: An encoding is trivially-decodable if
  // all possible code units expressed in that encoding are trivially-
  // decodable
  //
  // TODO: all code units, or all valid code units?
  static var isTriviallyDecodableEncoding: Bool {
    if (Self.self == Latin1.self) {
      return true
    }
    if (Self.self == UTF32.self) {
      // TODO: valid only? what about surrogate scalars (which are invalid)?
      //
      // For reference, Unicode 9.0,S3.9,D76 defined "Unicode scalar value" as
      // excluding surrogates. i.e.:
      //  ranges 0 to D7FF16 and E00016 to 10FFFF16, inclusive.

      return true
    }

    // TODO: others? valid variants?

    return false
  }

  // Whether the provided code unit is trivially decodable in this encoding.
  static func isTriviallyDecodable(_ codeUnit: CodeUnit) -> Bool {
    if isTriviallyDecodableEncoding {
      return true
    }

    // The encodings with thresholds: All code units below a certain threshold
    // are trivial
    if (Self.self == UTF8.self) {
      // Only ASCII
      return codeUnit < 0x80
    }
    if (Self.self == UTF16.self) {
      // All BMP scalars, that is anything not in a surrogate range.
      //
      // For efficiency and simplicity, currently implemented as a threshold.
      // But in theory, we could include scalars above surrogate ranges. But
      // that might be murkey for private use areas and might be more likely to
      // be affected by churn in future Unicode versions.
      //
      // TODO: cost/benefit investigation of more complex check here...
      return codeUnit < 0xD800
    }

    fatalError("unimplemented")
  }
}

// TODO: notion of mutually-trivially-decodable, where each code unit is
// trivially-decodable in its own encoding as well as the other one. For
// example, ASCII code units are mutually-trivially-decodable between UTF8 and
// UTF16, but not Latin1 code units.


// For now, sort order semantics are: the unicode scalar value order when in
// TODO-normal-form. This may be refined more in the future as I learn more
// about Unicode.


//
// Implementation-side, boils down to 4 general levels of comparision. These are
// presented from fastest (and most restricively applicable) to slowest (and
// most generally applicable), and thus in the order of preference if possible.
//
//  1) bitwise comparison: compare bits; roughly equivalent to C's memcmp.
//
//  2) zero-extend and compare: zero extend the smaller code unit and then
//  compare bits. Done using a lazy zero-extending view.
//
//  3) scalar value comparison: compare the unicode scalar values, and sort
//  based on those. This is a decode-and-bit-compare. Implemented through a
//  transcoded view targeting (valud?) UTF32. Fast path: use bit/zext comparison
//  whenever both code units happen to be trivially-decodable.
//   
//  4) normalized scalar comparison: compare sequence of normalized scalar
//  values. Currently implemented as grapheme-based comparison, but that does
//  more work than strictly necessary. Need to make a normalized view, then we
//  can just compare across that. Fast path: so long as the next CUs begin a new
//  grapheme, can compare non-normalized: scalars as above and corresponding
//  fast-paths from above.
//


//
// TODO: notion of mutually-bitwise-comparable encodings, wherein strings in
// these encodings can be ordered through bitwise comparison alone. I need a
// better name, as this notion needs to be paired with TODO-normal-form. This
// happens when the encodings are the same, or when they have the same bitsize
// and each of their contents are mutually-trivially-decodable.
//

//  TODO: notion of a equivalence-class-break, or a code unit that is definitely
// part of a different ...comparison unit?... than the one preceeding it. All
// grapheme breaks are equivalence-class-breaks, but some additional unicode
// scalars are as well...
//

//
// When they apply:
//
//  #1 can be used on entire strings when all of the following are satisfied:
//    * Both strings are in TODO-normal-form
//    * Both strings are mutually-bitwise-comparable
//    
//  #2 can be used on entire strings when all of the following are satisfied:
//    * Both strings are in TODO-normal-form
//    * Both strings are mutually-bitwise-comparable-modulo-zero-extension TODO:
//
//  #3 can be used on entire strings when all of the following are satisfied:
//    * Both strings are in TODO-normal-form
//
//  #4 is used for all the rest.
//
//  Note that this is a property of comparing two particular strings in two
//  particular encodings. The faster methods can situationally be used for
//  comparing segments of strings when some conditions are satisfied.
//

//
// Fast paths
//
// #1 needs no fast paths, it *is* the fastest path.
//
// #2 needs no fast paths. If #1 isn't possible, then #2 *is* the fastest path.
//
// #3 can do bitwise/zero-extension compare whenever both compared code units
//    are mutually-trivially-decodable.
//
// #4 can do bitwise/zero-extension compare whenever both compared code units are mutually-trivially-decodable AND to following code units  definitely do 
//
//


// Test that all trivially-decodable code units are in fact trivially-
// decodable.
testSuite.test("trivially-decodable") {
  let aceOfSpacesScalarValue: UInt32 = 0x1F0A1 // "🂡"
  let aceOfSpacesScalar = UnicodeScalar(aceOfSpacesScalarValue)!

  //
  // UTF16
  //
  for i in 0..<0xD800 {
    expectTrue(UTF16.isTriviallyDecodable(UInt16(i)))
    let utf16CUs = UTF16.EncodedScalar(UnicodeScalar(i)!)
    expectEqual(utf16CUs.count, 1)
    expectEqual(UInt32(utf16CUs[0]), UInt32(i))
  }

  // Test 🂡
  let utf16CUs = UTF16.EncodedScalar(aceOfSpacesScalar)
  expectEqual(utf16CUs.count, 2)
  expectFalse(UTF16.isTriviallyDecodable(utf16CUs[0]))
  expectFalse(UTF16.isTriviallyDecodable(utf16CUs[1]))
  expectEqual(utf16CUs[0], 0xD83C)
  expectEqual(utf16CUs[1], 0xDCA1)

  //
  // UTF8
  //
  for i in 0..<0x80 {
    expectTrue(UTF8.isTriviallyDecodable(UInt8(i)))
    let utf8CUs = UTF8.EncodedScalar(UnicodeScalar(i)!)
    expectEqual(utf8CUs.count, 1)
    expectEqual(UInt32(utf8CUs[0]), UInt32(i))
  }

  // Test 🂡
  let utf8CUs = UTF8.EncodedScalar(aceOfSpacesScalar)
  expectEqual(utf8CUs.count, 4)
  expectFalse(UTF8.isTriviallyDecodable(utf8CUs[0]))
  expectFalse(UTF8.isTriviallyDecodable(utf8CUs[1]))
  expectFalse(UTF8.isTriviallyDecodable(utf8CUs[2]))
  expectFalse(UTF8.isTriviallyDecodable(utf8CUs[3]))
  expectEqual(utf8CUs[0], 0xF0)
  expectEqual(utf8CUs[1], 0x9F)
  expectEqual(utf8CUs[2], 0x82)
  expectEqual(utf8CUs[3], 0xA1)
}

///
/// String comparison
///


// TODO: incorporate more gracefully, rather than hacky free functions






runAllTests()

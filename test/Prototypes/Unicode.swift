// RUN: rm -f %t && %target-build-swift -I %S/icu -licucore %s -o %t
// RUN: %target-run %t
// REQUIRES: executable_test

import StdlibUnittest
import ICU

//===----------------------------------------------------------------------===//
//===--- Logging ----------------------------------------------------------===//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//

protocol Unicode {
  associatedtype Encoding: UnicodeEncoding
  associatedtype CodeUnits: RandomAccessCollection
  /* where CodeUnits.Iterator.Element == Encoding.CodeUnit */
  var codeUnits: CodeUnits {get}
  
  associatedtype ValidUTF8View : BidirectionalCollection
  // where ValidUTF8View.Iterator.Element == UTF8.CodeUnit */
  // = TranscodedView<CodeUnits, Encoding, UTF8>
  var utf8: ValidUTF8View {get}
  
  associatedtype ValidUTF16View : BidirectionalCollection
  // where ValidUTF16View.Iterator.Element == UTF16.CodeUnit
  // = TranscodedView<CodeUnits, Encoding, UTF16>
  var utf16: ValidUTF16View {get}
  
  associatedtype ValidUTF32View : BidirectionalCollection
  // where ValidUTF32View.Iterator.Element == UTF32.CodeUnit
  // = TranscodedView<CodeUnits, Encoding, UTF32>
  var utf32: ValidUTF32View {get}
  
  associatedtype ExtendedASCII : BidirectionalCollection // FIXME: Can this be Random Access?
  /* where ExtendedASCII.Iterator.Element == UInt32 */
  var extendedASCII: ExtendedASCII {get}

  associatedtype Characters : BidirectionalCollection
  /* where Characters.Iterator.Element == Character */
  var characters: Characters { get }
  
  func isASCII(scan: Bool/* = true */) -> Bool 
  func isLatin1(scan: Bool/* = true */) -> Bool 
  func isNormalizedNFC(scan: Bool/* = true*/) -> Bool
  func isNormalizedNFD(scan: Bool/* = true*/) -> Bool
  func isInFastCOrDForm(scan: Bool/* = true*/) -> Bool
}

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
  
  typealias ValidUTF8View = Storage.TranscodedView<UTF8>
  var utf8: ValidUTF8View { return ValidUTF8View(codeUnits) }
  
  typealias ValidUTF16View = Storage.TranscodedView<UTF16>
  var utf16: ValidUTF16View { return ValidUTF16View(codeUnits) }
  
  typealias ValidUTF32View = Storage.TranscodedView<UTF32>
  var utf32: ValidUTF32View { return ValidUTF32View(codeUnits) }
  
  typealias ExtendedASCII = LazyMapRandomAccessCollection<CodeUnits, UInt32>
  var extendedASCII: ExtendedASCII {
    return codeUnits.lazy.map { UInt32($0) }
  }

  typealias Characters = LazyMapRandomAccessCollection<CodeUnits, Character>
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

var t = TestSuite("t")
t.test("basic") {
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

t.test("CharacterView") {
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
runAllTests()

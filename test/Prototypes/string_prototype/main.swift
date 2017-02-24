// Run the tests for the whole String prototype
// RUN: %target-build-swift %s %S/String.swift %S/StringStorage.swift %S/Unicode.swift %S/UnicodeStorage.swift %S/StringComparison.swift -parse-stdlib -Xfrontend -disable-access-control -Onone -o %t
// RUN: %target-run %t
// REQUIRES: executable_test

import Swift
import SwiftShims
import StdlibUnittest

//
// The following is a left over prototype, it's here for test purposes but might
// end up not existing in the new String
//
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

var testSuite = TestSuite("t")

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
  let _ = UnicodeStorage.TranscodedView(s8, from: ValidUTF8.self, to: UTF16.self)

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

testSuite.test("literals") {
  let ascii: String = "abcdef"
  expectEqual(ascii.characters.count,6)
  expectTrue(ascii.characters.elementsEqual(("abcdef" as Swift.String).characters))
  let unicode: String = "abcdef🦊"
  expectEqual(unicode.characters.count,7)
  expectTrue(unicode.characters.elementsEqual(("abcdef🦊" as Swift.String).characters))

  // TODO: Fix Character so it can handle this kind of grapheme fully
  // let couple: String = "12👩‍❤️‍👩"
  // print(couple)
  // print(couple.count)
  // fatalError()
  
}

testSuite.test("printing") {
  let s: String = "a \"string\" with \"quotes\""
  expectEqual(s.description, "a \"string\" with \"quotes\"")
  expectEqual(s.debugDescription, "a \"string\" with \"quotes\"".debugDescription)
}

testSuite.text("character") {
  
}

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

testSuite.test("string-compare") {
  let s1 = "abcdez"
  let s2 = "abcdfz"
  let s3 = "abcde\(UnicodeScalar(0x304)!)z"
  let s4 = "abcd\(UnicodeScalar(0x113)!)z"

  typealias UTF16String = UnicodeStorage<[UInt16], UTF16>
  let s1u16 = UTF16String(s1.utf16.map { $0 })
  let s2u16 = UTF16String(s2.utf16.map { $0 })
  let s3u16 = UTF16String(s3.utf16.map { $0 })
  let s4u16 = UTF16String(s4.utf16.map { $0 })

  // FIXME: doesn't work, as UInt16 is not FixedWidthInteger...
  //
  expectEqual(.same, s1u16.ordered(with: s1u16))
  expectEqual(.same, s2u16.ordered(with: s2u16))
  expectEqual(.same, s3u16.ordered(with: s3u16))
  expectEqual(.same, s4u16.ordered(with: s4u16))

  expectEqual(.before, s1u16.ordered(with: s2u16))
  // FIXME: bug, or Character following UCA instead?: expectEqual(.before, s2u16.ordered(with: s3u16))
  expectEqual(.same, s3u16.ordered(with: s4u16))

  expectEqual(.after, s2u16.ordered(with: s1u16))
  // FIXME: bug, or Character following UCA instead?: expectEqual(.after, s3u16.ordered(with: s2u16))
  expectEqual(.same, s4u16.ordered(with: s3u16))
}

runAllTests()

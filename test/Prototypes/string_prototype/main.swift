// Run the tests for the whole String prototype
// RUN: %target-build-swift %s %S/String.swift %S/StringStorage.swift %S/Unicode.swift %S/Latin1String.swift %S/CanonicalString.swift %S/StringComparison.swift %S/StringCompatibility.swift %S/Substring.swift -parse-stdlib -Xfrontend -disable-access-control -Onone -g -o %t
// RUN: %target-run %t
// REQUIRES: executable_test

import Swift
import Darwin
import SwiftShims
import StdlibUnittest

//
// WIP: for debugging use within the prototype, only
//
var _printDebugging = false
func _debug(_ s: Swift.String) {
  guard _printDebugging else { return }
  print(s)
}
func _withDebugging(_ f: ()->()) {
  let prior = _printDebugging
  _printDebugging = true
  f()
  _printDebugging = prior
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
  let v8 = _UnicodeViews<Array<UInt8>, UTF8>.CharacterView(Array(s.utf8), UTF8.self)
  expectEqual(a, Array(v8))

  // FIXME: We need to wrap s.utf16 in Array because of <rdar://30386193> Unaccountable link errors
  // FIXME: the generic arguments should be deducible; <rdar://30323161>
  let v16 = _UnicodeViews<Array<UInt16>, UTF16>.CharacterView(Array(s.utf16), UTF16.self)
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
  let s16to32 = _UnicodeViews.TranscodedView(s16, from: UTF16.self, to: UTF32.self)
  let s16to8 = _UnicodeViews.TranscodedView(s16, from: UTF16.self, to: UTF8.self)
  let s8to16 = _UnicodeViews.TranscodedView(s8, from: UTF8.self, to: UTF16.self)
  let s8Vto16 = _UnicodeViews.TranscodedView(s8, from: ValidUTF8.self, to: UTF16.self)
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
    let alphabet = String(latin1: Latin1String(codeUnits: s8.prefix(27), encodedWith: Latin1.self))
    expectTrue(alphabet.isASCII(scan: true))
    expectFalse(alphabet.isASCII(scan: false))

    // We know that if you interpret s8 as Latin1, it has a lot of non-ASCII
    let nonASCII = String(latin1: Latin1String(codeUnits: s8, encodedWith: Latin1.self))
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
  let s16to32 = _UnicodeViews.TranscodedView(s16, from: UTF16.self, to: UTF32.self)
  let s16to8 = _UnicodeViews.TranscodedView(s16, from: UTF16.self, to: UTF8.self)
  let s8to16 = _UnicodeViews.TranscodedView(s8, from: UTF8.self, to: UTF16.self)
  let _ = _UnicodeViews.TranscodedView(s8, from: ValidUTF8.self, to: UTF16.self)

  let sncFrom32 = String(canonical: SwiftCanonicalString(
    codeUnits: Array(s32), encodedWith: UTF32.self
  ))
  let sncFrom16 = String(canonical: SwiftCanonicalString(
    codeUnits: s16, encodedWith: UTF16.self
  ))
  let sncFrom8 = String(canonical: SwiftCanonicalString(
    codeUnits: Array(s8), encodedWith: UTF8.self
  ))
  let sncFrom16to32 = String(canonical: SwiftCanonicalString(
    codeUnits: Array(s16to32), encodedWith: UTF32.self
  ))
  let sncFrom16to8 = String(canonical: SwiftCanonicalString(
    codeUnits: Array(s16to8), encodedWith: UTF8.self
  ))
  let sncFrom8to16 = String(canonical: SwiftCanonicalString(
    codeUnits: Array(s8to16), encodedWith: UTF16.self
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
}

testSuite.test("printing") {
  let s: String = "a \"string\" with \"quotes\""
  expectEqual(s.description, "a \"string\" with \"quotes\"")
  expectEqual(s.debugDescription, "a \"string\" with \"quotes\"".debugDescription)
}

testSuite.test("character") {
  let s1: String = "Héllo, 🌍!"
  var s2: Swift.String = ""
  for c in s1 {
    s2.append(c)
  }
  // expectTrue(s1.elementsEqual(s2.characters))
  expectEqual(s1.count, 9)

  // emoji with skin tone should be 1 character (are 2 in Swift 3)
  let skinTone: String = "✌🏾"
  expectEqual(skinTone.count, 1)

  let couple: String = "abc👩‍❤️‍👩def"
  expectEqual(couple.count, 7)
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

  typealias UTF16String = _UnicodeViews<[UInt16], UTF16>
  let s1u16 = UTF16String(Array(s1.utf16))
  let s2u16 = UTF16String(Array(s2.utf16))
  let s3u16 = UTF16String(Array(s3.utf16))
  let s4u16 = UTF16String(Array(s4.utf16))

  // FIXME: doesn't work, as UInt16 is not FixedWidthInteger...
  //
  expectEqual(.same, s1u16.ordered(with: s1u16))
  expectEqual(.same, s2u16.ordered(with: s2u16))
  expectEqual(.same, s3u16.ordered(with: s3u16))
  expectEqual(.same, s4u16.ordered(with: s4u16))

  expectEqual(.before, s1u16.ordered(with: s2u16))
  expectEqual(.before, s2u16.ordered(with: s3u16))
  expectEqual(.same, s3u16.ordered(with: s4u16))

  expectEqual(.after, s2u16.ordered(with: s1u16))
  expectEqual(.after, s3u16.ordered(with: s2u16))
  expectEqual(.same, s4u16.ordered(with: s3u16))
}

testSuite.test("string-compare-hash") {
  let s1 = "abcdez"
  let s2 = "abcdfz"
  let s3 = "abcde\(UnicodeScalar(0x304)!)z"
  let s4 = "abcd\(UnicodeScalar(0x113)!)z"

  typealias UTF16String = _UnicodeViews<[UInt16], UTF16>
  let s1u16 = UTF16String(Array(s1.utf16))
  let s2u16 = UTF16String(Array(s2.utf16))
  let s3u16 = UTF16String(Array(s3.utf16))
  let s4u16 = UTF16String(Array(s4.utf16))

  // FIXME: doesn't work, as UInt16 is not FixedWidthInteger...
  //
  expectEqual(.same, s1u16.ordered(with: s1u16))
  expectEqual(.same, s2u16.ordered(with: s2u16))
  expectEqual(.same, s3u16.ordered(with: s3u16))
  expectEqual(.same, s4u16.ordered(with: s4u16))

  expectEqual(.before, s1u16.ordered(with: s2u16))
  expectNotEqual(
    SwiftCanonicalString(s1u16).hashValue,
    SwiftCanonicalString(s2u16).hashValue)
  expectEqual(.before, s2u16.ordered(with: s3u16))
  expectNotEqual(
    SwiftCanonicalString(s2u16).hashValue,
    SwiftCanonicalString(s3u16).hashValue)
  expectEqual(.same, s3u16.ordered(with: s4u16))
  expectEqual(
    SwiftCanonicalString(s3u16).hashValue,
    SwiftCanonicalString(s4u16).hashValue)

  expectEqual(.after, s2u16.ordered(with: s1u16))
  expectEqual(.after, s3u16.ordered(with: s2u16))
  expectEqual(.same, s4u16.ordered(with: s3u16))
}

testSuite.test("replaceSubrange") {
  let initial: String = "hello world!"
  expectEqual(MemoryLayout<Int64>.size, MemoryLayout<String>.size)

  let cases: [(Int, String, String, String)] = 
    [(0, "hello", "goodbye", "goodbye world!"),        // Edit start
     (8, "world!", "moon?", "goodbye moon?"),          // Edit end
     (4, "bye", " night", "good night moon?"),         // Edit middle
     (4, "", " 🦊🦊🦊", "good 🦊🦊🦊 night moon?")]  // wide Characters

  // TODO: String subrange search impl to eliminate need for `start`
  func findSubrange(start: Int, haystack: String, needle: String) -> Range<Int> {
    let start = start // haystack.index(of: needle)
    let end = start.advanced(by: needle.count)
    return start..<end
  }

  print(unsafeBitCast(initial.codeUnits, to: Int.self))
  var testSubject = initial;
  for (start, needle, replacement, result) in cases {
    let subrange = findSubrange(start: start, haystack: testSubject, needle: needle)
    testSubject.replaceSubrange(subrange, with: replacement)
    expectEqual(testSubject, result)
  }



  // Make sure the initial value wasn't mutated
  expectEqual(initial, "hello world!")

  // Check implicit RangeReplaceable stuff works
  var hello: String = "Hello!"
  hello.removeLast()
  let newElements: String = ", 🌎!"
  hello += newElements
  expectEqual(hello, "Hello, 🌎!")
}

testSuite.test("cstring") {
  let s1: String = "abracadabra"
  expectEqual(s1.withCString(strlen), 11)
  let s2: String = "3.14159"
  expectEqual(3.14159,s2.withCString(atof))
  
  let s3: Swift.String = "some string"
  s3.withCString {
    let s = String(cString: $0)
    expectEqual("some string", s)
  }

  let utf16 = Array(s3.utf16) + [0]
  let s4 = utf16.withUnsafeBufferPointer {
    String(cString: $0.baseAddress, encoding: UTF16.self)
  }
  expectEqual(s4, "some string")
}

testSuite.test("substring") {
  let s: String = "hello world"
  let worldRange: Range = s.index(s.startIndex, offsetBy: 6)..<s.endIndex
  expectEqualSequence("world" as String, s[worldRange] as Substring)
  expectEqualSequence("world" as String, s[worldRange] as String)

  var tail = s.dropFirst()
  expectType(Substring.self, &tail)
  expectEqualSequence("ello world", tail)
}

typealias _String = Swift.String

testSuite.test("fcc-normalized-view") {
  let a: UInt16 = 0x0061
  let aTic: UInt16 = 0x00e0
  let aBackTic: UInt16 = 0x00e1
  typealias UTF16String = _UnicodeViews<[UInt16], UTF16>
  typealias NormalizedView = FCCNormalizedUTF16View_2<[UInt16], UTF16>

  // Helper old/new functions, eagerly forms arrays of the forwards and reverse
  // FCC normalized UTF16 code units
  func oldFCCNormView(_ codeUnits: [UInt16])
    -> (forward: [UInt16], reversed: [UInt16]) {
    let view = UTF16String(codeUnits).fccNormalizedUTF16
    return (forward: Array(view),
            reversed: Array(view.reversed()))
  }
  func newFCCNormView(_ codeUnits: [UInt16])
    -> (forward: [UInt16], reversed: [UInt16]) {
    let view = NormalizedView(FCCNormalizedLazySegments(UTF16String(codeUnits)))
    return (forward: Array(view),
            reversed: Array(view.reversed()))
  }

  // Test canonical equivalence for:
  //   1) a + ̀ + ́ == à + ́
  //   2) a + ́ + ̀ == á + ̀
  // BUT, the two are distinct, #1 != #2
  do {
    let str1form1 = [a, 0x0300, 0x0301]
    let str1form2 = [aTic, 0x0301]
    let str2form1 = [a, 0x0301, 0x0300]
    let str2form2 = [aBackTic, 0x0300]

    let (norm1_1, norm1_1rev) = oldFCCNormView(str1form1)
    let (norm1_2, norm1_2rev) = oldFCCNormView(str1form2)
    let (norm2_1, norm2_1rev) = oldFCCNormView(str2form1)
    let (norm2_2, norm2_2rev) = oldFCCNormView(str2form2)

    expectEqualSequence(norm1_1, norm1_2)
    expectEqualSequence(norm2_1, norm2_2)
    for (cu1, cu2) in zip(norm1_1, norm2_1) {
      expectNotEqual(cu1, cu2)
    }

    let (newNorm1_1, newNorm1_1rev) = newFCCNormView(str1form1)
    let (newNorm1_2, newNorm1_2rev) = newFCCNormView(str1form2)
    let (newNorm2_1, newNorm2_1rev) = newFCCNormView(str2form1)
    let (newNorm2_2, newNorm2_2rev) = newFCCNormView(str2form2)

    expectEqualSequence(newNorm1_1, newNorm1_2)
    expectEqualSequence(newNorm1_1, norm1_1)
    expectEqualSequence(newNorm2_1, newNorm2_2)
    expectEqualSequence(newNorm2_1, norm2_1)

    // Test other direction
    expectEqualSequence(newNorm1_1rev, newNorm1_2rev)
    expectEqualSequence(newNorm1_1rev, norm1_1rev)
    expectEqualSequence(newNorm2_1rev, newNorm2_2rev)
    expectEqualSequence(newNorm2_1rev, norm2_1rev)
  }

  // Test canonical equivalence, and non-combining-ness of FCC for:
  //   1) a + ̖ + ̀ == à + ̖ == a + ̀ + ̖
  //   All will normalize under FCC as a + ̖ + ̀
  do {
    let form1 = [a, 0x0316, 0x0300]
    let form2 = [a, 0x0300, 0x0316]
    let form3 = [aTic, 0x0316]

    let (norm1, norm1rev) = oldFCCNormView(form1)
    let (norm2, norm2rev) = oldFCCNormView(form2)
    let (norm3, norm3rev) = oldFCCNormView(form3)

    // Sanity check existing normal form yields same results
    expectEqualSequence(norm1, norm2)
    expectEqualSequence(norm2, norm3)

    // Form 1 is already in FCC
    expectEqualSequence(norm3, form1)

    // Test the new one
    let (newNorm1, newNorm1rev) = newFCCNormView(form1)
    let (newNorm2, newNorm2rev) = newFCCNormView(form2)
    let (newNorm3, newNorm3rev) = newFCCNormView(form3)

    expectEqualSequence(newNorm1, newNorm2)
    expectEqualSequence(newNorm2, newNorm3)
    expectEqualSequence(newNorm3, norm3)

    // And in reverse
    expectEqualSequence(newNorm1rev, newNorm2rev)
    expectEqualSequence(newNorm2rev, newNorm3rev)
    expectEqualSequence(newNorm3rev, norm3rev)
  }

  // Test non-start first scalars
  do {
    let form1 = [0x0300, a, 0x0300]
    let form2 = [0x0300, aTic] // In FCC normal form
    let (norm1, norm1rev) = oldFCCNormView(form1)
    let (norm2, norm2rev) = oldFCCNormView(form2)

    // Sanity check existing impl
    expectEqualSequence(norm1, norm2)
    expectEqualSequence(norm1rev, norm2rev)
    expectEqualSequence(norm1, form2)

    // Test the new one
    let (newNorm1, newNorm1rev) = newFCCNormView(form1)
    let (newNorm2, newNorm2rev) = newFCCNormView(form2)
    expectEqualSequence(newNorm1, newNorm2)
    expectEqualSequence(newNorm2, norm2)
    expectEqualSequence(newNorm1rev, newNorm2rev)
    expectEqualSequence(newNorm2rev, norm2rev)
  }

  do {
    // Test that the new normalizer is same result as old normalizer
    let s = "abcdefghijklmnopqrstuvwxyz\n"
    + "🇸🇸🇬🇱🇱🇸🇩🇯🇺🇸\n"
    + "Σὲ 👥🥓γνωρίζω ἀπὸ τὴν κόψη χαῖρε, ὦ χαῖρε, ᾿Ελευθεριά!\n"
    + "Οὐχὶ ταὐτὰ παρίσταταί μοι γιγνώσκειν, ὦ ἄνδρες ᾿Αθηναῖοι,\n"
    + "გთხოვთ ახლავე გაიაროთ რეგისტრაცია Unicode-ის მეათე საერთაშორისო\n"
    + "Зарегистрируйтесь сейчас на Десятую Международную Конференцию по\n"
    + "  ๏ แผ่นดินฮั่นเสื่อมโทรมแสนสังเวช  พระปกเกศกองบู๊กู้ขึ้นใหม่\n"
    + "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ"
    let s16 = Array(s.utf16)

    let (norm1, norm1rev) = oldFCCNormView(s16)
    let (newNorm1, newNorm1rev) = newFCCNormView(s16)

    expectEqualSequence(norm1, newNorm1)
    expectEqualSequence(norm1rev, newNorm1rev)

    expectTrue(norm1 != newNorm1rev)
  }
}

import Foundation
testSuite.test("bridging") {
  defer { _debugLogging = false }
  _debugLogging = false
  let s : main.String
    = "abc\n🇸🇸🇬🇱🇱🇸🇩🇯🇺🇸\nΣὲ 👥🥓γνωρίζω\nგთხოვთ\nงบู๊กู้ขึ้นม่\nᚹᛖᛥᚫ"
  let n = s as NSString
  let s2 = n as main.String
  expectEqual(s, s2)
}

runAllTests()

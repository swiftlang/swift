// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var CharacterPropertiesTests = TestSuite("StringTests")

CharacterPropertiesTests.test("ASCII queries") {
  for cu in (0 as UInt32)...(0x7F as UInt32) {
    let c = Character(Unicode.Scalar(cu)!)
    expectTrue(c.isASCII)
    expectEqual(cu, UInt32(c.asciiValue!))
  }
  expectTrue(Character("\r\n").isASCII)
  expectEqual(Character("\n").asciiValue, Character("\r\n").asciiValue)

  expectFalse(Character("⅚").isASCII)
  expectFalse(Character("“").isASCII)
  expectFalse(Character("e\u{301}").isASCII)
}

CharacterPropertiesTests.test("Hex queries") {
  let hexDigits: Array<Character> = [
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", // 0-9
    "a", "b", "c", "d", "e", "f", // 10-15
    "A", "B", "C", "D", "E", "F", // 16-21
    "０", "１", "２", "３", "４", "５", "６", "７", "８", "９", // 22-31
    "Ａ", "Ｂ", "Ｃ", "Ｄ", "Ｅ", "Ｆ", // 32-37
    "ａ", "ｂ", "ｃ", "ｄ", "ｅ", "ｆ", // 38-43
  ]

  // Ensure below loop logic is correct with a couple hard-coded checks
  expectEqual(1, Character("1").hexDigitValue!)
  expectEqual(2, Character("２").hexDigitValue!)
  expectEqual(11, Character("Ｂ").hexDigitValue!)
  expectEqual(12, Character("c").hexDigitValue!)
  expectEqual(14, Character("ｅ").hexDigitValue!)
  expectEqual(15, Character("f").hexDigitValue!)

  for i in hexDigits.indices {
    let hexValue = hexDigits[i].hexDigitValue!
    switch i {
      case 0...15: expectEqual(i, hexValue)
      case 16...21: expectEqual(i-6, hexValue)
      case 22...37: expectEqual(i-22, hexValue)
      case 38...43: expectEqual(i-28, hexValue)
      default: print(i); fatalError("unreachable")
    }
  }

  expectNil(Character("㊈").hexDigitValue)
  expectNil(Character("Ⅴ").hexDigitValue)
  expectNil(Character("7\u{301}").hexDigitValue)
}

CharacterPropertiesTests.test("Numbers") {
  // Some hard coded tests
  expectTrue(Character("⅚").isNumber)
  expectTrue(Character("5️⃣").isNumber)
  expectTrue(Character("𝟠").isNumber)
  expectTrue(Character("㊈").isNumber)
  expectTrue(Character("7").isNumber)
  expectTrue(Character("𐹾").isNumber) // RUMI FRACTION 2/3

  expectFalse(Character("A").isNumber)
  expectFalse(Character("Z").isNumber)
}

CharacterPropertiesTests.test("Whole Numbers") {
  // Random smattering of hard-coded tests
  expectEqual(0, Character("↉").wholeNumberValue) // baseball scoring
  expectEqual(0, Character("𝟶").wholeNumberValue) // math monospace
  expectEqual(1, Character("1").wholeNumberValue)
  expectEqual(1, Character("𒐴").wholeNumberValue)
  expectEqual(1, Character("ⅰ").wholeNumberValue) // small roman numeral
  expectEqual(1, Character("𝟏").wholeNumberValue) // math bold
  expectEqual(2, Character("②").wholeNumberValue)
  expectEqual(2, Character("٢").wholeNumberValue)
  expectEqual(2, Character("২").wholeNumberValue)
  expectEqual(2, Character("੨").wholeNumberValue)
  expectEqual(2, Character("૨").wholeNumberValue)
  expectEqual(3, Character("³").wholeNumberValue)
  expectEqual(4, Character("٤").wholeNumberValue)
  expectEqual(4, Character("൪").wholeNumberValue)
  expectEqual(4, Character("೪").wholeNumberValue)
  expectEqual(4, Character("౪").wholeNumberValue)
  expectEqual(4, Character("௪").wholeNumberValue)
  expectEqual(4, Character("୪").wholeNumberValue)
  expectEqual(4, Character("૪").wholeNumberValue)
  expectEqual(4, Character("੪").wholeNumberValue)
  expectEqual(4, Character("๔").wholeNumberValue)
  expectEqual(4, Character("໔").wholeNumberValue)
  expectEqual(5, Character("५").wholeNumberValue)
  expectEqual(5, Character("༥").wholeNumberValue)
  expectEqual(5, Character("፭").wholeNumberValue)
  expectEqual(5, Character("᠕").wholeNumberValue)
  expectEqual(5, Character("Ⅴ").wholeNumberValue) // Roman numeral
  expectEqual(5, Character("𐌡").wholeNumberValue)
  expectEqual(5, Character("߅").wholeNumberValue)
  expectEqual(5, Character("᭕").wholeNumberValue)
  expectEqual(5, Character("𝍤").wholeNumberValue)
  expectEqual(5, Character("᮵").wholeNumberValue)
  expectEqual(6, Character("六").wholeNumberValue)
  expectEqual(6, Character("六").wholeNumberValue) // Compatibility
  expectEqual(7, Character("𝟩").wholeNumberValue) // Math san-serif
  expectEqual(7, Character("㈦").wholeNumberValue)
  expectEqual(7, Character("㊆").wholeNumberValue)
  expectEqual(7, Character("𑁭").wholeNumberValue)
  expectEqual(8, Character("꧘").wholeNumberValue)
  expectEqual(8, Character("᪈").wholeNumberValue)
  expectEqual(8, Character("᪘").wholeNumberValue)
  expectEqual(8, Character("꩘").wholeNumberValue)
  expectEqual(9, Character("๙").wholeNumberValue)

  expectEqual(18, Character("⒅").wholeNumberValue)
  expectEqual(20, Character("⑳").wholeNumberValue)
  expectEqual(20, Character("𐄑").wholeNumberValue)
  expectEqual(20, Character("𐏔").wholeNumberValue)
  expectEqual(20, Character("𐤘").wholeNumberValue)
  expectEqual(20, Character("〹").wholeNumberValue)
  expectEqual(50, Character("ↆ").wholeNumberValue)
  expectEqual(70, Character("𑁡").wholeNumberValue)
  expectEqual(90, Character("𐍁").wholeNumberValue)
  expectEqual(1_000, Character("𑁥").wholeNumberValue)
  expectEqual(5_000, Character("ↁ").wholeNumberValue)
  expectEqual(10_000, Character("万").wholeNumberValue)

  expectFalse(Character("7\u{301}").isWholeNumber)
}

CharacterPropertiesTests.test("Casing") {
  let eAccent = Character("\u{0065}\u{0301}")
  let EAccent = Character("\u{0045}\u{0301}")
  expectTrue(eAccent.isLowercase && eAccent.isCased)
  expectFalse(eAccent.isUppercase)
  expectTrue(EAccent.isUppercase && EAccent.isCased)
  expectFalse(EAccent.isLowercase)

  expectTrue(Character("И").isUppercase)
  expectTrue(Character("и").isLowercase)
  expectTrue(Character("Π").isUppercase)
  expectTrue(Character("π").isLowercase)

  expectEqual("SS", Character("ß").uppercased())
  expectEqual("ß", Character("ẞ").lowercased())
  expectEqual("и", Character("И").lowercased())
  expectEqual("И", Character("и").uppercased())
  expectEqual("π", Character("Π").lowercased())
  expectEqual("Π", Character("π").uppercased())
}

CharacterPropertiesTests.test("Punctuation") {
  expectTrue(Character("!").isPunctuation)
  expectTrue(Character("؟").isPunctuation)
  expectTrue(Character("…").isPunctuation)
  expectTrue(Character("—").isPunctuation)
  expectTrue(Character("“").isPunctuation)

  expectTrue(Character("﹏").isPunctuation) // compatibility
}

CharacterPropertiesTests.test("Symbols") {
  // Other symbols
  expectTrue(Character("🌍").isSymbol)
  expectTrue(Character("👽").isSymbol)
  expectTrue(Character("®").isSymbol)
  expectTrue(Character("⌹").isSymbol)
  expectTrue(Character("⡆").isSymbol)

  // Currency
  expectTrue(Character("$").isCurrencySymbol)
  expectTrue(Character("¥").isCurrencySymbol)
  expectTrue(Character("€").isCurrencySymbol)

  // Math symbols
  expectTrue(Character("∩").isSymbol)
  expectTrue(Character("∩").isMathSymbol)
  expectTrue(Character("+").isSymbol)
  expectTrue(Character("+").isMathSymbol)
  expectTrue(Character("⟺").isSymbol)
  expectTrue(Character("⟺").isMathSymbol)
  expectTrue(Character("∫").isSymbol)
  expectTrue(Character("∫").isMathSymbol)

  // Math symbols that are letters
  expectFalse(Character("ϰ").isSymbol)
  expectTrue(Character("ϰ").isMathSymbol)
}

CharacterPropertiesTests.test("Whitespace") {
  expectTrue(Character("\t").isWhitespace)
  expectTrue(Character(" ").isWhitespace)
  expectTrue(Character("\u{2029}").isWhitespace)
  expectTrue(Character("\u{3000}").isWhitespace)
}

CharacterPropertiesTests.test("Newline") {
  expectTrue(Character("\n").isNewline)
  expectTrue(Character("\r").isNewline)
  expectTrue(Character("\r\n").isNewline)
  expectTrue(Character("\u{0085}").isNewline)
  expectTrue(Character("\u{2028}").isNewline)
  expectTrue(Character("\u{2029}").isNewline)
}

runAllTests()


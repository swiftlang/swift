// RUN: %target-run-simple-swift %t
// REQUIRES: executable_test

import StdlibUnittest

var UnicodeScalarPropertiesTests = TestSuite("UnicodeScalarPropertiesTests")

func us(_ scalar: Unicode.Scalar) -> Unicode.Scalar { return scalar }

UnicodeScalarPropertiesTests.test("lowercased") {
  expectEqual("2", us("2").lowercased())
  expectEqual("i", us("I").lowercased())
  expectEqual("i\u{0307}", us("\u{0130}").lowercased())
  // There are currently no lowercase mappings that produce multiple graphemes.
}

UnicodeScalarPropertiesTests.test("uppercased") {
  expectEqual("2", us("2").uppercased())
  expectEqual("I", us("i").uppercased())
  expectEqual("\u{02BC}N", us("\u{0149}").uppercased())  // multiple scalars
  expectEqual("SS", us("ß").uppercased())  // multiple graphemes
  expectEqual("FFL", us("\u{FB04}").uppercased())  // multiple graphemes
}

UnicodeScalarPropertiesTests.test("titlecased") {
  expectEqual("2", us("2").titlecased())
  expectEqual("I", us("i").titlecased())
  expectEqual("\u{02BC}N", us("\u{0149}").titlecased())  // multiple scalars
  expectEqual("Ff", us("\u{FB00}").titlecased())  // multiple graphemes
  expectEqual("Ffl", us("\u{FB04}").titlecased())  // multiple graphemes
}

UnicodeScalarPropertiesTests.test("properties.name") {
  // A scalar with no assigned name returns nil.
  expectNil(us("\u{0000}").properties.name)

  // Try some results that should fit in small strings.
  expectEqual("CARE OF", us("\u{2105}").properties.name)
  expectEqual("ACCOUNT OF", us("\u{2100}").properties.name)

  // Try some results that need heap-allocated strings.
  expectEqual("LATIN SMALL LETTER A", us("\u{0061}").properties.name)
  expectEqual(
    "COMBINING LEFTWARDS HARPOON WITH BARB DOWNWARDS",
    us("\u{20ED}").properties.name)
  expectEqual(
    "PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRAKCET",  // [sic]
    us("\u{FE18}").properties.name)

  // Try some boundary cases around the length limit of a SmallUTF8String.
  expectEqual("COMBINING HORN", us("\u{031B}").properties.name)  // 14
  expectEqual("COMBINING TILDE", us("\u{0303}").properties.name)  // 15
  expectEqual("COMBINING MACRON", us("\u{0304}").properties.name)  // 16
}

UnicodeScalarPropertiesTests.test("properties.nameAlias") {
  // A scalar with no assigned alias returns nil.
  expectNil(us("\u{0000}").properties.nameAlias)
  expectNil(us("\u{0040}").properties.nameAlias)

  // Try some aliases of varying lengths, getting some small and large string
  // coverage.
  expectEqual("LAO LETTER RO", us("\u{0EA3}").properties.nameAlias)
  expectEqual("MICR DASH SYMBOL", us("\u{2449}").properties.nameAlias)
  expectEqual(
    "PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRACKET",
    us("\u{FE18}").properties.nameAlias)
}

UnicodeScalarPropertiesTests.test("properties.age") {
  func expectAgeEqual(
    _ expected: (Int, Int),
    _ scalar: Unicode.Scalar,
    _ message: String = "",
    file: String = #file,
    line: UInt = #line
  ) {
    expectNotNil(scalar.properties.age, message, file: file, line: line)
    expectEqual(
      expected, scalar.properties.age!, message, file: file, line: line)
  }

  expectNil(us("\u{0378}").properties.age)
  expectAgeEqual((1, 1), "\u{0040}")
  expectAgeEqual((3, 0), "\u{3500}")
  expectAgeEqual((3, 2), "\u{FE00}")
  expectAgeEqual((6, 1), "\u{11180}")
}

runAllTests()

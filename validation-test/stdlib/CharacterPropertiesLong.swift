// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var CharacterPropertiesTests = TestSuite("StringTests")

func genericPropertyValidation(_ c: Character) {
  // Newline is subset of whitespace
  if c.isNewline {
    expectTrue(c.isWhitespace)
  }

  // Currency symbol is subset of symbols
  if c.isCurrencySymbol {
    expectTrue(c.isSymbol)
  }

  // Whole number is subset of number
  if c.isWholeNumber {
    expectTrue(c.isNumber)
    let value = c.wholeNumberValue
    expectNotNil(value)
    if let hexValue = c.hexDigitValue {
      expectEqual(value, hexValue)
    }
  }

  // Hex includes letters like a-f
  if c.isHexDigit {
    let value = c.hexDigitValue
    expectNotNil(value)

    // For hex digits, number and letter are mutually exclusive
    if c.isWholeNumber {
      expectTrue(value! <= 9 && value! >= 0)
      expectTrue(c.isNumber)
      expectFalse(c.isLetter)
    } else {
      expectTrue(c.isLetter)
      expectFalse(c.isNumber)
      let value = c.hexDigitValue
      expectTrue(value! >= 10 && value! <= 15)
    }
  }

  if c.isUppercase || c.isLowercase {
    expectTrue(c.isCased)
    if c.isUppercase {
      expectEqual(c, c.uppercased().first!)
    }
    if c.isLowercase {
      expectEqual(c, c.lowercased().first!)
    }
  }

  // TODO: Any thing else we can check in the abstract?
}

CharacterPropertiesTests.test("Broad grapheme checks") {
  for cu in (0 as UInt32)...(0x10FFFF as UInt32) {
    guard let scalar = Unicode.Scalar(cu) else { continue }
    let c = Character(scalar)
    genericPropertyValidation(c)

    // Add a modifying scalar on the end
    let str = String(scalar) + "\u{301}"
    guard str.count == 1 else { continue }
    genericPropertyValidation(Character(str))
  }
}

runAllTests()


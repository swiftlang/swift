//===--- CharacterProperties.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

////////////////////////////////////////////////////////////////////////////////
// WARNING: This file is manually generated from .gyb template and should not
// be directly modified. Instead, make changes to CharacterProperties.swift.gyb
// and run scripts/generate_harness/generate_harness.py to regenerate this file.
////////////////////////////////////////////////////////////////////////////////

import TestsUtils
import Foundation

public let CharacterPropertiesFetch = BenchmarkInfo(
  name: "CharacterPropertiesFetch",
  runFunction: run_CharacterPropertiesFetch,
  tags: [.validation, .api, .String])

public let CharacterPropertiesStashed = BenchmarkInfo(
  name: "CharacterPropertiesStashed",
  runFunction: run_CharacterPropertiesStashed,
  tags: [.validation, .api, .String])


extension Character {
  var firstScalar: UnicodeScalar { return unicodeScalars.first! }
}


// Fetch the CharacterSet for every call
func isControl(_ c: Character) -> Bool {
  return CharacterSet.controlCharacters.contains(c.firstScalar)
}
func isAlphanumeric(_ c: Character) -> Bool {
  return CharacterSet.alphanumerics.contains(c.firstScalar)
}
func isLowercase(_ c: Character) -> Bool {
  return CharacterSet.lowercaseLetters.contains(c.firstScalar)
}
func isPunctuation(_ c: Character) -> Bool {
  return CharacterSet.punctuationCharacters.contains(c.firstScalar)
}
func isWhitespace(_ c: Character) -> Bool {
  return CharacterSet.whitespaces.contains(c.firstScalar)
}
func isLetter(_ c: Character) -> Bool {
  return CharacterSet.letters.contains(c.firstScalar)
}
func isUppercase(_ c: Character) -> Bool {
  return CharacterSet.uppercaseLetters.contains(c.firstScalar)
}
func isDecimal(_ c: Character) -> Bool {
  return CharacterSet.decimalDigits.contains(c.firstScalar)
}
func isNewline(_ c: Character) -> Bool {
  return CharacterSet.newlines.contains(c.firstScalar)
}
func isCapitalized(_ c: Character) -> Bool {
  return CharacterSet.capitalizedLetters.contains(c.firstScalar)
}

// Stash the set
let controlCharacters = CharacterSet.controlCharacters
func isControlStashed(_ c: Character) -> Bool {
  return controlCharacters.contains(c.firstScalar)
}
let alphanumerics = CharacterSet.alphanumerics
func isAlphanumericStashed(_ c: Character) -> Bool {
  return alphanumerics.contains(c.firstScalar)
}
let lowercaseLetters = CharacterSet.lowercaseLetters
func isLowercaseStashed(_ c: Character) -> Bool {
  return lowercaseLetters.contains(c.firstScalar)
}
let punctuationCharacters = CharacterSet.punctuationCharacters
func isPunctuationStashed(_ c: Character) -> Bool {
  return punctuationCharacters.contains(c.firstScalar)
}
let whitespaces = CharacterSet.whitespaces
func isWhitespaceStashed(_ c: Character) -> Bool {
  return whitespaces.contains(c.firstScalar)
}
let letters = CharacterSet.letters
func isLetterStashed(_ c: Character) -> Bool {
  return letters.contains(c.firstScalar)
}
let uppercaseLetters = CharacterSet.uppercaseLetters
func isUppercaseStashed(_ c: Character) -> Bool {
  return uppercaseLetters.contains(c.firstScalar)
}
let decimalDigits = CharacterSet.decimalDigits
func isDecimalStashed(_ c: Character) -> Bool {
  return decimalDigits.contains(c.firstScalar)
}
let newlines = CharacterSet.newlines
func isNewlineStashed(_ c: Character) -> Bool {
  return newlines.contains(c.firstScalar)
}
let capitalizedLetters = CharacterSet.capitalizedLetters
func isCapitalizedStashed(_ c: Character) -> Bool {
  return capitalizedLetters.contains(c.firstScalar)
}

// Compute on the fly
//
// TODO: If UnicodeScalars ever exposes category, etc., implement the others!
func isNewlineComputed(_ c: Character) -> Bool {
  switch c.firstScalar.value {
    case 0x000A...0x000D: return true
    case 0x0085: return true
    case 0x2028...0x2029: return true
    default: return false
  }
}

let workload = """
  the quick brown 🦊 jumped over the lazy 🐶.
  в чащах юга жил-был цитрус? да, но фальшивый экземпляр
  𓀀𓀤𓁓𓁲𓃔𓃗𓃀𓃁𓃂𓃃𓆌𓆍𓆎𓆏𓆐𓆑𓆒𓆲𓁿
  🝁ꃕ躍‾∾📦⺨
  👍👩‍👩‍👧‍👧👨‍👨‍👦‍👦🇺🇸🇨🇦🇲🇽👍🏻👍🏼👍🏽👍🏾👍🏿
  Lorem ipsum something something something...
"""

@inline(never)
public func run_CharacterPropertiesFetch(_ N: Int) {
  for _ in 1...N {
    for c in workload {
        blackHole(isControl(c))
        blackHole(isAlphanumeric(c))
        blackHole(isLowercase(c))
        blackHole(isPunctuation(c))
        blackHole(isWhitespace(c))
        blackHole(isLetter(c))
        blackHole(isUppercase(c))
        blackHole(isDecimal(c))
        blackHole(isNewline(c))
        blackHole(isCapitalized(c))
    }
  }
}

@inline(never)
public func run_CharacterPropertiesStashed(_ N: Int) {
  for _ in 1...N {
    for c in workload {
        blackHole(isControlStashed(c))
        blackHole(isAlphanumericStashed(c))
        blackHole(isLowercaseStashed(c))
        blackHole(isPunctuationStashed(c))
        blackHole(isWhitespaceStashed(c))
        blackHole(isLetterStashed(c))
        blackHole(isUppercaseStashed(c))
        blackHole(isDecimalStashed(c))
        blackHole(isNewlineStashed(c))
        blackHole(isCapitalizedStashed(c))
    }
  }
}

// TODO: run_CharacterPropertiesComputed


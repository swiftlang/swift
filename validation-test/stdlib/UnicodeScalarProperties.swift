// RUN: %target-run-simple-swift %S/../../utils/gen-unicode-data/Data/16/
// REQUIRES: executable_test
// REQUIRES: long_test
// REQUIRES: optimized_stdlib
// REQUIRES: objc_interop

// This test requires access to Unicode data files in the source tree, so
// it doesn't currently support testing on a remote device. (rdar://98993795)
// UNSUPPORTED: remote_run
// UNSUPPORTED: device_run

@_spi(_Unicode)
import Swift

import StdlibUnittest
import StdlibUnicodeUnittest

var UnicodeScalarPropertiesTest = TestSuite("UnicodeScalarProperties")

//===----------------------------------------------------------------------===//
// Binary Properties
//===----------------------------------------------------------------------===//

if #available(SwiftStdlib 6.1, *) {
  UnicodeScalarPropertiesTest.test("Binary Properties") {
    // First, check that we correctly parsed the unicode data tables to be able
    // to test against.

    // We have 48 properties, but some properties have 'Other_' properties which
    // are included in them, so count those as well.
    expectEqual(56, binaryProperties.keys.count)

    for i in 0x0 ... 0x10FFFF {
      guard let scalar = Unicode.Scalar(i) else {
        continue
      }

      func check(_ property: String) -> Bool {
        binaryProperties[property]!.contains(scalar)
      }

      let props = scalar.properties

      expectEqual(props.isAlphabetic, check("Alphabetic") || check("Other_Alphabetic"))
      expectEqual(props.isASCIIHexDigit, check("ASCII_Hex_Digit"))
      expectEqual(props.isBidiControl, check("Bidi_Control"))
      expectEqual(props.isBidiMirrored, check("Bidi_Mirrored"))
      expectEqual(props.isCased, check("Cased"))
      expectEqual(props.isCaseIgnorable, check("Case_Ignorable"))
      expectEqual(props.changesWhenCaseFolded, check("Changes_When_Casefolded"))
      expectEqual(props.changesWhenCaseMapped, check("Changes_When_Casemapped"))
      expectEqual(props.changesWhenLowercased, check("Changes_When_Lowercased"))
      expectEqual(props.changesWhenNFKCCaseFolded, check("Changes_When_NFKC_Casefolded"))
      expectEqual(props.changesWhenTitlecased, check("Changes_When_Titlecased"))
      expectEqual(props.changesWhenUppercased, check("Changes_When_Uppercased"))
      expectEqual(props.isDash, check("Dash"))
      expectEqual(props.isDefaultIgnorableCodePoint, check("Default_Ignorable_Code_Point") || check("Other_Default_Ignorable_Code_Point"))
      expectEqual(props.isDeprecated, check("Deprecated"))
      expectEqual(props.isDiacritic, check("Diacritic"))
      expectEqual(props.isEmoji, check("Emoji"))
      expectEqual(props.isEmojiModifier, check("Emoji_Modifier"))
      expectEqual(props.isEmojiModifierBase, check("Emoji_Modifier_Base"))
      expectEqual(props.isEmojiPresentation, check("Emoji_Presentation"))
      expectEqual(props.isExtender, check("Extender"))
      expectEqual(props.isFullCompositionExclusion, check("Full_Composition_Exclusion"))
      expectEqual(props.isGraphemeBase, check("Grapheme_Base"))
      expectEqual(props.isGraphemeExtend, check("Grapheme_Extend") || check("Other_Grapheme_Extend"))
      expectEqual(props.isHexDigit, check("Hex_Digit"))
      expectEqual(props.isIDContinue, check("ID_Continue") || check("Other_ID_Continue"))
      expectEqual(props.isIDStart, check("ID_Start") || check("Other_ID_Start"))
      expectEqual(props.isIdeographic, check("Ideographic"))
      expectEqual(props.isIDSBinaryOperator, check("IDS_Binary_Operator"))
      expectEqual(props.isIDSTrinaryOperator, check("IDS_Trinary_Operator"))
      expectEqual(props.isJoinControl, check("Join_Control"))
      expectEqual(props.isLogicalOrderException, check("Logical_Order_Exception"))
      expectEqual(props.isLowercase, check("Lowercase") || check("Other_Lowercase"))
      expectEqual(props.isMath, check("Math") || check("Other_Math"))
      expectEqual(props.isNoncharacterCodePoint, check("Noncharacter_Code_Point"))
      expectEqual(props.isPatternSyntax, check("Pattern_Syntax"))
      expectEqual(props.isPatternWhitespace, check("Pattern_White_Space"))
      expectEqual(props.isQuotationMark, check("Quotation_Mark"))
      expectEqual(props.isRadical, check("Radical"))
      expectEqual(props.isSentenceTerminal, check("Sentence_Terminal"))
      expectEqual(props.isSoftDotted, check("Soft_Dotted"))
      expectEqual(props.isTerminalPunctuation, check("Terminal_Punctuation"))
      expectEqual(props.isUnifiedIdeograph, check("Unified_Ideograph"))
      expectEqual(props.isUppercase, check("Uppercase") || check("Other_Uppercase"))
      expectEqual(props.isVariationSelector, check("Variation_Selector"))
      expectEqual(props.isWhitespace, check("White_Space"))
      expectEqual(props.isXIDContinue, check("XID_Continue"))
      expectEqual(props.isXIDStart, check("XID_Start"))
    }
  }
}

//===----------------------------------------------------------------------===//
// Numeric Properties
//===----------------------------------------------------------------------===//

if #available(SwiftStdlib 6.1, *) {
  UnicodeScalarPropertiesTest.test("Numeric Properties") {
    for i in 0x0 ... 0x10FFFF {
      guard let scalar = Unicode.Scalar(i) else {
        continue
      }

      expectEqual(scalar.properties.numericType, numericTypes[scalar])
      expectEqual(scalar.properties.numericValue, numericValues[scalar])
    }
  }
}

//===----------------------------------------------------------------------===//
// Scalar Mappings
//===----------------------------------------------------------------------===//

if #available(SwiftStdlib 6.1, *) {
  UnicodeScalarPropertiesTest.test("Scalar Mappings") {
    for i in 0x0 ... 0x10FFFF {
      guard let scalar = Unicode.Scalar(i) else {
        continue
      }

      if let upper = mappings[scalar]?["upper"] {
        expectEqual(scalar.properties.uppercaseMapping, upper)
      }

      if let lower = mappings[scalar]?["lower"] {
        expectEqual(scalar.properties.lowercaseMapping, lower)
      }

      if let title = mappings[scalar]?["title"] {
        expectEqual(scalar.properties.titlecaseMapping, title)
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// Scalar Age
//===----------------------------------------------------------------------===//

if #available(SwiftStdlib 6.1, *) {
  UnicodeScalarPropertiesTest.test("Scalar Age") {
    for i in 0x0 ... 0x10FFFF {
      guard let scalar = Unicode.Scalar(i) else {
        continue
      }

      expectEqual(scalar.properties.age?.major, ages[scalar]?.major)
      expectEqual(scalar.properties.age?.minor, ages[scalar]?.minor)
    }
  }
}

//===----------------------------------------------------------------------===//
// Scalar General Category
//===----------------------------------------------------------------------===//

if #available(SwiftStdlib 6.1, *) {
  UnicodeScalarPropertiesTest.test("Scalar General Category") {
    for i in 0x0 ... 0x10FFFF {
      guard let scalar = Unicode.Scalar(i) else {
        continue
      }

      expectEqual(scalar.properties.generalCategory, generalCategories[scalar])
    }
  }
}

//===----------------------------------------------------------------------===//
// Scalar Name Alias
//===----------------------------------------------------------------------===//

if #available(SwiftStdlib 6.1, *) {
  UnicodeScalarPropertiesTest.test("Scalar Name Alias") {
    for i in 0x0 ... 0x10FFFF {
      guard let scalar = Unicode.Scalar(i) else {
        continue
      }

      expectEqual(scalar.properties.nameAlias, nameAliases[scalar])
    }
  }
}

//===----------------------------------------------------------------------===//
// Scalar Name
//===----------------------------------------------------------------------===//

if #available(SwiftStdlib 6.1, *) {
  UnicodeScalarPropertiesTest.test("Scalar Name") {
    for i in 0x0 ... 0x10FFFF {
      guard let scalar = Unicode.Scalar(i) else {
        continue
      }

      expectEqual(scalar.properties.name, names[scalar])
    }
  }
}

//===----------------------------------------------------------------------===//
// Case Folded
//===----------------------------------------------------------------------===//

if #available(SwiftStdlib 6.1, *) {
  UnicodeScalarPropertiesTest.test("Scalar Case Folding") {
    for i in 0x0 ... 0x10FFFF {
      guard let scalar = Unicode.Scalar(i) else {
        continue
      }

      if let mapping = caseFolding[scalar] {
        expectEqual(scalar.properties._caseFolded, mapping)
      } else {
        expectEqual(scalar.properties._caseFolded, String(scalar))
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// Script/Script Extensions
//===----------------------------------------------------------------------===//

if #available(SwiftStdlib 6.1, *) {
  UnicodeScalarPropertiesTest.test("Scalar Scripts") {
    for i in 0x0 ... 0x10FFFF {
      guard let scalar = Unicode.Scalar(i) else {
        continue
      }

      let script = unsafeBitCast(
        scalar.properties._script,
        to: Unicode.Script.self
      )

      if let parsedScript = scripts[scalar] {
        expectEqual(script, parsedScript)
      } else {
        expectEqual(script, .unknown)
      }
    }
  }

  UnicodeScalarPropertiesTest.test("Scalar Script Extensions") {
    for i in 0x0 ... 0x10FFFF {
      guard let scalar = Unicode.Scalar(i) else {
        continue
      }

      let extensions = scalar.properties._scriptExtensions.map {
        unsafeBitCast($0, to: Unicode.Script.self)
      }

      let script = unsafeBitCast(
        scalar.properties._script,
        to: Unicode.Script.self
      )

      if let parsedExtensions = scriptExtensions[scalar] {
        expectEqual(extensions, parsedExtensions, "Scalar: \(String(scalar.value, radix: 16))")
      } else {
        expectEqual(extensions, [script], "Scalar: \(String(scalar.value, radix: 16))")
      }
    }
  }
}

runAllTests()

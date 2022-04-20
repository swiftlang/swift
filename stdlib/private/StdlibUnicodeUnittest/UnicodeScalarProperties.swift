//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Unicode scalar tests are currently only available on Darwin, awaiting a sensible
// file API...
#if _runtime(_ObjC)
import Foundation

// Cache of opened files 
var cachedFiles: [String: String] = [:]

func readInputFile(_ filename: String) -> String {
  let path = CommandLine.arguments[2]

  do {
    guard let cache = cachedFiles[filename] else {
      let contents = try String(contentsOfFile: path + filename, encoding: .utf8)
      cachedFiles[filename] = contents
      return contents
    }

    return cache
  } catch {
    fatalError(error.localizedDescription)
  }
}

func parseScalars(_ string: String) -> ClosedRange<UInt32> {
  // If we have . appear, it means we have a legitimate range. Otherwise,
  // it's a singular scalar.
  if string.contains(".") {
    let range = string.split(separator: ".")

    return UInt32(range[0], radix: 16)! ... UInt32(range[1], radix: 16)!
  } else {
    let scalar = UInt32(string, radix: 16)!

    return scalar ... scalar
  }
}

//===----------------------------------------------------------------------===//
// Binary Properties
//===----------------------------------------------------------------------===//

// Note: If one ever updates this list, be it adding new properties, removing,
// etc., please update the same list found in:
// 'stdlib/public/core/UnicodeScalarProperties.swift'.
let availableBinaryProperties: Set<String> = [
  "Alphabetic",
  "ASCII_Hex_Digit",
  "Bidi_Control",
  "Bidi_Mirrored",
  "Cased",
  "Case_Ignorable",
  "Changes_When_Casefolded",
  "Changes_When_Casemapped",
  "Changes_When_Lowercased",
  "Changes_When_NFKC_Casefolded",
  "Changes_When_Titlecased",
  "Changes_When_Uppercased",
  "Dash",
  "Default_Ignorable_Code_Point",
  "Deprecated",
  "Diacritic",
  "Emoji",
  "Emoji_Modifier",
  "Emoji_Modifier_Base",
  "Emoji_Presentation",
  "Extender",
  "Full_Composition_Exclusion",
  "Grapheme_Base",
  "Grapheme_Extend",
  "Hex_Digit",
  "ID_Continue",
  "ID_Start",
  "Ideographic",
  "IDS_Binary_Operator",
  "IDS_Trinary_Operator",
  "Join_Control",
  "Logical_Order_Exception",
  "Lowercase",
  "Math",
  "Noncharacter_Code_Point",
  "Other_Alphabetic",
  "Other_Default_Ignorable_Code_Point",
  "Other_Grapheme_Extend",
  "Other_ID_Continue",
  "Other_ID_Start",
  "Other_Lowercase",
  "Other_Math",
  "Other_Uppercase",
  "Pattern_Syntax",
  "Pattern_White_Space",
  "Quotation_Mark",
  "Radical",
  "Sentence_Terminal",
  "Soft_Dotted",
  "Terminal_Punctuation",
  "Unified_Ideograph",
  "Uppercase",
  "Variation_Selector",
  "White_Space",
  "XID_Continue",
  "XID_Start"
]

func parseBinaryProperties(
  _ data: String,
  into result: inout [String: Set<Unicode.Scalar>]
) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }

    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")
    
    // Get the property first because we may not care about it.
    let filteredProperty = components[1].filter { !$0.isWhitespace }
    
    guard availableBinaryProperties.contains(filteredProperty) else {
      continue
    }

    let filteredScalars = components[0].filter { !$0.isWhitespace }

    let scalars = parseScalars(String(filteredScalars))

    for scalar in scalars {
      result[filteredProperty, default: []].insert(Unicode.Scalar(scalar)!)
    }
  }
}

// A dictionary of all currently exposed Unicode Scalar Properties. Keyed by
// the literal property name and values being the set of scalars who all conform
// to said property.
public let binaryProperties: [String: Set<Unicode.Scalar>] = {
  var result: [String: Set<Unicode.Scalar>] = [:]

  #if canImport(Darwin)
  let derivedCoreProps = readInputFile("Apple/DerivedCoreProperties.txt")
  #else
  let derivedCoreProps = readInputFile("DerivedCoreProperties.txt")
  #endif
  parseBinaryProperties(derivedCoreProps, into: &result)

  let derivedNormalizationProps = readInputFile("DerivedNormalizationProps.txt")
  parseBinaryProperties(derivedNormalizationProps, into: &result)

  let derivedBinaryProperties = readInputFile("DerivedBinaryProperties.txt")
  parseBinaryProperties(derivedBinaryProperties, into: &result)

  let propList = readInputFile("PropList.txt")
  parseBinaryProperties(propList, into: &result)

  let emojiData = readInputFile("emoji-data.txt")
  parseBinaryProperties(emojiData, into: &result)

  return result
}()

//===----------------------------------------------------------------------===//
// Numeric Properties
//===----------------------------------------------------------------------===//

func parseNumericTypes(
  _ data: String,
  into result: inout [Unicode.Scalar: Unicode.NumericType]
) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }
    
    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")
    
    let filteredProperty = components[1].filter { !$0.isWhitespace }
    
    let numericType: Unicode.NumericType

    switch filteredProperty {
    case "Numeric":
      numericType = .numeric
    case "Decimal":
      numericType = .decimal
    case "Digit":
      numericType = .digit
    default:
      continue
    }
    
    let filteredScalars = components[0].filter { !$0.isWhitespace }
    
    let scalars = parseScalars(String(filteredScalars))

    for scalar in scalars {
      result[Unicode.Scalar(scalar)!] = numericType
    }
  }
}

func parseNumericValues(
  _ data: String,
  into result: inout [Unicode.Scalar: Double]
) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }
    
    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")
    
    let filteredProperty = components[3].filter { !$0.isWhitespace }
    
    let value: Double

    // If we have a division, split the numerator and denominator and perform
    // the division ourselves to get the correct double value.
    if filteredProperty.contains("/") {
      let splitDivision = filteredProperty.split(separator: "/")

      let numerator = Double(splitDivision[0])!
      let denominator = Double(splitDivision[1])!

      value = numerator / denominator
    } else {
      value = Double(filteredProperty)!
    }

    let filteredScalars = components[0].filter { !$0.isWhitespace }
    
    let scalars = parseScalars(String(filteredScalars))

    for scalar in scalars {
      result[Unicode.Scalar(scalar)!] = value
    }
  }
}

// A dictionary of every scalar who has a numeric type and it's value.
public let numericTypes: [Unicode.Scalar: Unicode.NumericType] = {
  var result: [Unicode.Scalar: Unicode.NumericType] = [:]

  let derivedNumericType = readInputFile("DerivedNumericType.txt")
  parseNumericTypes(derivedNumericType, into: &result)

  return result
}()

// A dictionary of scalar to numeric value.
public let numericValues: [Unicode.Scalar: Double] = {
  var result: [Unicode.Scalar: Double] = [:]

  let derivedNumericValues = readInputFile("DerivedNumericValues.txt")
  parseNumericValues(derivedNumericValues, into: &result)

  return result
}()

//===----------------------------------------------------------------------===//
// Scalar Mappings
//===----------------------------------------------------------------------===//

func parseMappings(
  _ data: String,
  into result: inout [Unicode.Scalar: [String: String]]
) {
  for line in data.split(separator: "\n") {
    let components = line.split(separator: ";", omittingEmptySubsequences: false)
    
    let scalarStr = components[0]
    guard let scalar = Unicode.Scalar(UInt32(scalarStr, radix: 16)!) else {
      continue
    }

    if let upper = UInt32(components[12], radix: 16) {
      let mapping = String(Unicode.Scalar(upper)!)

      result[scalar, default: [:]]["upper"] = mapping
    }

    if let lower = UInt32(components[13], radix: 16) {
      let mapping = String(Unicode.Scalar(lower)!)

      result[scalar, default: [:]]["lower"] = mapping
    }
    
    if let title = UInt32(components[14], radix: 16) {
      let mapping = String(Unicode.Scalar(title)!)

      result[scalar, default: [:]]["title"] = mapping
    }
  }
}

func parseSpecialMappings(
  _ data: String,
  into result: inout [Unicode.Scalar: [String: String]]
) {
  for line in data.split(separator: "\n") {
    guard !line.hasPrefix("#") else {
      continue
    }
    
    let components = line.split(separator: ";", omittingEmptySubsequences: false)
    
    // Conditional mappings have an extra component with the conditional name.
    // Ignore those.
    guard components.count == 5 else {
      continue
    }
    
    guard let scalar = Unicode.Scalar(UInt32(components[0], radix: 16)!) else {
      continue
    }
    
    let lower = components[1].split(separator: " ").map {
      Character(Unicode.Scalar(UInt32($0, radix: 16)!)!)
    }
    
    let title = components[2].split(separator: " ").map {
      Character(Unicode.Scalar(UInt32($0, radix: 16)!)!)
    }
    
    let upper = components[3].split(separator: " ").map {
      Character(Unicode.Scalar(UInt32($0, radix: 16)!)!)
    }

    if lower.count != 1 {
      result[scalar, default: [:]]["lower"] = String(lower)
    }

    if title.count != 1 {
      result[scalar, default: [:]]["title"] = String(title)
    }

    if upper.count != 1 {
      result[scalar, default: [:]]["upper"] = String(upper)
    }
  }
}

// A dictionary of every scalar mapping keyed by the scalar and returning another
// dictionary who is then keyed by either "lower", "upper", or "title".
public let mappings: [Unicode.Scalar: [String: String]] = {
  var result: [Unicode.Scalar: [String: String]] = [:]

  #if canImport(Darwin)
  let unicodeData = readInputFile("Apple/UnicodeData.txt")
  #else
  let unicodeData = readInputFile("UnicodeData.txt")
  #endif
  
  let specialCasing = readInputFile("SpecialCasing.txt")

  parseMappings(unicodeData, into: &result)
  parseSpecialMappings(specialCasing, into: &result)

  return result
}()

//===----------------------------------------------------------------------===//
// Scalar Age
//===----------------------------------------------------------------------===//

func parseAge(
  _ data: String,
  into result: inout [Unicode.Scalar: Unicode.Version]
) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }

    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")

    let filteredScalars = components[0].filter { !$0.isWhitespace }

    let scalars = parseScalars(String(filteredScalars))

    let version = components[1].filter { !$0.isWhitespace }
    let parts = version.split(separator: ".")

    let major = Int(parts[0])!
    let minor = Int(parts[1])!

    for scalar in scalars {
      guard let scalar = Unicode.Scalar(scalar) else {
        continue
      }

      result[scalar] = (major, minor)
    }
  }
}

public let ages: [Unicode.Scalar: Unicode.Version] = {
  var result: [Unicode.Scalar: Unicode.Version] = [:]

  let derivedAge = readInputFile("DerivedAge.txt")
  parseAge(derivedAge, into: &result)

  return result
}()

//===----------------------------------------------------------------------===//
// Scalar General Category
//===----------------------------------------------------------------------===//

func parseGeneralCategory(
  _ data: String,
  into result: inout [Unicode.Scalar: Unicode.GeneralCategory]
) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }

    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")

    let filteredCategory = components[1].filter { !$0.isWhitespace }

    let category: Unicode.GeneralCategory

    switch filteredCategory {
    case "Lu":
      category = .uppercaseLetter
    case "Ll":
      category = .lowercaseLetter
    case "Lt":
      category = .titlecaseLetter
    case "Lm":
      category = .modifierLetter
    case "Lo":
      category = .otherLetter
    case "Mn":
      category = .nonspacingMark
    case "Mc":
      category = .spacingMark
    case "Me":
      category = .enclosingMark
    case "Nd":
      category = .decimalNumber
    case "Nl":
      category = .letterNumber
    case "No":
      category = .otherNumber
    case "Pc":
      category = .connectorPunctuation
    case "Pd":
      category = .dashPunctuation
    case "Ps":
      category = .openPunctuation
    case "Pe":
      category = .closePunctuation
    case "Pi":
      category = .initialPunctuation
    case "Pf":
      category = .finalPunctuation
    case "Po":
      category = .otherPunctuation
    case "Sm":
      category = .mathSymbol
    case "Sc":
      category = .currencySymbol
    case "Sk":
      category = .modifierSymbol
    case "So":
      category = .otherSymbol
    case "Zs":
      category = .spaceSeparator
    case "Zl":
      category = .lineSeparator
    case "Zp":
      category = .paragraphSeparator
    case "Cc":
      category = .control
    case "Cf":
      category = .format
    case "Cs":
      category = .surrogate
    case "Co":
      category = .privateUse
    case "Cn":
      category = .unassigned
    default:
      continue
    }

    let filteredScalars = components[0].filter { !$0.isWhitespace }

    let scalars = parseScalars(String(filteredScalars))

    for scalar in scalars {
      guard let scalar = Unicode.Scalar(scalar) else {
        continue
      }

      result[scalar] = category
    }
  }
}

public let generalCategories: [Unicode.Scalar: Unicode.GeneralCategory] = {
  var result: [Unicode.Scalar: Unicode.GeneralCategory] = [:]

  let derivedGeneralCategory = readInputFile("DerivedGeneralCategory.txt")
  parseGeneralCategory(derivedGeneralCategory, into: &result)

  return result
}()

//===----------------------------------------------------------------------===//
// Scalar Name Alias
//===----------------------------------------------------------------------===//

func parseNameAliases(
  _ data: String,
  into result: inout [Unicode.Scalar: String]
) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }

    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")

    // Name aliases are only found with correction attribute.
    guard components[2] == "correction" else {
      continue
    }

    let filteredScalars = components[0].filter { !$0.isWhitespace }

    guard let scalar = Unicode.Scalar(UInt32(filteredScalars, radix: 16)!) else {
      continue
    }

    let nameAlias = String(components[1])

    result[scalar] = nameAlias
  }
}

public let nameAliases: [Unicode.Scalar: String] = {
  var result: [Unicode.Scalar: String] = [:]

  let nameAliases = readInputFile("NameAliases.txt")
  parseNameAliases(nameAliases, into: &result)

  return result
}()

//===----------------------------------------------------------------------===//
// Scalar Name
//===----------------------------------------------------------------------===//

func parseNames(
  _ data: String,
  into result: inout [Unicode.Scalar: String]
) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }

    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")

    let filteredScalars = components[0].filter { !$0.isWhitespace }

    let scalars = parseScalars(String(filteredScalars))

    // If our scalar is a range of scalars, then the name is usually something
    // like NAME-HERE-* where * is the scalar value.
    if scalars.count > 1 {
      for scalar in scalars {
        guard let scalar = Unicode.Scalar(scalar) else {
          continue
        }

        var name = String(components[1])
        // There's leftover spacing in the beginning of the name that we need to
        // get rid of.
        name.removeFirst()
        name.removeLast()
        name += "\(String(scalar.value, radix: 16, uppercase: true))"

        result[scalar] = name
      }
    } else {
      guard let scalar = Unicode.Scalar(scalars.lowerBound) else {
        continue
      }

      var name = String(components[1])
      // There's leftover spacing in the beginning of the name that we need to
      // get rid of.
      name.removeFirst()

      result[scalar] = name
    }
  }
}

public let names: [Unicode.Scalar: String] = {
  var result: [Unicode.Scalar: String] = [:]

  let derivedName = readInputFile("DerivedName.txt")
  parseNames(derivedName, into: &result)

  return result
}()

//===----------------------------------------------------------------------===//
// Case Folding
//===----------------------------------------------------------------------===//

func parseCaseFoldings(
  _ data: String,
  into result: inout [Unicode.Scalar: String]
) {
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }
    
    let components = line.split(separator: ";")
    
    let status = components[1].filter { !$0.isWhitespace }
    
    // We only care about Common and Full case mappings.
    guard status == "C" || status == "F" else {
      continue
    }
    
    let scalar = Unicode.Scalar(parseScalars(String(components[0])).lowerBound)!
    
    let mapping = components[2].split(separator: " ").map {
      Unicode.Scalar(UInt32($0, radix: 16)!)!
    }
    
    var mappingString = ""

    for scalar in mapping {
      mappingString.unicodeScalars.append(scalar)
    }

    result[scalar] = mappingString
  }
}

public let caseFolding: [Unicode.Scalar: String] = {
  var result: [Unicode.Scalar: String] = [:]

  let caseFolding = readInputFile("CaseFolding.txt")
  parseCaseFoldings(caseFolding, into: &result)

  return result
}()

#endif

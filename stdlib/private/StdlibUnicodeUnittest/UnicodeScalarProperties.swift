//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 - 2023 Apple Inc. and the Swift project authors
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
  let string = string.filter { !$0.isWhitespace }

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

//===----------------------------------------------------------------------===//
// Script/Script Extensions
//===----------------------------------------------------------------------===//

extension Unicode {
  /// Character script types.
  ///
  /// Note this includes the "meta" script type "Katakana_Or_Hiragana", which
  /// isn't defined by https://www.unicode.org/Public/UCD/latest/ucd/Scripts.txt,
  /// but is defined by https://www.unicode.org/Public/UCD/latest/ucd/PropertyValueAliases.txt.
  /// We may want to split it out, as it's the only case that is a union of
  /// other script types.
  public enum Script: String, Hashable {
    case adlam = "Adlam"
    case ahom = "Ahom"
    case anatolianHieroglyphs = "Anatolian_Hieroglyphs"
    case arabic = "Arabic"
    case armenian = "Armenian"
    case avestan = "Avestan"
    case balinese = "Balinese"
    case bamum = "Bamum"
    case bassaVah = "Bassa_Vah"
    case batak = "Batak"
    case bengali = "Bengali"
    case bhaiksuki = "Bhaiksuki"
    case bopomofo = "Bopomofo"
    case brahmi = "Brahmi"
    case braille = "Braille"
    case buginese = "Buginese"
    case buhid = "Buhid"
    case canadianAboriginal = "Canadian_Aboriginal"
    case carian = "Carian"
    case caucasianAlbanian = "Caucasian_Albanian"
    case chakma = "Chakma"
    case cham = "Cham"
    case cherokee = "Cherokee"
    case chorasmian = "Chorasmian"
    case common = "Common"
    case coptic = "Coptic"
    case cuneiform = "Cuneiform"
    case cypriot = "Cypriot"
    case cyrillic = "Cyrillic"
    case cyproMinoan = "Cypro_Minoan"
    case deseret = "Deseret"
    case devanagari = "Devanagari"
    case divesAkuru = "Dives_Akuru"
    case dogra = "Dogra"
    case duployan = "Duployan"
    case egyptianHieroglyphs = "Egyptian_Hieroglyphs"
    case elbasan = "Elbasan"
    case elymaic = "Elymaic"
    case ethiopic = "Ethiopic"
    case garay = "Garay"
    case georgian = "Georgian"
    case glagolitic = "Glagolitic"
    case gothic = "Gothic"
    case grantha = "Grantha"
    case greek = "Greek"
    case gujarati = "Gujarati"
    case gunjalaGondi = "Gunjala_Gondi"
    case gurmukhi = "Gurmukhi"
    case gurungKhema = "Gurung_Khema"
    case han = "Han"
    case hangul = "Hangul"
    case hanifiRohingya = "Hanifi_Rohingya"
    case hanunoo = "Hanunoo"
    case hatran = "Hatran"
    case hebrew = "Hebrew"
    case hiragana = "Hiragana"
    case imperialAramaic = "Imperial_Aramaic"
    case inherited = "Inherited"
    case inscriptionalPahlavi = "Inscriptional_Pahlavi"
    case inscriptionalParthian = "Inscriptional_Parthian"
    case javanese = "Javanese"
    case kaithi = "Kaithi"
    case kannada = "Kannada"
    case katakana = "Katakana"
    case katakanaOrHiragana = "Katakana_Or_Hiragana"
    case kawi = "Kawi"
    case kayahLi = "Kayah_Li"
    case kharoshthi = "Kharoshthi"
    case khitanSmallScript = "Khitan_Small_Script"
    case khmer = "Khmer"
    case khojki = "Khojki"
    case khudawadi = "Khudawadi"
    case lao = "Lao"
    case latin = "Latin"
    case lepcha = "Lepcha"
    case limbu = "Limbu"
    case linearA = "Linear_A"
    case linearB = "Linear_B"
    case lisu = "Lisu"
    case lycian = "Lycian"
    case lydian = "Lydian"
    case kiratRai = "Kirat_Rai"
    case mahajani = "Mahajani"
    case makasar = "Makasar"
    case malayalam = "Malayalam"
    case mandaic = "Mandaic"
    case manichaean = "Manichaean"
    case marchen = "Marchen"
    case masaramGondi = "Masaram_Gondi"
    case medefaidrin = "Medefaidrin"
    case meeteiMayek = "Meetei_Mayek"
    case mendeKikakui = "Mende_Kikakui"
    case meroiticCursive = "Meroitic_Cursive"
    case meroiticHieroglyphs = "Meroitic_Hieroglyphs"
    case miao = "Miao"
    case modi = "Modi"
    case mongolian = "Mongolian"
    case mro = "Mro"
    case multani = "Multani"
    case myanmar = "Myanmar"
    case nabataean = "Nabataean"
    case nagMundari = "Nag_Mundari"
    case nandinagari = "Nandinagari"
    case newa = "Newa"
    case newTaiLue = "New_Tai_Lue"
    case nko = "Nko"
    case nushu = "Nushu"
    case nyiakengPuachueHmong = "Nyiakeng_Puachue_Hmong"
    case ogham = "Ogham"
    case olChiki = "Ol_Chiki"
    case oldHungarian = "Old_Hungarian"
    case oldItalic = "Old_Italic"
    case oldNorthArabian = "Old_North_Arabian"
    case oldPermic = "Old_Permic"
    case oldPersian = "Old_Persian"
    case oldSogdian = "Old_Sogdian"
    case oldSouthArabian = "Old_South_Arabian"
    case oldTurkic = "Old_Turkic"
    case oldUyghur = "Old_Uyghur"
    case olOnal = "Ol_Onal"
    case oriya = "Oriya"
    case osage = "Osage"
    case osmanya = "Osmanya"
    case pahawhHmong = "Pahawh_Hmong"
    case palmyrene = "Palmyrene"
    case pauCinHau = "Pau_Cin_Hau"
    case phagsPa = "Phags_Pa"
    case phoenician = "Phoenician"
    case psalterPahlavi = "Psalter_Pahlavi"
    case rejang = "Rejang"
    case runic = "Runic"
    case samaritan = "Samaritan"
    case saurashtra = "Saurashtra"
    case sharada = "Sharada"
    case shavian = "Shavian"
    case siddham = "Siddham"
    case signWriting = "SignWriting"
    case sinhala = "Sinhala"
    case sogdian = "Sogdian"
    case soraSompeng = "Sora_Sompeng"
    case soyombo = "Soyombo"
    case sundanese = "Sundanese"
    case sunuwar = "Sunuwar"
    case sylotiNagri = "Syloti_Nagri"
    case syriac = "Syriac"
    case tagalog = "Tagalog"
    case tagbanwa = "Tagbanwa"
    case taiLe = "Tai_Le"
    case taiTham = "Tai_Tham"
    case taiViet = "Tai_Viet"
    case takri = "Takri"
    case tamil = "Tamil"
    case tangsa = "Tangsa"
    case tangut = "Tangut"
    case telugu = "Telugu"
    case thaana = "Thaana"
    case thai = "Thai"
    case tibetan = "Tibetan"
    case tifinagh = "Tifinagh"
    case tirhuta = "Tirhuta"
    case todhri = "Todhri"
    case toto = "Toto"
    case tuluTigalari = "Tulu_Tigalari"
    case ugaritic = "Ugaritic"
    case unknown = "Unknown"
    case vai = "Vai"
    case vithkuqi = "Vithkuqi"
    case wancho = "Wancho"
    case warangCiti = "Warang_Citi"
    case yezidi = "Yezidi"
    case yi = "Yi"
    case zanabazarSquare = "Zanabazar_Square"
  }
}

extension Character {
  /// Whether this character represents whitespace,
  /// for the purposes of pattern parsing.
  var isPatternWhitespace: Bool {
    unicodeScalars.first!.properties.isPatternWhitespace
  }
}

func withNormalizedForms<T>(
  _ str: String, requireInPrefix: Bool = false, match: (String) throws -> T?
) rethrows -> T? {
  // This follows the rules provided by UAX44-LM3, including trying to drop an
  // "is" prefix, which isn't required by UTS#18 RL1.2, but is nice for
  // consistency with other engines and the Unicode.Scalar.Properties names.
  let str = str.filter { !$0.isPatternWhitespace && $0 != "_" && $0 != "-" }
               .lowercased()
  if requireInPrefix {
    guard str.hasPrefix("in") else { return nil }
    return try match(String(str.dropFirst(2)))
  }
  if let m = try match(str) {
    return m
  }
  if str.hasPrefix("is"), let m = try match(String(str.dropFirst(2))) {
    return m
  }
  return nil
}

func classifyScriptProperty(
  _ value: String
) -> Unicode.Script? {
  // This uses the aliases defined in
  // https://www.unicode.org/Public/UCD/latest/ucd/PropertyValueAliases.txt.
  withNormalizedForms(value) { str in
    switch str {
    case "adlm", "adlam":                 return .adlam
    case "aghb", "caucasianalbanian":     return .caucasianAlbanian
    case "ahom":                          return .ahom
    case "arab", "arabic":                return .arabic
    case "armi", "imperialaramaic":       return .imperialAramaic
    case "armn", "armenian":              return .armenian
    case "avst", "avestan":               return .avestan
    case "bali", "balinese":              return .balinese
    case "bamu", "bamum":                 return .bamum
    case "bass", "bassavah":              return .bassaVah
    case "batk", "batak":                 return .batak
    case "beng", "bengali":               return .bengali
    case "bhks", "bhaiksuki":             return .bhaiksuki
    case "bopo", "bopomofo":              return .bopomofo
    case "brah", "brahmi":                return .brahmi
    case "brai", "braille":               return .braille
    case "bugi", "buginese":              return .buginese
    case "buhd", "buhid":                 return .buhid
    case "cakm", "chakma":                return .chakma
    case "cans", "canadianaboriginal":    return .canadianAboriginal
    case "cari", "carian":                return .carian
    case "cham":                          return .cham
    case "cher", "cherokee":              return .cherokee
    case "chrs", "chorasmian":            return .chorasmian
    case "copt", "coptic", "qaac":        return .coptic
    case "cpmn", "cyprominoan":           return .cyproMinoan
    case "cprt", "cypriot":               return .cypriot
    case "cyrl", "cyrillic":              return .cyrillic
    case "deva", "devanagari":            return .devanagari
    case "diak", "divesakuru":            return .divesAkuru
    case "dogr", "dogra":                 return .dogra
    case "dsrt", "deseret":               return .deseret
    case "dupl", "duployan":              return .duployan
    case "egyp", "egyptianhieroglyphs":   return .egyptianHieroglyphs
    case "elba", "elbasan":               return .elbasan
    case "elym", "elymaic":               return .elymaic
    case "ethi", "ethiopic":              return .ethiopic
    case "gara", "garay":                 return .garay
    case "geor", "georgian":              return .georgian
    case "glag", "glagolitic":            return .glagolitic
    case "gong", "gunjalagondi":          return .gunjalaGondi
    case "gonm", "masaramgondi":          return .masaramGondi
    case "goth", "gothic":                return .gothic
    case "gran", "grantha":               return .grantha
    case "grek", "greek":                 return .greek
    case "gujr", "gujarati":              return .gujarati
    case "gukh", "gurungkhema":           return .gurungKhema
    case "guru", "gurmukhi":              return .gurmukhi
    case "hang", "hangul":                return .hangul
    case "hani", "han":                   return .han
    case "hano", "hanunoo":               return .hanunoo
    case "hatr", "hatran":                return .hatran
    case "hebr", "hebrew":                return .hebrew
    case "hira", "hiragana":              return .hiragana
    case "hluw", "anatolianhieroglyphs":  return .anatolianHieroglyphs
    case "hmng", "pahawhhmong":           return .pahawhHmong
    case "hmnp", "nyiakengpuachuehmong":  return .nyiakengPuachueHmong
    case "hrkt", "katakanaorhiragana":    return .katakanaOrHiragana
    case "hung", "oldhungarian":          return .oldHungarian
    case "ital", "olditalic":             return .oldItalic
    case "java", "javanese":              return .javanese
    case "kali", "kayahli":               return .kayahLi
    case "kana", "katakana":              return .katakana
    case "kawi":                          return .kawi
    case "khar", "kharoshthi":            return .kharoshthi
    case "khmr", "khmer":                 return .khmer
    case "khoj", "khojki":                return .khojki
    case "kits", "khitansmallscript":     return .khitanSmallScript
    case "knda", "kannada":               return .kannada
    case "krai", "kiratrai":              return .kiratRai
    case "kthi", "kaithi":                return .kaithi
    case "lana", "taitham":               return .taiTham
    case "laoo", "lao":                   return .lao
    case "latn", "latin":                 return .latin
    case "lepc", "lepcha":                return .lepcha
    case "limb", "limbu":                 return .limbu
    case "lina", "lineara":               return .linearA
    case "linb", "linearb":               return .linearB
    case "lisu":                          return .lisu
    case "lyci", "lycian":                return .lycian
    case "lydi", "lydian":                return .lydian
    case "mahj", "mahajani":              return .mahajani
    case "maka", "makasar":               return .makasar
    case "mand", "mandaic":               return .mandaic
    case "mani", "manichaean":            return .manichaean
    case "marc", "marchen":               return .marchen
    case "medf", "medefaidrin":           return .medefaidrin
    case "mend", "mendekikakui":          return .mendeKikakui
    case "merc", "meroiticcursive":       return .meroiticCursive
    case "mero", "meroitichieroglyphs":   return .meroiticHieroglyphs
    case "mlym", "malayalam":             return .malayalam
    case "modi":                          return .modi
    case "mong", "mongolian":             return .mongolian
    case "mroo", "mro":                   return .mro
    case "mtei", "meeteimayek":           return .meeteiMayek
    case "mult", "multani":               return .multani
    case "mymr", "myanmar":               return .myanmar
    case "nagm", "nagmundari":            return .nagMundari
    case "nand", "nandinagari":           return .nandinagari
    case "narb", "oldnortharabian":       return .oldNorthArabian
    case "nbat", "nabataean":             return .nabataean
    case "newa":                          return .newa
    case "nkoo", "nko":                   return .nko
    case "nshu", "nushu":                 return .nushu
    case "ogam", "ogham":                 return .ogham
    case "olck", "olchiki":               return .olChiki
    case "onao", "olonal":                return .olOnal
    case "orkh", "oldturkic":             return .oldTurkic
    case "orya", "oriya":                 return .oriya
    case "osge", "osage":                 return .osage
    case "osma", "osmanya":               return .osmanya
    case "ougr", "olduyghur":             return .oldUyghur
    case "palm", "palmyrene":             return .palmyrene
    case "pauc", "paucinhau":             return .pauCinHau
    case "perm", "oldpermic":             return .oldPermic
    case "phag", "phagspa":               return .phagsPa
    case "phli", "inscriptionalpahlavi":  return .inscriptionalPahlavi
    case "phlp", "psalterpahlavi":        return .psalterPahlavi
    case "phnx", "phoenician":            return .phoenician
    case "plrd", "miao":                  return .miao
    case "prti", "inscriptionalparthian": return .inscriptionalParthian
    case "rjng", "rejang":                return .rejang
    case "rohg", "hanifirohingya":        return .hanifiRohingya
    case "runr", "runic":                 return .runic
    case "samr", "samaritan":             return .samaritan
    case "sarb", "oldsoutharabian":       return .oldSouthArabian
    case "saur", "saurashtra":            return .saurashtra
    case "sgnw", "signwriting":           return .signWriting
    case "shaw", "shavian":               return .shavian
    case "shrd", "sharada":               return .sharada
    case "sidd", "siddham":               return .siddham
    case "sind", "khudawadi":             return .khudawadi
    case "sinh", "sinhala":               return .sinhala
    case "sogd", "sogdian":               return .sogdian
    case "sogo", "oldsogdian":            return .oldSogdian
    case "sora", "sorasompeng":           return .soraSompeng
    case "soyo", "soyombo":               return .soyombo
    case "sund", "sundanese":             return .sundanese
    case "sunu", "sunuwar":               return .sunuwar
    case "sylo", "sylotinagri":           return .sylotiNagri
    case "syrc", "syriac":                return .syriac
    case "tagb", "tagbanwa":              return .tagbanwa
    case "takr", "takri":                 return .takri
    case "tale", "taile":                 return .taiLe
    case "talu", "newtailue":             return .newTaiLue
    case "taml", "tamil":                 return .tamil
    case "tang", "tangut":                return .tangut
    case "tavt", "taiviet":               return .taiViet
    case "telu", "telugu":                return .telugu
    case "tfng", "tifinagh":              return .tifinagh
    case "tglg", "tagalog":               return .tagalog
    case "thaa", "thaana":                return .thaana
    case "thai":                          return .thai
    case "tibt", "tibetan":               return .tibetan
    case "tirh", "tirhuta":               return .tirhuta
    case "tnsa", "tangsa":                return .tangsa
    case "todr", "todhri":                return .todhri
    case "toto":                          return .toto
    case "tutg", "tulutigalari":          return .tuluTigalari
    case "ugar", "ugaritic":              return .ugaritic
    case "vaii", "vai":                   return .vai
    case "vith", "vithkuqi":              return .vithkuqi
    case "wara", "warangciti":            return .warangCiti
    case "wcho", "wancho":                return .wancho
    case "xpeo", "oldpersian":            return .oldPersian
    case "xsux", "cuneiform":             return .cuneiform
    case "yezi", "yezidi":                return .yezidi
    case "yiii", "yi":                    return .yi
    case "zanb", "zanabazarsquare":       return .zanabazarSquare
    case "zinh", "inherited", "qaai":     return .inherited
    case "zyyy", "common":                return .common
    case "zzzz", "unknown":               return .unknown
    default:                              return nil
    }
  }
}

func parseScripts(
  _ data: String
) -> [Unicode.Scalar: [Unicode.Script]] {
  var result: [Unicode.Scalar: [Unicode.Script]] = [:]

  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }

    let components = line.split(separator: ";")
    let scriptStr = components[1].split(separator: "#")[0].split(separator: " ")

    let scripts = scriptStr.map {
      classifyScriptProperty(String($0))!
    }

    let scalars = parseScalars(String(components[0]))

    for scalar in scalars {
      result[Unicode.Scalar(scalar)!] = scripts
    }
  }

  return result
}

public let scripts: [Unicode.Scalar: Unicode.Script] = {
  let scripts = readInputFile("Scripts.txt")
  // Parse scripts will return an array for each scalar, but this file only
  // defines a single script per scalar.
  let result = parseScripts(scripts).mapValues {
    $0[0]
  }

  return result
}()

public let scriptExtensions: [Unicode.Scalar: [Unicode.Script]] = {
  let scripts = readInputFile("ScriptExtensions.txt")
  let result = parseScripts(scripts)

  return result
}()

#endif

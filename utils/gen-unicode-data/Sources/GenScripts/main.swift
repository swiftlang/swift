//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import GenUtils

extension Unicode {
  /// Character script types.
  ///
  /// Note this includes the "meta" script type "Katakana_Or_Hiragana", which
  /// isn't defined by https://www.unicode.org/Public/UCD/latest/ucd/Scripts.txt,
  /// but is defined by https://www.unicode.org/Public/UCD/latest/ucd/PropertyValueAliases.txt.
  /// We may want to split it out, as it's the only case that is a union of
  /// other script types.
  public enum Script: String, Hashable, CaseIterable {
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

func scriptAbbr2Enum(_ str: String) -> Unicode.Script {
  switch str {
  case "Adlm", "adlam":                 return .adlam
  case "Aghb", "caucasianalbanian":     return .caucasianAlbanian
  case "Ahom":                          return .ahom
  case "Arab", "arabic":                return .arabic
  case "Armi", "imperialaramaic":       return .imperialAramaic
  case "Armn", "armenian":              return .armenian
  case "Avst", "avestan":               return .avestan
  case "Bali", "balinese":              return .balinese
  case "Bamu", "bamum":                 return .bamum
  case "Bass", "bassavah":              return .bassaVah
  case "Batk", "batak":                 return .batak
  case "Beng", "bengali":               return .bengali
  case "Bhks", "bhaiksuki":             return .bhaiksuki
  case "Bopo", "bopomofo":              return .bopomofo
  case "Brah", "brahmi":                return .brahmi
  case "Brai", "braille":               return .braille
  case "Bugi", "buginese":              return .buginese
  case "Buhd", "buhid":                 return .buhid
  case "Cakm", "chakma":                return .chakma
  case "Cans", "canadianaboriginal":    return .canadianAboriginal
  case "Cari", "carian":                return .carian
  case "Cham":                          return .cham
  case "Cher", "cherokee":              return .cherokee
  case "Chrs", "chorasmian":            return .chorasmian
  case "Copt", "coptic", "qaac":        return .coptic
  case "Cpmn", "cyprominoan":           return .cyproMinoan
  case "Cprt", "cypriot":               return .cypriot
  case "Cyrl", "cyrillic":              return .cyrillic
  case "Deva", "devanagari":            return .devanagari
  case "Diak", "divesakuru":            return .divesAkuru
  case "Dogr", "dogra":                 return .dogra
  case "Dsrt", "deseret":               return .deseret
  case "Dupl", "duployan":              return .duployan
  case "Egyp", "egyptianhieroglyphs":   return .egyptianHieroglyphs
  case "Elba", "elbasan":               return .elbasan
  case "Elym", "elymaic":               return .elymaic
  case "Ethi", "ethiopic":              return .ethiopic
  case "Gara":                          return .garay
  case "Geor", "georgian":              return .georgian
  case "Glag", "glagolitic":            return .glagolitic
  case "Gong", "gunjalagondi":          return .gunjalaGondi
  case "Gonm", "masaramgondi":          return .masaramGondi
  case "Goth", "gothic":                return .gothic
  case "Gran", "grantha":               return .grantha
  case "Grek", "greek":                 return .greek
  case "Gujr", "gujarati":              return .gujarati
  case "Gukh":                          return .gurungKhema
  case "Guru", "gurmukhi":              return .gurmukhi
  case "Hang", "hangul":                return .hangul
  case "Hani", "han":                   return .han
  case "Hano", "hanunoo":               return .hanunoo
  case "Hatr", "hatran":                return .hatran
  case "Hebr", "hebrew":                return .hebrew
  case "Hira", "hiragana":              return .hiragana
  case "Hluw", "anatolianhieroglyphs":  return .anatolianHieroglyphs
  case "Hmng", "pahawhhmong":           return .pahawhHmong
  case "Hmnp", "nyiakengpuachuehmong":  return .nyiakengPuachueHmong
  case "Hrkt", "katakanaorhiragana":    return .katakanaOrHiragana
  case "Hung", "oldhungarian":          return .oldHungarian
  case "Ital", "olditalic":             return .oldItalic
  case "Java", "javanese":              return .javanese
  case "Kali", "kayahli":               return .kayahLi
  case "Kawi":                          return .kawi
  case "Kana", "katakana":              return .katakana
  case "Khar", "kharoshthi":            return .kharoshthi
  case "Khmr", "khmer":                 return .khmer
  case "Khoj", "khojki":                return .khojki
  case "Kits", "khitansmallscript":     return .khitanSmallScript
  case "Knda", "kannada":               return .kannada
  case "Krai":                          return .kiratRai
  case "Kthi", "kaithi":                return .kaithi
  case "Lana", "taitham":               return .taiTham
  case "Laoo", "lao":                   return .lao
  case "Latn", "latin":                 return .latin
  case "Lepc", "lepcha":                return .lepcha
  case "Limb", "limbu":                 return .limbu
  case "Lina", "lineara":               return .linearA
  case "Linb", "linearb":               return .linearB
  case "Lisu":                          return .lisu
  case "Lyci", "lycian":                return .lycian
  case "Lydi", "lydian":                return .lydian
  case "Mahj", "mahajani":              return .mahajani
  case "Maka", "makasar":               return .makasar
  case "Mand", "mandaic":               return .mandaic
  case "Mani", "manichaean":            return .manichaean
  case "Marc", "marchen":               return .marchen
  case "Medf", "medefaidrin":           return .medefaidrin
  case "Mend", "mendekikakui":          return .mendeKikakui
  case "Merc", "meroiticcursive":       return .meroiticCursive
  case "Mero", "meroitichieroglyphs":   return .meroiticHieroglyphs
  case "Mlym", "malayalam":             return .malayalam
  case "Modi":                          return .modi
  case "Mong", "mongolian":             return .mongolian
  case "Mroo", "mro":                   return .mro
  case "Mtei", "meeteimayek":           return .meeteiMayek
  case "Mult", "multani":               return .multani
  case "Mymr", "myanmar":               return .myanmar
  case "Nagm":                          return .nagMundari
  case "Nand", "nandinagari":           return .nandinagari
  case "Narb", "oldnortharabian":       return .oldNorthArabian
  case "Nbat", "nabataean":             return .nabataean
  case "Newa":                          return .newa
  case "Nkoo", "nko":                   return .nko
  case "Nshu", "nushu":                 return .nushu
  case "Ogam", "ogham":                 return .ogham
  case "Olck", "olchiki":               return .olChiki
  case "Onao":                          return .olOnal
  case "Orkh", "oldturkic":             return .oldTurkic
  case "Orya", "oriya":                 return .oriya
  case "Osge", "osage":                 return .osage
  case "Osma", "osmanya":               return .osmanya
  case "Ougr", "olduyghur":             return .oldUyghur
  case "Palm", "palmyrene":             return .palmyrene
  case "Pauc", "paucinhau":             return .pauCinHau
  case "Perm", "oldpermic":             return .oldPermic
  case "Phag", "phagspa":               return .phagsPa
  case "Phli", "inscriptionalpahlavi":  return .inscriptionalPahlavi
  case "Phlp", "psalterpahlavi":        return .psalterPahlavi
  case "Phnx", "phoenician":            return .phoenician
  case "Plrd", "miao":                  return .miao
  case "Prti", "inscriptionalparthian": return .inscriptionalParthian
  case "Rjng", "rejang":                return .rejang
  case "Rohg", "hanifirohingya":        return .hanifiRohingya
  case "Runr", "runic":                 return .runic
  case "Samr", "samaritan":             return .samaritan
  case "Sarb", "oldsoutharabian":       return .oldSouthArabian
  case "Saur", "saurashtra":            return .saurashtra
  case "Sgnw", "signwriting":           return .signWriting
  case "Shaw", "shavian":               return .shavian
  case "Shrd", "sharada":               return .sharada
  case "Sidd", "siddham":               return .siddham
  case "Sind", "khudawadi":             return .khudawadi
  case "Sinh", "sinhala":               return .sinhala
  case "Sogd", "sogdian":               return .sogdian
  case "Sogo", "oldsogdian":            return .oldSogdian
  case "Sora", "sorasompeng":           return .soraSompeng
  case "Soyo", "soyombo":               return .soyombo
  case "Sund", "sundanese":             return .sundanese
  case "Sunu":                          return .sunuwar
  case "Sylo", "sylotinagri":           return .sylotiNagri
  case "Syrc", "syriac":                return .syriac
  case "Tagb", "tagbanwa":              return .tagbanwa
  case "Takr", "takri":                 return .takri
  case "Tale", "taile":                 return .taiLe
  case "Talu", "newtailue":             return .newTaiLue
  case "Taml", "tamil":                 return .tamil
  case "Tang", "tangut":                return .tangut
  case "Tavt", "taiviet":               return .taiViet
  case "Telu", "telugu":                return .telugu
  case "Tfng", "tifinagh":              return .tifinagh
  case "Tglg", "tagalog":               return .tagalog
  case "Thaa", "thaana":                return .thaana
  case "Thai":                          return .thai
  case "Tibt", "tibetan":               return .tibetan
  case "Tirh", "tirhuta":               return .tirhuta
  case "Tnsa", "tangsa":                return .tangsa
  case "Todr":                          return .todhri
  case "Toto":                          return .toto
  case "Tutg":                          return .tuluTigalari
  case "Ugar", "ugaritic":              return .ugaritic
  case "Vaii", "vai":                   return .vai
  case "Vith", "vithkuqi":              return .vithkuqi
  case "Wara", "warangciti":            return .warangCiti
  case "Wcho", "wancho":                return .wancho
  case "Xpeo", "oldpersian":            return .oldPersian
  case "Xsux", "cuneiform":             return .cuneiform
  case "Yezi", "yezidi":                return .yezidi
  case "Yiii", "yi":                    return .yi
  case "Zanb", "zanabazarsquare":       return .zanabazarSquare
  case "Zinh", "inherited", "qaai":     return .inherited
  case "Zyyy", "common":                return .common
  case "Zzzz", "unknown":               return .unknown
  default:                              fatalError("Unknown script: \(str)")
  }
}

func getScriptData(
  for path: String
) -> [(ClosedRange<UInt32>, String)] {
  let data = readFile(path)
  
  var unflattened: [(ClosedRange<UInt32>, String)] = []
  
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }
    
    // Each line in this file is broken up into two sections:
    // 1: Either the singular scalar or a range of scalars who conform to said
    //    grapheme break property.
    // 2: The script that said scalar(s) conforms to.
    let components = line.split(separator: ";")
    
    // Get the script first because it may be one we don't care about.
    let splitProperty = components[1].split(separator: "#")
    let filteredProperty = splitProperty[0].filter { !$0.isWhitespace }
    
    guard Unicode.Script(rawValue: filteredProperty) != nil else {
      fatalError("Please add the following script: \(filteredProperty)")
    }

    let scalars: ClosedRange<UInt32>
    
    let filteredScalars = components[0].filter { !$0.isWhitespace }
    
    // If we have . appear, it means we have a legitimate range. Otherwise,
    // it's a singular scalar.
    if filteredScalars.contains(".") {
      let range = filteredScalars.split(separator: ".")
      
      scalars = UInt32(range[0], radix: 16)! ... UInt32(range[1], radix: 16)!
    } else {
      let scalar = UInt32(filteredScalars, radix: 16)!
      
      scalars = scalar ... scalar
    }
    
    unflattened.append((scalars, filteredProperty))
  }
  
  return flatten(unflattened)
}

func getScriptExtensionData(
  for path: String
) -> [(ClosedRange<UInt32>, [String])] {
  let data = readFile(path)
  
  var unflattened: [(ClosedRange<UInt32>, [String])] = []
  
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }
    
    // Each line in this file is broken up into two sections:
    // 1: Either the singular scalar or a range of scalars who conform to said
    //    grapheme break property.
    // 2: The grapheme break property that said scalar(s) conform to (with
    //    additional comments noting the character category, name and amount of
    //    scalars the range represents).
    let components = line.split(separator: ";")
    
    // Get the property first because it may be one we don't care about.
    let splitProperty = components[1].split(separator: "#")
    let scripts = splitProperty[0].split(separator: " ").map { String($0) }
    
    let scalars: ClosedRange<UInt32>
    
    let filteredScalars = components[0].filter { !$0.isWhitespace }
    
    // If we have . appear, it means we have a legitimate range. Otherwise,
    // it's a singular scalar.
    if filteredScalars.contains(".") {
      let range = filteredScalars.split(separator: ".")
      
      scalars = UInt32(range[0], radix: 16)! ... UInt32(range[1], radix: 16)!
    } else {
      let scalar = UInt32(filteredScalars, radix: 16)!
      
      scalars = scalar ... scalar
    }
    
    unflattened.append((scalars, scripts))
  }
  
  return flatten(unflattened)
}

func emitScriptData(
  _ data: [(ClosedRange<UInt32>, String)],
  into result: inout String
) {
  var scriptData: [UInt32: Unicode.Script] = [:]
  
  for (range, property) in data {
    for scalar in range {
      scriptData[scalar] = Unicode.Script(rawValue: property)
    }
  }
  
  for i in 0x0 ... 0x10FFFF {
    guard let scalar = Unicode.Scalar(i) else {
      continue
    }
    
    if !scriptData.keys.contains(scalar.value) {
      scriptData[scalar.value] = .unknown
    }
  }
  
  let data = flatten(Array(scriptData))

  result += """
  #define SCRIPTS_COUNT \(data.count)

  
  """

  emitCollection(
    data,
    name: "_swift_stdlib_scripts",
    type: "__swift_uint32_t",
    into: &result
  ) {
    var value = $0.0.lowerBound
    value |= UInt32(unsafeBitCast($0.1, to: UInt8.self)) << 21
    
    return "0x\(String(value, radix: 16, uppercase: true))"
  }
}

func emitScriptExtensionData(
  _ data: [(ClosedRange<UInt32>, [String])],
  into result: inout String
) {
  var indices: [[String]: UInt16] = [:]
  var bytes: [UInt8] = []
  var currentIndex: UInt16 = 0
  
  for (_, scripts) in data {
    guard !indices.keys.contains(scripts) else {
      continue
    }
    
    indices[scripts] = currentIndex | (UInt16(scripts.count) << 11)
    
    for script in scripts {
      let scriptEnum = scriptAbbr2Enum(script)
      let byte = unsafeBitCast(scriptEnum, to: UInt8.self)
      
      bytes.append(byte)
      currentIndex += 1
    }
  }
  
  // 64 bit arrays * 8 bytes = .512 KB
  var bitArrays: [BitArray] = .init(repeating: .init(size: 64), count: 64)
  
  let chunkSize = 0x110000 / 64 / 64
  
  var chunks: [Int] = []
  
  for i in 0 ..< 64 * 64 {
    let lower = i * chunkSize
    let upper = lower + chunkSize - 1
    
    let idx = i / 64
    let bit = i % 64
    
    for scalar in lower ... upper {
      if data.contains(where: { $0.0.contains(UInt32(scalar)) }) {
        chunks.append(i)
        
        bitArrays[idx][bit] = true
        break
      }
    }
  }
  
  // Remove the trailing 0s. Currently this reduces quick look size down to
  // 96 bytes from 512 bytes.
  var reducedBA = Array(bitArrays.reversed())
  reducedBA = Array(reducedBA.drop {
    $0.words == [0x0]
  })
  
  bitArrays = reducedBA.reversed()
  
  // Keep a record of every rank for all the bitarrays.
  var ranks: [UInt16] = []
  
  // Record our quick look ranks.
  var lastRank: UInt16 = 0
  for (i, _) in bitArrays.enumerated() {
    guard i != 0 else {
      ranks.append(0)
      continue
    }
    
    var rank = UInt16(bitArrays[i - 1].words[0].nonzeroBitCount)
    rank += lastRank
    
    ranks.append(rank)
    
    lastRank = rank
  }
  
  // Insert our quick look size at the beginning.
  var size = BitArray(size: 64)
  size.words = [UInt64(bitArrays.count)]
  bitArrays.insert(size, at: 0)
  
  var dataIndices: [UInt16] = []
  
  for chunk in chunks {
    var chunkBA = BitArray(size: chunkSize)
    
    let lower = chunk * chunkSize
    let upper = lower + chunkSize
    
    let chunkDataIdx = UInt64(dataIndices.endIndex)
    
    // Insert our chunk's data index in the upper bits of the last word of our
    // bit array.
    chunkBA.words[chunkBA.words.endIndex - 1] |= chunkDataIdx << 16
    
    for scalar in lower ..< upper {
      if data.contains(where: { $0.0.contains(UInt32(scalar)) }) {
        chunkBA[scalar % chunkSize] = true
        
        let data = data[data.firstIndex {
          $0.0.contains(UInt32(scalar))
        }!].1
        
        dataIndices.append(indices[data]!)
      }
    }
    
    // Append our chunk bit array's rank.
    var lastRank: UInt16 = 0
    for (i, _) in chunkBA.words.enumerated() {
      guard i != 0 else {
        ranks.append(0)
        continue
      }
      
      var rank = UInt16(chunkBA.words[i - 1].nonzeroBitCount)
      rank += lastRank
      
      ranks.append(rank)
      lastRank = rank
    }
    
    bitArrays += chunkBA.words.map {
      var ba = BitArray(size: 64)
      ba.words = [$0]
      return ba
    }
  }
  
  emitCollection(
    bytes,
    name: "_swift_stdlib_script_extensions_data",
    into: &result
  )
  
  emitCollection(
    dataIndices,
    name: "_swift_stdlib_script_extensions_data_indices",
    into: &result
  )
  
  emitCollection(
    ranks,
    name: "_swift_stdlib_script_extensions_ranks",
    into: &result
  )
  
  emitCollection(
    bitArrays,
    name: "_swift_stdlib_script_extensions",
    type: "__swift_uint64_t",
    into: &result
  ) {
    assert($0.words.count == 1)
    return "0x\(String($0.words[0], radix: 16, uppercase: true))"
  }
}

func generateScriptProperties() {
  var result = readFile("Input/ScriptData.h")
  
  let data = getScriptData(for: "Data/16/Scripts.txt")
  emitScriptData(data, into: &result)
  
  let extensionData = getScriptExtensionData(for: "Data/16/ScriptExtensions.txt")
  emitScriptExtensionData(extensionData, into: &result)

  result += """
  #endif // #ifndef SCRIPT_DATA_H

  """

  write(result, to: "Output/Common/ScriptData.h")
}

generateScriptProperties()


struct sfntDirectoryEntry {
  var tableTag: FourCharCode
  var checkSum: UInt32
  var offset: UInt32
  var length: UInt32
  init()
  init(tableTag tableTag: FourCharCode, checkSum checkSum: UInt32, offset offset: UInt32, length length: UInt32)
}
struct sfntDirectory {
  var format: FourCharCode
  var numOffsets: UInt16
  var searchRange: UInt16
  var entrySelector: UInt16
  var rangeShift: UInt16
  var table: (sfntDirectoryEntry)
  init()
  init(format format: FourCharCode, numOffsets numOffsets: UInt16, searchRange searchRange: UInt16, entrySelector entrySelector: UInt16, rangeShift rangeShift: UInt16, table table: (sfntDirectoryEntry))
}
var sizeof_sfntDirectory: Int { get }
var cmapFontTableTag: Int { get }
var kFontUnicodePlatform: Int { get }
var kFontMacintoshPlatform: Int { get }
var kFontReservedPlatform: Int { get }
var kFontMicrosoftPlatform: Int { get }
var kFontCustomPlatform: Int { get }
var kFontUnicodeDefaultSemantics: Int { get }
var kFontUnicodeV1_1Semantics: Int { get }
var kFontISO10646_1993Semantics: Int { get }
var kFontUnicodeV2_0BMPOnlySemantics: Int { get }
var kFontUnicodeV2_0FullCoverageSemantics: Int { get }
var kFontUnicodeV4_0VariationSequenceSemantics: Int { get }
var kFontUnicode_FullRepertoire: Int { get }
var kFontRomanScript: Int { get }
var kFontJapaneseScript: Int { get }
var kFontTraditionalChineseScript: Int { get }
var kFontChineseScript: Int { get }
var kFontKoreanScript: Int { get }
var kFontArabicScript: Int { get }
var kFontHebrewScript: Int { get }
var kFontGreekScript: Int { get }
var kFontCyrillicScript: Int { get }
var kFontRussian: Int { get }
var kFontRSymbolScript: Int { get }
var kFontDevanagariScript: Int { get }
var kFontGurmukhiScript: Int { get }
var kFontGujaratiScript: Int { get }
var kFontOriyaScript: Int { get }
var kFontBengaliScript: Int { get }
var kFontTamilScript: Int { get }
var kFontTeluguScript: Int { get }
var kFontKannadaScript: Int { get }
var kFontMalayalamScript: Int { get }
var kFontSinhaleseScript: Int { get }
var kFontBurmeseScript: Int { get }
var kFontKhmerScript: Int { get }
var kFontThaiScript: Int { get }
var kFontLaotianScript: Int { get }
var kFontGeorgianScript: Int { get }
var kFontArmenianScript: Int { get }
var kFontSimpleChineseScript: Int { get }
var kFontTibetanScript: Int { get }
var kFontMongolianScript: Int { get }
var kFontGeezScript: Int { get }
var kFontEthiopicScript: Int { get }
var kFontAmharicScript: Int { get }
var kFontSlavicScript: Int { get }
var kFontEastEuropeanRomanScript: Int { get }
var kFontVietnameseScript: Int { get }
var kFontExtendedArabicScript: Int { get }
var kFontSindhiScript: Int { get }
var kFontUninterpretedScript: Int { get }
var kFontMicrosoftSymbolScript: Int { get }
var kFontMicrosoftStandardScript: Int { get }
var kFontMicrosoftUCS4Script: Int { get }
var kFontCustom8BitScript: Int { get }
var kFontCustom816BitScript: Int { get }
var kFontCustom16BitScript: Int { get }
var kFontEnglishLanguage: Int { get }
var kFontFrenchLanguage: Int { get }
var kFontGermanLanguage: Int { get }
var kFontItalianLanguage: Int { get }
var kFontDutchLanguage: Int { get }
var kFontSwedishLanguage: Int { get }
var kFontSpanishLanguage: Int { get }
var kFontDanishLanguage: Int { get }
var kFontPortugueseLanguage: Int { get }
var kFontNorwegianLanguage: Int { get }
var kFontHebrewLanguage: Int { get }
var kFontJapaneseLanguage: Int { get }
var kFontArabicLanguage: Int { get }
var kFontFinnishLanguage: Int { get }
var kFontGreekLanguage: Int { get }
var kFontIcelandicLanguage: Int { get }
var kFontMalteseLanguage: Int { get }
var kFontTurkishLanguage: Int { get }
var kFontCroatianLanguage: Int { get }
var kFontTradChineseLanguage: Int { get }
var kFontUrduLanguage: Int { get }
var kFontHindiLanguage: Int { get }
var kFontThaiLanguage: Int { get }
var kFontKoreanLanguage: Int { get }
var kFontLithuanianLanguage: Int { get }
var kFontPolishLanguage: Int { get }
var kFontHungarianLanguage: Int { get }
var kFontEstonianLanguage: Int { get }
var kFontLettishLanguage: Int { get }
var kFontLatvianLanguage: Int { get }
var kFontSaamiskLanguage: Int { get }
var kFontLappishLanguage: Int { get }
var kFontFaeroeseLanguage: Int { get }
var kFontFarsiLanguage: Int { get }
var kFontPersianLanguage: Int { get }
var kFontRussianLanguage: Int { get }
var kFontSimpChineseLanguage: Int { get }
var kFontFlemishLanguage: Int { get }
var kFontIrishLanguage: Int { get }
var kFontAlbanianLanguage: Int { get }
var kFontRomanianLanguage: Int { get }
var kFontCzechLanguage: Int { get }
var kFontSlovakLanguage: Int { get }
var kFontSlovenianLanguage: Int { get }
var kFontYiddishLanguage: Int { get }
var kFontSerbianLanguage: Int { get }
var kFontMacedonianLanguage: Int { get }
var kFontBulgarianLanguage: Int { get }
var kFontUkrainianLanguage: Int { get }
var kFontByelorussianLanguage: Int { get }
var kFontUzbekLanguage: Int { get }
var kFontKazakhLanguage: Int { get }
var kFontAzerbaijaniLanguage: Int { get }
var kFontAzerbaijanArLanguage: Int { get }
var kFontArmenianLanguage: Int { get }
var kFontGeorgianLanguage: Int { get }
var kFontMoldavianLanguage: Int { get }
var kFontKirghizLanguage: Int { get }
var kFontTajikiLanguage: Int { get }
var kFontTurkmenLanguage: Int { get }
var kFontMongolianLanguage: Int { get }
var kFontMongolianCyrLanguage: Int { get }
var kFontPashtoLanguage: Int { get }
var kFontKurdishLanguage: Int { get }
var kFontKashmiriLanguage: Int { get }
var kFontSindhiLanguage: Int { get }
var kFontTibetanLanguage: Int { get }
var kFontNepaliLanguage: Int { get }
var kFontSanskritLanguage: Int { get }
var kFontMarathiLanguage: Int { get }
var kFontBengaliLanguage: Int { get }
var kFontAssameseLanguage: Int { get }
var kFontGujaratiLanguage: Int { get }
var kFontPunjabiLanguage: Int { get }
var kFontOriyaLanguage: Int { get }
var kFontMalayalamLanguage: Int { get }
var kFontKannadaLanguage: Int { get }
var kFontTamilLanguage: Int { get }
var kFontTeluguLanguage: Int { get }
var kFontSinhaleseLanguage: Int { get }
var kFontBurmeseLanguage: Int { get }
var kFontKhmerLanguage: Int { get }
var kFontLaoLanguage: Int { get }
var kFontVietnameseLanguage: Int { get }
var kFontIndonesianLanguage: Int { get }
var kFontTagalogLanguage: Int { get }
var kFontMalayRomanLanguage: Int { get }
var kFontMalayArabicLanguage: Int { get }
var kFontAmharicLanguage: Int { get }
var kFontTigrinyaLanguage: Int { get }
var kFontGallaLanguage: Int { get }
var kFontOromoLanguage: Int { get }
var kFontSomaliLanguage: Int { get }
var kFontSwahiliLanguage: Int { get }
var kFontRuandaLanguage: Int { get }
var kFontRundiLanguage: Int { get }
var kFontChewaLanguage: Int { get }
var kFontMalagasyLanguage: Int { get }
var kFontEsperantoLanguage: Int { get }
var kFontWelshLanguage: Int { get }
var kFontBasqueLanguage: Int { get }
var kFontCatalanLanguage: Int { get }
var kFontLatinLanguage: Int { get }
var kFontQuechuaLanguage: Int { get }
var kFontGuaraniLanguage: Int { get }
var kFontAymaraLanguage: Int { get }
var kFontTatarLanguage: Int { get }
var kFontUighurLanguage: Int { get }
var kFontDzongkhaLanguage: Int { get }
var kFontJavaneseRomLanguage: Int { get }
var kFontSundaneseRomLanguage: Int { get }
var kFontNoPlatformCode: UInt32 { get }
var kFontNoScriptCode: UInt32 { get }
var kFontNoLanguageCode: UInt32 { get }
struct sfntCMapSubHeader {
  var format: UInt16
  var length: UInt16
  var languageID: UInt16
  init()
  init(format format: UInt16, length length: UInt16, languageID languageID: UInt16)
}
var sizeof_sfntCMapSubHeader: Int { get }
struct sfntCMapExtendedSubHeader {
  var format: UInt16
  var reserved: UInt16
  var length: UInt32
  var language: UInt32
  init()
  init(format format: UInt16, reserved reserved: UInt16, length length: UInt32, language language: UInt32)
}
var sizeof_sfntCMapExtendedSubHeader: Int { get }
struct sfntCMapEncoding {
  var platformID: UInt16
  var scriptID: UInt16
  var offset: UInt32
  init()
  init(platformID platformID: UInt16, scriptID scriptID: UInt16, offset offset: UInt32)
}
var sizeof_sfntCMapEncoding: Int { get }
struct sfntCMapHeader {
  var version: UInt16
  var numTables: UInt16
  var encoding: (sfntCMapEncoding)
  init()
  init(version version: UInt16, numTables numTables: UInt16, encoding encoding: (sfntCMapEncoding))
}
var sizeof_sfntCMapHeader: Int { get }
var nameFontTableTag: Int { get }
var kFontCopyrightName: Int { get }
var kFontFamilyName: Int { get }
var kFontStyleName: Int { get }
var kFontUniqueName: Int { get }
var kFontFullName: Int { get }
var kFontVersionName: Int { get }
var kFontPostscriptName: Int { get }
var kFontTrademarkName: Int { get }
var kFontManufacturerName: Int { get }
var kFontDesignerName: Int { get }
var kFontDescriptionName: Int { get }
var kFontVendorURLName: Int { get }
var kFontDesignerURLName: Int { get }
var kFontLicenseDescriptionName: Int { get }
var kFontLicenseInfoURLName: Int { get }
var kFontPreferredFamilyName: Int { get }
var kFontPreferredSubfamilyName: Int { get }
var kFontMacCompatibleFullName: Int { get }
var kFontSampleTextName: Int { get }
var kFontPostScriptCIDName: Int { get }
var kFontLastReservedName: Int { get }
var kFontNoNameCode: UInt32 { get }
struct sfntNameRecord {
  var platformID: UInt16
  var scriptID: UInt16
  var languageID: UInt16
  var nameID: UInt16
  var length: UInt16
  var offset: UInt16
  init()
  init(platformID platformID: UInt16, scriptID scriptID: UInt16, languageID languageID: UInt16, nameID nameID: UInt16, length length: UInt16, offset offset: UInt16)
}
var sizeof_sfntNameRecord: Int { get }
struct sfntNameHeader {
  var format: UInt16
  var count: UInt16
  var stringOffset: UInt16
  var rec: (sfntNameRecord)
  init()
  init(format format: UInt16, count count: UInt16, stringOffset stringOffset: UInt16, rec rec: (sfntNameRecord))
}
var sizeof_sfntNameHeader: Int { get }
var variationFontTableTag: Int { get }
struct sfntVariationAxis {
  var axisTag: FourCharCode
  var minValue: Fixed
  var defaultValue: Fixed
  var maxValue: Fixed
  var flags: Int16
  var nameID: Int16
  init()
  init(axisTag axisTag: FourCharCode, minValue minValue: Fixed, defaultValue defaultValue: Fixed, maxValue maxValue: Fixed, flags flags: Int16, nameID nameID: Int16)
}
var sizeof_sfntVariationAxis: Int { get }
struct sfntInstance {
  var nameID: Int16
  var flags: Int16
  var coord: (Fixed)
  init()
  init(nameID nameID: Int16, flags flags: Int16, coord coord: (Fixed))
}
var sizeof_sfntInstance: Int { get }
struct sfntVariationHeader {
  var version: Fixed
  var offsetToData: UInt16
  var countSizePairs: UInt16
  var axisCount: UInt16
  var axisSize: UInt16
  var instanceCount: UInt16
  var instanceSize: UInt16
  var axis: (sfntVariationAxis)
  var instance: (sfntInstance)
  init()
  init(version version: Fixed, offsetToData offsetToData: UInt16, countSizePairs countSizePairs: UInt16, axisCount axisCount: UInt16, axisSize axisSize: UInt16, instanceCount instanceCount: UInt16, instanceSize instanceSize: UInt16, axis axis: (sfntVariationAxis), instance instance: (sfntInstance))
}
var sizeof_sfntVariationHeader: Int { get }
var descriptorFontTableTag: Int { get }
struct sfntFontDescriptor {
  var name: FourCharCode
  var value: Fixed
  init()
  init(name name: FourCharCode, value value: Fixed)
}
struct sfntDescriptorHeader {
  var version: Fixed
  var descriptorCount: Int32
  var descriptor: (sfntFontDescriptor)
  init()
  init(version version: Fixed, descriptorCount descriptorCount: Int32, descriptor descriptor: (sfntFontDescriptor))
}
var sizeof_sfntDescriptorHeader: Int { get }
var featureFontTableTag: Int { get }
struct sfntFeatureName {
  var featureType: UInt16
  var settingCount: UInt16
  var offsetToSettings: Int32
  var featureFlags: UInt16
  var nameID: Int16
  init()
  init(featureType featureType: UInt16, settingCount settingCount: UInt16, offsetToSettings offsetToSettings: Int32, featureFlags featureFlags: UInt16, nameID nameID: Int16)
}
struct sfntFontFeatureSetting {
  var setting: UInt16
  var nameID: Int16
  init()
  init(setting setting: UInt16, nameID nameID: Int16)
}
struct sfntFontRunFeature {
  var featureType: UInt16
  var setting: UInt16
  init()
  init(featureType featureType: UInt16, setting setting: UInt16)
}
struct sfntFeatureHeader {
  var version: Int32
  var featureNameCount: UInt16
  var featureSetCount: UInt16
  var reserved: Int32
  var names: (sfntFeatureName)
  var settings: (sfntFontFeatureSetting)
  var runs: (sfntFontRunFeature)
  init()
  init(version version: Int32, featureNameCount featureNameCount: UInt16, featureSetCount featureSetCount: UInt16, reserved reserved: Int32, names names: (sfntFeatureName), settings settings: (sfntFontFeatureSetting), runs runs: (sfntFontRunFeature))
}
var os2FontTableTag: Int { get }
var nonGlyphID: Int { get }
typealias FontNameCode = UInt32
typealias FontPlatformCode = UInt32
typealias FontScriptCode = UInt32
typealias FontLanguageCode = UInt32
struct FontVariation {
  var name: FourCharCode
  var value: Fixed
  init()
  init(name name: FourCharCode, value value: Fixed)
}

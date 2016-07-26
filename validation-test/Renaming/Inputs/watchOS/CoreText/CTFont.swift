
class CTFont {
}
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetTypeID() -> CFTypeID
@available(watchOS 2.0, *)
let kCTFontCopyrightNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontFamilyNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontSubFamilyNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontStyleNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontUniqueNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontFullNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontVersionNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontPostScriptNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontTrademarkNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontManufacturerNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontDesignerNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontDescriptionNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontVendorURLNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontDesignerURLNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontLicenseNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontLicenseURLNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontSampleTextNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontPostScriptCIDNameKey: CFString
@available(watchOS 2.0, *)
@discardableResult
func CTFontCreateWithName(_ name: CFString?, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?) -> CTFont
@available(watchOS 2.0, *)
@discardableResult
func CTFontCreateWithFontDescriptor(_ descriptor: CTFontDescriptor, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?) -> CTFont
struct CTFontOptions : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var preventAutoActivation: CTFontOptions { get }
  static var preferSystemFont: CTFontOptions { get }
}
@available(watchOS 2.0, *)
@discardableResult
func CTFontCreateWithNameAndOptions(_ name: CFString, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?, _ options: CTFontOptions) -> CTFont
@available(watchOS 2.0, *)
@discardableResult
func CTFontCreateWithFontDescriptorAndOptions(_ descriptor: CTFontDescriptor, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?, _ options: CTFontOptions) -> CTFont
enum CTFontUIFontType : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  @available(watchOS 2.0, *)
  case none
  @available(watchOS 2.0, *)
  case user
  @available(watchOS 2.0, *)
  case userFixedPitch
  @available(watchOS 2.0, *)
  case system
  @available(watchOS 2.0, *)
  case emphasizedSystem
  @available(watchOS 2.0, *)
  case smallSystem
  @available(watchOS 2.0, *)
  case smallEmphasizedSystem
  @available(watchOS 2.0, *)
  case miniSystem
  @available(watchOS 2.0, *)
  case miniEmphasizedSystem
  @available(watchOS 2.0, *)
  case views
  @available(watchOS 2.0, *)
  case application
  @available(watchOS 2.0, *)
  case label
  @available(watchOS 2.0, *)
  case menuTitle
  @available(watchOS 2.0, *)
  case menuItem
  @available(watchOS 2.0, *)
  case menuItemMark
  @available(watchOS 2.0, *)
  case menuItemCmdKey
  @available(watchOS 2.0, *)
  case windowTitle
  @available(watchOS 2.0, *)
  case pushButton
  @available(watchOS 2.0, *)
  case utilityWindowTitle
  @available(watchOS 2.0, *)
  case alertHeader
  @available(watchOS 2.0, *)
  case systemDetail
  @available(watchOS 2.0, *)
  case emphasizedSystemDetail
  @available(watchOS 2.0, *)
  case toolbar
  @available(watchOS 2.0, *)
  case smallToolbar
  @available(watchOS 2.0, *)
  case message
  @available(watchOS 2.0, *)
  case palette
  @available(watchOS 2.0, *)
  case toolTip
  @available(watchOS 2.0, *)
  case controlContent
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontNoFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontUserFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontUserFixedPitchFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontSystemFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontEmphasizedSystemFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontSmallSystemFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontSmallEmphasizedSystemFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontMiniSystemFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontMiniEmphasizedSystemFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontViewsFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontApplicationFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontLabelFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontMenuTitleFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontMenuItemFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontMenuItemMarkFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontMenuItemCmdKeyFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontWindowTitleFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontPushButtonFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontUtilityWindowTitleFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontAlertHeaderFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontSystemDetailFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontEmphasizedSystemDetailFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontToolbarFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontSmallToolbarFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontMessageFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontPaletteFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontToolTipFontType: CTFontUIFontType { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var kCTFontControlContentFontType: CTFontUIFontType { get }
}
@available(watchOS 2.0, *)
@discardableResult
func CTFontCreateUIFontForLanguage(_ uiType: CTFontUIFontType, _ size: CGFloat, _ language: CFString?) -> CTFont?
@available(watchOS 2.0, *)
@discardableResult
func CTFontCreateCopyWithAttributes(_ font: CTFont, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?, _ attributes: CTFontDescriptor?) -> CTFont
@available(watchOS 2.0, *)
@discardableResult
func CTFontCreateCopyWithSymbolicTraits(_ font: CTFont, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?, _ symTraitValue: CTFontSymbolicTraits, _ symTraitMask: CTFontSymbolicTraits) -> CTFont?
@available(watchOS 2.0, *)
@discardableResult
func CTFontCreateCopyWithFamily(_ font: CTFont, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?, _ family: CFString) -> CTFont?
@available(watchOS 2.0, *)
@discardableResult
func CTFontCreateForString(_ currentFont: CTFont, _ string: CFString, _ range: CFRange) -> CTFont
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyFontDescriptor(_ font: CTFont) -> CTFontDescriptor
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyAttribute(_ font: CTFont, _ attribute: CFString) -> CFTypeRef?
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetSize(_ font: CTFont) -> CGFloat
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetMatrix(_ font: CTFont) -> CGAffineTransform
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetSymbolicTraits(_ font: CTFont) -> CTFontSymbolicTraits
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyTraits(_ font: CTFont) -> CFDictionary
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyPostScriptName(_ font: CTFont) -> CFString
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyFamilyName(_ font: CTFont) -> CFString
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyFullName(_ font: CTFont) -> CFString
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyDisplayName(_ font: CTFont) -> CFString
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyName(_ font: CTFont, _ nameKey: CFString) -> CFString?
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyLocalizedName(_ font: CTFont, _ nameKey: CFString, _ actualLanguage: UnsafeMutablePointer<Unmanaged<CFString>?>?) -> CFString?
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyCharacterSet(_ font: CTFont) -> CFCharacterSet
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetStringEncoding(_ font: CTFont) -> CFStringEncoding
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopySupportedLanguages(_ font: CTFont) -> CFArray
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetGlyphsForCharacters(_ font: CTFont, _ characters: UnsafePointer<UniChar>!, _ glyphs: UnsafeMutablePointer<CGGlyph>!, _ count: CFIndex) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetAscent(_ font: CTFont) -> CGFloat
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetDescent(_ font: CTFont) -> CGFloat
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetLeading(_ font: CTFont) -> CGFloat
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetUnitsPerEm(_ font: CTFont) -> UInt32
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetGlyphCount(_ font: CTFont) -> CFIndex
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetBoundingBox(_ font: CTFont) -> CGRect
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetUnderlinePosition(_ font: CTFont) -> CGFloat
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetUnderlineThickness(_ font: CTFont) -> CGFloat
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetSlantAngle(_ font: CTFont) -> CGFloat
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetCapHeight(_ font: CTFont) -> CGFloat
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetXHeight(_ font: CTFont) -> CGFloat
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetGlyphWithName(_ font: CTFont, _ glyphName: CFString) -> CGGlyph
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetBoundingRectsForGlyphs(_ font: CTFont, _ orientation: CTFontOrientation, _ glyphs: UnsafePointer<CGGlyph>!, _ boundingRects: UnsafeMutablePointer<CGRect>?, _ count: CFIndex) -> CGRect
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetOpticalBoundsForGlyphs(_ font: CTFont, _ glyphs: UnsafePointer<CGGlyph>!, _ boundingRects: UnsafeMutablePointer<CGRect>?, _ count: CFIndex, _ options: CFOptionFlags) -> CGRect
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetAdvancesForGlyphs(_ font: CTFont, _ orientation: CTFontOrientation, _ glyphs: UnsafePointer<CGGlyph>!, _ advances: UnsafeMutablePointer<CGSize>?, _ count: CFIndex) -> Double
@available(watchOS 2.0, *)
func CTFontGetVerticalTranslationsForGlyphs(_ font: CTFont, _ glyphs: UnsafePointer<CGGlyph>!, _ translations: UnsafeMutablePointer<CGSize>!, _ count: CFIndex)
@available(watchOS 2.0, *)
@discardableResult
func CTFontCreatePathForGlyph(_ font: CTFont, _ glyph: CGGlyph, _ matrix: UnsafePointer<CGAffineTransform>?) -> CGPath?
@available(watchOS 2.0, *)
let kCTFontVariationAxisIdentifierKey: CFString
@available(watchOS 2.0, *)
let kCTFontVariationAxisMinimumValueKey: CFString
@available(watchOS 2.0, *)
let kCTFontVariationAxisMaximumValueKey: CFString
@available(watchOS 2.0, *)
let kCTFontVariationAxisDefaultValueKey: CFString
@available(watchOS 2.0, *)
let kCTFontVariationAxisNameKey: CFString
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyVariationAxes(_ font: CTFont) -> CFArray?
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyVariation(_ font: CTFont) -> CFDictionary?
@available(watchOS 2.0, *)
let kCTFontOpenTypeFeatureTag: CFString
@available(watchOS 2.0, *)
let kCTFontOpenTypeFeatureValue: CFString
@available(watchOS 2.0, *)
let kCTFontFeatureTypeIdentifierKey: CFString
@available(watchOS 2.0, *)
let kCTFontFeatureTypeNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontFeatureTypeExclusiveKey: CFString
@available(watchOS 2.0, *)
let kCTFontFeatureTypeSelectorsKey: CFString
@available(watchOS 2.0, *)
let kCTFontFeatureSelectorIdentifierKey: CFString
@available(watchOS 2.0, *)
let kCTFontFeatureSelectorNameKey: CFString
@available(watchOS 2.0, *)
let kCTFontFeatureSelectorDefaultKey: CFString
@available(watchOS 2.0, *)
let kCTFontFeatureSelectorSettingKey: CFString
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyFeatures(_ font: CTFont) -> CFArray?
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyFeatureSettings(_ font: CTFont) -> CFArray?
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyGraphicsFont(_ font: CTFont, _ attributes: UnsafeMutablePointer<Unmanaged<CTFontDescriptor>?>?) -> CGFont
@available(watchOS 2.0, *)
@discardableResult
func CTFontCreateWithGraphicsFont(_ graphicsFont: CGFont, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?, _ attributes: CTFontDescriptor?) -> CTFont
typealias ATSFontRef = UInt32
var ATSFONTREF_DEFINED: Int32 { get }
var kCTFontTableBASE: Int { get }
var kCTFontTableCFF: Int { get }
var kCTFontTableDSIG: Int { get }
var kCTFontTableEBDT: Int { get }
var kCTFontTableEBLC: Int { get }
var kCTFontTableEBSC: Int { get }
var kCTFontTableGDEF: Int { get }
var kCTFontTableGPOS: Int { get }
var kCTFontTableGSUB: Int { get }
var kCTFontTableJSTF: Int { get }
var kCTFontTableLTSH: Int { get }
var kCTFontTableMATH: Int { get }
var kCTFontTableOS2: Int { get }
var kCTFontTablePCLT: Int { get }
var kCTFontTableVDMX: Int { get }
var kCTFontTableVORG: Int { get }
var kCTFontTableZapf: Int { get }
var kCTFontTableAcnt: Int { get }
var kCTFontTableAnkr: Int { get }
var kCTFontTableAvar: Int { get }
var kCTFontTableBdat: Int { get }
var kCTFontTableBhed: Int { get }
var kCTFontTableBloc: Int { get }
var kCTFontTableBsln: Int { get }
var kCTFontTableCmap: Int { get }
var kCTFontTableCvar: Int { get }
var kCTFontTableCvt: Int { get }
var kCTFontTableFdsc: Int { get }
var kCTFontTableFeat: Int { get }
var kCTFontTableFmtx: Int { get }
var kCTFontTableFpgm: Int { get }
var kCTFontTableFvar: Int { get }
var kCTFontTableGasp: Int { get }
var kCTFontTableGlyf: Int { get }
var kCTFontTableGvar: Int { get }
var kCTFontTableHdmx: Int { get }
var kCTFontTableHead: Int { get }
var kCTFontTableHhea: Int { get }
var kCTFontTableHmtx: Int { get }
var kCTFontTableHsty: Int { get }
var kCTFontTableJust: Int { get }
var kCTFontTableKern: Int { get }
var kCTFontTableKerx: Int { get }
var kCTFontTableLcar: Int { get }
var kCTFontTableLtag: Int { get }
var kCTFontTableLoca: Int { get }
var kCTFontTableMaxp: Int { get }
var kCTFontTableMort: Int { get }
var kCTFontTableMorx: Int { get }
var kCTFontTableName: Int { get }
var kCTFontTableOpbd: Int { get }
var kCTFontTablePost: Int { get }
var kCTFontTablePrep: Int { get }
var kCTFontTableProp: Int { get }
var kCTFontTableSbit: Int { get }
var kCTFontTableSbix: Int { get }
var kCTFontTableTrak: Int { get }
var kCTFontTableVhea: Int { get }
var kCTFontTableVmtx: Int { get }
typealias CTFontTableTag = FourCharCode
struct CTFontTableOptions : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  @available(watchOS 2.0, *)
  static var noOptions: CTFontTableOptions { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  static var excludeSynthetic: CTFontTableOptions { get }
}
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyAvailableTables(_ font: CTFont, _ options: CTFontTableOptions) -> CFArray?
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyTable(_ font: CTFont, _ table: CTFontTableTag, _ options: CTFontTableOptions) -> CFData?
@available(watchOS 2.0, *)
func CTFontDrawGlyphs(_ font: CTFont, _ glyphs: UnsafePointer<CGGlyph>!, _ positions: UnsafePointer<CGPoint>!, _ count: Int, _ context: CGContext)
@available(watchOS 2.0, *)
@discardableResult
func CTFontGetLigatureCaretPositions(_ font: CTFont, _ glyph: CGGlyph, _ positions: UnsafeMutablePointer<CGFloat>?, _ maxPositions: CFIndex) -> CFIndex
@available(watchOS 2.0, *)
let kCTBaselineClassRoman: CFString
@available(watchOS 2.0, *)
let kCTBaselineClassIdeographicCentered: CFString
@available(watchOS 2.0, *)
let kCTBaselineClassIdeographicLow: CFString
@available(watchOS 2.0, *)
let kCTBaselineClassIdeographicHigh: CFString
@available(watchOS 2.0, *)
let kCTBaselineClassHanging: CFString
@available(watchOS 2.0, *)
let kCTBaselineClassMath: CFString
@available(watchOS 2.0, *)
let kCTBaselineReferenceFont: CFString
@available(watchOS 2.0, *)
let kCTBaselineOriginalFont: CFString
@available(watchOS 2.0, *)
@discardableResult
func CTFontCopyDefaultCascadeListForLanguages(_ font: CTFont, _ languagePrefList: CFArray?) -> CFArray?

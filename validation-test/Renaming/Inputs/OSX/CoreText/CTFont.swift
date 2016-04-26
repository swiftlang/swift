
class CTFont {
}
@available(OSX 10.5, *)
@discardableResult
func CTFontGetTypeID() -> CFTypeID
@available(OSX 10.5, *)
let kCTFontCopyrightNameKey: CFString
@available(OSX 10.5, *)
let kCTFontFamilyNameKey: CFString
@available(OSX 10.5, *)
let kCTFontSubFamilyNameKey: CFString
@available(OSX 10.5, *)
let kCTFontStyleNameKey: CFString
@available(OSX 10.5, *)
let kCTFontUniqueNameKey: CFString
@available(OSX 10.5, *)
let kCTFontFullNameKey: CFString
@available(OSX 10.5, *)
let kCTFontVersionNameKey: CFString
@available(OSX 10.5, *)
let kCTFontPostScriptNameKey: CFString
@available(OSX 10.5, *)
let kCTFontTrademarkNameKey: CFString
@available(OSX 10.5, *)
let kCTFontManufacturerNameKey: CFString
@available(OSX 10.5, *)
let kCTFontDesignerNameKey: CFString
@available(OSX 10.5, *)
let kCTFontDescriptionNameKey: CFString
@available(OSX 10.5, *)
let kCTFontVendorURLNameKey: CFString
@available(OSX 10.5, *)
let kCTFontDesignerURLNameKey: CFString
@available(OSX 10.5, *)
let kCTFontLicenseNameKey: CFString
@available(OSX 10.5, *)
let kCTFontLicenseURLNameKey: CFString
@available(OSX 10.5, *)
let kCTFontSampleTextNameKey: CFString
@available(OSX 10.5, *)
let kCTFontPostScriptCIDNameKey: CFString
@available(OSX 10.5, *)
@discardableResult
func CTFontCreateWithName(_ name: CFString?, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?) -> CTFont
@available(OSX 10.5, *)
@discardableResult
func CTFontCreateWithFontDescriptor(_ descriptor: CTFontDescriptor, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?) -> CTFont
struct CTFontOptions : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var preventAutoActivation: CTFontOptions { get }
  static var preferSystemFont: CTFontOptions { get }
}
@available(OSX 10.6, *)
@discardableResult
func CTFontCreateWithNameAndOptions(_ name: CFString, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?, _ options: CTFontOptions) -> CTFont
@available(OSX 10.6, *)
@discardableResult
func CTFontCreateWithFontDescriptorAndOptions(_ descriptor: CTFontDescriptor, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?, _ options: CTFontOptions) -> CTFont
enum CTFontUIFontType : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  @available(OSX 10.8, *)
  case none
  @available(OSX 10.8, *)
  case user
  @available(OSX 10.8, *)
  case userFixedPitch
  @available(OSX 10.8, *)
  case system
  @available(OSX 10.8, *)
  case emphasizedSystem
  @available(OSX 10.8, *)
  case smallSystem
  @available(OSX 10.8, *)
  case smallEmphasizedSystem
  @available(OSX 10.8, *)
  case miniSystem
  @available(OSX 10.8, *)
  case miniEmphasizedSystem
  @available(OSX 10.8, *)
  case views
  @available(OSX 10.8, *)
  case application
  @available(OSX 10.8, *)
  case label
  @available(OSX 10.8, *)
  case menuTitle
  @available(OSX 10.8, *)
  case menuItem
  @available(OSX 10.8, *)
  case menuItemMark
  @available(OSX 10.8, *)
  case menuItemCmdKey
  @available(OSX 10.8, *)
  case windowTitle
  @available(OSX 10.8, *)
  case pushButton
  @available(OSX 10.8, *)
  case utilityWindowTitle
  @available(OSX 10.8, *)
  case alertHeader
  @available(OSX 10.8, *)
  case systemDetail
  @available(OSX 10.8, *)
  case emphasizedSystemDetail
  @available(OSX 10.8, *)
  case toolbar
  @available(OSX 10.8, *)
  case smallToolbar
  @available(OSX 10.8, *)
  case message
  @available(OSX 10.8, *)
  case palette
  @available(OSX 10.8, *)
  case toolTip
  @available(OSX 10.8, *)
  case controlContent
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontNoFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontUserFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontUserFixedPitchFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontSystemFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontEmphasizedSystemFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontSmallSystemFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontSmallEmphasizedSystemFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontMiniSystemFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontMiniEmphasizedSystemFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontViewsFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontApplicationFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontLabelFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontMenuTitleFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontMenuItemFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontMenuItemMarkFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontMenuItemCmdKeyFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontWindowTitleFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontPushButtonFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontUtilityWindowTitleFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontAlertHeaderFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontSystemDetailFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontEmphasizedSystemDetailFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontToolbarFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontSmallToolbarFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontMessageFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontPaletteFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontToolTipFontType: CTFontUIFontType { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontControlContentFontType: CTFontUIFontType { get }
}
@available(OSX 10.5, *)
@discardableResult
func CTFontCreateUIFontForLanguage(_ uiType: CTFontUIFontType, _ size: CGFloat, _ language: CFString?) -> CTFont?
@available(OSX 10.5, *)
@discardableResult
func CTFontCreateCopyWithAttributes(_ font: CTFont, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?, _ attributes: CTFontDescriptor?) -> CTFont
@available(OSX 10.5, *)
@discardableResult
func CTFontCreateCopyWithSymbolicTraits(_ font: CTFont, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?, _ symTraitValue: CTFontSymbolicTraits, _ symTraitMask: CTFontSymbolicTraits) -> CTFont?
@available(OSX 10.5, *)
@discardableResult
func CTFontCreateCopyWithFamily(_ font: CTFont, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?, _ family: CFString) -> CTFont?
@available(OSX 10.5, *)
@discardableResult
func CTFontCreateForString(_ currentFont: CTFont, _ string: CFString, _ range: CFRange) -> CTFont
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyFontDescriptor(_ font: CTFont) -> CTFontDescriptor
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyAttribute(_ font: CTFont, _ attribute: CFString) -> CFTypeRef?
@available(OSX 10.5, *)
@discardableResult
func CTFontGetSize(_ font: CTFont) -> CGFloat
@available(OSX 10.5, *)
@discardableResult
func CTFontGetMatrix(_ font: CTFont) -> CGAffineTransform
@available(OSX 10.5, *)
@discardableResult
func CTFontGetSymbolicTraits(_ font: CTFont) -> CTFontSymbolicTraits
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyTraits(_ font: CTFont) -> CFDictionary
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyPostScriptName(_ font: CTFont) -> CFString
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyFamilyName(_ font: CTFont) -> CFString
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyFullName(_ font: CTFont) -> CFString
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyDisplayName(_ font: CTFont) -> CFString
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyName(_ font: CTFont, _ nameKey: CFString) -> CFString?
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyLocalizedName(_ font: CTFont, _ nameKey: CFString, _ actualLanguage: UnsafeMutablePointer<Unmanaged<CFString>?>?) -> CFString?
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyCharacterSet(_ font: CTFont) -> CFCharacterSet
@available(OSX 10.5, *)
@discardableResult
func CTFontGetStringEncoding(_ font: CTFont) -> CFStringEncoding
@available(OSX 10.5, *)
@discardableResult
func CTFontCopySupportedLanguages(_ font: CTFont) -> CFArray
@available(OSX 10.5, *)
@discardableResult
func CTFontGetGlyphsForCharacters(_ font: CTFont, _ characters: UnsafePointer<UniChar>!, _ glyphs: UnsafeMutablePointer<CGGlyph>!, _ count: CFIndex) -> Bool
@available(OSX 10.5, *)
@discardableResult
func CTFontGetAscent(_ font: CTFont) -> CGFloat
@available(OSX 10.5, *)
@discardableResult
func CTFontGetDescent(_ font: CTFont) -> CGFloat
@available(OSX 10.5, *)
@discardableResult
func CTFontGetLeading(_ font: CTFont) -> CGFloat
@available(OSX 10.5, *)
@discardableResult
func CTFontGetUnitsPerEm(_ font: CTFont) -> UInt32
@available(OSX 10.5, *)
@discardableResult
func CTFontGetGlyphCount(_ font: CTFont) -> CFIndex
@available(OSX 10.5, *)
@discardableResult
func CTFontGetBoundingBox(_ font: CTFont) -> CGRect
@available(OSX 10.5, *)
@discardableResult
func CTFontGetUnderlinePosition(_ font: CTFont) -> CGFloat
@available(OSX 10.5, *)
@discardableResult
func CTFontGetUnderlineThickness(_ font: CTFont) -> CGFloat
@available(OSX 10.5, *)
@discardableResult
func CTFontGetSlantAngle(_ font: CTFont) -> CGFloat
@available(OSX 10.5, *)
@discardableResult
func CTFontGetCapHeight(_ font: CTFont) -> CGFloat
@available(OSX 10.5, *)
@discardableResult
func CTFontGetXHeight(_ font: CTFont) -> CGFloat
@available(OSX 10.5, *)
@discardableResult
func CTFontGetGlyphWithName(_ font: CTFont, _ glyphName: CFString) -> CGGlyph
@available(OSX 10.5, *)
@discardableResult
func CTFontGetBoundingRectsForGlyphs(_ font: CTFont, _ orientation: CTFontOrientation, _ glyphs: UnsafePointer<CGGlyph>!, _ boundingRects: UnsafeMutablePointer<CGRect>?, _ count: CFIndex) -> CGRect
@available(OSX 10.8, *)
@discardableResult
func CTFontGetOpticalBoundsForGlyphs(_ font: CTFont, _ glyphs: UnsafePointer<CGGlyph>!, _ boundingRects: UnsafeMutablePointer<CGRect>?, _ count: CFIndex, _ options: CFOptionFlags) -> CGRect
@available(OSX 10.5, *)
@discardableResult
func CTFontGetAdvancesForGlyphs(_ font: CTFont, _ orientation: CTFontOrientation, _ glyphs: UnsafePointer<CGGlyph>!, _ advances: UnsafeMutablePointer<CGSize>?, _ count: CFIndex) -> Double
@available(OSX 10.5, *)
func CTFontGetVerticalTranslationsForGlyphs(_ font: CTFont, _ glyphs: UnsafePointer<CGGlyph>!, _ translations: UnsafeMutablePointer<CGSize>!, _ count: CFIndex)
@available(OSX 10.5, *)
@discardableResult
func CTFontCreatePathForGlyph(_ font: CTFont, _ glyph: CGGlyph, _ matrix: UnsafePointer<CGAffineTransform>?) -> CGPath?
@available(OSX 10.5, *)
let kCTFontVariationAxisIdentifierKey: CFString
@available(OSX 10.5, *)
let kCTFontVariationAxisMinimumValueKey: CFString
@available(OSX 10.5, *)
let kCTFontVariationAxisMaximumValueKey: CFString
@available(OSX 10.5, *)
let kCTFontVariationAxisDefaultValueKey: CFString
@available(OSX 10.5, *)
let kCTFontVariationAxisNameKey: CFString
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyVariationAxes(_ font: CTFont) -> CFArray?
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyVariation(_ font: CTFont) -> CFDictionary?
@available(OSX 10.10, *)
let kCTFontOpenTypeFeatureTag: CFString
@available(OSX 10.10, *)
let kCTFontOpenTypeFeatureValue: CFString
@available(OSX 10.5, *)
let kCTFontFeatureTypeIdentifierKey: CFString
@available(OSX 10.5, *)
let kCTFontFeatureTypeNameKey: CFString
@available(OSX 10.5, *)
let kCTFontFeatureTypeExclusiveKey: CFString
@available(OSX 10.5, *)
let kCTFontFeatureTypeSelectorsKey: CFString
@available(OSX 10.5, *)
let kCTFontFeatureSelectorIdentifierKey: CFString
@available(OSX 10.5, *)
let kCTFontFeatureSelectorNameKey: CFString
@available(OSX 10.5, *)
let kCTFontFeatureSelectorDefaultKey: CFString
@available(OSX 10.5, *)
let kCTFontFeatureSelectorSettingKey: CFString
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyFeatures(_ font: CTFont) -> CFArray?
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyFeatureSettings(_ font: CTFont) -> CFArray?
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyGraphicsFont(_ font: CTFont, _ attributes: UnsafeMutablePointer<Unmanaged<CTFontDescriptor>?>?) -> CGFont
@available(OSX 10.5, *)
@discardableResult
func CTFontCreateWithGraphicsFont(_ graphicsFont: CGFont, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?, _ attributes: CTFontDescriptor?) -> CTFont
typealias ATSFontRef = UInt32
var ATSFONTREF_DEFINED: Int32 { get }
@available(OSX 10.5, *)
@discardableResult
func CTFontGetPlatformFont(_ font: CTFont, _ attributes: UnsafeMutablePointer<Unmanaged<CTFontDescriptor>?>?) -> ATSFontRef
@available(OSX 10.5, *)
@discardableResult
func CTFontCreateWithPlatformFont(_ platformFont: ATSFontRef, _ size: CGFloat, _ matrix: UnsafePointer<CGAffineTransform>?, _ attributes: CTFontDescriptor?) -> CTFont?
@available(OSX 10.5, *)
@discardableResult
func CTFontCreateWithQuickdrawInstance(_ name: ConstStr255Param?, _ identifier: Int16, _ style: UInt8, _ size: CGFloat) -> CTFont
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
  @available(OSX 10.5, *)
  static var noOptions: CTFontTableOptions { get }
}
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyAvailableTables(_ font: CTFont, _ options: CTFontTableOptions) -> CFArray?
@available(OSX 10.5, *)
@discardableResult
func CTFontCopyTable(_ font: CTFont, _ table: CTFontTableTag, _ options: CTFontTableOptions) -> CFData?
@available(OSX 10.7, *)
func CTFontDrawGlyphs(_ font: CTFont, _ glyphs: UnsafePointer<CGGlyph>!, _ positions: UnsafePointer<CGPoint>!, _ count: Int, _ context: CGContext)
@available(OSX 10.5, *)
@discardableResult
func CTFontGetLigatureCaretPositions(_ font: CTFont, _ glyph: CGGlyph, _ positions: UnsafeMutablePointer<CGFloat>?, _ maxPositions: CFIndex) -> CFIndex
@available(OSX 10.8, *)
let kCTBaselineClassRoman: CFString
@available(OSX 10.8, *)
let kCTBaselineClassIdeographicCentered: CFString
@available(OSX 10.8, *)
let kCTBaselineClassIdeographicLow: CFString
@available(OSX 10.8, *)
let kCTBaselineClassIdeographicHigh: CFString
@available(OSX 10.8, *)
let kCTBaselineClassHanging: CFString
@available(OSX 10.8, *)
let kCTBaselineClassMath: CFString
@available(OSX 10.8, *)
let kCTBaselineReferenceFont: CFString
@available(OSX 10.8, *)
let kCTBaselineOriginalFont: CFString
@available(OSX 10.8, *)
@discardableResult
func CTFontCopyDefaultCascadeListForLanguages(_ font: CTFont, _ languagePrefList: CFArray?) -> CFArray?

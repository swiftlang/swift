
@available(OSX 10.5, *)
let kCTFontSymbolicTrait: CFString
@available(OSX 10.5, *)
let kCTFontWeightTrait: CFString
@available(OSX 10.5, *)
let kCTFontWidthTrait: CFString
@available(OSX 10.5, *)
let kCTFontSlantTrait: CFString
var kCTFontClassMaskShift: Int { get }
struct CTFontSymbolicTraits : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var traitItalic: CTFontSymbolicTraits { get }
  static var traitBold: CTFontSymbolicTraits { get }
  static var traitExpanded: CTFontSymbolicTraits { get }
  static var traitCondensed: CTFontSymbolicTraits { get }
  static var traitMonoSpace: CTFontSymbolicTraits { get }
  static var traitVertical: CTFontSymbolicTraits { get }
  static var traitUIOptimized: CTFontSymbolicTraits { get }
  static var traitColorGlyphs: CTFontSymbolicTraits { get }
  static var traitComposite: CTFontSymbolicTraits { get }
  static var traitClassMask: CTFontSymbolicTraits { get }
  static var italicTrait: CTFontSymbolicTraits { get }
  static var boldTrait: CTFontSymbolicTraits { get }
  static var expandedTrait: CTFontSymbolicTraits { get }
  static var condensedTrait: CTFontSymbolicTraits { get }
  static var monoSpaceTrait: CTFontSymbolicTraits { get }
  static var verticalTrait: CTFontSymbolicTraits { get }
  static var uiOptimizedTrait: CTFontSymbolicTraits { get }
  static var colorGlyphsTrait: CTFontSymbolicTraits { get }
  static var compositeTrait: CTFontSymbolicTraits { get }
  static var classMaskTrait: CTFontSymbolicTraits { get }
}
struct CTFontStylisticClass : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var classOldStyleSerifs: CTFontStylisticClass { get }
  static var classTransitionalSerifs: CTFontStylisticClass { get }
  static var classModernSerifs: CTFontStylisticClass { get }
  static var classClarendonSerifs: CTFontStylisticClass { get }
  static var classSlabSerifs: CTFontStylisticClass { get }
  static var classFreeformSerifs: CTFontStylisticClass { get }
  static var classSansSerif: CTFontStylisticClass { get }
  static var classOrnamentals: CTFontStylisticClass { get }
  static var classScripts: CTFontStylisticClass { get }
  static var classSymbolic: CTFontStylisticClass { get }
  static var oldStyleSerifsClass: CTFontStylisticClass { get }
  static var transitionalSerifsClass: CTFontStylisticClass { get }
  static var modernSerifsClass: CTFontStylisticClass { get }
  static var clarendonSerifsClass: CTFontStylisticClass { get }
  static var slabSerifsClass: CTFontStylisticClass { get }
  static var freeformSerifsClass: CTFontStylisticClass { get }
  static var sansSerifClass: CTFontStylisticClass { get }
  static var ornamentalsClass: CTFontStylisticClass { get }
  static var scriptsClass: CTFontStylisticClass { get }
  static var symbolicClass: CTFontStylisticClass { get }
}

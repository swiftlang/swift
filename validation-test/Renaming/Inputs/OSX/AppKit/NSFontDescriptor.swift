
typealias NSFontSymbolicTraits = UInt32
var NSFontUnknownClass: Int { get }
var NSFontOldStyleSerifsClass: Int { get }
var NSFontTransitionalSerifsClass: Int { get }
var NSFontModernSerifsClass: Int { get }
var NSFontClarendonSerifsClass: Int { get }
var NSFontSlabSerifsClass: Int { get }
var NSFontFreeformSerifsClass: Int { get }
var NSFontSansSerifClass: Int { get }
var NSFontOrnamentalsClass: Int { get }
var NSFontScriptsClass: Int { get }
var NSFontSymbolicClass: Int { get }
typealias NSFontFamilyClass = UInt32
var NSFontFamilyClassMask: UInt32 { get }
var NSFontItalicTrait: Int { get }
var NSFontBoldTrait: Int { get }
var NSFontExpandedTrait: Int { get }
var NSFontCondensedTrait: Int { get }
var NSFontMonoSpaceTrait: Int { get }
var NSFontVerticalTrait: Int { get }
var NSFontUIOptimizedTrait: Int { get }
class NSFontDescriptor : NSObject, NSCopying, NSSecureCoding {
  var postscriptName: String? { get }
  var pointSize: CGFloat { get }
  @NSCopying var matrix: NSAffineTransform? { get }
  var symbolicTraits: NSFontSymbolicTraits { get }
  @discardableResult
  func object(forKey anAttribute: String) -> AnyObject?
  var fontAttributes: [String : AnyObject] { get }
  /*not inherited*/ init(name fontName: String, size size: CGFloat)
  /*not inherited*/ init(name fontName: String, matrix matrix: NSAffineTransform)
  init(fontAttributes attributes: [String : AnyObject]? = [:])
  @discardableResult
  func matchingFontDescriptors(withMandatoryKeys mandatoryKeys: Set<String>?) -> [NSFontDescriptor]
  @available(OSX 10.5, *)
  @discardableResult
  func matchingFontDescriptor(withMandatoryKeys mandatoryKeys: Set<String>?) -> NSFontDescriptor?
  @discardableResult
  func addingAttributes(_ attributes: [String : AnyObject] = [:]) -> NSFontDescriptor
  @discardableResult
  func withSymbolicTraits(_ symbolicTraits: NSFontSymbolicTraits) -> NSFontDescriptor
  @discardableResult
  func withSize(_ newPointSize: CGFloat) -> NSFontDescriptor
  @discardableResult
  func withMatrix(_ matrix: NSAffineTransform) -> NSFontDescriptor
  @discardableResult
  func withFace(_ newFace: String) -> NSFontDescriptor
  @discardableResult
  func withFamily(_ newFamily: String) -> NSFontDescriptor
}
let NSFontFamilyAttribute: String
let NSFontNameAttribute: String
let NSFontFaceAttribute: String
let NSFontSizeAttribute: String
let NSFontVisibleNameAttribute: String
let NSFontMatrixAttribute: String
let NSFontVariationAttribute: String
let NSFontCharacterSetAttribute: String
let NSFontCascadeListAttribute: String
let NSFontTraitsAttribute: String
let NSFontFixedAdvanceAttribute: String
@available(OSX 10.5, *)
let NSFontFeatureSettingsAttribute: String
let NSFontSymbolicTrait: String
let NSFontWeightTrait: String
let NSFontWidthTrait: String
let NSFontSlantTrait: String
let NSFontVariationAxisIdentifierKey: String
let NSFontVariationAxisMinimumValueKey: String
let NSFontVariationAxisMaximumValueKey: String
let NSFontVariationAxisDefaultValueKey: String
let NSFontVariationAxisNameKey: String
@available(OSX 10.5, *)
let NSFontFeatureTypeIdentifierKey: String
@available(OSX 10.5, *)
let NSFontFeatureSelectorIdentifierKey: String
@available(OSX 10.11, *)
let NSFontWeightUltraLight: CGFloat
@available(OSX 10.11, *)
let NSFontWeightThin: CGFloat
@available(OSX 10.11, *)
let NSFontWeightLight: CGFloat
@available(OSX 10.11, *)
let NSFontWeightRegular: CGFloat
@available(OSX 10.11, *)
let NSFontWeightMedium: CGFloat
@available(OSX 10.11, *)
let NSFontWeightSemibold: CGFloat
@available(OSX 10.11, *)
let NSFontWeightBold: CGFloat
@available(OSX 10.11, *)
let NSFontWeightHeavy: CGFloat
@available(OSX 10.11, *)
let NSFontWeightBlack: CGFloat

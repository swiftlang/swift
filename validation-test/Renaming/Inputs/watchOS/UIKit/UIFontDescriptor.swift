
@available(watchOS 2.0, *)
struct UIFontDescriptorSymbolicTraits : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var traitItalic: UIFontDescriptorSymbolicTraits { get }
  static var traitBold: UIFontDescriptorSymbolicTraits { get }
  static var traitExpanded: UIFontDescriptorSymbolicTraits { get }
  static var traitCondensed: UIFontDescriptorSymbolicTraits { get }
  static var traitMonoSpace: UIFontDescriptorSymbolicTraits { get }
  static var traitVertical: UIFontDescriptorSymbolicTraits { get }
  static var traitUIOptimized: UIFontDescriptorSymbolicTraits { get }
  static var traitTightLeading: UIFontDescriptorSymbolicTraits { get }
  static var traitLooseLeading: UIFontDescriptorSymbolicTraits { get }
  static var classMask: UIFontDescriptorSymbolicTraits { get }
  static var classOldStyleSerifs: UIFontDescriptorSymbolicTraits { get }
  static var classTransitionalSerifs: UIFontDescriptorSymbolicTraits { get }
  static var classModernSerifs: UIFontDescriptorSymbolicTraits { get }
  static var classClarendonSerifs: UIFontDescriptorSymbolicTraits { get }
  static var classSlabSerifs: UIFontDescriptorSymbolicTraits { get }
  static var classFreeformSerifs: UIFontDescriptorSymbolicTraits { get }
  static var classSansSerif: UIFontDescriptorSymbolicTraits { get }
  static var classOrnamentals: UIFontDescriptorSymbolicTraits { get }
  static var classScripts: UIFontDescriptorSymbolicTraits { get }
  static var classSymbolic: UIFontDescriptorSymbolicTraits { get }
}
typealias UIFontDescriptorClass = Int
@available(watchOS 2.0, *)
class UIFontDescriptor : NSObject, NSCopying, NSSecureCoding {
  var postscriptName: String { get }
  var pointSize: CGFloat { get }
  var matrix: CGAffineTransform { get }
  var symbolicTraits: UIFontDescriptorSymbolicTraits { get }
  @discardableResult
  func object(forKey anAttribute: String) -> AnyObject?
  @discardableResult
  func fontAttributes() -> [String : AnyObject]
  @discardableResult
  func matchingFontDescriptors(withMandatoryKeys mandatoryKeys: Set<String>?) -> [UIFontDescriptor]
  /*not inherited*/ init(name fontName: String, size size: CGFloat)
  /*not inherited*/ init(name fontName: String, matrix matrix: CGAffineTransform)
  @discardableResult
  class func preferredFontDescriptor(withTextStyle style: String) -> UIFontDescriptor
  init(fontAttributes attributes: [String : AnyObject] = [:])
  @discardableResult
  func addingAttributes(_ attributes: [String : AnyObject] = [:]) -> UIFontDescriptor
  @discardableResult
  func withSymbolicTraits(_ symbolicTraits: UIFontDescriptorSymbolicTraits) -> UIFontDescriptor
  @discardableResult
  func withSize(_ newPointSize: CGFloat) -> UIFontDescriptor
  @discardableResult
  func withMatrix(_ matrix: CGAffineTransform) -> UIFontDescriptor
  @discardableResult
  func withFace(_ newFace: String) -> UIFontDescriptor
  @discardableResult
  func withFamily(_ newFamily: String) -> UIFontDescriptor
}
@available(watchOS 2.0, *)
let UIFontDescriptorFamilyAttribute: String
@available(watchOS 2.0, *)
let UIFontDescriptorNameAttribute: String
@available(watchOS 2.0, *)
let UIFontDescriptorFaceAttribute: String
@available(watchOS 2.0, *)
let UIFontDescriptorSizeAttribute: String
@available(watchOS 2.0, *)
let UIFontDescriptorVisibleNameAttribute: String
@available(watchOS 2.0, *)
let UIFontDescriptorMatrixAttribute: String
@available(watchOS 2.0, *)
let UIFontDescriptorCharacterSetAttribute: String
@available(watchOS 2.0, *)
let UIFontDescriptorCascadeListAttribute: String
@available(watchOS 2.0, *)
let UIFontDescriptorTraitsAttribute: String
@available(watchOS 2.0, *)
let UIFontDescriptorFixedAdvanceAttribute: String
@available(watchOS 2.0, *)
let UIFontDescriptorFeatureSettingsAttribute: String
@available(watchOS 2.0, *)
let UIFontDescriptorTextStyleAttribute: String
@available(watchOS 2.0, *)
let UIFontSymbolicTrait: String
@available(watchOS 2.0, *)
let UIFontWeightTrait: String
@available(watchOS 2.0, *)
let UIFontWidthTrait: String
@available(watchOS 2.0, *)
let UIFontSlantTrait: String
@available(watchOS 2.0, *)
let UIFontWeightUltraLight: CGFloat
@available(watchOS 2.0, *)
let UIFontWeightThin: CGFloat
@available(watchOS 2.0, *)
let UIFontWeightLight: CGFloat
@available(watchOS 2.0, *)
let UIFontWeightRegular: CGFloat
@available(watchOS 2.0, *)
let UIFontWeightMedium: CGFloat
@available(watchOS 2.0, *)
let UIFontWeightSemibold: CGFloat
@available(watchOS 2.0, *)
let UIFontWeightBold: CGFloat
@available(watchOS 2.0, *)
let UIFontWeightHeavy: CGFloat
@available(watchOS 2.0, *)
let UIFontWeightBlack: CGFloat
@available(watchOS 2.0, *)
let UIFontFeatureTypeIdentifierKey: String
@available(watchOS 2.0, *)
let UIFontFeatureSelectorIdentifierKey: String
@available(watchOS 2.0, *)
let UIFontTextStyleTitle1: String
@available(watchOS 2.0, *)
let UIFontTextStyleTitle2: String
@available(watchOS 2.0, *)
let UIFontTextStyleTitle3: String
@available(watchOS 2.0, *)
let UIFontTextStyleHeadline: String
@available(watchOS 2.0, *)
let UIFontTextStyleSubheadline: String
@available(watchOS 2.0, *)
let UIFontTextStyleBody: String
@available(watchOS 2.0, *)
let UIFontTextStyleCallout: String
@available(watchOS 2.0, *)
let UIFontTextStyleFootnote: String
@available(watchOS 2.0, *)
let UIFontTextStyleCaption1: String
@available(watchOS 2.0, *)
let UIFontTextStyleCaption2: String

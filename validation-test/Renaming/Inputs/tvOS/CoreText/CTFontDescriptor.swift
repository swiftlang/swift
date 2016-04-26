
class CTFontDescriptor {
}
@available(tvOS 3.2, *)
@discardableResult
func CTFontDescriptorGetTypeID() -> CFTypeID
@available(tvOS 3.2, *)
let kCTFontURLAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontNameAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontDisplayNameAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontFamilyNameAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontStyleNameAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontTraitsAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontVariationAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontSizeAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontMatrixAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontCascadeListAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontCharacterSetAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontLanguagesAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontBaselineAdjustAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontMacintoshEncodingsAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontFeaturesAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontFeatureSettingsAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontFixedAdvanceAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontOrientationAttribute: CFString
enum CTFontOrientation : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  @available(tvOS 6.0, *)
  case `default`
  @available(tvOS 6.0, *)
  case horizontal
  @available(tvOS 6.0, *)
  case vertical
  @available(tvOS, introduced: 3.2, deprecated: 9.0)
  static var kCTFontDefaultOrientation: CTFontOrientation { get }
  @available(tvOS, introduced: 3.2, deprecated: 9.0)
  static var kCTFontHorizontalOrientation: CTFontOrientation { get }
  @available(tvOS, introduced: 3.2, deprecated: 9.0)
  static var kCTFontVerticalOrientation: CTFontOrientation { get }
}
@available(tvOS 3.2, *)
let kCTFontFormatAttribute: CFString
enum CTFontFormat : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case unrecognized
  case openTypePostScript
  case openTypeTrueType
  case trueType
  case postScript
  case bitmap
}
@available(tvOS 3.2, *)
let kCTFontRegistrationScopeAttribute: CFString
@available(tvOS 3.2, *)
let kCTFontPriorityAttribute: CFString
var kCTFontPrioritySystem: Int { get }
var kCTFontPriorityNetwork: Int { get }
var kCTFontPriorityComputer: Int { get }
var kCTFontPriorityUser: Int { get }
var kCTFontPriorityDynamic: Int { get }
var kCTFontPriorityProcess: Int { get }
typealias CTFontPriority = UInt32
@available(tvOS 3.2, *)
let kCTFontEnabledAttribute: CFString
@available(tvOS 6.0, *)
let kCTFontDownloadableAttribute: CFString
@available(tvOS 7.0, *)
let kCTFontDownloadedAttribute: CFString
@available(tvOS 3.2, *)
@discardableResult
func CTFontDescriptorCreateWithNameAndSize(_ name: CFString, _ size: CGFloat) -> CTFontDescriptor
@available(tvOS 3.2, *)
@discardableResult
func CTFontDescriptorCreateWithAttributes(_ attributes: CFDictionary) -> CTFontDescriptor
@available(tvOS 3.2, *)
@discardableResult
func CTFontDescriptorCreateCopyWithAttributes(_ original: CTFontDescriptor, _ attributes: CFDictionary) -> CTFontDescriptor
@available(tvOS 7.0, *)
@discardableResult
func CTFontDescriptorCreateCopyWithFamily(_ original: CTFontDescriptor, _ family: CFString) -> CTFontDescriptor?
@available(tvOS 7.0, *)
@discardableResult
func CTFontDescriptorCreateCopyWithSymbolicTraits(_ original: CTFontDescriptor, _ symTraitValue: CTFontSymbolicTraits, _ symTraitMask: CTFontSymbolicTraits) -> CTFontDescriptor?
@available(tvOS 3.2, *)
@discardableResult
func CTFontDescriptorCreateCopyWithVariation(_ original: CTFontDescriptor, _ variationIdentifier: CFNumber, _ variationValue: CGFloat) -> CTFontDescriptor
@available(tvOS 3.2, *)
@discardableResult
func CTFontDescriptorCreateCopyWithFeature(_ original: CTFontDescriptor, _ featureTypeIdentifier: CFNumber, _ featureSelectorIdentifier: CFNumber) -> CTFontDescriptor
@available(tvOS 3.2, *)
@discardableResult
func CTFontDescriptorCreateMatchingFontDescriptors(_ descriptor: CTFontDescriptor, _ mandatoryAttributes: CFSet?) -> CFArray?
@available(tvOS 3.2, *)
@discardableResult
func CTFontDescriptorCreateMatchingFontDescriptor(_ descriptor: CTFontDescriptor, _ mandatoryAttributes: CFSet?) -> CTFontDescriptor?
enum CTFontDescriptorMatchingState : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case didBegin
  case didFinish
  case willBeginQuerying
  case stalled
  case willBeginDownloading
  case downloading
  case didFinishDownloading
  case didMatch
  case didFailWithError
}
@available(tvOS 6.0, *)
let kCTFontDescriptorMatchingSourceDescriptor: CFString
@available(tvOS 6.0, *)
let kCTFontDescriptorMatchingDescriptors: CFString
@available(tvOS 6.0, *)
let kCTFontDescriptorMatchingResult: CFString
@available(tvOS 6.0, *)
let kCTFontDescriptorMatchingPercentage: CFString
@available(tvOS 6.0, *)
let kCTFontDescriptorMatchingCurrentAssetSize: CFString
@available(tvOS 6.0, *)
let kCTFontDescriptorMatchingTotalDownloadedSize: CFString
@available(tvOS 6.0, *)
let kCTFontDescriptorMatchingTotalAssetSize: CFString
@available(tvOS 6.0, *)
let kCTFontDescriptorMatchingError: CFString
typealias CTFontDescriptorProgressHandler = (CTFontDescriptorMatchingState, CFDictionary) -> Bool
@available(tvOS 6.0, *)
@discardableResult
func CTFontDescriptorMatchFontDescriptorsWithProgressHandler(_ descriptors: CFArray, _ mandatoryAttributes: CFSet?, _ progressBlock: CTFontDescriptorProgressHandler) -> Bool
@available(tvOS 3.2, *)
@discardableResult
func CTFontDescriptorCopyAttributes(_ descriptor: CTFontDescriptor) -> CFDictionary
@available(tvOS 3.2, *)
@discardableResult
func CTFontDescriptorCopyAttribute(_ descriptor: CTFontDescriptor, _ attribute: CFString) -> CFTypeRef?
@available(tvOS 3.2, *)
@discardableResult
func CTFontDescriptorCopyLocalizedAttribute(_ descriptor: CTFontDescriptor, _ attribute: CFString, _ language: UnsafeMutablePointer<Unmanaged<CFString>?>?) -> CFTypeRef?

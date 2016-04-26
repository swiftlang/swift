
class CTFontDescriptor {
}
@available(OSX 10.5, *)
@discardableResult
func CTFontDescriptorGetTypeID() -> CFTypeID
@available(OSX 10.6, *)
let kCTFontURLAttribute: CFString
@available(OSX 10.5, *)
let kCTFontNameAttribute: CFString
@available(OSX 10.5, *)
let kCTFontDisplayNameAttribute: CFString
@available(OSX 10.5, *)
let kCTFontFamilyNameAttribute: CFString
@available(OSX 10.5, *)
let kCTFontStyleNameAttribute: CFString
@available(OSX 10.5, *)
let kCTFontTraitsAttribute: CFString
@available(OSX 10.5, *)
let kCTFontVariationAttribute: CFString
@available(OSX 10.5, *)
let kCTFontSizeAttribute: CFString
@available(OSX 10.5, *)
let kCTFontMatrixAttribute: CFString
@available(OSX 10.5, *)
let kCTFontCascadeListAttribute: CFString
@available(OSX 10.5, *)
let kCTFontCharacterSetAttribute: CFString
@available(OSX 10.5, *)
let kCTFontLanguagesAttribute: CFString
@available(OSX 10.5, *)
let kCTFontBaselineAdjustAttribute: CFString
@available(OSX 10.5, *)
let kCTFontMacintoshEncodingsAttribute: CFString
@available(OSX 10.5, *)
let kCTFontFeaturesAttribute: CFString
@available(OSX 10.5, *)
let kCTFontFeatureSettingsAttribute: CFString
@available(OSX 10.5, *)
let kCTFontFixedAdvanceAttribute: CFString
@available(OSX 10.5, *)
let kCTFontOrientationAttribute: CFString
enum CTFontOrientation : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  @available(OSX 10.8, *)
  case `default`
  @available(OSX 10.8, *)
  case horizontal
  @available(OSX 10.8, *)
  case vertical
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontDefaultOrientation: CTFontOrientation { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontHorizontalOrientation: CTFontOrientation { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  static var kCTFontVerticalOrientation: CTFontOrientation { get }
}
@available(OSX 10.6, *)
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
@available(OSX 10.6, *)
let kCTFontRegistrationScopeAttribute: CFString
@available(OSX 10.6, *)
let kCTFontPriorityAttribute: CFString
var kCTFontPrioritySystem: Int { get }
var kCTFontPriorityNetwork: Int { get }
var kCTFontPriorityComputer: Int { get }
var kCTFontPriorityUser: Int { get }
var kCTFontPriorityDynamic: Int { get }
var kCTFontPriorityProcess: Int { get }
typealias CTFontPriority = UInt32
@available(OSX 10.6, *)
let kCTFontEnabledAttribute: CFString
@available(OSX 10.8, *)
let kCTFontDownloadableAttribute: CFString
@available(OSX 10.5, *)
@discardableResult
func CTFontDescriptorCreateWithNameAndSize(_ name: CFString, _ size: CGFloat) -> CTFontDescriptor
@available(OSX 10.5, *)
@discardableResult
func CTFontDescriptorCreateWithAttributes(_ attributes: CFDictionary) -> CTFontDescriptor
@available(OSX 10.5, *)
@discardableResult
func CTFontDescriptorCreateCopyWithAttributes(_ original: CTFontDescriptor, _ attributes: CFDictionary) -> CTFontDescriptor
@available(OSX 10.9, *)
@discardableResult
func CTFontDescriptorCreateCopyWithFamily(_ original: CTFontDescriptor, _ family: CFString) -> CTFontDescriptor?
@available(OSX 10.9, *)
@discardableResult
func CTFontDescriptorCreateCopyWithSymbolicTraits(_ original: CTFontDescriptor, _ symTraitValue: CTFontSymbolicTraits, _ symTraitMask: CTFontSymbolicTraits) -> CTFontDescriptor?
@available(OSX 10.5, *)
@discardableResult
func CTFontDescriptorCreateCopyWithVariation(_ original: CTFontDescriptor, _ variationIdentifier: CFNumber, _ variationValue: CGFloat) -> CTFontDescriptor
@available(OSX 10.5, *)
@discardableResult
func CTFontDescriptorCreateCopyWithFeature(_ original: CTFontDescriptor, _ featureTypeIdentifier: CFNumber, _ featureSelectorIdentifier: CFNumber) -> CTFontDescriptor
@available(OSX 10.5, *)
@discardableResult
func CTFontDescriptorCreateMatchingFontDescriptors(_ descriptor: CTFontDescriptor, _ mandatoryAttributes: CFSet?) -> CFArray?
@available(OSX 10.5, *)
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
@available(OSX 10.8, *)
let kCTFontDescriptorMatchingSourceDescriptor: CFString
@available(OSX 10.8, *)
let kCTFontDescriptorMatchingDescriptors: CFString
@available(OSX 10.8, *)
let kCTFontDescriptorMatchingResult: CFString
@available(OSX 10.8, *)
let kCTFontDescriptorMatchingPercentage: CFString
@available(OSX 10.8, *)
let kCTFontDescriptorMatchingCurrentAssetSize: CFString
@available(OSX 10.8, *)
let kCTFontDescriptorMatchingTotalDownloadedSize: CFString
@available(OSX 10.8, *)
let kCTFontDescriptorMatchingTotalAssetSize: CFString
@available(OSX 10.8, *)
let kCTFontDescriptorMatchingError: CFString
typealias CTFontDescriptorProgressHandler = (CTFontDescriptorMatchingState, CFDictionary) -> Bool
@available(OSX 10.9, *)
@discardableResult
func CTFontDescriptorMatchFontDescriptorsWithProgressHandler(_ descriptors: CFArray, _ mandatoryAttributes: CFSet?, _ progressBlock: CTFontDescriptorProgressHandler) -> Bool
@available(OSX 10.5, *)
@discardableResult
func CTFontDescriptorCopyAttributes(_ descriptor: CTFontDescriptor) -> CFDictionary
@available(OSX 10.5, *)
@discardableResult
func CTFontDescriptorCopyAttribute(_ descriptor: CTFontDescriptor, _ attribute: CFString) -> CFTypeRef?
@available(OSX 10.5, *)
@discardableResult
func CTFontDescriptorCopyLocalizedAttribute(_ descriptor: CTFontDescriptor, _ attribute: CFString, _ language: UnsafeMutablePointer<Unmanaged<CFString>?>?) -> CFTypeRef?

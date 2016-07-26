
let kMACaptionAppearanceSettingsChangedNotification: CFString
@available(OSX 10.9, *)
enum MACaptionAppearanceDomain : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case `default`
  case user
}
@available(OSX 10.9, *)
enum MACaptionAppearanceDisplayType : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case forcedOnly
  case automatic
  case alwaysOn
}
@available(OSX 10.9, *)
enum MACaptionAppearanceBehavior : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case useValue
  case useContentIfAvailable
}
@available(OSX 10.9, *)
enum MACaptionAppearanceFontStyle : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case `default`
  case monospacedWithSerif
  case proportionalWithSerif
  case monospacedWithoutSerif
  case proportionalWithoutSerif
  case casual
  case cursive
  case smallCapital
}
@available(OSX 10.9, *)
enum MACaptionAppearanceTextEdgeStyle : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case undefined
  case none
  case raised
  case depressed
  case uniform
  case dropShadow
}
@available(OSX 10.9, *)
let MAMediaCharacteristicDescribesMusicAndSoundForAccessibility: CFString
@available(OSX 10.9, *)
let MAMediaCharacteristicTranscribesSpokenDialogForAccessibility: CFString
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceAddSelectedLanguage(_ domain: MACaptionAppearanceDomain, _ language: CFString) -> Bool
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceCopySelectedLanguages(_ domain: MACaptionAppearanceDomain) -> Unmanaged<CFArray>
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceGetDisplayType(_ domain: MACaptionAppearanceDomain) -> MACaptionAppearanceDisplayType
@available(OSX 10.9, *)
func MACaptionAppearanceSetDisplayType(_ domain: MACaptionAppearanceDomain, _ displayType: MACaptionAppearanceDisplayType)
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceCopyPreferredCaptioningMediaCharacteristics(_ domain: MACaptionAppearanceDomain) -> Unmanaged<CFArray>
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceCopyForegroundColor(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> Unmanaged<CGColor>
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceCopyBackgroundColor(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> Unmanaged<CGColor>
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceCopyWindowColor(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> Unmanaged<CGColor>
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceGetForegroundOpacity(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> CGFloat
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceGetBackgroundOpacity(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> CGFloat
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceGetWindowOpacity(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> CGFloat
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceGetWindowRoundedCornerRadius(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> CGFloat
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceCopyFontDescriptorForStyle(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?, _ fontStyle: MACaptionAppearanceFontStyle) -> Unmanaged<CTFontDescriptor>
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceGetRelativeCharacterSize(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> CGFloat
@available(OSX 10.9, *)
@discardableResult
func MACaptionAppearanceGetTextEdgeStyle(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> MACaptionAppearanceTextEdgeStyle

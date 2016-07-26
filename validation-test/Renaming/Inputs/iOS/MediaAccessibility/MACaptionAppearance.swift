
let kMACaptionAppearanceSettingsChangedNotification: CFString
@available(iOS 7.0, *)
enum MACaptionAppearanceDomain : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case `default`
  case user
}
@available(iOS 7.0, *)
enum MACaptionAppearanceDisplayType : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case forcedOnly
  case automatic
  case alwaysOn
}
@available(iOS 7.0, *)
enum MACaptionAppearanceBehavior : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case useValue
  case useContentIfAvailable
}
@available(iOS 7.0, *)
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
@available(iOS 7.0, *)
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
@available(iOS 7.0, *)
let MAMediaCharacteristicDescribesMusicAndSoundForAccessibility: CFString
@available(iOS 7.0, *)
let MAMediaCharacteristicTranscribesSpokenDialogForAccessibility: CFString
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceAddSelectedLanguage(_ domain: MACaptionAppearanceDomain, _ language: CFString) -> Bool
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceCopySelectedLanguages(_ domain: MACaptionAppearanceDomain) -> Unmanaged<CFArray>
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceGetDisplayType(_ domain: MACaptionAppearanceDomain) -> MACaptionAppearanceDisplayType
@available(iOS 7.0, *)
func MACaptionAppearanceSetDisplayType(_ domain: MACaptionAppearanceDomain, _ displayType: MACaptionAppearanceDisplayType)
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceCopyPreferredCaptioningMediaCharacteristics(_ domain: MACaptionAppearanceDomain) -> Unmanaged<CFArray>
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceCopyForegroundColor(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> Unmanaged<CGColor>
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceCopyBackgroundColor(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> Unmanaged<CGColor>
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceCopyWindowColor(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> Unmanaged<CGColor>
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceGetForegroundOpacity(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> CGFloat
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceGetBackgroundOpacity(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> CGFloat
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceGetWindowOpacity(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> CGFloat
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceGetWindowRoundedCornerRadius(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> CGFloat
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceCopyFontDescriptorForStyle(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?, _ fontStyle: MACaptionAppearanceFontStyle) -> Unmanaged<CTFontDescriptor>
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceGetRelativeCharacterSize(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> CGFloat
@available(iOS 7.0, *)
@discardableResult
func MACaptionAppearanceGetTextEdgeStyle(_ domain: MACaptionAppearanceDomain, _ behavior: UnsafeMutablePointer<MACaptionAppearanceBehavior>?) -> MACaptionAppearanceTextEdgeStyle

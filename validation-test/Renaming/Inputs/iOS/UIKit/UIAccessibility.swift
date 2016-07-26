
extension NSObject {
  var isAccessibilityElement: Bool
  var accessibilityLabel: String?
  var accessibilityHint: String?
  var accessibilityValue: String?
  var accessibilityTraits: UIAccessibilityTraits
  var accessibilityFrame: CGRect
  @available(iOS 7.0, *)
  @NSCopying var accessibilityPath: UIBezierPath?
  @available(iOS 5.0, *)
  var accessibilityActivationPoint: CGPoint
  var accessibilityLanguage: String?
  @available(iOS 5.0, *)
  var accessibilityElementsHidden: Bool
  @available(iOS 5.0, *)
  var accessibilityViewIsModal: Bool
  @available(iOS 6.0, *)
  var shouldGroupAccessibilityChildren: Bool
  @available(iOS 8.0, *)
  var accessibilityNavigationStyle: UIAccessibilityNavigationStyle
  class func isAccessibilityElement() -> Bool
  class func setIsAccessibilityElement(_ isAccessibilityElement: Bool)
  class func accessibilityLabel() -> String?
  class func setAccessibilityLabel(_ accessibilityLabel: String?)
  class func accessibilityHint() -> String?
  class func setAccessibilityHint(_ accessibilityHint: String?)
  class func accessibilityValue() -> String?
  class func setAccessibilityValue(_ accessibilityValue: String?)
  class func accessibilityTraits() -> UIAccessibilityTraits
  class func setAccessibilityTraits(_ accessibilityTraits: UIAccessibilityTraits)
  class func accessibilityFrame() -> CGRect
  class func setAccessibilityFrame(_ accessibilityFrame: CGRect)
  class func accessibilityPath() -> UIBezierPath?
  class func setAccessibilityPath(_ accessibilityPath: UIBezierPath?)
  class func accessibilityActivationPoint() -> CGPoint
  class func setAccessibilityActivationPoint(_ accessibilityActivationPoint: CGPoint)
  class func accessibilityLanguage() -> String?
  class func setAccessibilityLanguage(_ accessibilityLanguage: String?)
  class func accessibilityElementsHidden() -> Bool
  class func setAccessibilityElementsHidden(_ accessibilityElementsHidden: Bool)
  class func accessibilityViewIsModal() -> Bool
  class func setAccessibilityViewIsModal(_ accessibilityViewIsModal: Bool)
  class func shouldGroupAccessibilityChildren() -> Bool
  class func setShouldGroupAccessibilityChildren(_ shouldGroupAccessibilityChildren: Bool)
  class func accessibilityNavigationStyle() -> UIAccessibilityNavigationStyle
  class func setAccessibilityNavigationStyle(_ accessibilityNavigationStyle: UIAccessibilityNavigationStyle)
  class func accessibilityHeaderElements() -> [AnyObject]?
  class func setAccessibilityHeaderElements(_ accessibilityHeaderElements: [AnyObject]?)
}
@available(iOS 7.0, *)
@discardableResult
func UIAccessibilityConvertFrameToScreenCoordinates(_ rect: CGRect, _ view: UIView) -> CGRect
@available(iOS 7.0, *)
@discardableResult
func UIAccessibilityConvertPathToScreenCoordinates(_ path: UIBezierPath, _ view: UIView) -> UIBezierPath
extension NSObject {
  @discardableResult
  class func accessibilityElementCount() -> Int
  @discardableResult
  func accessibilityElementCount() -> Int
  @discardableResult
  class func accessibilityElement(at index: Int) -> AnyObject?
  @discardableResult
  func accessibilityElement(at index: Int) -> AnyObject?
  @discardableResult
  class func index(ofAccessibilityElement element: AnyObject) -> Int
  @discardableResult
  func index(ofAccessibilityElement element: AnyObject) -> Int
  @available(iOS 8.0, *)
  var accessibilityElements: [AnyObject]?
  class func accessibilityElements() -> [AnyObject]?
  class func setAccessibilityElements(_ accessibilityElements: [AnyObject]?)
}
extension NSObject {
  @available(iOS 4.0, *)
  class func accessibilityElementDidBecomeFocused()
  @available(iOS 4.0, *)
  func accessibilityElementDidBecomeFocused()
  @available(iOS 4.0, *)
  class func accessibilityElementDidLoseFocus()
  @available(iOS 4.0, *)
  func accessibilityElementDidLoseFocus()
  @available(iOS 4.0, *)
  @discardableResult
  class func accessibilityElementIsFocused() -> Bool
  @available(iOS 4.0, *)
  @discardableResult
  func accessibilityElementIsFocused() -> Bool
  @available(iOS 9.0, *)
  @discardableResult
  class func accessibilityAssistiveTechnologyFocusedIdentifiers() -> Set<String>?
  @available(iOS 9.0, *)
  @discardableResult
  func accessibilityAssistiveTechnologyFocusedIdentifiers() -> Set<String>?
}
@available(iOS 9.0, *)
@discardableResult
func UIAccessibilityFocusedElement(_ assistiveTechnologyIdentifier: String?) -> AnyObject?
extension NSObject {
  @available(iOS 7.0, *)
  @discardableResult
  class func accessibilityActivate() -> Bool
  @available(iOS 7.0, *)
  @discardableResult
  func accessibilityActivate() -> Bool
  @available(iOS 4.0, *)
  class func accessibilityIncrement()
  @available(iOS 4.0, *)
  func accessibilityIncrement()
  @available(iOS 4.0, *)
  class func accessibilityDecrement()
  @available(iOS 4.0, *)
  func accessibilityDecrement()
  @available(iOS 4.2, *)
  @discardableResult
  class func accessibilityScroll(_ direction: UIAccessibilityScrollDirection) -> Bool
  @available(iOS 4.2, *)
  @discardableResult
  func accessibilityScroll(_ direction: UIAccessibilityScrollDirection) -> Bool
  @available(iOS 5.0, *)
  @discardableResult
  class func accessibilityPerformEscape() -> Bool
  @available(iOS 5.0, *)
  @discardableResult
  func accessibilityPerformEscape() -> Bool
  @available(iOS 6.0, *)
  @discardableResult
  class func accessibilityPerformMagicTap() -> Bool
  @available(iOS 6.0, *)
  @discardableResult
  func accessibilityPerformMagicTap() -> Bool
  @available(iOS 8.0, *)
  var accessibilityCustomActions: [UIAccessibilityCustomAction]?
  class func accessibilityCustomActions() -> [UIAccessibilityCustomAction]?
  class func setAccessibilityCustomActions(_ accessibilityCustomActions: [UIAccessibilityCustomAction]?)
}
enum UIAccessibilityScrollDirection : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case right
  case left
  case up
  case down
  @available(iOS 5.0, *)
  case next
  @available(iOS 5.0, *)
  case previous
}
protocol UIAccessibilityReadingContent {
  @available(iOS 5.0, *)
  @discardableResult
  func accessibilityLineNumber(for point: CGPoint) -> Int
  @available(iOS 5.0, *)
  @discardableResult
  func accessibilityContent(forLineNumber lineNumber: Int) -> String?
  @available(iOS 5.0, *)
  @discardableResult
  func accessibilityFrame(forLineNumber lineNumber: Int) -> CGRect
  @available(iOS 5.0, *)
  @discardableResult
  func accessibilityPageContent() -> String?
}
func UIAccessibilityPostNotification(_ notification: UIAccessibilityNotifications, _ argument: AnyObject?)
@available(iOS 4.0, *)
@discardableResult
func UIAccessibilityIsVoiceOverRunning() -> Bool
@available(iOS 4.0, *)
let UIAccessibilityVoiceOverStatusChanged: String
@available(iOS 5.0, *)
@discardableResult
func UIAccessibilityIsMonoAudioEnabled() -> Bool
@available(iOS 5.0, *)
let UIAccessibilityMonoAudioStatusDidChangeNotification: String
@available(iOS 5.0, *)
@discardableResult
func UIAccessibilityIsClosedCaptioningEnabled() -> Bool
@available(iOS 5.0, *)
let UIAccessibilityClosedCaptioningStatusDidChangeNotification: String
@available(iOS 6.0, *)
@discardableResult
func UIAccessibilityIsInvertColorsEnabled() -> Bool
@available(iOS 6.0, *)
let UIAccessibilityInvertColorsStatusDidChangeNotification: String
@available(iOS 6.0, *)
@discardableResult
func UIAccessibilityIsGuidedAccessEnabled() -> Bool
@available(iOS 6.0, *)
let UIAccessibilityGuidedAccessStatusDidChangeNotification: String
@available(iOS 8.0, *)
@discardableResult
func UIAccessibilityIsBoldTextEnabled() -> Bool
@available(iOS 8.0, *)
let UIAccessibilityBoldTextStatusDidChangeNotification: String
@available(iOS 8.0, *)
@discardableResult
func UIAccessibilityIsGrayscaleEnabled() -> Bool
@available(iOS 8.0, *)
let UIAccessibilityGrayscaleStatusDidChangeNotification: String
@available(iOS 8.0, *)
@discardableResult
func UIAccessibilityIsReduceTransparencyEnabled() -> Bool
@available(iOS 8.0, *)
let UIAccessibilityReduceTransparencyStatusDidChangeNotification: String
@available(iOS 8.0, *)
@discardableResult
func UIAccessibilityIsReduceMotionEnabled() -> Bool
@available(iOS 8.0, *)
let UIAccessibilityReduceMotionStatusDidChangeNotification: String
@available(iOS 8.0, *)
@discardableResult
func UIAccessibilityDarkerSystemColorsEnabled() -> Bool
@available(iOS 8.0, *)
let UIAccessibilityDarkerSystemColorsStatusDidChangeNotification: String
@available(iOS 8.0, *)
@discardableResult
func UIAccessibilityIsSwitchControlRunning() -> Bool
@available(iOS 8.0, *)
let UIAccessibilitySwitchControlStatusDidChangeNotification: String
@available(iOS 8.0, *)
@discardableResult
func UIAccessibilityIsSpeakSelectionEnabled() -> Bool
@available(iOS 8.0, *)
let UIAccessibilitySpeakSelectionStatusDidChangeNotification: String
@available(iOS 8.0, *)
@discardableResult
func UIAccessibilityIsSpeakScreenEnabled() -> Bool
@available(iOS 8.0, *)
let UIAccessibilitySpeakScreenStatusDidChangeNotification: String
@available(iOS 9.0, *)
@discardableResult
func UIAccessibilityIsShakeToUndoEnabled() -> Bool
@available(iOS 9.0, *)
let UIAccessibilityShakeToUndoDidChangeNotification: String
@available(iOS 7.0, *)
func UIAccessibilityRequestGuidedAccessSession(_ enable: Bool, _ completionHandler: (Bool) -> Void)

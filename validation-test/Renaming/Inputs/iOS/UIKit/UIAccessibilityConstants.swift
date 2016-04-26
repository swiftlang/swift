
typealias UIAccessibilityTraits = UInt64
var UIAccessibilityTraitNone: UIAccessibilityTraits
var UIAccessibilityTraitButton: UIAccessibilityTraits
var UIAccessibilityTraitLink: UIAccessibilityTraits
@available(iOS 6.0, *)
var UIAccessibilityTraitHeader: UIAccessibilityTraits
var UIAccessibilityTraitSearchField: UIAccessibilityTraits
var UIAccessibilityTraitImage: UIAccessibilityTraits
var UIAccessibilityTraitSelected: UIAccessibilityTraits
var UIAccessibilityTraitPlaysSound: UIAccessibilityTraits
var UIAccessibilityTraitKeyboardKey: UIAccessibilityTraits
var UIAccessibilityTraitStaticText: UIAccessibilityTraits
var UIAccessibilityTraitSummaryElement: UIAccessibilityTraits
var UIAccessibilityTraitNotEnabled: UIAccessibilityTraits
var UIAccessibilityTraitUpdatesFrequently: UIAccessibilityTraits
@available(iOS 4.0, *)
var UIAccessibilityTraitStartsMediaSession: UIAccessibilityTraits
@available(iOS 4.0, *)
var UIAccessibilityTraitAdjustable: UIAccessibilityTraits
@available(iOS 5.0, *)
var UIAccessibilityTraitAllowsDirectInteraction: UIAccessibilityTraits
@available(iOS 5.0, *)
var UIAccessibilityTraitCausesPageTurn: UIAccessibilityTraits
typealias UIAccessibilityNotifications = UInt32
var UIAccessibilityScreenChangedNotification: UIAccessibilityNotifications
var UIAccessibilityLayoutChangedNotification: UIAccessibilityNotifications
@available(iOS 4.0, *)
var UIAccessibilityAnnouncementNotification: UIAccessibilityNotifications
@available(iOS 6.0, *)
let UIAccessibilityAnnouncementDidFinishNotification: String
@available(iOS 6.0, *)
let UIAccessibilityAnnouncementKeyStringValue: String
@available(iOS 6.0, *)
let UIAccessibilityAnnouncementKeyWasSuccessful: String
@available(iOS 9.0, *)
let UIAccessibilityElementFocusedNotification: String
@available(iOS 9.0, *)
let UIAccessibilityFocusedElementKey: String
@available(iOS 9.0, *)
let UIAccessibilityUnfocusedElementKey: String
@available(iOS 9.0, *)
let UIAccessibilityAssistiveTechnologyKey: String
@available(iOS 4.2, *)
var UIAccessibilityPageScrolledNotification: UIAccessibilityNotifications
@available(iOS 8.0, *)
var UIAccessibilityPauseAssistiveTechnologyNotification: UIAccessibilityNotifications
@available(iOS 8.0, *)
var UIAccessibilityResumeAssistiveTechnologyNotification: UIAccessibilityNotifications
@available(iOS 8.0, *)
let UIAccessibilityNotificationSwitchControlIdentifier: String
@available(iOS 9.0, *)
let UIAccessibilityNotificationVoiceOverIdentifier: String
@available(iOS 8.0, *)
enum UIAccessibilityNavigationStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case automatic
  case separate
  case combined
}
@available(iOS 7.0, *)
let UIAccessibilitySpeechAttributePunctuation: String
@available(iOS 7.0, *)
let UIAccessibilitySpeechAttributeLanguage: String
@available(iOS 7.0, *)
let UIAccessibilitySpeechAttributePitch: String

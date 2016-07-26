
typealias UIAccessibilityTraits = UInt64
var UIAccessibilityTraitNone: UIAccessibilityTraits
var UIAccessibilityTraitButton: UIAccessibilityTraits
var UIAccessibilityTraitLink: UIAccessibilityTraits
@available(watchOS 2.0, *)
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
@available(watchOS 2.0, *)
var UIAccessibilityTraitStartsMediaSession: UIAccessibilityTraits
@available(watchOS 2.0, *)
var UIAccessibilityTraitAdjustable: UIAccessibilityTraits
@available(watchOS 2.0, *)
var UIAccessibilityTraitAllowsDirectInteraction: UIAccessibilityTraits
@available(watchOS 2.0, *)
var UIAccessibilityTraitCausesPageTurn: UIAccessibilityTraits
typealias UIAccessibilityNotifications = UInt32
var UIAccessibilityScreenChangedNotification: UIAccessibilityNotifications
var UIAccessibilityLayoutChangedNotification: UIAccessibilityNotifications
@available(watchOS 2.0, *)
var UIAccessibilityAnnouncementNotification: UIAccessibilityNotifications
@available(watchOS 2.0, *)
let UIAccessibilityAnnouncementDidFinishNotification: String
@available(watchOS 2.0, *)
let UIAccessibilityAnnouncementKeyStringValue: String
@available(watchOS 2.0, *)
let UIAccessibilityAnnouncementKeyWasSuccessful: String
@available(watchOS 2.0, *)
let UIAccessibilityElementFocusedNotification: String
@available(watchOS 2.0, *)
let UIAccessibilityFocusedElementKey: String
@available(watchOS 2.0, *)
let UIAccessibilityUnfocusedElementKey: String
@available(watchOS 2.0, *)
let UIAccessibilityAssistiveTechnologyKey: String
@available(watchOS 2.0, *)
var UIAccessibilityPageScrolledNotification: UIAccessibilityNotifications
@available(watchOS 2.0, *)
var UIAccessibilityPauseAssistiveTechnologyNotification: UIAccessibilityNotifications
@available(watchOS 2.0, *)
var UIAccessibilityResumeAssistiveTechnologyNotification: UIAccessibilityNotifications
@available(watchOS 2.0, *)
let UIAccessibilityNotificationSwitchControlIdentifier: String
@available(watchOS 2.0, *)
let UIAccessibilityNotificationVoiceOverIdentifier: String
@available(watchOS 2.0, *)
enum UIAccessibilityNavigationStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case automatic
  case separate
  case combined
}
@available(watchOS 2.0, *)
let UIAccessibilitySpeechAttributePunctuation: String
@available(watchOS 2.0, *)
let UIAccessibilitySpeechAttributeLanguage: String
@available(watchOS 2.0, *)
let UIAccessibilitySpeechAttributePitch: String

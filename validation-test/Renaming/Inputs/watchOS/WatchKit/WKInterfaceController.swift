
@available(watchOS 2.0, *)
enum WKUserNotificationInterfaceType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case custom
}
@available(watchOS 2.0, *)
enum WKMenuItemIcon : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case accept
  case add
  case block
  case decline
  case info
  case maybe
  case more
  case mute
  case pause
  case play
  case `repeat`
  case resume
  case share
  case shuffle
  case speaker
  case trash
}
enum WKTextInputMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case plain
  case allowEmoji
  case allowAnimatedEmoji
}
@available(watchOS 2.0, *)
enum WKAlertControllerStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case alert
  case sideBySideButtonsAlert
  case actionSheet
}
@available(watchOS 2.0, *)
enum WKVideoGravity : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case resizeAspect
  case resizeAspectFill
  case resize
}
@available(watchOS 2.0, *)
enum WKAudioRecorderPreset : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case narrowBandSpeech
  case wideBandSpeech
  case highQualityAudio
}
@available(watchOS 2.0, *)
class WKInterfaceController : NSObject {
  func awake(withContext context: AnyObject?)
  var contentFrame: CGRect { get }
  func willActivate()
  func didDeactivate()
  @available(watchOS 2.0, *)
  func didAppear()
  @available(watchOS 2.0, *)
  func willDisappear()
  @available(watchOS 2.0, *)
  func pickerDidFocus(_ picker: WKInterfacePicker)
  @available(watchOS 2.0, *)
  func pickerDidResignFocus(_ picker: WKInterfacePicker)
  @available(watchOS 2.0, *)
  func pickerDidSettle(_ picker: WKInterfacePicker)
  func table(_ table: WKInterfaceTable, didSelectRowAt rowIndex: Int)
  func handleAction(withIdentifier identifier: String?, forRemoteNotification remoteNotification: [NSObject : AnyObject])
  func handleAction(withIdentifier identifier: String?, for localNotification: UILocalNotification)
  func handleUserActivity(_ userInfo: [NSObject : AnyObject]?)
  func setTitle(_ title: String?)
  func pushController(withName name: String, context context: AnyObject?)
  func pop()
  func popToRootController()
  class func reloadRootControllers(withNames names: [String], contexts contexts: [AnyObject]?)
  func becomeCurrentPage()
  func presentController(withName name: String, context context: AnyObject?)
  func presentController(withNames names: [String], contexts contexts: [AnyObject]?)
  func dismiss()
  func presentTextInputController(withSuggestions suggestions: [String]?, allowedInputMode inputMode: WKTextInputMode, completion completion: ([AnyObject]?) -> Void)
  func presentTextInputControllerWithSuggestions(forLanguage suggestionsHandler: ((String) -> [AnyObject]?)?, allowedInputMode inputMode: WKTextInputMode, completion completion: ([AnyObject]?) -> Void)
  func dismissTextInputController()
  @available(watchOS 2.0, *)
  func presentMediaPlayerController(with URL: NSURL, options options: [NSObject : AnyObject]? = [:], completion completion: (Bool, NSTimeInterval, NSError?) -> Void)
  @available(watchOS 2.0, *)
  func dismissMediaPlayerController()
  @available(watchOS 2.0, *)
  func presentAudioRecorderController(withOutputURL URL: NSURL, preset preset: WKAudioRecorderPreset, options options: [NSObject : AnyObject]? = [:], completion completion: (Bool, NSError?) -> Void)
  @available(watchOS 2.0, *)
  func dismissAudioRecorderController()
  @discardableResult
  func contextForSegue(withIdentifier segueIdentifier: String) -> AnyObject?
  @discardableResult
  func contextsForSegue(withIdentifier segueIdentifier: String) -> [AnyObject]?
  @discardableResult
  func contextForSegue(withIdentifier segueIdentifier: String, in table: WKInterfaceTable, rowIndex rowIndex: Int) -> AnyObject?
  @discardableResult
  func contextsForSegue(withIdentifier segueIdentifier: String, in table: WKInterfaceTable, rowIndex rowIndex: Int) -> [AnyObject]?
  @available(watchOS 2.0, *)
  func animate(withDuration duration: NSTimeInterval, animations animations: () -> Void)
  @available(watchOS 2.0, *)
  func presentAlert(withTitle title: String?, message message: String?, preferredStyle preferredStyle: WKAlertControllerStyle, actions actions: [WKAlertAction])
  @available(watchOS 2.0, *)
  func dismissAddPassesController()
  func addMenuItem(with image: UIImage, title title: String, action action: Selector)
  func addMenuItem(withImageNamed imageName: String, title title: String, action action: Selector)
  func addMenuItem(with itemIcon: WKMenuItemIcon, title title: String, action action: Selector)
  func clearAllMenuItems()
  func updateUserActivity(_ type: String, userInfo userInfo: [NSObject : AnyObject]? = [:], webpageURL webpageURL: NSURL?)
  func invalidateUserActivity()
  @available(watchOS 2.0, *)
  func beginGlanceUpdates()
  @available(watchOS 2.0, *)
  func endGlanceUpdates()
}

@available(iOS 8.2, *)
extension WKInterfaceController {
  class func reloadRootControllers(_ namesAndContexts: [(name: String, context: AnyObject)])
  func presentController(_ namesAndContexts: [(name: String, context: AnyObject)])
}
@available(watchOS 2.0, *)
let UIUserNotificationActionResponseTypedTextKey: String
@available(watchOS 2.0, *)
let WKMediaPlayerControllerOptionsAutoplayKey: String
@available(watchOS 2.0, *)
let WKMediaPlayerControllerOptionsStartTimeKey: String
@available(watchOS 2.0, *)
let WKMediaPlayerControllerOptionsVideoGravityKey: String
@available(watchOS 2.0, *)
let WKMediaPlayerControllerOptionsLoopsKey: String
@available(watchOS 2.0, *)
let WKAudioRecorderControllerOptionsActionTitleKey: String
@available(watchOS 2.0, *)
let WKAudioRecorderControllerOptionsAlwaysShowActionTitleKey: String
@available(watchOS 2.0, *)
let WKAudioRecorderControllerOptionsAutorecordKey: String
@available(watchOS 2.0, *)
let WKAudioRecorderControllerOptionsMaximumDurationKey: String
@available(watchOS 2.0, *)
class WKUserNotificationInterfaceController : WKInterfaceController {
  func didReceiveRemoteNotification(_ remoteNotification: [NSObject : AnyObject], withCompletion completionHandler: (WKUserNotificationInterfaceType) -> Void)
  func didReceive(_ localNotification: UILocalNotification, withCompletion completionHandler: (WKUserNotificationInterfaceType) -> Void)
  @available(watchOS 2.0, *)
  @discardableResult
  func suggestionsForResponseToAction(withIdentifier identifier: String, forRemoteNotification remoteNotification: [NSObject : AnyObject], inputLanguage inputLanguage: String) -> [String]
  @available(watchOS 2.0, *)
  @discardableResult
  func suggestionsForResponseToAction(withIdentifier identifier: String, for localNotification: UILocalNotification, inputLanguage inputLanguage: String) -> [String]
}

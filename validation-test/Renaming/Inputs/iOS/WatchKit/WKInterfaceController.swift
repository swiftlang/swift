
@available(iOS 8.2, *)
enum WKUserNotificationInterfaceType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case custom
}
@available(iOS 8.2, *)
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
@available(iOS 8.2, *)
class WKInterfaceController : NSObject {
  func awake(withContext context: AnyObject?)
  var contentFrame: CGRect { get }
  func willActivate()
  func didDeactivate()
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
  @discardableResult
  func contextForSegue(withIdentifier segueIdentifier: String) -> AnyObject?
  @discardableResult
  func contextsForSegue(withIdentifier segueIdentifier: String) -> [AnyObject]?
  @discardableResult
  func contextForSegue(withIdentifier segueIdentifier: String, in table: WKInterfaceTable, rowIndex rowIndex: Int) -> AnyObject?
  @discardableResult
  func contextsForSegue(withIdentifier segueIdentifier: String, in table: WKInterfaceTable, rowIndex rowIndex: Int) -> [AnyObject]?
  func addMenuItem(with image: UIImage, title title: String, action action: Selector)
  func addMenuItem(withImageNamed imageName: String, title title: String, action action: Selector)
  func addMenuItem(with itemIcon: WKMenuItemIcon, title title: String, action action: Selector)
  func clearAllMenuItems()
  func updateUserActivity(_ type: String, userInfo userInfo: [NSObject : AnyObject]? = [:], webpageURL webpageURL: NSURL?)
  func invalidateUserActivity()
  @available(iOS 8.2, *)
  @discardableResult
  class func openParentApplication(_ userInfo: [NSObject : AnyObject], reply reply: (([NSObject : AnyObject], NSError?) -> Void)? = nil) -> Bool
}

@available(iOS 8.2, *)
extension WKInterfaceController {
  class func reloadRootControllers(_ namesAndContexts: [(name: String, context: AnyObject)])
  func presentController(_ namesAndContexts: [(name: String, context: AnyObject)])
}
@available(iOS 8.2, *)
class WKUserNotificationInterfaceController : WKInterfaceController {
  func didReceiveRemoteNotification(_ remoteNotification: [NSObject : AnyObject], withCompletion completionHandler: (WKUserNotificationInterfaceType) -> Void)
  func didReceive(_ localNotification: UILocalNotification, withCompletion completionHandler: (WKUserNotificationInterfaceType) -> Void)
}


enum UIStatusBarStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  @available(iOS 7.0, *)
  case lightContent
}
enum UIStatusBarAnimation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  @available(iOS 3.2, *)
  case fade
  @available(iOS 3.2, *)
  case slide
}
enum UIInterfaceOrientation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case portrait
  case portraitUpsideDown
  case landscapeLeft
  case landscapeRight
}

extension UIInterfaceOrientation {
  var isLandscape: Bool { get }
  var isPortrait: Bool { get }
}
@available(iOS 6.0, *)
let UIApplicationInvalidInterfaceOrientationException: String
struct UIInterfaceOrientationMask : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var portrait: UIInterfaceOrientationMask { get }
  static var landscapeLeft: UIInterfaceOrientationMask { get }
  static var landscapeRight: UIInterfaceOrientationMask { get }
  static var portraitUpsideDown: UIInterfaceOrientationMask { get }
  static var landscape: UIInterfaceOrientationMask { get }
  static var all: UIInterfaceOrientationMask { get }
  static var allButUpsideDown: UIInterfaceOrientationMask { get }
}
@discardableResult
func UIInterfaceOrientationIsPortrait(_ orientation: UIInterfaceOrientation) -> Bool
@discardableResult
func UIInterfaceOrientationIsLandscape(_ orientation: UIInterfaceOrientation) -> Bool
@available(iOS, introduced: 3.0, deprecated: 8.0, message: "Use UIUserNotificationType for user notifications and registerForRemoteNotifications for receiving remote notifications instead.")
struct UIRemoteNotificationType : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var badge: UIRemoteNotificationType { get }
  static var sound: UIRemoteNotificationType { get }
  static var alert: UIRemoteNotificationType { get }
  static var newsstandContentAvailability: UIRemoteNotificationType { get }
}
@available(iOS 7.0, *)
enum UIBackgroundFetchResult : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case newData
  case noData
  case failed
}
@available(iOS 7.0, *)
enum UIBackgroundRefreshStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case restricted
  case denied
  case available
}
@available(iOS 4.0, *)
enum UIApplicationState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case active
  case inactive
  case background
}
typealias UIBackgroundTaskIdentifier = Int
@available(iOS 4.0, *)
let UIBackgroundTaskInvalid: UIBackgroundTaskIdentifier
@available(iOS 4.0, *)
let UIMinimumKeepAliveTimeout: NSTimeInterval
@available(iOS 7.0, *)
let UIApplicationBackgroundFetchIntervalMinimum: NSTimeInterval
@available(iOS 7.0, *)
let UIApplicationBackgroundFetchIntervalNever: NSTimeInterval
@available(iOS 2.0, *)
class UIApplication : UIResponder {
  @discardableResult
  class func shared() -> UIApplication
  unowned(unsafe) var delegate: @sil_unmanaged UIApplicationDelegate?
  func beginIgnoringInteractionEvents()
  func endIgnoringInteractionEvents()
  @discardableResult
  func isIgnoringInteractionEvents() -> Bool
  var isIdleTimerDisabled: Bool
  @discardableResult
  func open(_ url: NSURL) -> Bool
  @available(iOS 3.0, *)
  @discardableResult
  func canOpen(_ url: NSURL) -> Bool
  func send(_ event: UIEvent)
  var keyWindow: UIWindow? { get }
  var windows: [UIWindow] { get }
  @discardableResult
  func sendAction(_ action: Selector, to target: AnyObject?, from sender: AnyObject?, for event: UIEvent?) -> Bool
  var isNetworkActivityIndicatorVisible: Bool
  @available(iOS 6.0, *)
  @discardableResult
  func supportedInterfaceOrientations(for window: UIWindow?) -> UIInterfaceOrientationMask
  var statusBarOrientationAnimationDuration: NSTimeInterval { get }
  var statusBarFrame: CGRect { get }
  var applicationIconBadgeNumber: Int
  @available(iOS 3.0, *)
  var applicationSupportsShakeToEdit: Bool
  @available(iOS 4.0, *)
  var applicationState: UIApplicationState { get }
  @available(iOS 4.0, *)
  var backgroundTimeRemaining: NSTimeInterval { get }
  @available(iOS 4.0, *)
  @discardableResult
  func beginBackgroundTask(expirationHandler handler: (() -> Void)? = nil) -> UIBackgroundTaskIdentifier
  @available(iOS 7.0, *)
  @discardableResult
  func beginBackgroundTask(withName taskName: String?, expirationHandler handler: (() -> Void)? = nil) -> UIBackgroundTaskIdentifier
  @available(iOS 4.0, *)
  func endBackgroundTask(_ identifier: UIBackgroundTaskIdentifier)
  @available(iOS 7.0, *)
  func setMinimumBackgroundFetchInterval(_ minimumBackgroundFetchInterval: NSTimeInterval)
  @available(iOS 7.0, *)
  var backgroundRefreshStatus: UIBackgroundRefreshStatus { get }
  @available(iOS 4.0, *)
  var isProtectedDataAvailable: Bool { get }
  @available(iOS 5.0, *)
  var userInterfaceLayoutDirection: UIUserInterfaceLayoutDirection { get }
  @available(iOS 7.0, *)
  var preferredContentSizeCategory: String { get }
}
extension UIApplication {
  @available(iOS 8.0, *)
  func registerForRemoteNotifications()
  @available(iOS 3.0, *)
  func unregisterForRemoteNotifications()
  @available(iOS 8.0, *)
  @discardableResult
  func isRegisteredForRemoteNotifications() -> Bool
  @available(iOS, introduced: 3.0, deprecated: 8.0, message: "Please use registerForRemoteNotifications and registerUserNotificationSettings: instead")
  func register(forRemoteNotificationTypes types: UIRemoteNotificationType)
  @available(iOS, introduced: 3.0, deprecated: 8.0, message: "Please use -[UIApplication isRegisteredForRemoteNotifications], or -[UIApplication currentUserNotificationSettings] to retrieve user-enabled remote notification and user notification settings")
  @discardableResult
  func enabledRemoteNotificationTypes() -> UIRemoteNotificationType
}
extension UIApplication {
  @available(iOS 4.0, *)
  func presentLocalNotificationNow(_ notification: UILocalNotification)
  @available(iOS 4.0, *)
  func scheduleLocalNotification(_ notification: UILocalNotification)
  @available(iOS 4.0, *)
  func cancel(_ notification: UILocalNotification)
  @available(iOS 4.0, *)
  func cancelAllLocalNotifications()
  @available(iOS 4.0, *)
  var scheduledLocalNotifications: [UILocalNotification]?
}
extension UIApplication {
  @available(iOS 8.0, *)
  func register(_ notificationSettings: UIUserNotificationSettings)
  @available(iOS 8.0, *)
  @discardableResult
  func currentUserNotificationSettings() -> UIUserNotificationSettings?
}
extension UIApplication {
  @available(iOS 4.0, *)
  func beginReceivingRemoteControlEvents()
  @available(iOS 4.0, *)
  func endReceivingRemoteControlEvents()
}
extension UIApplication {
  @available(iOS, introduced: 9.0, deprecated: 9.0, message: "Newsstand apps now behave like normal apps on SpringBoard")
  func setNewsstandIconImage(_ image: UIImage?)
}
extension UIApplication {
  @available(iOS 9.0, *)
  var shortcutItems: [UIApplicationShortcutItem]?
}
extension UIApplication {
  @available(iOS 6.0, *)
  func extendStateRestoration()
  @available(iOS 6.0, *)
  func completeStateRestoration()
  @available(iOS 7.0, *)
  func ignoreSnapshotOnNextApplicationLaunch()
  @available(iOS 7.0, *)
  class func registerObject(forStateRestoration object: UIStateRestoring, restorationIdentifier restorationIdentifier: String)
}
protocol UIApplicationDelegate : NSObjectProtocol {
  @available(iOS 2.0, *)
  optional func applicationDidFinishLaunching(_ application: UIApplication)
  @available(iOS 6.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, willFinishLaunchingWithOptions launchOptions: [NSObject : AnyObject]? = [:]) -> Bool
  @available(iOS 3.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [NSObject : AnyObject]? = [:]) -> Bool
  @available(iOS 2.0, *)
  optional func applicationDidBecomeActive(_ application: UIApplication)
  @available(iOS 2.0, *)
  optional func applicationWillResignActive(_ application: UIApplication)
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Please use application:openURL:options:")
  @discardableResult
  optional func application(_ application: UIApplication, handleOpen url: NSURL) -> Bool
  @available(iOS, introduced: 4.2, deprecated: 9.0, message: "Please use application:openURL:options:")
  @discardableResult
  optional func application(_ application: UIApplication, open url: NSURL, sourceApplication sourceApplication: String?, annotation annotation: AnyObject) -> Bool
  @available(iOS 9.0, *)
  @discardableResult
  optional func application(_ app: UIApplication, open url: NSURL, options options: [String : AnyObject] = [:]) -> Bool
  @available(iOS 2.0, *)
  optional func applicationDidReceiveMemoryWarning(_ application: UIApplication)
  @available(iOS 2.0, *)
  optional func applicationWillTerminate(_ application: UIApplication)
  @available(iOS 2.0, *)
  optional func applicationSignificantTimeChange(_ application: UIApplication)
  @available(iOS 2.0, *)
  optional func application(_ application: UIApplication, willChangeStatusBarOrientation newStatusBarOrientation: UIInterfaceOrientation, duration duration: NSTimeInterval)
  @available(iOS 2.0, *)
  optional func application(_ application: UIApplication, didChangeStatusBarOrientation oldStatusBarOrientation: UIInterfaceOrientation)
  @available(iOS 2.0, *)
  optional func application(_ application: UIApplication, willChangeStatusBarFrame newStatusBarFrame: CGRect)
  @available(iOS 2.0, *)
  optional func application(_ application: UIApplication, didChangeStatusBarFrame oldStatusBarFrame: CGRect)
  @available(iOS 8.0, *)
  optional func application(_ application: UIApplication, didRegister notificationSettings: UIUserNotificationSettings)
  @available(iOS 3.0, *)
  optional func application(_ application: UIApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: NSData)
  @available(iOS 3.0, *)
  optional func application(_ application: UIApplication, didFailToRegisterForRemoteNotificationsWithError error: NSError)
  @available(iOS 3.0, *)
  optional func application(_ application: UIApplication, didReceiveRemoteNotification userInfo: [NSObject : AnyObject])
  @available(iOS 4.0, *)
  optional func application(_ application: UIApplication, didReceive notification: UILocalNotification)
  @available(iOS 8.0, *)
  optional func application(_ application: UIApplication, handleActionWithIdentifier identifier: String?, for notification: UILocalNotification, completionHandler completionHandler: () -> Void)
  @available(iOS 9.0, *)
  optional func application(_ application: UIApplication, handleActionWithIdentifier identifier: String?, forRemoteNotification userInfo: [NSObject : AnyObject], withResponseInfo responseInfo: [NSObject : AnyObject], completionHandler completionHandler: () -> Void)
  @available(iOS 8.0, *)
  optional func application(_ application: UIApplication, handleActionWithIdentifier identifier: String?, forRemoteNotification userInfo: [NSObject : AnyObject], completionHandler completionHandler: () -> Void)
  @available(iOS 9.0, *)
  optional func application(_ application: UIApplication, handleActionWithIdentifier identifier: String?, for notification: UILocalNotification, withResponseInfo responseInfo: [NSObject : AnyObject], completionHandler completionHandler: () -> Void)
  @available(iOS 7.0, *)
  optional func application(_ application: UIApplication, didReceiveRemoteNotification userInfo: [NSObject : AnyObject], fetchCompletionHandler completionHandler: (UIBackgroundFetchResult) -> Void)
  @available(iOS 7.0, *)
  optional func application(_ application: UIApplication, performFetchWithCompletionHandler completionHandler: (UIBackgroundFetchResult) -> Void)
  @available(iOS 9.0, *)
  optional func application(_ application: UIApplication, performActionFor shortcutItem: UIApplicationShortcutItem, completionHandler completionHandler: (Bool) -> Void)
  @available(iOS 7.0, *)
  optional func application(_ application: UIApplication, handleEventsForBackgroundURLSession identifier: String, completionHandler completionHandler: () -> Void)
  @available(iOS 8.2, *)
  optional func application(_ application: UIApplication, handleWatchKitExtensionRequest userInfo: [NSObject : AnyObject]?, reply reply: ([NSObject : AnyObject]?) -> Void)
  @available(iOS 9.0, *)
  optional func applicationShouldRequestHealthAuthorization(_ application: UIApplication)
  @available(iOS 4.0, *)
  optional func applicationDidEnterBackground(_ application: UIApplication)
  @available(iOS 4.0, *)
  optional func applicationWillEnterForeground(_ application: UIApplication)
  @available(iOS 4.0, *)
  optional func applicationProtectedDataWillBecomeUnavailable(_ application: UIApplication)
  @available(iOS 4.0, *)
  optional func applicationProtectedDataDidBecomeAvailable(_ application: UIApplication)
  @available(iOS 5.0, *)
  optional var window: UIWindow? { get set }
  @available(iOS 6.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, supportedInterfaceOrientationsFor window: UIWindow?) -> UIInterfaceOrientationMask
  @available(iOS 8.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, shouldAllowExtensionPointIdentifier extensionPointIdentifier: String) -> Bool
  @available(iOS 6.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, viewControllerWithRestorationIdentifierPath identifierComponents: [AnyObject], coder coder: NSCoder) -> UIViewController?
  @available(iOS 6.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, shouldSaveApplicationState coder: NSCoder) -> Bool
  @available(iOS 6.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, shouldRestoreApplicationState coder: NSCoder) -> Bool
  @available(iOS 6.0, *)
  optional func application(_ application: UIApplication, willEncodeRestorableStateWith coder: NSCoder)
  @available(iOS 6.0, *)
  optional func application(_ application: UIApplication, didDecodeRestorableStateWith coder: NSCoder)
  @available(iOS 8.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, willContinueUserActivityWithType userActivityType: String) -> Bool
  @available(iOS 8.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, continue userActivity: NSUserActivity, restorationHandler restorationHandler: ([AnyObject]?) -> Void) -> Bool
  @available(iOS 8.0, *)
  optional func application(_ application: UIApplication, didFailToContinueUserActivityWithType userActivityType: String, error error: NSError)
  @available(iOS 8.0, *)
  optional func application(_ application: UIApplication, didUpdate userActivity: NSUserActivity)
}
extension UIApplication {
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Explicit setting of the status bar orientation is more limited in iOS 6.0 and later")
  var statusBarOrientation: UIInterfaceOrientation
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Explicit setting of the status bar orientation is more limited in iOS 6.0 and later")
  func setStatusBarOrientation(_ interfaceOrientation: UIInterfaceOrientation, animated animated: Bool)
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use -[UIViewController preferredStatusBarStyle]")
  var statusBarStyle: UIStatusBarStyle
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use -[UIViewController preferredStatusBarStyle]")
  func setStatusBarStyle(_ statusBarStyle: UIStatusBarStyle, animated animated: Bool)
  @available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use -[UIViewController prefersStatusBarHidden]")
  var isStatusBarHidden: Bool
  @available(iOS, introduced: 3.2, deprecated: 9.0, message: "Use -[UIViewController prefersStatusBarHidden]")
  func setStatusBarHidden(_ hidden: Bool, with animation: UIStatusBarAnimation)
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Please use UIRemoteNotificationTypeVoIP remote notifications for VoIP applications")
  @discardableResult
  func setKeepAliveTimeout(_ timeout: NSTimeInterval, handler keepAliveHandler: (() -> Void)? = nil) -> Bool
  @available(iOS, introduced: 4.0, deprecated: 9.0, message: "Please use UIRemoteNotificationTypeVoIP remote notifications for VoIP applications")
  func clearKeepAliveTimeout()
}
@discardableResult
func UIApplicationMain(_ argc: Int32, _ argv: UnsafeMutablePointer<UnsafeMutablePointer<Int8>>!, _ principalClassName: String?, _ delegateClassName: String?) -> Int32
let UITrackingRunLoopMode: String
@available(iOS 4.0, *)
let UIApplicationDidEnterBackgroundNotification: String
@available(iOS 4.0, *)
let UIApplicationWillEnterForegroundNotification: String
let UIApplicationDidFinishLaunchingNotification: String
let UIApplicationDidBecomeActiveNotification: String
let UIApplicationWillResignActiveNotification: String
let UIApplicationDidReceiveMemoryWarningNotification: String
let UIApplicationWillTerminateNotification: String
let UIApplicationSignificantTimeChangeNotification: String
let UIApplicationWillChangeStatusBarOrientationNotification: String
let UIApplicationDidChangeStatusBarOrientationNotification: String
let UIApplicationStatusBarOrientationUserInfoKey: String
let UIApplicationWillChangeStatusBarFrameNotification: String
let UIApplicationDidChangeStatusBarFrameNotification: String
let UIApplicationStatusBarFrameUserInfoKey: String
@available(iOS 7.0, *)
let UIApplicationBackgroundRefreshStatusDidChangeNotification: String
@available(iOS 3.0, *)
let UIApplicationLaunchOptionsURLKey: String
@available(iOS 3.0, *)
let UIApplicationLaunchOptionsSourceApplicationKey: String
@available(iOS 3.0, *)
let UIApplicationLaunchOptionsRemoteNotificationKey: String
@available(iOS 4.0, *)
let UIApplicationLaunchOptionsLocalNotificationKey: String
@available(iOS 3.2, *)
let UIApplicationLaunchOptionsAnnotationKey: String
@available(iOS 4.0, *)
let UIApplicationProtectedDataWillBecomeUnavailable: String
@available(iOS 4.0, *)
let UIApplicationProtectedDataDidBecomeAvailable: String
@available(iOS 4.0, *)
let UIApplicationLaunchOptionsLocationKey: String
@available(iOS 5.0, *)
let UIApplicationLaunchOptionsNewsstandDownloadsKey: String
@available(iOS 7.0, *)
let UIApplicationLaunchOptionsBluetoothCentralsKey: String
@available(iOS 7.0, *)
let UIApplicationLaunchOptionsBluetoothPeripheralsKey: String
@available(iOS 9.0, *)
let UIApplicationLaunchOptionsShortcutItemKey: String
@available(iOS 8.0, *)
let UIApplicationLaunchOptionsUserActivityDictionaryKey: String
@available(iOS 8.0, *)
let UIApplicationLaunchOptionsUserActivityTypeKey: String
@available(iOS 8.0, *)
let UIApplicationOpenSettingsURLString: String
@available(iOS 9.0, *)
let UIApplicationOpenURLOptionsSourceApplicationKey: String
@available(iOS 9.0, *)
let UIApplicationOpenURLOptionsAnnotationKey: String
@available(iOS 9.0, *)
let UIApplicationOpenURLOptionsOpenInPlaceKey: String
@available(iOS 7.0, *)
let UIContentSizeCategoryExtraSmall: String
@available(iOS 7.0, *)
let UIContentSizeCategorySmall: String
@available(iOS 7.0, *)
let UIContentSizeCategoryMedium: String
@available(iOS 7.0, *)
let UIContentSizeCategoryLarge: String
@available(iOS 7.0, *)
let UIContentSizeCategoryExtraLarge: String
@available(iOS 7.0, *)
let UIContentSizeCategoryExtraExtraLarge: String
@available(iOS 7.0, *)
let UIContentSizeCategoryExtraExtraExtraLarge: String
@available(iOS 7.0, *)
let UIContentSizeCategoryAccessibilityMedium: String
@available(iOS 7.0, *)
let UIContentSizeCategoryAccessibilityLarge: String
@available(iOS 7.0, *)
let UIContentSizeCategoryAccessibilityExtraLarge: String
@available(iOS 7.0, *)
let UIContentSizeCategoryAccessibilityExtraExtraLarge: String
@available(iOS 7.0, *)
let UIContentSizeCategoryAccessibilityExtraExtraExtraLarge: String
@available(iOS 7.0, *)
let UIContentSizeCategoryDidChangeNotification: String
@available(iOS 7.0, *)
let UIContentSizeCategoryNewValueKey: String
@available(iOS 7.0, *)
let UIApplicationUserDidTakeScreenshotNotification: String
@available(iOS 8.0, *)
let UIApplicationKeyboardExtensionPointIdentifier: String

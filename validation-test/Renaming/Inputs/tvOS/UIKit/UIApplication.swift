
@available(tvOS 4.0, *)
enum UIApplicationState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case active
  case inactive
  case background
}
typealias UIBackgroundTaskIdentifier = Int
@available(tvOS 4.0, *)
let UIBackgroundTaskInvalid: UIBackgroundTaskIdentifier
@available(tvOS 4.0, *)
let UIMinimumKeepAliveTimeout: NSTimeInterval
@available(tvOS 2.0, *)
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
  @available(tvOS 3.0, *)
  @discardableResult
  func canOpen(_ url: NSURL) -> Bool
  func send(_ event: UIEvent)
  var keyWindow: UIWindow? { get }
  var windows: [UIWindow] { get }
  @discardableResult
  func sendAction(_ action: Selector, to target: AnyObject?, from sender: AnyObject?, for event: UIEvent?) -> Bool
  @available(tvOS 4.0, *)
  var applicationState: UIApplicationState { get }
  @available(tvOS 4.0, *)
  var backgroundTimeRemaining: NSTimeInterval { get }
  @available(tvOS 4.0, *)
  @discardableResult
  func beginBackgroundTask(expirationHandler handler: (() -> Void)? = nil) -> UIBackgroundTaskIdentifier
  @available(tvOS 7.0, *)
  @discardableResult
  func beginBackgroundTask(withName taskName: String?, expirationHandler handler: (() -> Void)? = nil) -> UIBackgroundTaskIdentifier
  @available(tvOS 4.0, *)
  func endBackgroundTask(_ identifier: UIBackgroundTaskIdentifier)
  @available(tvOS 4.0, *)
  var isProtectedDataAvailable: Bool { get }
  @available(tvOS 5.0, *)
  var userInterfaceLayoutDirection: UIUserInterfaceLayoutDirection { get }
  @available(tvOS 7.0, *)
  var preferredContentSizeCategory: String { get }
}
extension UIApplication {
  @available(tvOS 8.0, *)
  func registerForRemoteNotifications()
  @available(tvOS 3.0, *)
  func unregisterForRemoteNotifications()
  @available(tvOS 8.0, *)
  @discardableResult
  func isRegisteredForRemoteNotifications() -> Bool
}
extension UIApplication {
}
extension UIApplication {
}
extension UIApplication {
  @available(tvOS 4.0, *)
  func beginReceivingRemoteControlEvents()
  @available(tvOS 4.0, *)
  func endReceivingRemoteControlEvents()
}
extension UIApplication {
}
extension UIApplication {
}
extension UIApplication {
  @available(tvOS 6.0, *)
  func extendStateRestoration()
  @available(tvOS 6.0, *)
  func completeStateRestoration()
  @available(tvOS 7.0, *)
  func ignoreSnapshotOnNextApplicationLaunch()
  @available(tvOS 7.0, *)
  class func registerObject(forStateRestoration object: UIStateRestoring, restorationIdentifier restorationIdentifier: String)
}
protocol UIApplicationDelegate : NSObjectProtocol {
  @available(tvOS 2.0, *)
  optional func applicationDidFinishLaunching(_ application: UIApplication)
  @available(tvOS 6.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, willFinishLaunchingWithOptions launchOptions: [NSObject : AnyObject]? = [:]) -> Bool
  @available(tvOS 3.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [NSObject : AnyObject]? = [:]) -> Bool
  @available(tvOS 2.0, *)
  optional func applicationDidBecomeActive(_ application: UIApplication)
  @available(tvOS 2.0, *)
  optional func applicationWillResignActive(_ application: UIApplication)
  @available(tvOS 9.0, *)
  @discardableResult
  optional func application(_ app: UIApplication, open url: NSURL, options options: [String : AnyObject] = [:]) -> Bool
  @available(tvOS 2.0, *)
  optional func applicationDidReceiveMemoryWarning(_ application: UIApplication)
  @available(tvOS 2.0, *)
  optional func applicationWillTerminate(_ application: UIApplication)
  @available(tvOS 2.0, *)
  optional func applicationSignificantTimeChange(_ application: UIApplication)
  @available(tvOS 3.0, *)
  optional func application(_ application: UIApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: NSData)
  @available(tvOS 3.0, *)
  optional func application(_ application: UIApplication, didFailToRegisterForRemoteNotificationsWithError error: NSError)
  @available(tvOS 3.0, *)
  optional func application(_ application: UIApplication, didReceiveRemoteNotification userInfo: [NSObject : AnyObject])
  @available(tvOS 7.0, *)
  optional func application(_ application: UIApplication, handleEventsForBackgroundURLSession identifier: String, completionHandler completionHandler: () -> Void)
  @available(tvOS 8.2, *)
  optional func application(_ application: UIApplication, handleWatchKitExtensionRequest userInfo: [NSObject : AnyObject]?, reply reply: ([NSObject : AnyObject]?) -> Void)
  @available(tvOS 9.0, *)
  optional func applicationShouldRequestHealthAuthorization(_ application: UIApplication)
  @available(tvOS 4.0, *)
  optional func applicationDidEnterBackground(_ application: UIApplication)
  @available(tvOS 4.0, *)
  optional func applicationWillEnterForeground(_ application: UIApplication)
  @available(tvOS 4.0, *)
  optional func applicationProtectedDataWillBecomeUnavailable(_ application: UIApplication)
  @available(tvOS 4.0, *)
  optional func applicationProtectedDataDidBecomeAvailable(_ application: UIApplication)
  @available(tvOS 5.0, *)
  optional var window: UIWindow? { get set }
  @available(tvOS 8.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, shouldAllowExtensionPointIdentifier extensionPointIdentifier: String) -> Bool
  @available(tvOS 6.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, viewControllerWithRestorationIdentifierPath identifierComponents: [AnyObject], coder coder: NSCoder) -> UIViewController?
  @available(tvOS 6.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, shouldSaveApplicationState coder: NSCoder) -> Bool
  @available(tvOS 6.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, shouldRestoreApplicationState coder: NSCoder) -> Bool
  @available(tvOS 6.0, *)
  optional func application(_ application: UIApplication, willEncodeRestorableStateWith coder: NSCoder)
  @available(tvOS 6.0, *)
  optional func application(_ application: UIApplication, didDecodeRestorableStateWith coder: NSCoder)
  @available(tvOS 8.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, willContinueUserActivityWithType userActivityType: String) -> Bool
  @available(tvOS 8.0, *)
  @discardableResult
  optional func application(_ application: UIApplication, continue userActivity: NSUserActivity, restorationHandler restorationHandler: ([AnyObject]?) -> Void) -> Bool
  @available(tvOS 8.0, *)
  optional func application(_ application: UIApplication, didFailToContinueUserActivityWithType userActivityType: String, error error: NSError)
  @available(tvOS 8.0, *)
  optional func application(_ application: UIApplication, didUpdate userActivity: NSUserActivity)
}
extension UIApplication {
}
@discardableResult
func UIApplicationMain(_ argc: Int32, _ argv: UnsafeMutablePointer<UnsafeMutablePointer<Int8>>!, _ principalClassName: String?, _ delegateClassName: String?) -> Int32
let UITrackingRunLoopMode: String
@available(tvOS 4.0, *)
let UIApplicationDidEnterBackgroundNotification: String
@available(tvOS 4.0, *)
let UIApplicationWillEnterForegroundNotification: String
let UIApplicationDidFinishLaunchingNotification: String
let UIApplicationDidBecomeActiveNotification: String
let UIApplicationWillResignActiveNotification: String
let UIApplicationDidReceiveMemoryWarningNotification: String
let UIApplicationWillTerminateNotification: String
let UIApplicationSignificantTimeChangeNotification: String
@available(tvOS 3.0, *)
let UIApplicationLaunchOptionsURLKey: String
@available(tvOS 3.0, *)
let UIApplicationLaunchOptionsSourceApplicationKey: String
@available(tvOS 3.2, *)
let UIApplicationLaunchOptionsAnnotationKey: String
@available(tvOS 4.0, *)
let UIApplicationProtectedDataWillBecomeUnavailable: String
@available(tvOS 4.0, *)
let UIApplicationProtectedDataDidBecomeAvailable: String
@available(tvOS 4.0, *)
let UIApplicationLaunchOptionsLocationKey: String
@available(tvOS 7.0, *)
let UIApplicationLaunchOptionsBluetoothCentralsKey: String
@available(tvOS 7.0, *)
let UIApplicationLaunchOptionsBluetoothPeripheralsKey: String
@available(tvOS 8.0, *)
let UIApplicationLaunchOptionsUserActivityDictionaryKey: String
@available(tvOS 8.0, *)
let UIApplicationLaunchOptionsUserActivityTypeKey: String
@available(tvOS 8.0, *)
let UIApplicationOpenSettingsURLString: String
@available(tvOS 9.0, *)
let UIApplicationOpenURLOptionsSourceApplicationKey: String
@available(tvOS 9.0, *)
let UIApplicationOpenURLOptionsAnnotationKey: String
@available(tvOS 9.0, *)
let UIApplicationOpenURLOptionsOpenInPlaceKey: String
@available(tvOS 7.0, *)
let UIContentSizeCategoryExtraSmall: String
@available(tvOS 7.0, *)
let UIContentSizeCategorySmall: String
@available(tvOS 7.0, *)
let UIContentSizeCategoryMedium: String
@available(tvOS 7.0, *)
let UIContentSizeCategoryLarge: String
@available(tvOS 7.0, *)
let UIContentSizeCategoryExtraLarge: String
@available(tvOS 7.0, *)
let UIContentSizeCategoryExtraExtraLarge: String
@available(tvOS 7.0, *)
let UIContentSizeCategoryExtraExtraExtraLarge: String
@available(tvOS 7.0, *)
let UIContentSizeCategoryAccessibilityMedium: String
@available(tvOS 7.0, *)
let UIContentSizeCategoryAccessibilityLarge: String
@available(tvOS 7.0, *)
let UIContentSizeCategoryAccessibilityExtraLarge: String
@available(tvOS 7.0, *)
let UIContentSizeCategoryAccessibilityExtraExtraLarge: String
@available(tvOS 7.0, *)
let UIContentSizeCategoryAccessibilityExtraExtraExtraLarge: String
@available(tvOS 7.0, *)
let UIContentSizeCategoryDidChangeNotification: String
@available(tvOS 7.0, *)
let UIContentSizeCategoryNewValueKey: String
@available(tvOS 7.0, *)
let UIApplicationUserDidTakeScreenshotNotification: String
@available(tvOS 8.0, *)
let UIApplicationKeyboardExtensionPointIdentifier: String

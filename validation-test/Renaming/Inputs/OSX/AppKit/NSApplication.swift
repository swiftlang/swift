
let NSAppKitVersionNumber: Double
var NSAppKitVersionNumber10_0: Int32 { get }
var NSAppKitVersionNumber10_1: Int32 { get }
var NSAppKitVersionNumber10_2: Int32 { get }
var NSAppKitVersionNumber10_2_3: Double { get }
var NSAppKitVersionNumber10_3: Int32 { get }
var NSAppKitVersionNumber10_3_2: Double { get }
var NSAppKitVersionNumber10_3_3: Double { get }
var NSAppKitVersionNumber10_3_5: Double { get }
var NSAppKitVersionNumber10_3_7: Double { get }
var NSAppKitVersionNumber10_3_9: Double { get }
var NSAppKitVersionNumber10_4: Int32 { get }
var NSAppKitVersionNumber10_4_1: Double { get }
var NSAppKitVersionNumber10_4_3: Double { get }
var NSAppKitVersionNumber10_4_4: Double { get }
var NSAppKitVersionNumber10_4_7: Double { get }
var NSAppKitVersionNumber10_5: Int32 { get }
var NSAppKitVersionNumber10_5_2: Double { get }
var NSAppKitVersionNumber10_5_3: Double { get }
var NSAppKitVersionNumber10_6: Int32 { get }
var NSAppKitVersionNumber10_7: Int32 { get }
var NSAppKitVersionNumber10_7_2: Double { get }
var NSAppKitVersionNumber10_7_3: Double { get }
var NSAppKitVersionNumber10_7_4: Double { get }
var NSAppKitVersionNumber10_8: Int32 { get }
var NSAppKitVersionNumber10_9: Int32 { get }
var NSAppKitVersionNumber10_10: Int32 { get }
var NSAppKitVersionNumber10_10_2: Int32 { get }
var NSAppKitVersionNumber10_10_3: Int32 { get }
var NSAppKitVersionNumber10_10_4: Int32 { get }
var NSAppKitVersionNumber10_10_5: Int32 { get }
var NSAppKitVersionNumber10_10_Max: Int32 { get }
let NSModalPanelRunLoopMode: String
let NSEventTrackingRunLoopMode: String
var NSModalResponseStop: Int { get }
var NSModalResponseAbort: Int { get }
var NSModalResponseContinue: Int { get }
@available(OSX 10.9, *)
typealias NSModalResponse = Int
var NSUpdateWindowsRunLoopOrdering: Int { get }
@available(OSX 10.6, *)
struct NSApplicationPresentationOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var autoHideDock: NSApplicationPresentationOptions { get }
  static var hideDock: NSApplicationPresentationOptions { get }
  static var autoHideMenuBar: NSApplicationPresentationOptions { get }
  static var hideMenuBar: NSApplicationPresentationOptions { get }
  static var disableAppleMenu: NSApplicationPresentationOptions { get }
  static var disableProcessSwitching: NSApplicationPresentationOptions { get }
  static var disableForceQuit: NSApplicationPresentationOptions { get }
  static var disableSessionTermination: NSApplicationPresentationOptions { get }
  static var disableHideApplication: NSApplicationPresentationOptions { get }
  static var disableMenuBarTransparency: NSApplicationPresentationOptions { get }
  @available(OSX 10.7, *)
  static var fullScreen: NSApplicationPresentationOptions { get }
  @available(OSX 10.7, *)
  static var autoHideToolbar: NSApplicationPresentationOptions { get }
  @available(OSX 10.11.2, *)
  static var disableCursorLocationAssistance: NSApplicationPresentationOptions { get }
}
@available(OSX 10.9, *)
struct NSApplicationOcclusionState : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var visible: NSApplicationOcclusionState { get }
}
typealias NSModalSession = OpaquePointer
class NSApplication : NSResponder, NSUserInterfaceValidations, NSAccessibilityElementProtocol, NSAccessibility {
  @discardableResult
  class func shared() -> NSApplication
  unowned(unsafe) var delegate: @sil_unmanaged NSApplicationDelegate?
  var context: NSGraphicsContext? { get }
  func hide(_ sender: AnyObject?)
  func unhide(_ sender: AnyObject?)
  func unhideWithoutActivation()
  @discardableResult
  func window(withWindowNumber windowNum: Int) -> NSWindow?
  unowned(unsafe) var mainWindow: @sil_unmanaged NSWindow? { get }
  unowned(unsafe) var keyWindow: @sil_unmanaged NSWindow? { get }
  var isActive: Bool { get }
  var isHidden: Bool { get }
  var isRunning: Bool { get }
  func deactivate()
  func activateIgnoringOtherApps(_ flag: Bool)
  func hideOtherApplications(_ sender: AnyObject?)
  func unhideAllApplications(_ sender: AnyObject?)
  func finishLaunching()
  func run()
  @discardableResult
  func runModal(for theWindow: NSWindow) -> Int
  func stop(_ sender: AnyObject?)
  func stopModal()
  func stopModal(withCode returnCode: Int)
  func abortModal()
  var modalWindow: NSWindow? { get }
  @discardableResult
  func beginModalSession(for theWindow: NSWindow) -> NSModalSession
  @discardableResult
  func run(_ session: NSModalSession) -> Int
  func end(_ session: NSModalSession)
  func terminate(_ sender: AnyObject?)
  @discardableResult
  func requestUserAttention(_ requestType: NSRequestUserAttentionType) -> Int
  func cancelUserAttentionRequest(_ request: Int)
  @discardableResult
  func nextEvent(matchingMask mask: Int, until expiration: NSDate?, inMode mode: String, dequeue deqFlag: Bool) -> NSEvent?
  func discardEvents(matchingMask mask: Int, before lastEvent: NSEvent?)
  func post(_ event: NSEvent, atStart flag: Bool)
  var currentEvent: NSEvent? { get }
  func send(_ theEvent: NSEvent)
  func preventWindowOrdering()
  @discardableResult
  func makeWindowsPerform(_ aSelector: Selector, inOrder flag: Bool) -> NSWindow?
  var windows: [NSWindow] { get }
  func setWindowsNeedUpdate(_ needUpdate: Bool)
  func updateWindows()
  var mainMenu: NSMenu?
  @available(OSX 10.6, *)
  var helpMenu: NSMenu?
  var applicationIconImage: NSImage!
  @available(OSX 10.6, *)
  @discardableResult
  func activationPolicy() -> NSApplicationActivationPolicy
  @available(OSX 10.6, *)
  @discardableResult
  func setActivationPolicy(_ activationPolicy: NSApplicationActivationPolicy) -> Bool
  @available(OSX 10.5, *)
  var dockTile: NSDockTile { get }
  @discardableResult
  func sendAction(_ theAction: Selector, to theTarget: AnyObject?, from sender: AnyObject?) -> Bool
  @discardableResult
  func target(forAction theAction: Selector) -> AnyObject?
  @discardableResult
  func target(forAction theAction: Selector, to theTarget: AnyObject?, from sender: AnyObject?) -> AnyObject?
  func report(_ theException: NSException)
  class func detachDrawingThread(_ selector: Selector, toTarget target: AnyObject, with argument: AnyObject?)
  func reply(toApplicationShouldTerminate shouldTerminate: Bool)
  func reply(toOpenOrPrint reply: NSApplicationDelegateReply)
  func orderFrontCharacterPalette(_ sender: AnyObject?)
  @available(OSX 10.6, *)
  var presentationOptions: NSApplicationPresentationOptions
  @available(OSX 10.6, *)
  var currentSystemPresentationOptions: NSApplicationPresentationOptions { get }
  @available(OSX 10.9, *)
  var occlusionState: NSApplicationOcclusionState { get }
}
struct __appFlags {
  var _hidden: UInt32
  var _appleEventActivationInProgress: UInt32
  var _active: UInt32
  var _hasBeenRun: UInt32
  var _doingUnhide: UInt32
  var _delegateReturnsValidRequestor: UInt32
  var _deactPending: UInt32
  var _invalidState: UInt32
  var _invalidEvent: UInt32
  var _postedWindowsNeedUpdateNote: UInt32
  var _wantsToActivate: UInt32
  var _doingHide: UInt32
  var _dontSendShouldTerminate: UInt32
  var _ignoresFullScreen: UInt32
  var _finishedLaunching: UInt32
  var _hasEventDelegate: UInt32
  var _appDying: UInt32
  var _didNSOpenOrPrint: UInt32
  var _inDealloc: UInt32
  var _pendingDidFinish: UInt32
  var _hasKeyFocus: UInt32
  var _panelsNonactivating: UInt32
  var _hiddenOnLaunch: UInt32
  var _openStatus: UInt32
  var _batchOrdering: UInt32
  var _waitingForTerminationReply: UInt32
  var _windowMoveDisabled: UInt32
  var _enumeratingMemoryPressureHandlers: UInt32
  var _didTryRestoringPersistentState: UInt32
  var _reservedN: UInt32
  var _mightBeSwitching: UInt32
  init()
  init(_hidden _hidden: UInt32, _appleEventActivationInProgress _appleEventActivationInProgress: UInt32, _active _active: UInt32, _hasBeenRun _hasBeenRun: UInt32, _doingUnhide _doingUnhide: UInt32, _delegateReturnsValidRequestor _delegateReturnsValidRequestor: UInt32, _deactPending _deactPending: UInt32, _invalidState _invalidState: UInt32, _invalidEvent _invalidEvent: UInt32, _postedWindowsNeedUpdateNote _postedWindowsNeedUpdateNote: UInt32, _wantsToActivate _wantsToActivate: UInt32, _doingHide _doingHide: UInt32, _dontSendShouldTerminate _dontSendShouldTerminate: UInt32, _ignoresFullScreen _ignoresFullScreen: UInt32, _finishedLaunching _finishedLaunching: UInt32, _hasEventDelegate _hasEventDelegate: UInt32, _appDying _appDying: UInt32, _didNSOpenOrPrint _didNSOpenOrPrint: UInt32, _inDealloc _inDealloc: UInt32, _pendingDidFinish _pendingDidFinish: UInt32, _hasKeyFocus _hasKeyFocus: UInt32, _panelsNonactivating _panelsNonactivating: UInt32, _hiddenOnLaunch _hiddenOnLaunch: UInt32, _openStatus _openStatus: UInt32, _batchOrdering _batchOrdering: UInt32, _waitingForTerminationReply _waitingForTerminationReply: UInt32, _windowMoveDisabled _windowMoveDisabled: UInt32, _enumeratingMemoryPressureHandlers _enumeratingMemoryPressureHandlers: UInt32, _didTryRestoringPersistentState _didTryRestoringPersistentState: UInt32, _reservedN _reservedN: UInt32, _mightBeSwitching _mightBeSwitching: UInt32)
}
var NSApp: NSApplication!
enum NSRequestUserAttentionType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case criticalRequest
  case informationalRequest
}
enum NSApplicationDelegateReply : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case success
  case cancel
  case failure
}
extension NSApplication {
  var windowsMenu: NSMenu?
  func arrangeInFront(_ sender: AnyObject?)
  func removeWindowsItem(_ win: NSWindow)
  func addWindowsItem(_ win: NSWindow, title aString: String, filename isFilename: Bool)
  func changeWindowsItem(_ win: NSWindow, title aString: String, filename isFilename: Bool)
  func updateWindowsItem(_ win: NSWindow)
  func miniaturizeAll(_ sender: AnyObject?)
}
extension NSApplication {
  @available(OSX 10.6, *)
  var isFullKeyboardAccessEnabled: Bool { get }
}
enum NSApplicationTerminateReply : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case terminateCancel
  case terminateNow
  case terminateLater
}
enum NSApplicationPrintReply : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case printingCancelled
  case printingSuccess
  case printingFailure
  case printingReplyLater
}
protocol NSApplicationDelegate : NSObjectProtocol {
  @discardableResult
  optional func applicationShouldTerminate(_ sender: NSApplication) -> NSApplicationTerminateReply
  @discardableResult
  optional func application(_ sender: NSApplication, openFile filename: String) -> Bool
  optional func application(_ sender: NSApplication, openFiles filenames: [String])
  @discardableResult
  optional func application(_ sender: NSApplication, openTempFile filename: String) -> Bool
  @discardableResult
  optional func applicationShouldOpenUntitledFile(_ sender: NSApplication) -> Bool
  @discardableResult
  optional func applicationOpenUntitledFile(_ sender: NSApplication) -> Bool
  @discardableResult
  optional func application(_ sender: AnyObject, openFileWithoutUI filename: String) -> Bool
  @discardableResult
  optional func application(_ sender: NSApplication, printFile filename: String) -> Bool
  @discardableResult
  optional func application(_ application: NSApplication, printFiles fileNames: [String], withSettings printSettings: [String : AnyObject], showPrintPanels showPrintPanels: Bool) -> NSApplicationPrintReply
  @discardableResult
  optional func applicationShouldTerminate(afterLastWindowClosed sender: NSApplication) -> Bool
  @discardableResult
  optional func applicationShouldHandleReopen(_ sender: NSApplication, hasVisibleWindows flag: Bool) -> Bool
  @discardableResult
  optional func applicationDockMenu(_ sender: NSApplication) -> NSMenu?
  @discardableResult
  optional func application(_ application: NSApplication, willPresentError error: NSError) -> NSError
  @available(OSX 10.7, *)
  optional func application(_ application: NSApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: NSData)
  @available(OSX 10.7, *)
  optional func application(_ application: NSApplication, didFailToRegisterForRemoteNotificationsWithError error: NSError)
  @available(OSX 10.7, *)
  optional func application(_ application: NSApplication, didReceiveRemoteNotification userInfo: [String : AnyObject])
  @available(OSX 10.7, *)
  optional func application(_ app: NSApplication, willEncodeRestorableState coder: NSCoder)
  @available(OSX 10.7, *)
  optional func application(_ app: NSApplication, didDecodeRestorableState coder: NSCoder)
  @available(OSX 10.10, *)
  @discardableResult
  optional func application(_ application: NSApplication, willContinueUserActivityWithType userActivityType: String) -> Bool
  @available(OSX 10.10, *)
  @discardableResult
  optional func application(_ application: NSApplication, continue userActivity: NSUserActivity, restorationHandler restorationHandler: ([AnyObject]) -> Void) -> Bool
  @available(OSX 10.10, *)
  optional func application(_ application: NSApplication, didFailToContinueUserActivityWithType userActivityType: String, error error: NSError)
  @available(OSX 10.10, *)
  optional func application(_ application: NSApplication, didUpdate userActivity: NSUserActivity)
  optional func applicationWillFinishLaunching(_ notification: NSNotification)
  optional func applicationDidFinishLaunching(_ notification: NSNotification)
  optional func applicationWillHide(_ notification: NSNotification)
  optional func applicationDidHide(_ notification: NSNotification)
  optional func applicationWillUnhide(_ notification: NSNotification)
  optional func applicationDidUnhide(_ notification: NSNotification)
  optional func applicationWillBecomeActive(_ notification: NSNotification)
  optional func applicationDidBecomeActive(_ notification: NSNotification)
  optional func applicationWillResignActive(_ notification: NSNotification)
  optional func applicationDidResignActive(_ notification: NSNotification)
  optional func applicationWillUpdate(_ notification: NSNotification)
  optional func applicationDidUpdate(_ notification: NSNotification)
  optional func applicationWillTerminate(_ notification: NSNotification)
  optional func applicationDidChangeScreenParameters(_ notification: NSNotification)
  @available(OSX 10.9, *)
  optional func applicationDidChangeOcclusionState(_ notification: NSNotification)
}
extension NSApplication {
  var servicesMenu: NSMenu?
  func registerServicesMenuSendTypes(_ sendTypes: [String], returnTypes returnTypes: [String])
}
protocol NSServicesMenuRequestor : NSObjectProtocol {
  @discardableResult
  optional func writeSelection(to pboard: NSPasteboard, types types: [String]) -> Bool
  @discardableResult
  optional func readSelection(from pboard: NSPasteboard) -> Bool
}
extension NSApplication {
  var servicesProvider: AnyObject?
}
extension NSApplication {
  func orderFrontStandardAboutPanel(_ sender: AnyObject?)
  func orderFrontStandardAboutPanel(options optionsDictionary: [String : AnyObject] = [:])
}
extension NSApplication {
  @available(OSX 10.6, *)
  var userInterfaceLayoutDirection: NSUserInterfaceLayoutDirection { get }
}
extension NSApplication {
  @available(OSX 10.7, *)
  func disableRelaunchOnLogin()
  @available(OSX 10.7, *)
  func enableRelaunchOnLogin()
}
struct NSRemoteNotificationType : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  @available(OSX 10.7, *)
  static var none: NSRemoteNotificationType { get }
  @available(OSX 10.7, *)
  static var badge: NSRemoteNotificationType { get }
  @available(OSX 10.8, *)
  static var sound: NSRemoteNotificationType { get }
  @available(OSX 10.8, *)
  static var alert: NSRemoteNotificationType { get }
}
extension NSApplication {
  @available(OSX 10.7, *)
  func register(forRemoteNotificationTypes types: NSRemoteNotificationType)
  @available(OSX 10.7, *)
  func unregisterForRemoteNotifications()
  @available(OSX 10.7, *)
  var enabledRemoteNotificationTypes: NSRemoteNotificationType { get }
}
@discardableResult
func NSApplicationMain(_ argc: Int32, _ argv: UnsafeMutablePointer<UnsafePointer<Int8>>!) -> Int32
@discardableResult
func NSApplicationLoad() -> Bool
@discardableResult
func NSShowsServicesMenuItem(_ itemName: String) -> Bool
@discardableResult
func NSSetShowsServicesMenuItem(_ itemName: String, _ enabled: Bool) -> Int
func NSUpdateDynamicServices()
@discardableResult
func NSPerformService(_ itemName: String, _ pboard: NSPasteboard?) -> Bool
func NSRegisterServicesProvider(_ provider: AnyObject?, _ name: String)
func NSUnregisterServicesProvider(_ name: String)
let NSApplicationDidBecomeActiveNotification: String
let NSApplicationDidHideNotification: String
let NSApplicationDidFinishLaunchingNotification: String
let NSApplicationDidResignActiveNotification: String
let NSApplicationDidUnhideNotification: String
let NSApplicationDidUpdateNotification: String
let NSApplicationWillBecomeActiveNotification: String
let NSApplicationWillHideNotification: String
let NSApplicationWillFinishLaunchingNotification: String
let NSApplicationWillResignActiveNotification: String
let NSApplicationWillUnhideNotification: String
let NSApplicationWillUpdateNotification: String
let NSApplicationWillTerminateNotification: String
let NSApplicationDidChangeScreenParametersNotification: String
@available(OSX 10.7, *)
let NSApplicationLaunchIsDefaultLaunchKey: String
@available(OSX 10.8, *)
let NSApplicationLaunchUserNotificationKey: String
@available(OSX 10.9, *)
let NSApplicationDidChangeOcclusionStateNotification: String
extension NSApplication {
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use -[NSWindow beginSheet:completionHandler:] instead")
  func beginSheet(_ sheet: NSWindow, modalFor docWindow: NSWindow, modalDelegate modalDelegate: AnyObject?, didEnd didEndSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>!)
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use -[NSWindow endSheet:] instead")
  func endSheet(_ sheet: NSWindow)
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use -[NSWindow endSheet:returnCode:] instead")
  func endSheet(_ sheet: NSWindow, returnCode returnCode: Int)
}
@available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use NSModalResponseStop instead")
var NSRunStoppedResponse: Int { get }
@available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use NSModalResponseAbort instead")
var NSRunAbortedResponse: Int { get }
@available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use NSModalResponseContinue instead")
var NSRunContinuesResponse: Int { get }

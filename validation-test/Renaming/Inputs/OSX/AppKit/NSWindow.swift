
var NSAppKitVersionNumberWithCustomSheetPosition: Double { get }
var NSAppKitVersionNumberWithDeferredWindowDisplaySupport: Double { get }
var NSBorderlessWindowMask: Int { get }
var NSTitledWindowMask: Int { get }
var NSClosableWindowMask: Int { get }
var NSMiniaturizableWindowMask: Int { get }
var NSResizableWindowMask: Int { get }
var NSTexturedBackgroundWindowMask: Int { get }
var NSUnifiedTitleAndToolbarWindowMask: Int { get }
@available(OSX 10.7, *)
var NSFullScreenWindowMask: Int { get }
@available(OSX 10.10, *)
var NSFullSizeContentViewWindowMask: Int { get }
var NSModalResponseOK: Int { get }
var NSModalResponseCancel: Int { get }
var NSDisplayWindowRunLoopOrdering: Int { get }
var NSResetCursorRectsRunLoopOrdering: Int { get }
@available(OSX 10.5, *)
enum NSWindowSharingType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case none
  case readOnly
  case readWrite
}
@available(OSX 10.5, *)
enum NSWindowBackingLocation : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case `default`
  case videoMemory
  case mainMemory
}
@available(OSX 10.5, *)
struct NSWindowCollectionBehavior : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var canJoinAllSpaces: NSWindowCollectionBehavior { get }
  static var moveToActiveSpace: NSWindowCollectionBehavior { get }
  @available(OSX 10.6, *)
  static var managed: NSWindowCollectionBehavior { get }
  @available(OSX 10.6, *)
  static var transient: NSWindowCollectionBehavior { get }
  @available(OSX 10.6, *)
  static var stationary: NSWindowCollectionBehavior { get }
  @available(OSX 10.6, *)
  static var participatesInCycle: NSWindowCollectionBehavior { get }
  @available(OSX 10.6, *)
  static var ignoresCycle: NSWindowCollectionBehavior { get }
  @available(OSX 10.7, *)
  static var fullScreenPrimary: NSWindowCollectionBehavior { get }
  @available(OSX 10.7, *)
  static var fullScreenAuxiliary: NSWindowCollectionBehavior { get }
  @available(OSX 10.11, *)
  static var fullScreenAllowsTiling: NSWindowCollectionBehavior { get }
  @available(OSX 10.11, *)
  static var fullScreenDisallowsTiling: NSWindowCollectionBehavior { get }
}
@available(OSX 10.7, *)
enum NSWindowAnimationBehavior : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case none
  case documentWindow
  case utilityWindow
  case alertPanel
}
var NSWindowNumberListAllApplications: Int { get }
var NSWindowNumberListAllSpaces: Int { get }
@available(OSX 10.9, *)
struct NSWindowOcclusionState : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var visible: NSWindowOcclusionState { get }
}
typealias NSWindowNumberListOptions = Int
enum NSSelectionDirection : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case directSelection
  case selectingNext
  case selectingPrevious
}
enum NSWindowButton : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case closeButton
  case miniaturizeButton
  case zoomButton
  case toolbarButton
  case documentIconButton
  @available(OSX 10.7, *)
  case documentVersionsButton
  @available(OSX 10.7, *)
  case fullScreenButton
}
@available(OSX 10.10, *)
enum NSWindowTitleVisibility : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case visible
  case hidden
}
var NSEventDurationForever: Double { get }
class NSWindow : NSResponder, NSAnimatablePropertyContainer, NSUserInterfaceValidations, NSUserInterfaceItemIdentification, NSAppearanceCustomization, NSAccessibilityElementProtocol, NSAccessibility {
  @discardableResult
  class func frameRect(forContentRect cRect: NSRect, styleMask aStyle: Int) -> NSRect
  @discardableResult
  class func contentRect(forFrameRect fRect: NSRect, styleMask aStyle: Int) -> NSRect
  @discardableResult
  class func minFrameWidth(withTitle aTitle: String, styleMask aStyle: Int) -> CGFloat
  @discardableResult
  class func defaultDepthLimit() -> NSWindowDepth
  @discardableResult
  func frameRect(forContentRect contentRect: NSRect) -> NSRect
  @discardableResult
  func contentRect(forFrameRect frameRect: NSRect) -> NSRect
  init(contentRect contentRect: NSRect, styleMask aStyle: Int, backing bufferingType: NSBackingStoreType, defer flag: Bool)
  convenience init(contentRect contentRect: NSRect, styleMask aStyle: Int, backing bufferingType: NSBackingStoreType, defer flag: Bool, screen screen: NSScreen?)
  var title: String
  @available(OSX 10.10, *)
  var titleVisibility: NSWindowTitleVisibility
  @available(OSX 10.10, *)
  var titlebarAppearsTransparent: Bool
  @available(OSX 10.10, *)
  var contentLayoutRect: NSRect { get }
  @available(OSX 10.10, *)
  var contentLayoutGuide: AnyObject? { get }
  @available(OSX 10.10, *)
  var titlebarAccessoryViewControllers: [NSTitlebarAccessoryViewController]
  @available(OSX 10.10, *)
  func addTitlebarAccessoryViewController(_ childViewController: NSTitlebarAccessoryViewController)
  @available(OSX 10.10, *)
  func insertTitlebarAccessoryViewController(_ childViewController: NSTitlebarAccessoryViewController, at index: Int)
  @available(OSX 10.10, *)
  func removeTitlebarAccessoryViewController(at index: Int)
  @available(OSX 10.5, *)
  @NSCopying var representedURL: NSURL?
  var representedFilename: String
  func setTitleWithRepresentedFilename(_ filename: String)
  var isExcludedFromWindowsMenu: Bool
  var contentView: NSView?
  unowned(unsafe) var delegate: @sil_unmanaged NSWindowDelegate?
  var windowNumber: Int { get }
  var styleMask: Int
  @discardableResult
  func fieldEditor(_ createFlag: Bool, for anObject: AnyObject?) -> NSText?
  func endEditing(for anObject: AnyObject?)
  @discardableResult
  func constrainFrameRect(_ frameRect: NSRect, to screen: NSScreen?) -> NSRect
  func setFrame(_ frameRect: NSRect, display flag: Bool)
  func setContentSize(_ aSize: NSSize)
  func setFrameOrigin(_ aPoint: NSPoint)
  func setFrameTopLeftPoint(_ aPoint: NSPoint)
  @discardableResult
  func cascadeTopLeft(from topLeftPoint: NSPoint) -> NSPoint
  var frame: NSRect { get }
  @discardableResult
  func animationResizeTime(_ newFrame: NSRect) -> NSTimeInterval
  func setFrame(_ frameRect: NSRect, display displayFlag: Bool, animate animateFlag: Bool)
  @available(OSX 10.6, *)
  var inLiveResize: Bool { get }
  var showsResizeIndicator: Bool
  var resizeIncrements: NSSize
  var aspectRatio: NSSize
  var contentResizeIncrements: NSSize
  var contentAspectRatio: NSSize
  func disableFlushWindow()
  func enableFlushWindow()
  var isFlushWindowDisabled: Bool { get }
  func flushWindow()
  func flushWindowIfNeeded()
  var viewsNeedDisplay: Bool
  func displayIfNeeded()
  func display()
  var isAutodisplay: Bool
  var preservesContentDuringLiveResize: Bool
  func update()
  @discardableResult
  func makeFirstResponder(_ aResponder: NSResponder?) -> Bool
  unowned(unsafe) var firstResponder: @sil_unmanaged NSResponder { get }
  var resizeFlags: Int { get }
  func close()
  var isReleasedWhenClosed: Bool
  func miniaturize(_ sender: AnyObject?)
  func deminiaturize(_ sender: AnyObject?)
  var isZoomed: Bool { get }
  func zoom(_ sender: AnyObject?)
  var isMiniaturized: Bool { get }
  @NSCopying var backgroundColor: NSColor!
  @available(OSX 10.5, *)
  func setContentBorderThickness(_ thickness: CGFloat, for edge: NSRectEdge)
  @available(OSX 10.5, *)
  @discardableResult
  func contentBorderThickness(for edge: NSRectEdge) -> CGFloat
  @available(OSX 10.5, *)
  func setAutorecalculatesContentBorderThickness(_ flag: Bool, for edge: NSRectEdge)
  @available(OSX 10.5, *)
  @discardableResult
  func autorecalculatesContentBorderThickness(for edge: NSRectEdge) -> Bool
  @available(OSX 10.6, *)
  var isMovable: Bool
  var isMovableByWindowBackground: Bool
  var hidesOnDeactivate: Bool
  var canHide: Bool
  func center()
  func makeKeyAndOrderFront(_ sender: AnyObject?)
  func orderFront(_ sender: AnyObject?)
  func orderBack(_ sender: AnyObject?)
  func orderOut(_ sender: AnyObject?)
  func order(_ place: NSWindowOrderingMode, relativeTo otherWin: Int)
  func orderFrontRegardless()
  var miniwindowImage: NSImage?
  var miniwindowTitle: String!
  @available(OSX 10.5, *)
  var dockTile: NSDockTile { get }
  var isDocumentEdited: Bool
  var isVisible: Bool { get }
  var isKeyWindow: Bool { get }
  var isMainWindow: Bool { get }
  var canBecomeKeyWindow: Bool { get }
  var canBecomeMainWindow: Bool { get }
  func makeKeyWindow()
  func makeMainWindow()
  func becomeKeyWindow()
  func resignKeyWindow()
  func becomeMainWindow()
  func resignMainWindow()
  var worksWhenModal: Bool { get }
  @available(OSX 10.6, *)
  var preventsApplicationTerminationWhenModal: Bool
  @available(OSX 10.7, *)
  @discardableResult
  func convertRect(toScreen aRect: NSRect) -> NSRect
  @available(OSX 10.7, *)
  @discardableResult
  func convertRect(fromScreen aRect: NSRect) -> NSRect
  @available(OSX 10.7, *)
  @discardableResult
  func convertRectToBacking(_ aRect: NSRect) -> NSRect
  @available(OSX 10.7, *)
  @discardableResult
  func convertRectFromBacking(_ aRect: NSRect) -> NSRect
  @available(OSX 10.7, *)
  @discardableResult
  func backingAlignedRect(_ aRect: NSRect, options options: NSAlignmentOptions = []) -> NSRect
  @available(OSX 10.7, *)
  var backingScaleFactor: CGFloat { get }
  func performClose(_ sender: AnyObject?)
  func performMiniaturize(_ sender: AnyObject?)
  func performZoom(_ sender: AnyObject?)
  var isOneShot: Bool
  @discardableResult
  func dataWithEPS(inside rect: NSRect) -> NSData
  @discardableResult
  func dataWithPDF(inside rect: NSRect) -> NSData
  @warn_unqualified_access
  func print(_ sender: AnyObject?)
  func disableCursorRects()
  func enableCursorRects()
  func discardCursorRects()
  var areCursorRectsEnabled: Bool { get }
  func invalidateCursorRects(for aView: NSView)
  func resetCursorRects()
  var allowsToolTipsWhenApplicationIsInactive: Bool
  var backingType: NSBackingStoreType
  var level: Int
  var depthLimit: NSWindowDepth
  func setDynamicDepthLimit(_ flag: Bool)
  var hasDynamicDepthLimit: Bool { get }
  var screen: NSScreen? { get }
  var deepestScreen: NSScreen? { get }
  var hasShadow: Bool
  func invalidateShadow()
  var alphaValue: CGFloat
  var isOpaque: Bool
  @available(OSX 10.5, *)
  var sharingType: NSWindowSharingType
  @available(OSX 10.5, *)
  var preferredBackingLocation: NSWindowBackingLocation
  @available(OSX 10.5, *)
  var backingLocation: NSWindowBackingLocation { get }
  @available(OSX 10.6, *)
  var allowsConcurrentViewDrawing: Bool
  var displaysWhenScreenProfileChanges: Bool
  func disableScreenUpdatesUntilFlush()
  @available(OSX 10.5, *)
  var canBecomeVisibleWithoutLogin: Bool
  @available(OSX 10.5, *)
  var collectionBehavior: NSWindowCollectionBehavior
  @available(OSX 10.7, *)
  var animationBehavior: NSWindowAnimationBehavior
  @available(OSX 10.6, *)
  var isOnActiveSpace: Bool { get }
  @available(OSX 10.7, *)
  func toggleFullScreen(_ sender: AnyObject?)
  var stringWithSavedFrame: String { get }
  func setFrameFrom(_ string: String)
  func saveFrame(usingName name: String)
  @discardableResult
  func setFrameUsingName(_ name: String, force force: Bool) -> Bool
  @discardableResult
  func setFrameUsingName(_ name: String) -> Bool
  @discardableResult
  func setFrameAutosaveName(_ name: String) -> Bool
  var frameAutosaveName: String { get }
  class func removeFrame(usingName name: String)
  func cacheImage(in aRect: NSRect)
  func restoreCachedImage()
  func discardCachedImage()
  var minSize: NSSize
  var maxSize: NSSize
  var contentMinSize: NSSize
  var contentMaxSize: NSSize
  @available(OSX 10.11, *)
  var minFullScreenContentSize: NSSize
  @available(OSX 10.11, *)
  var maxFullScreenContentSize: NSSize
  @available(OSX 10.10, *)
  func trackEvents(matching mask: NSEventMask, timeout timeout: NSTimeInterval, mode mode: String, handler trackingHandler: (NSEvent, UnsafeMutablePointer<ObjCBool>) -> Void)
  @discardableResult
  func nextEvent(matchingMask mask: Int) -> NSEvent?
  @discardableResult
  func nextEvent(matchingMask mask: Int, until expiration: NSDate?, inMode mode: String, dequeue deqFlag: Bool) -> NSEvent?
  func discardEvents(matchingMask mask: Int, before lastEvent: NSEvent?)
  func post(_ event: NSEvent, atStart flag: Bool)
  var currentEvent: NSEvent? { get }
  var acceptsMouseMovedEvents: Bool
  var ignoresMouseEvents: Bool
  var deviceDescription: [String : AnyObject] { get }
  func send(_ theEvent: NSEvent)
  var mouseLocationOutsideOfEventStream: NSPoint { get }
  unowned(unsafe) var windowController: @sil_unmanaged NSWindowController?
  @available(OSX 10.9, *)
  func beginSheet(_ sheetWindow: NSWindow, completionHandler handler: ((NSModalResponse) -> Void)? = nil)
  @available(OSX 10.9, *)
  func beginCriticalSheet(_ sheetWindow: NSWindow, completionHandler handler: ((NSModalResponse) -> Void)? = nil)
  @available(OSX 10.9, *)
  func endSheet(_ sheetWindow: NSWindow)
  @available(OSX 10.9, *)
  func endSheet(_ sheetWindow: NSWindow, returnCode returnCode: NSModalResponse)
  @available(OSX 10.9, *)
  var sheets: [NSWindow] { get }
  var attachedSheet: NSWindow? { get }
  var isSheet: Bool { get }
  @available(OSX 10.9, *)
  var sheetParent: NSWindow? { get }
  @discardableResult
  class func standardWindowButton(_ b: NSWindowButton, forStyleMask styleMask: Int) -> NSButton?
  @discardableResult
  func standardWindowButton(_ b: NSWindowButton) -> NSButton?
  func addChildWindow(_ childWin: NSWindow, ordered place: NSWindowOrderingMode)
  func removeChildWindow(_ childWin: NSWindow)
  var childWindows: [NSWindow]? { get }
  unowned(unsafe) var parent: @sil_unmanaged NSWindow?
  var graphicsContext: NSGraphicsContext? { get }
  @available(OSX 10.6, *)
  var colorSpace: NSColorSpace?
  @available(OSX 10.6, *)
  @discardableResult
  class func windowNumbers(withOptions options: NSWindowNumberListOptions) -> [NSNumber]?
  @available(OSX 10.6, *)
  @discardableResult
  class func windowNumber(at point: NSPoint, belowWindowWithWindowNumber windowNumber: Int) -> Int
  @available(OSX 10.9, *)
  var occlusionState: NSWindowOcclusionState { get }
  @available(OSX 10.10, *)
  var contentViewController: NSViewController?
  @available(OSX 10.10, *)
  convenience init(contentViewController contentViewController: NSViewController)
  @available(OSX 10.11, *)
  func performDrag(with event: NSEvent)
}
struct __wFlags {
  var backing: UInt32
  var visible: UInt32
  var isMainWindow: UInt32
  var isKeyWindow: UInt32
  var hidesOnDeactivate: UInt32
  var dontFreeWhenClosed: UInt32
  var oneShot: UInt32
  var deferred: UInt32
  var cursorRectsDisabled: UInt32
  var haveFreeCursorRects: UInt32
  var validCursorRects: UInt32
  var docEdited: UInt32
  var dynamicDepthLimit: UInt32
  var worksWhenModal: UInt32
  var limitedBecomeKey: UInt32
  var needsFlush: UInt32
  var viewsNeedDisplay: UInt32
  var ignoredFirstMouse: UInt32
  var repostedFirstMouse: UInt32
  var windowDying: UInt32
  var tempHidden: UInt32
  var floatingPanel: UInt32
  var wantsToBeOnMainScreen: UInt32
  var optimizedDrawingOk: UInt32
  var optimizeDrawing: UInt32
  var titleIsRepresentedFilename: UInt32
  var excludedFromWindowsMenu: UInt32
  var depthLimit: UInt32
  var delegateReturnsValidRequestor: UInt32
  var lmouseupPending: UInt32
  var rmouseupPending: UInt32
  var wantsToDestroyRealWindow: UInt32
  var wantsToRegDragTypes: UInt32
  var sentInvalidateCursorRectsMsg: UInt32
  var avoidsActivation: UInt32
  var frameSavedUsingTitle: UInt32
  var didRegDragTypes: UInt32
  var delayedOneShot: UInt32
  var postedNeedsDisplayNote: UInt32
  var postedInvalidCursorRectsNote: UInt32
  var initialFirstResponderTempSet: UInt32
  var autodisplay: UInt32
  var tossedFirstEvent: UInt32
  var isImageCache: UInt32
  var autolayoutEngagedSomewhere: UInt32
  var hasRegisteredBackdropViews: UInt32
  var hasSubLevel: UInt32
  var keyViewSelectionDirection: UInt32
  var defaultButtonCellKETemporarilyDisabled: UInt32
  var defaultButtonCellKEDisabled: UInt32
  var menuHasBeenSet: UInt32
  var wantsToBeModal: UInt32
  var showingModalFrame: UInt32
  var isTerminating: UInt32
  var makingFirstResponderForMouseDown: UInt32
  var needsZoom: UInt32
  var sentWindowNeedsDisplayMsg: UInt32
  var unused: UInt32
  init()
  init(backing backing: UInt32, visible visible: UInt32, isMainWindow isMainWindow: UInt32, isKeyWindow isKeyWindow: UInt32, hidesOnDeactivate hidesOnDeactivate: UInt32, dontFreeWhenClosed dontFreeWhenClosed: UInt32, oneShot oneShot: UInt32, deferred deferred: UInt32, cursorRectsDisabled cursorRectsDisabled: UInt32, haveFreeCursorRects haveFreeCursorRects: UInt32, validCursorRects validCursorRects: UInt32, docEdited docEdited: UInt32, dynamicDepthLimit dynamicDepthLimit: UInt32, worksWhenModal worksWhenModal: UInt32, limitedBecomeKey limitedBecomeKey: UInt32, needsFlush needsFlush: UInt32, viewsNeedDisplay viewsNeedDisplay: UInt32, ignoredFirstMouse ignoredFirstMouse: UInt32, repostedFirstMouse repostedFirstMouse: UInt32, windowDying windowDying: UInt32, tempHidden tempHidden: UInt32, floatingPanel floatingPanel: UInt32, wantsToBeOnMainScreen wantsToBeOnMainScreen: UInt32, optimizedDrawingOk optimizedDrawingOk: UInt32, optimizeDrawing optimizeDrawing: UInt32, titleIsRepresentedFilename titleIsRepresentedFilename: UInt32, excludedFromWindowsMenu excludedFromWindowsMenu: UInt32, depthLimit depthLimit: UInt32, delegateReturnsValidRequestor delegateReturnsValidRequestor: UInt32, lmouseupPending lmouseupPending: UInt32, rmouseupPending rmouseupPending: UInt32, wantsToDestroyRealWindow wantsToDestroyRealWindow: UInt32, wantsToRegDragTypes wantsToRegDragTypes: UInt32, sentInvalidateCursorRectsMsg sentInvalidateCursorRectsMsg: UInt32, avoidsActivation avoidsActivation: UInt32, frameSavedUsingTitle frameSavedUsingTitle: UInt32, didRegDragTypes didRegDragTypes: UInt32, delayedOneShot delayedOneShot: UInt32, postedNeedsDisplayNote postedNeedsDisplayNote: UInt32, postedInvalidCursorRectsNote postedInvalidCursorRectsNote: UInt32, initialFirstResponderTempSet initialFirstResponderTempSet: UInt32, autodisplay autodisplay: UInt32, tossedFirstEvent tossedFirstEvent: UInt32, isImageCache isImageCache: UInt32, autolayoutEngagedSomewhere autolayoutEngagedSomewhere: UInt32, hasRegisteredBackdropViews hasRegisteredBackdropViews: UInt32, hasSubLevel hasSubLevel: UInt32, keyViewSelectionDirection keyViewSelectionDirection: UInt32, defaultButtonCellKETemporarilyDisabled defaultButtonCellKETemporarilyDisabled: UInt32, defaultButtonCellKEDisabled defaultButtonCellKEDisabled: UInt32, menuHasBeenSet menuHasBeenSet: UInt32, wantsToBeModal wantsToBeModal: UInt32, showingModalFrame showingModalFrame: UInt32, isTerminating isTerminating: UInt32, makingFirstResponderForMouseDown makingFirstResponderForMouseDown: UInt32, needsZoom needsZoom: UInt32, sentWindowNeedsDisplayMsg sentWindowNeedsDisplayMsg: UInt32, unused unused: UInt32)
}
extension NSWindow {
  unowned(unsafe) var initialFirstResponder: @sil_unmanaged NSView?
  func selectNextKeyView(_ sender: AnyObject?)
  func selectPreviousKeyView(_ sender: AnyObject?)
  func selectKeyView(following aView: NSView)
  func selectKeyViewPreceding(_ aView: NSView)
  var keyViewSelectionDirection: NSSelectionDirection { get }
  var defaultButtonCell: NSButtonCell?
  func disableKeyEquivalentForDefaultButtonCell()
  func enableKeyEquivalentForDefaultButtonCell()
  var autorecalculatesKeyViewLoop: Bool
  func recalculateKeyViewLoop()
}
extension NSWindow {
  var toolbar: NSToolbar?
  func toggleToolbarShown(_ sender: AnyObject?)
  func runToolbarCustomizationPalette(_ sender: AnyObject?)
  var showsToolbarButton: Bool
}
extension NSWindow {
  func drag(_ anImage: NSImage, at baseLocation: NSPoint, offset initialOffset: NSSize, event event: NSEvent, pasteboard pboard: NSPasteboard, source sourceObj: AnyObject, slideBack slideFlag: Bool)
  func register(forDraggedTypes newTypes: [String])
  func unregisterDraggedTypes()
}
extension NSWindow {
  convenience init?(windowRef windowRef: UnsafeMutablePointer<Void>)
  var windowRef: UnsafeMutablePointer<Void> { get }
}
extension NSWindow {
  @available(OSX, introduced: 10.0, deprecated: 10.11, message: "This method does not do anything and should not be called.")
  class func menuChanged(_ menu: NSMenu)
  @available(OSX, introduced: 10.0, deprecated: 10.10)
  @discardableResult
  func gState() -> Int
  @available(OSX, introduced: 10.0, deprecated: 10.10)
  func useOptimizedDrawing(_ flag: Bool)
  @available(OSX, introduced: 10.0, deprecated: 10.10)
  @discardableResult
  func canStoreColor() -> Bool
}
protocol NSWindowDelegate : NSObjectProtocol {
  @discardableResult
  optional func windowShouldClose(_ sender: AnyObject) -> Bool
  @discardableResult
  optional func windowWillReturnFieldEditor(_ sender: NSWindow, to client: AnyObject?) -> AnyObject?
  @discardableResult
  optional func windowWillResize(_ sender: NSWindow, to frameSize: NSSize) -> NSSize
  @discardableResult
  optional func windowWillUseStandardFrame(_ window: NSWindow, defaultFrame newFrame: NSRect) -> NSRect
  @discardableResult
  optional func windowShouldZoom(_ window: NSWindow, toFrame newFrame: NSRect) -> Bool
  @available(OSX 10.0, *)
  @discardableResult
  optional func windowWillReturnUndoManager(_ window: NSWindow) -> NSUndoManager?
  @discardableResult
  optional func window(_ window: NSWindow, willPositionSheet sheet: NSWindow, using rect: NSRect) -> NSRect
  @available(OSX 10.5, *)
  @discardableResult
  optional func window(_ window: NSWindow, shouldPopUpDocumentPathMenu menu: NSMenu) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func window(_ window: NSWindow, shouldDragDocumentWith event: NSEvent, from dragImageLocation: NSPoint, with pasteboard: NSPasteboard) -> Bool
  @available(OSX 10.7, *)
  @discardableResult
  optional func window(_ window: NSWindow, willUseFullScreenContentSize proposedSize: NSSize) -> NSSize
  @available(OSX 10.7, *)
  @discardableResult
  optional func window(_ window: NSWindow, willUseFullScreenPresentationOptions proposedOptions: NSApplicationPresentationOptions = []) -> NSApplicationPresentationOptions
  @available(OSX 10.7, *)
  @discardableResult
  optional func customWindowsToEnterFullScreen(for window: NSWindow) -> [NSWindow]?
  @available(OSX 10.7, *)
  optional func window(_ window: NSWindow, startCustomAnimationToEnterFullScreenWithDuration duration: NSTimeInterval)
  @available(OSX 10.7, *)
  optional func windowDidFail(toEnterFullScreen window: NSWindow)
  @available(OSX 10.7, *)
  @discardableResult
  optional func customWindowsToExitFullScreen(for window: NSWindow) -> [NSWindow]?
  @available(OSX 10.7, *)
  optional func window(_ window: NSWindow, startCustomAnimationToExitFullScreenWithDuration duration: NSTimeInterval)
  @available(OSX 10.9, *)
  @discardableResult
  optional func customWindowsToEnterFullScreen(for window: NSWindow, on screen: NSScreen) -> [NSWindow]?
  @available(OSX 10.9, *)
  optional func window(_ window: NSWindow, startCustomAnimationToEnterFullScreenOn screen: NSScreen, withDuration duration: NSTimeInterval)
  @available(OSX 10.7, *)
  optional func windowDidFail(toExitFullScreen window: NSWindow)
  @available(OSX 10.7, *)
  @discardableResult
  optional func window(_ window: NSWindow, willResizeForVersionBrowserWithMaxPreferredSize maxPreferredFrameSize: NSSize, maxAllowedSize maxAllowedFrameSize: NSSize) -> NSSize
  @available(OSX 10.7, *)
  optional func window(_ window: NSWindow, willEncodeRestorableState state: NSCoder)
  @available(OSX 10.7, *)
  optional func window(_ window: NSWindow, didDecodeRestorableState state: NSCoder)
  optional func windowDidResize(_ notification: NSNotification)
  optional func windowDidExpose(_ notification: NSNotification)
  optional func windowWillMove(_ notification: NSNotification)
  optional func windowDidMove(_ notification: NSNotification)
  optional func windowDidBecomeKey(_ notification: NSNotification)
  optional func windowDidResignKey(_ notification: NSNotification)
  optional func windowDidBecomeMain(_ notification: NSNotification)
  optional func windowDidResignMain(_ notification: NSNotification)
  optional func windowWillClose(_ notification: NSNotification)
  optional func windowWillMiniaturize(_ notification: NSNotification)
  optional func windowDidMiniaturize(_ notification: NSNotification)
  optional func windowDidDeminiaturize(_ notification: NSNotification)
  optional func windowDidUpdate(_ notification: NSNotification)
  optional func windowDidChangeScreen(_ notification: NSNotification)
  optional func windowDidChangeScreenProfile(_ notification: NSNotification)
  @available(OSX 10.7, *)
  optional func windowDidChangeBackingProperties(_ notification: NSNotification)
  optional func windowWillBeginSheet(_ notification: NSNotification)
  optional func windowDidEndSheet(_ notification: NSNotification)
  @available(OSX 10.6, *)
  optional func windowWillStartLiveResize(_ notification: NSNotification)
  @available(OSX 10.6, *)
  optional func windowDidEndLiveResize(_ notification: NSNotification)
  @available(OSX 10.7, *)
  optional func windowWillEnterFullScreen(_ notification: NSNotification)
  @available(OSX 10.7, *)
  optional func windowDidEnterFullScreen(_ notification: NSNotification)
  @available(OSX 10.7, *)
  optional func windowWillExitFullScreen(_ notification: NSNotification)
  @available(OSX 10.7, *)
  optional func windowDidExitFullScreen(_ notification: NSNotification)
  @available(OSX 10.7, *)
  optional func windowWillEnterVersionBrowser(_ notification: NSNotification)
  @available(OSX 10.7, *)
  optional func windowDidEnterVersionBrowser(_ notification: NSNotification)
  @available(OSX 10.7, *)
  optional func windowWillExitVersionBrowser(_ notification: NSNotification)
  @available(OSX 10.7, *)
  optional func windowDidExitVersionBrowser(_ notification: NSNotification)
  @available(OSX 10.9, *)
  optional func windowDidChangeOcclusionState(_ notification: NSNotification)
}
let NSWindowDidBecomeKeyNotification: String
let NSWindowDidBecomeMainNotification: String
let NSWindowDidChangeScreenNotification: String
let NSWindowDidDeminiaturizeNotification: String
let NSWindowDidExposeNotification: String
let NSWindowDidMiniaturizeNotification: String
let NSWindowDidMoveNotification: String
let NSWindowDidResignKeyNotification: String
let NSWindowDidResignMainNotification: String
let NSWindowDidResizeNotification: String
let NSWindowDidUpdateNotification: String
let NSWindowWillCloseNotification: String
let NSWindowWillMiniaturizeNotification: String
let NSWindowWillMoveNotification: String
let NSWindowWillBeginSheetNotification: String
let NSWindowDidEndSheetNotification: String
@available(OSX 10.7, *)
let NSWindowDidChangeBackingPropertiesNotification: String
@available(OSX 10.7, *)
let NSBackingPropertyOldScaleFactorKey: String
@available(OSX 10.7, *)
let NSBackingPropertyOldColorSpaceKey: String
let NSWindowDidChangeScreenProfileNotification: String
@available(OSX 10.6, *)
let NSWindowWillStartLiveResizeNotification: String
@available(OSX 10.6, *)
let NSWindowDidEndLiveResizeNotification: String
@available(OSX 10.7, *)
let NSWindowWillEnterFullScreenNotification: String
@available(OSX 10.7, *)
let NSWindowDidEnterFullScreenNotification: String
@available(OSX 10.7, *)
let NSWindowWillExitFullScreenNotification: String
@available(OSX 10.7, *)
let NSWindowDidExitFullScreenNotification: String
@available(OSX 10.7, *)
let NSWindowWillEnterVersionBrowserNotification: String
@available(OSX 10.7, *)
let NSWindowDidEnterVersionBrowserNotification: String
@available(OSX 10.7, *)
let NSWindowWillExitVersionBrowserNotification: String
@available(OSX 10.7, *)
let NSWindowDidExitVersionBrowserNotification: String
@available(OSX 10.9, *)
let NSWindowDidChangeOcclusionStateNotification: String
var NSUnscaledWindowMask: Int { get }

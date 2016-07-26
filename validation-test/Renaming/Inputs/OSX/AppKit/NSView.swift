
struct NSAutoresizingMaskOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var viewMinXMargin: NSAutoresizingMaskOptions { get }
  static var viewWidthSizable: NSAutoresizingMaskOptions { get }
  static var viewMaxXMargin: NSAutoresizingMaskOptions { get }
  static var viewMinYMargin: NSAutoresizingMaskOptions { get }
  static var viewHeightSizable: NSAutoresizingMaskOptions { get }
  static var viewMaxYMargin: NSAutoresizingMaskOptions { get }
}
enum NSBorderType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case noBorder
  case lineBorder
  case bezelBorder
  case grooveBorder
}
@available(OSX 10.6, *)
enum NSViewLayerContentsRedrawPolicy : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case never
  case onSetNeedsDisplay
  case duringViewResize
  case beforeViewResize
  @available(OSX 10.9, *)
  case crossfade
}
@available(OSX 10.6, *)
enum NSViewLayerContentsPlacement : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case scaleAxesIndependently
  case scaleProportionallyToFit
  case scaleProportionallyToFill
  case center
  case top
  case topRight
  case right
  case bottomRight
  case bottom
  case bottomLeft
  case left
  case topLeft
}
struct __VFlags {
  var aboutToResize: UInt32
  var isOpaque: UInt32
  var unused3: UInt32
  var hasNotMessedWithIsFlipped: UInt32
  var ignoreHitTest: UInt32
  var specialArchiving: UInt32
  var needsDisplayForBounds: UInt32
  var isFlipped: UInt32
  var removingWithoutInvalidation: UInt32
  var needsBoundsChangeNote: UInt32
  var boundsChangeNotesSuspended: UInt32
  var unused2: UInt32
  var needsFrameChangeNote: UInt32
  var frameChangeNotesSuspended: UInt32
  var canDrawSubviewsIntoLayer: UInt32
  var allowsVibrancy: UInt32
  var needsDisplay: UInt32
  var wantsGState: UInt32
  var autoresizeSubviews: UInt32
  var autosizing: UInt32
  var rotatedOrScaledFromBase: UInt32
  var rotatedFromBase: UInt32
  init()
  init(aboutToResize aboutToResize: UInt32, isOpaque isOpaque: UInt32, unused3 unused3: UInt32, hasNotMessedWithIsFlipped hasNotMessedWithIsFlipped: UInt32, ignoreHitTest ignoreHitTest: UInt32, specialArchiving specialArchiving: UInt32, needsDisplayForBounds needsDisplayForBounds: UInt32, isFlipped isFlipped: UInt32, removingWithoutInvalidation removingWithoutInvalidation: UInt32, needsBoundsChangeNote needsBoundsChangeNote: UInt32, boundsChangeNotesSuspended boundsChangeNotesSuspended: UInt32, unused2 unused2: UInt32, needsFrameChangeNote needsFrameChangeNote: UInt32, frameChangeNotesSuspended frameChangeNotesSuspended: UInt32, canDrawSubviewsIntoLayer canDrawSubviewsIntoLayer: UInt32, allowsVibrancy allowsVibrancy: UInt32, needsDisplay needsDisplay: UInt32, wantsGState wantsGState: UInt32, autoresizeSubviews autoresizeSubviews: UInt32, autosizing autosizing: UInt32, rotatedOrScaledFromBase rotatedOrScaledFromBase: UInt32, rotatedFromBase rotatedFromBase: UInt32)
}
typealias _VFlags = __VFlags
typealias NSTrackingRectTag = Int
typealias NSToolTipTag = Int
class NSView : NSResponder, NSAnimatablePropertyContainer, NSUserInterfaceItemIdentification, NSDraggingDestination, NSAppearanceCustomization, NSAccessibilityElementProtocol, NSAccessibility {
  init(frame frameRect: NSRect)
  unowned(unsafe) var window: @sil_unmanaged NSWindow? { get }
  unowned(unsafe) var superview: @sil_unmanaged NSView? { get }
  var subviews: [NSView]
  @discardableResult
  func isDescendant(of aView: NSView) -> Bool
  @discardableResult
  func ancestorShared(with aView: NSView) -> NSView?
  unowned(unsafe) var opaqueAncestor: @sil_unmanaged NSView? { get }
  var isHidden: Bool
  var isHiddenOrHasHiddenAncestor: Bool { get }
  func getRectsBeingDrawn(_ rects: UnsafeMutablePointer<UnsafePointer<NSRect>?>?, count count: UnsafeMutablePointer<Int>?)
  @discardableResult
  func needs(toDraw aRect: NSRect) -> Bool
  var wantsDefaultClipping: Bool { get }
  @available(OSX 10.5, *)
  func viewDidHide()
  @available(OSX 10.5, *)
  func viewDidUnhide()
  func addSubview(_ aView: NSView)
  func addSubview(_ aView: NSView, positioned place: NSWindowOrderingMode, relativeTo otherView: NSView?)
  func sortSubviews(_ compare: @convention(c) (NSView, NSView, UnsafeMutablePointer<Void>?) -> NSComparisonResult, context context: UnsafeMutablePointer<Void>?)
  func viewWillMove(to newWindow: NSWindow?)
  func viewDidMoveToWindow()
  func viewWillMove(toSuperview newSuperview: NSView?)
  func viewDidMoveToSuperview()
  func didAddSubview(_ subview: NSView)
  func willRemoveSubview(_ subview: NSView)
  func removeFromSuperview()
  func replaceSubview(_ oldView: NSView, with newView: NSView)
  func removeFromSuperviewWithoutNeedingDisplay()
  @available(OSX 10.7, *)
  func viewDidChangeBackingProperties()
  var postsFrameChangedNotifications: Bool
  func resizeSubviews(withOldSize oldSize: NSSize)
  func resize(withOldSuperviewSize oldSize: NSSize)
  var autoresizesSubviews: Bool
  var autoresizingMask: NSAutoresizingMaskOptions
  func setFrameOrigin(_ newOrigin: NSPoint)
  func setFrameSize(_ newSize: NSSize)
  var frame: NSRect
  var frameRotation: CGFloat
  @available(OSX 10.5, *)
  var frameCenterRotation: CGFloat
  func setBoundsOrigin(_ newOrigin: NSPoint)
  func setBoundsSize(_ newSize: NSSize)
  var boundsRotation: CGFloat
  func translateOrigin(to translation: NSPoint)
  func scaleUnitSquare(to newUnitSize: NSSize)
  func rotate(byAngle angle: CGFloat)
  var bounds: NSRect
  var isFlipped: Bool { get }
  var isRotatedFromBase: Bool { get }
  var isRotatedOrScaledFromBase: Bool { get }
  var isOpaque: Bool { get }
  @discardableResult
  func convert(_ aPoint: NSPoint, from aView: NSView?) -> NSPoint
  @discardableResult
  func convert(_ aPoint: NSPoint, to aView: NSView?) -> NSPoint
  @discardableResult
  func convert(_ aSize: NSSize, from aView: NSView?) -> NSSize
  @discardableResult
  func convert(_ aSize: NSSize, to aView: NSView?) -> NSSize
  @discardableResult
  func convert(_ aRect: NSRect, from aView: NSView?) -> NSRect
  @discardableResult
  func convert(_ aRect: NSRect, to aView: NSView?) -> NSRect
  @available(OSX 10.7, *)
  @discardableResult
  func backingAlignedRect(_ aRect: NSRect, options options: NSAlignmentOptions = []) -> NSRect
  @discardableResult
  func centerScanRect(_ aRect: NSRect) -> NSRect
  @available(OSX 10.7, *)
  @discardableResult
  func convertPointToBacking(_ aPoint: NSPoint) -> NSPoint
  @available(OSX 10.7, *)
  @discardableResult
  func convertPointFromBacking(_ aPoint: NSPoint) -> NSPoint
  @available(OSX 10.7, *)
  @discardableResult
  func convertSizeToBacking(_ aSize: NSSize) -> NSSize
  @available(OSX 10.7, *)
  @discardableResult
  func convertSizeFromBacking(_ aSize: NSSize) -> NSSize
  @available(OSX 10.7, *)
  @discardableResult
  func convertRectToBacking(_ aRect: NSRect) -> NSRect
  @available(OSX 10.7, *)
  @discardableResult
  func convertRectFromBacking(_ aRect: NSRect) -> NSRect
  @available(OSX 10.7, *)
  @discardableResult
  func convertPoint(toLayer aPoint: NSPoint) -> NSPoint
  @available(OSX 10.7, *)
  @discardableResult
  func convertPoint(fromLayer aPoint: NSPoint) -> NSPoint
  @available(OSX 10.7, *)
  @discardableResult
  func convertSize(toLayer aSize: NSSize) -> NSSize
  @available(OSX 10.7, *)
  @discardableResult
  func convertSize(fromLayer aSize: NSSize) -> NSSize
  @available(OSX 10.7, *)
  @discardableResult
  func convertRect(toLayer aRect: NSRect) -> NSRect
  @available(OSX 10.7, *)
  @discardableResult
  func convertRect(fromLayer aRect: NSRect) -> NSRect
  @available(OSX 10.6, *)
  var canDrawConcurrently: Bool
  var canDraw: Bool { get }
  func setNeedsDisplayIn(_ invalidRect: NSRect)
  var needsDisplay: Bool
  func lockFocus()
  func unlockFocus()
  @discardableResult
  func lockFocusIfCanDraw() -> Bool
  @discardableResult
  func lockFocusIfCanDraw(in context: NSGraphicsContext) -> Bool
  @discardableResult
  class func focus() -> NSView?
  var visibleRect: NSRect { get }
  func display()
  func displayIfNeeded()
  func displayIfNeededIgnoringOpacity()
  func display(_ rect: NSRect)
  func displayIfNeeded(in rect: NSRect)
  func displayRectIgnoringOpacity(_ rect: NSRect)
  func displayIfNeeded(inRectIgnoringOpacity rect: NSRect)
  func draw(_ dirtyRect: NSRect)
  func displayRectIgnoringOpacity(_ aRect: NSRect, in context: NSGraphicsContext)
  @discardableResult
  func bitmapImageRepForCachingDisplay(in rect: NSRect) -> NSBitmapImageRep?
  func cacheDisplay(in rect: NSRect, to bitmapImageRep: NSBitmapImageRep)
  @available(OSX 10.5, *)
  func viewWillDraw()
  func scroll(_ aPoint: NSPoint)
  @discardableResult
  func scrollRectToVisible(_ aRect: NSRect) -> Bool
  @discardableResult
  func autoscroll(_ theEvent: NSEvent) -> Bool
  @discardableResult
  func adjustScroll(_ newVisible: NSRect) -> NSRect
  func scroll(_ aRect: NSRect, by delta: NSSize)
  @available(OSX 10.5, *)
  func translateRectsNeedingDisplay(in clipRect: NSRect, by delta: NSSize)
  @discardableResult
  func hitTest(_ aPoint: NSPoint) -> NSView?
  @discardableResult
  func mouse(_ aPoint: NSPoint, in aRect: NSRect) -> Bool
  @discardableResult
  func withTag(_ aTag: Int) -> NSView?
  var tag: Int { get }
  @discardableResult
  func acceptsFirstMouse(_ theEvent: NSEvent?) -> Bool
  @discardableResult
  func shouldDelayWindowOrdering(for theEvent: NSEvent) -> Bool
  var needsPanelToBecomeKey: Bool { get }
  var mouseDownCanMoveWindow: Bool { get }
  @available(OSX 10.6, *)
  var acceptsTouchEvents: Bool
  @available(OSX 10.6, *)
  var wantsRestingTouches: Bool
  func addCursorRect(_ aRect: NSRect, cursor anObj: NSCursor)
  func removeCursorRect(_ aRect: NSRect, cursor anObj: NSCursor)
  func discardCursorRects()
  func resetCursorRects()
  @discardableResult
  func addTrackingRect(_ aRect: NSRect, owner anObject: AnyObject, userData data: UnsafeMutablePointer<Void>?, assumeInside flag: Bool) -> NSTrackingRectTag
  func removeTrackingRect(_ tag: NSTrackingRectTag)
  @available(OSX 10.6, *)
  @discardableResult
  func makeBackingLayer() -> CALayer
  @available(OSX 10.6, *)
  var layerContentsRedrawPolicy: NSViewLayerContentsRedrawPolicy
  @available(OSX 10.6, *)
  var layerContentsPlacement: NSViewLayerContentsPlacement
  @available(OSX 10.5, *)
  var wantsLayer: Bool
  @available(OSX 10.5, *)
  var layer: CALayer?
  @available(OSX 10.8, *)
  var wantsUpdateLayer: Bool { get }
  @available(OSX 10.8, *)
  func updateLayer()
  @available(OSX 10.9, *)
  var canDrawSubviewsIntoLayer: Bool
  @available(OSX 10.5, *)
  var alphaValue: CGFloat
  @available(OSX 10.9, *)
  var layerUsesCoreImageFilters: Bool
  @available(OSX 10.5, *)
  var backgroundFilters: [CIFilter]
  @available(OSX 10.5, *)
  var compositingFilter: CIFilter?
  @available(OSX 10.5, *)
  var contentFilters: [CIFilter]
  @available(OSX 10.5, *)
  @NSCopying var shadow: NSShadow?
  @available(OSX 10.5, *)
  func addTrackingArea(_ trackingArea: NSTrackingArea)
  @available(OSX 10.5, *)
  func removeTrackingArea(_ trackingArea: NSTrackingArea)
  @available(OSX 10.5, *)
  var trackingAreas: [NSTrackingArea] { get }
  @available(OSX 10.5, *)
  func updateTrackingAreas()
  var postsBoundsChangedNotifications: Bool
  var enclosingScrollView: NSScrollView? { get }
  @discardableResult
  func menu(for event: NSEvent) -> NSMenu?
  @discardableResult
  class func defaultMenu() -> NSMenu?
  @available(OSX 10.11, *)
  func willOpenMenu(_ menu: NSMenu, with event: NSEvent)
  @available(OSX 10.11, *)
  func didCloseMenu(_ menu: NSMenu, with event: NSEvent?)
  var toolTip: String?
  @discardableResult
  func addToolTip(_ aRect: NSRect, owner anObject: AnyObject, userData data: UnsafeMutablePointer<Void>?) -> NSToolTipTag
  func removeToolTip(_ tag: NSToolTipTag)
  func removeAllToolTips()
  func viewWillStartLiveResize()
  func viewDidEndLiveResize()
  var inLiveResize: Bool { get }
  var preservesContentDuringLiveResize: Bool { get }
  var rectPreservedDuringLiveResize: NSRect { get }
  func getRectsExposedDuringLiveResize(_ exposedRects: UnsafeMutablePointer<NSRect>!, count count: UnsafeMutablePointer<Int>)
  @available(OSX 10.6, *)
  var inputContext: NSTextInputContext? { get }
  @available(OSX 10.8, *)
  @discardableResult
  func rectForSmartMagnification(at location: NSPoint, in visibleRect: NSRect) -> NSRect
  @available(OSX 10.8, *)
  var userInterfaceLayoutDirection: NSUserInterfaceLayoutDirection
  @available(OSX 10.7, *)
  func prepareForReuse()
  @available(OSX 10.9, *)
  @discardableResult
  class func isCompatibleWithResponsiveScrolling() -> Bool
  @available(OSX 10.9, *)
  func prepareContent(in rect: NSRect)
  @available(OSX 10.9, *)
  var preparedContentRect: NSRect
  @available(OSX 10.10, *)
  var allowsVibrancy: Bool { get }
}

extension NSView : CustomPlaygroundQuickLookable {
}
struct __VFlags2 {
  var nextKeyViewRefCount: UInt32
  var previousKeyViewRefCount: UInt32
  var isVisibleRect: UInt32
  var hasToolTip: UInt32
  var cachedIsFlipped: UInt32
  var menuWasSet: UInt32
  init()
  init(nextKeyViewRefCount nextKeyViewRefCount: UInt32, previousKeyViewRefCount previousKeyViewRefCount: UInt32, isVisibleRect isVisibleRect: UInt32, hasToolTip hasToolTip: UInt32, cachedIsFlipped cachedIsFlipped: UInt32, menuWasSet menuWasSet: UInt32)
}
extension NSObject {
  @available(OSX 10.7, *)
  @discardableResult
  class func layer(_ layer: CALayer, shouldInheritContentsScale newScale: CGFloat, from window: NSWindow) -> Bool
  @available(OSX 10.7, *)
  @discardableResult
  func layer(_ layer: CALayer, shouldInheritContentsScale newScale: CGFloat, from window: NSWindow) -> Bool
}
extension NSObject {
  @discardableResult
  class func view(_ view: NSView, stringForToolTip tag: NSToolTipTag, point point: NSPoint, userData data: UnsafeMutablePointer<Void>?) -> String
  @discardableResult
  func view(_ view: NSView, stringForToolTip tag: NSToolTipTag, point point: NSPoint, userData data: UnsafeMutablePointer<Void>?) -> String
}
extension NSView {
  unowned(unsafe) var nextKey: @sil_unmanaged NSView?
  unowned(unsafe) var previousKey: @sil_unmanaged NSView? { get }
  unowned(unsafe) var nextValidKey: @sil_unmanaged NSView? { get }
  unowned(unsafe) var previousValidKey: @sil_unmanaged NSView? { get }
  var canBecomeKeyView: Bool { get }
  func setKeyboardFocusRingNeedsDisplayIn(_ rect: NSRect)
  var focusRingType: NSFocusRingType
  @discardableResult
  class func defaultFocusRingType() -> NSFocusRingType
  @available(OSX 10.7, *)
  func drawFocusRingMask()
  @available(OSX 10.7, *)
  var focusRingMaskBounds: NSRect { get }
  @available(OSX 10.7, *)
  func noteFocusRingMaskChanged()
}
extension NSView {
  func writeEPS(inside rect: NSRect, to pasteboard: NSPasteboard)
  @discardableResult
  func dataWithEPS(inside rect: NSRect) -> NSData
  func writePDF(inside rect: NSRect, to pasteboard: NSPasteboard)
  @discardableResult
  func dataWithPDF(inside rect: NSRect) -> NSData
  @warn_unqualified_access
  func print(_ sender: AnyObject?)
  @discardableResult
  func knowsPageRange(_ range: NSRangePointer) -> Bool
  var heightAdjustLimit: CGFloat { get }
  var widthAdjustLimit: CGFloat { get }
  func adjustPageWidthNew(_ newRight: UnsafeMutablePointer<CGFloat>, left oldLeft: CGFloat, right oldRight: CGFloat, limit rightLimit: CGFloat)
  func adjustPageHeightNew(_ newBottom: UnsafeMutablePointer<CGFloat>, top oldTop: CGFloat, bottom oldBottom: CGFloat, limit bottomLimit: CGFloat)
  @discardableResult
  func rect(forPage page: Int) -> NSRect
  @discardableResult
  func location(ofPrint aRect: NSRect) -> NSPoint
  func drawPageBorder(with borderSize: NSSize)
  @NSCopying var pageHeader: NSAttributedString { get }
  @NSCopying var pageFooter: NSAttributedString { get }
  func drawSheetBorder(with borderSize: NSSize)
  var printJobTitle: String { get }
  func beginDocument()
  func endDocument()
  func beginPage(in aRect: NSRect, atPlacement location: NSPoint)
  func endPage()
}
extension NSView {
  @available(OSX 10.7, *)
  @discardableResult
  func beginDraggingSession(with items: [NSDraggingItem], event event: NSEvent, source source: NSDraggingSource) -> NSDraggingSession
  var registeredDraggedTypes: [String] { get }
  func register(forDraggedTypes newTypes: [String])
  func unregisterDraggedTypes()
  @discardableResult
  func dragFile(_ filename: String, from rect: NSRect, slideBack aFlag: Bool, event event: NSEvent) -> Bool
  @discardableResult
  func dragPromisedFiles(ofTypes typeArray: [String], from rect: NSRect, source sourceObject: AnyObject, slideBack aFlag: Bool, event event: NSEvent) -> Bool
}
extension NSView {
  @available(OSX 10.5, *)
  @discardableResult
  func enterFullScreenMode(_ screen: NSScreen, withOptions options: [String : AnyObject]? = [:]) -> Bool
  @available(OSX 10.5, *)
  func exitFullScreenMode(options options: [String : AnyObject]? = [:])
  @available(OSX 10.5, *)
  var isInFullScreenMode: Bool { get }
}
@available(OSX 10.5, *)
let NSFullScreenModeAllScreens: String
@available(OSX 10.5, *)
let NSFullScreenModeSetting: String
@available(OSX 10.5, *)
let NSFullScreenModeWindowLevel: String
@available(OSX 10.5, *)
let NSFullScreenModeApplicationPresentationOptions: String
extension NSView {
  @available(OSX 10.6, *)
  func showDefinition(for attrString: NSAttributedString?, at textBaselineOrigin: NSPoint)
  @available(OSX 10.6, *)
  func showDefinition(for attrString: NSAttributedString?, range targetRange: NSRange, options options: [String : AnyObject]? = [:], baselineOriginProvider originProvider: ((NSRange) -> NSPoint)? = nil)
}
@available(OSX 10.6, *)
let NSDefinitionPresentationTypeKey: String
@available(OSX 10.6, *)
let NSDefinitionPresentationTypeOverlay: String
@available(OSX 10.6, *)
let NSDefinitionPresentationTypeDictionaryApplication: String
extension NSView {
  @available(OSX 10.7, *)
  var isDrawingFindIndicator: Bool { get }
}
extension NSView {
  @available(OSX 10.10, *)
  var gestureRecognizers: [NSGestureRecognizer]
  @available(OSX 10.10, *)
  func addGestureRecognizer(_ gestureRecognizer: NSGestureRecognizer)
  @available(OSX 10.10, *)
  func removeGestureRecognizer(_ gestureRecognizer: NSGestureRecognizer)
}
extension NSView {
  @available(OSX, introduced: 10.0, deprecated: 10.10)
  @discardableResult
  func shouldDrawColor() -> Bool
  @available(OSX, introduced: 10.0, deprecated: 10.10)
  @discardableResult
  func gState() -> Int
  @available(OSX, introduced: 10.0, deprecated: 10.10)
  func allocateGState()
  @available(OSX, introduced: 10.0, deprecated: 10.10)
  func releaseGState()
  @available(OSX, introduced: 10.0, deprecated: 10.10)
  func setUpGState()
  @available(OSX, introduced: 10.0, deprecated: 10.10)
  func renewGState()
}
let NSViewFrameDidChangeNotification: String
let NSViewFocusDidChangeNotification: String
let NSViewBoundsDidChangeNotification: String
let NSViewGlobalFrameDidChangeNotification: String
@available(OSX 10.5, *)
let NSViewDidUpdateTrackingAreasNotification: String

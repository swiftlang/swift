
@available(OSX 10.7, *)
enum NSScrollElasticity : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case automatic
  case none
  case allowed
}
struct __SFlags {
  var RESERVED: UInt32
  var unarchiving: UInt32
  var registeredForWindowWillClose: UInt32
  var findBarPosition: UInt32
  var predominantAxisScrolling: UInt32
  var hContentElasticity: UInt32
  var vContentElasticity: UInt32
  var unused: UInt32
  var findBarVisible: UInt32
  var autoforwardsScrollWheelEvents: UInt32
  var autohidesScrollers: UInt32
  var hasCustomLineBorderColor: UInt32
  var focusRingNeedsRedisplay: UInt32
  var skipRemoveSuperviewCheck: UInt32
  var doesNotDrawBackground: UInt32
  var needsTile: UInt32
  var hasVerticalRuler: UInt32
  var hasHorizontalRuler: UInt32
  var showRulers: UInt32
  var oldRulerInstalled: UInt32
  var borderType: NSBorderType
  var noDynamicScrolling: UInt32
  var hScrollerStatus: UInt32
  var vScrollerStatus: UInt32
  var hScrollerRequired: UInt32
  var vScrollerRequired: UInt32
  init()
  init(RESERVED RESERVED: UInt32, unarchiving unarchiving: UInt32, registeredForWindowWillClose registeredForWindowWillClose: UInt32, findBarPosition findBarPosition: UInt32, predominantAxisScrolling predominantAxisScrolling: UInt32, hContentElasticity hContentElasticity: UInt32, vContentElasticity vContentElasticity: UInt32, unused unused: UInt32, findBarVisible findBarVisible: UInt32, autoforwardsScrollWheelEvents autoforwardsScrollWheelEvents: UInt32, autohidesScrollers autohidesScrollers: UInt32, hasCustomLineBorderColor hasCustomLineBorderColor: UInt32, focusRingNeedsRedisplay focusRingNeedsRedisplay: UInt32, skipRemoveSuperviewCheck skipRemoveSuperviewCheck: UInt32, doesNotDrawBackground doesNotDrawBackground: UInt32, needsTile needsTile: UInt32, hasVerticalRuler hasVerticalRuler: UInt32, hasHorizontalRuler hasHorizontalRuler: UInt32, showRulers showRulers: UInt32, oldRulerInstalled oldRulerInstalled: UInt32, borderType borderType: NSBorderType, noDynamicScrolling noDynamicScrolling: UInt32, hScrollerStatus hScrollerStatus: UInt32, vScrollerStatus vScrollerStatus: UInt32, hScrollerRequired hScrollerRequired: UInt32, vScrollerRequired vScrollerRequired: UInt32)
}
typealias _SFlags = __SFlags
class NSScrollView : NSView, NSTextFinderBarContainer {
  @available(OSX 10.7, *)
  @discardableResult
  class func frameSize(forContentSize cSize: NSSize, horizontalScrollerClass horizontalScrollerClass: AnyClass?, verticalScrollerClass verticalScrollerClass: AnyClass?, borderType aType: NSBorderType, controlSize controlSize: NSControlSize, scrollerStyle scrollerStyle: NSScrollerStyle) -> NSSize
  @available(OSX 10.7, *)
  @discardableResult
  class func contentSize(forFrameSize fSize: NSSize, horizontalScrollerClass horizontalScrollerClass: AnyClass?, verticalScrollerClass verticalScrollerClass: AnyClass?, borderType aType: NSBorderType, controlSize controlSize: NSControlSize, scrollerStyle scrollerStyle: NSScrollerStyle) -> NSSize
  var documentVisibleRect: NSRect { get }
  var contentSize: NSSize { get }
  unowned(unsafe) var documentView: @sil_unmanaged AnyObject?
  var documentCursor: NSCursor?
  var borderType: NSBorderType
  @NSCopying var backgroundColor: NSColor
  var drawsBackground: Bool
  var hasVerticalScroller: Bool
  var hasHorizontalScroller: Bool
  var verticalScroller: NSScroller?
  var horizontalScroller: NSScroller?
  var autohidesScrollers: Bool
  var horizontalLineScroll: CGFloat
  var verticalLineScroll: CGFloat
  var lineScroll: CGFloat
  var horizontalPageScroll: CGFloat
  var verticalPageScroll: CGFloat
  var pageScroll: CGFloat
  var scrollsDynamically: Bool
  func tile()
  @available(OSX 10.7, *)
  var scrollerStyle: NSScrollerStyle
  @available(OSX 10.7, *)
  var scrollerKnobStyle: NSScrollerKnobStyle
  @available(OSX 10.7, *)
  func flashScrollers()
  @available(OSX 10.7, *)
  var horizontalScrollElasticity: NSScrollElasticity
  @available(OSX 10.7, *)
  var verticalScrollElasticity: NSScrollElasticity
  @available(OSX 10.7, *)
  var usesPredominantAxisScrolling: Bool
  @available(OSX 10.8, *)
  var allowsMagnification: Bool
  @available(OSX 10.8, *)
  var magnification: CGFloat
  @available(OSX 10.8, *)
  var maxMagnification: CGFloat
  @available(OSX 10.8, *)
  var minMagnification: CGFloat
  @available(OSX 10.8, *)
  func magnify(toFit rect: NSRect)
  @available(OSX 10.8, *)
  func setMagnification(_ magnification: CGFloat, centeredAt point: NSPoint)
  @available(OSX 10.9, *)
  func addFloatingSubview(_ view: NSView, for axis: NSEventGestureAxis)
  @available(OSX 10.10, *)
  var automaticallyAdjustsContentInsets: Bool
  @available(OSX 10.10, *)
  var contentInsets: NSEdgeInsets
  @available(OSX 10.10, *)
  var scrollerInsets: NSEdgeInsets
}
@available(OSX 10.8, *)
let NSScrollViewWillStartLiveMagnifyNotification: String
@available(OSX 10.8, *)
let NSScrollViewDidEndLiveMagnifyNotification: String
@available(OSX 10.9, *)
let NSScrollViewWillStartLiveScrollNotification: String
@available(OSX 10.9, *)
let NSScrollViewDidLiveScrollNotification: String
@available(OSX 10.9, *)
let NSScrollViewDidEndLiveScrollNotification: String
extension NSScrollView {
  class func setRulerViewClass(_ rulerViewClass: AnyClass?)
  @discardableResult
  class func rulerViewClass() -> AnyClass
  var rulersVisible: Bool
  var hasHorizontalRuler: Bool
  var hasVerticalRuler: Bool
  var horizontalRulerView: NSRulerView?
  var verticalRulerView: NSRulerView?
}
@available(OSX 10.7, *)
enum NSScrollViewFindBarPosition : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case aboveHorizontalRuler
  case aboveContent
  case belowContent
}
extension NSScrollView {
  @available(OSX 10.7, *)
  var findBarPosition: NSScrollViewFindBarPosition
}

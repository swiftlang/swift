
class NSClipView : NSView {
  @NSCopying var backgroundColor: NSColor
  var drawsBackground: Bool
  unowned(unsafe) var documentView: @sil_unmanaged AnyObject?
  var documentRect: NSRect { get }
  var documentCursor: NSCursor?
  var documentVisibleRect: NSRect { get }
  func viewFrameChanged(_ notification: NSNotification)
  func viewBoundsChanged(_ notification: NSNotification)
  var copiesOnScroll: Bool
  func scroll(to newOrigin: NSPoint)
  @available(OSX 10.9, *)
  @discardableResult
  func constrainBoundsRect(_ proposedBounds: NSRect) -> NSRect
  @available(OSX 10.10, *)
  var contentInsets: NSEdgeInsets
  @available(OSX 10.10, *)
  var automaticallyAdjustsContentInsets: Bool
}
struct __cvFlags {
  var onlyUncovered: UInt32
  var reflectScroll: UInt32
  var usedByCell: UInt32
  var scrollClipTo: UInt32
  var noCopyOnScroll: UInt32
  var drawsBackground: UInt32
  var scrollInProgress: UInt32
  var skipRemoveSuperviewCheck: UInt32
  var animateCurrentScroll: UInt32
  var canAnimateScrolls: UInt32
  var nextScrollRelativeToCurrentPosition: UInt32
  var viewBoundsChangedOverridden: UInt32
  var viewFrameChangedOverridden: UInt32
  var documentViewAlignment: UInt32
  var redrawnWhileScrolling: UInt32
  var dontConstrainScroll: UInt32
  var lastAtEdgesState: UInt32
  var showOverlayScrollersForScrollStep: UInt32
  var scrollerKnobFlashSpecifier: UInt32
  var drawsContentShadow: UInt32
  var dontConstrainBoundsChange: UInt32
  var isScrollDueToUserAction: UInt32
  var hasOverlappingViews: UInt32
  var automaticallyCalculateContentSize: UInt32
  init()
  init(onlyUncovered onlyUncovered: UInt32, reflectScroll reflectScroll: UInt32, usedByCell usedByCell: UInt32, scrollClipTo scrollClipTo: UInt32, noCopyOnScroll noCopyOnScroll: UInt32, drawsBackground drawsBackground: UInt32, scrollInProgress scrollInProgress: UInt32, skipRemoveSuperviewCheck skipRemoveSuperviewCheck: UInt32, animateCurrentScroll animateCurrentScroll: UInt32, canAnimateScrolls canAnimateScrolls: UInt32, nextScrollRelativeToCurrentPosition nextScrollRelativeToCurrentPosition: UInt32, viewBoundsChangedOverridden viewBoundsChangedOverridden: UInt32, viewFrameChangedOverridden viewFrameChangedOverridden: UInt32, documentViewAlignment documentViewAlignment: UInt32, redrawnWhileScrolling redrawnWhileScrolling: UInt32, dontConstrainScroll dontConstrainScroll: UInt32, lastAtEdgesState lastAtEdgesState: UInt32, showOverlayScrollersForScrollStep showOverlayScrollersForScrollStep: UInt32, scrollerKnobFlashSpecifier scrollerKnobFlashSpecifier: UInt32, drawsContentShadow drawsContentShadow: UInt32, dontConstrainBoundsChange dontConstrainBoundsChange: UInt32, isScrollDueToUserAction isScrollDueToUserAction: UInt32, hasOverlappingViews hasOverlappingViews: UInt32, automaticallyCalculateContentSize automaticallyCalculateContentSize: UInt32)
}
extension NSClipView {
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use -constrainBoundsRect: instead")
  @discardableResult
  func constrainScroll(_ newOrigin: NSPoint) -> NSPoint
}
extension NSView {
  func reflectScrolledClipView(_ aClipView: NSClipView)
  func scroll(_ aClipView: NSClipView, to aPoint: NSPoint)
}

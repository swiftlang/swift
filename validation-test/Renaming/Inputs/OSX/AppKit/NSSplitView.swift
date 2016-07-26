
@available(OSX 10.5, *)
enum NSSplitViewDividerStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case thick
  case thin
  @available(OSX 10.6, *)
  case paneSplitter
}
class NSSplitView : NSView {
  var isVertical: Bool
  @available(OSX 10.5, *)
  var dividerStyle: NSSplitViewDividerStyle
  @available(OSX 10.5, *)
  var autosaveName: String?
  unowned(unsafe) var delegate: @sil_unmanaged NSSplitViewDelegate?
  func drawDivider(in rect: NSRect)
  @available(OSX 10.5, *)
  @NSCopying var dividerColor: NSColor { get }
  var dividerThickness: CGFloat { get }
  func adjustSubviews()
  @discardableResult
  func isSubviewCollapsed(_ subview: NSView) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func minPossiblePositionOfDivider(at dividerIndex: Int) -> CGFloat
  @available(OSX 10.5, *)
  @discardableResult
  func maxPossiblePositionOfDivider(at dividerIndex: Int) -> CGFloat
  @available(OSX 10.5, *)
  func setPosition(_ position: CGFloat, ofDividerAt dividerIndex: Int)
  @available(OSX 10.8, *)
  @discardableResult
  func holdingPriorityForSubview(at subviewIndex: Int) -> NSLayoutPriority
  @available(OSX 10.8, *)
  func setHoldingPriority(_ priority: NSLayoutPriority, forSubviewAt subviewIndex: Int)
}
extension NSSplitView {
  @available(OSX 10.11, *)
  var arrangesAllSubviews: Bool
  @available(OSX 10.11, *)
  var arrangedSubviews: [NSView] { get }
  @available(OSX 10.11, *)
  func addArrangedSubview(_ view: NSView)
  @available(OSX 10.11, *)
  func insertArrangedSubview(_ view: NSView, at index: Int)
  @available(OSX 10.11, *)
  func removeArrangedSubview(_ view: NSView)
}
protocol NSSplitViewDelegate : NSObjectProtocol {
  @discardableResult
  optional func splitView(_ splitView: NSSplitView, canCollapseSubview subview: NSView) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func splitView(_ splitView: NSSplitView, shouldCollapseSubview subview: NSView, forDoubleClickOnDividerAt dividerIndex: Int) -> Bool
  @discardableResult
  optional func splitView(_ splitView: NSSplitView, constrainMinCoordinate proposedMinimumPosition: CGFloat, ofSubviewAt dividerIndex: Int) -> CGFloat
  @discardableResult
  optional func splitView(_ splitView: NSSplitView, constrainMaxCoordinate proposedMaximumPosition: CGFloat, ofSubviewAt dividerIndex: Int) -> CGFloat
  @discardableResult
  optional func splitView(_ splitView: NSSplitView, constrainSplitPosition proposedPosition: CGFloat, ofSubviewAt dividerIndex: Int) -> CGFloat
  optional func splitView(_ splitView: NSSplitView, resizeSubviewsWithOldSize oldSize: NSSize)
  @available(OSX 10.6, *)
  @discardableResult
  optional func splitView(_ splitView: NSSplitView, shouldAdjustSizeOfSubview view: NSView) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func splitView(_ splitView: NSSplitView, shouldHideDividerAt dividerIndex: Int) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func splitView(_ splitView: NSSplitView, effectiveRect proposedEffectiveRect: NSRect, forDrawnRect drawnRect: NSRect, ofDividerAt dividerIndex: Int) -> NSRect
  @available(OSX 10.5, *)
  @discardableResult
  optional func splitView(_ splitView: NSSplitView, additionalEffectiveRectOfDividerAt dividerIndex: Int) -> NSRect
  optional func splitViewWillResizeSubviews(_ notification: NSNotification)
  optional func splitViewDidResizeSubviews(_ notification: NSNotification)
}
let NSSplitViewWillResizeSubviewsNotification: String
let NSSplitViewDidResizeSubviewsNotification: String
extension NSSplitView {
}

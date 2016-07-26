
enum NSDrawerState : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case closedState
  case openingState
  case openState
  case closingState
}
class NSDrawer : NSResponder, NSAccessibilityElementProtocol, NSAccessibility {
  init(contentSize contentSize: NSSize, preferredEdge edge: NSRectEdge)
  unowned(unsafe) var parentWindow: @sil_unmanaged NSWindow?
  var contentView: NSView?
  var preferredEdge: NSRectEdge
  unowned(unsafe) var delegate: @sil_unmanaged NSDrawerDelegate?
  func open()
  func open(on edge: NSRectEdge)
  func close()
  func open(_ sender: AnyObject?)
  func close(_ sender: AnyObject?)
  func toggle(_ sender: AnyObject?)
  var state: Int { get }
  var edge: NSRectEdge { get }
  var contentSize: NSSize
  var minContentSize: NSSize
  var maxContentSize: NSSize
  var leadingOffset: CGFloat
  var trailingOffset: CGFloat
}
extension NSWindow {
  var drawers: [NSDrawer]? { get }
}
protocol NSDrawerDelegate : NSObjectProtocol {
  @discardableResult
  optional func drawerShouldOpen(_ sender: NSDrawer) -> Bool
  @discardableResult
  optional func drawerShouldClose(_ sender: NSDrawer) -> Bool
  @discardableResult
  optional func drawerWillResizeContents(_ sender: NSDrawer, to contentSize: NSSize) -> NSSize
  optional func drawerWillOpen(_ notification: NSNotification)
  optional func drawerDidOpen(_ notification: NSNotification)
  optional func drawerWillClose(_ notification: NSNotification)
  optional func drawerDidClose(_ notification: NSNotification)
}
let NSDrawerWillOpenNotification: String
let NSDrawerDidOpenNotification: String
let NSDrawerWillCloseNotification: String
let NSDrawerDidCloseNotification: String

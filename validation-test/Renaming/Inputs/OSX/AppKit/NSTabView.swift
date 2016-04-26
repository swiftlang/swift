
var NSAppKitVersionNumberWithDirectionalTabs: Double { get }
enum NSTabViewType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case topTabsBezelBorder
  case leftTabsBezelBorder
  case bottomTabsBezelBorder
  case rightTabsBezelBorder
  case noTabsBezelBorder
  case noTabsLineBorder
  case noTabsNoBorder
}
class NSTabView : NSView {
  func selectTabViewItem(_ tabViewItem: NSTabViewItem?)
  func selectItem(at index: Int)
  func selectItem(withIdentifier identifier: AnyObject)
  func takeSelectedTabViewItemFromSender(_ sender: AnyObject?)
  func selectFirstTabViewItem(_ sender: AnyObject?)
  func selectLastTabViewItem(_ sender: AnyObject?)
  func selectNextTabViewItem(_ sender: AnyObject?)
  func selectPreviousTabViewItem(_ sender: AnyObject?)
  var selectedTabViewItem: NSTabViewItem? { get }
  var font: NSFont
  var tabViewType: NSTabViewType
  var tabViewItems: [NSTabViewItem] { get }
  var allowsTruncatedLabels: Bool
  var minimumSize: NSSize { get }
  var drawsBackground: Bool
  var controlTint: NSControlTint
  var controlSize: NSControlSize
  func add(_ tabViewItem: NSTabViewItem)
  func insertTabViewItem(_ tabViewItem: NSTabViewItem, at index: Int)
  func removeTabViewItem(_ tabViewItem: NSTabViewItem)
  unowned(unsafe) var delegate: @sil_unmanaged NSTabViewDelegate?
  @discardableResult
  func tabViewItem(at point: NSPoint) -> NSTabViewItem?
  var contentRect: NSRect { get }
  var numberOfTabViewItems: Int { get }
  @discardableResult
  func index(of tabViewItem: NSTabViewItem) -> Int
  @discardableResult
  func tabViewItem(at index: Int) -> NSTabViewItem
  @discardableResult
  func indexOfTabViewItem(withIdentifier identifier: AnyObject) -> Int
}
struct __NSTabViewDelegateRespondTo {
  var shouldSelectTabViewItem: UInt32
  var willSelectTabViewItem: UInt32
  var didSelectTabViewItem: UInt32
  var didChangeNumberOfTabViewItems: UInt32
  var reserved: UInt32
  init()
  init(shouldSelectTabViewItem shouldSelectTabViewItem: UInt32, willSelectTabViewItem willSelectTabViewItem: UInt32, didSelectTabViewItem didSelectTabViewItem: UInt32, didChangeNumberOfTabViewItems didChangeNumberOfTabViewItems: UInt32, reserved reserved: UInt32)
}
struct __NSTabViewFlags {
  var needsLayout: UInt32
  var controlTint: UInt32
  var controlSize: UInt32
  var wiringNibConnections: UInt32
  var wiringInteriorLastKeyView: UInt32
  var originalNextKeyViewChanged: UInt32
  var liveResizeSkippedResetToolTips: UInt32
  var subviewsAddedForTabs: UInt32
  var allowsPropertyChange: UInt32
  var ownedByTabViewController: UInt32
  var reserved: UInt32
  init()
  init(needsLayout needsLayout: UInt32, controlTint controlTint: UInt32, controlSize controlSize: UInt32, wiringNibConnections wiringNibConnections: UInt32, wiringInteriorLastKeyView wiringInteriorLastKeyView: UInt32, originalNextKeyViewChanged originalNextKeyViewChanged: UInt32, liveResizeSkippedResetToolTips liveResizeSkippedResetToolTips: UInt32, subviewsAddedForTabs subviewsAddedForTabs: UInt32, allowsPropertyChange allowsPropertyChange: UInt32, ownedByTabViewController ownedByTabViewController: UInt32, reserved reserved: UInt32)
}
protocol NSTabViewDelegate : NSObjectProtocol {
  @discardableResult
  optional func tabView(_ tabView: NSTabView, shouldSelect tabViewItem: NSTabViewItem?) -> Bool
  optional func tabView(_ tabView: NSTabView, willSelect tabViewItem: NSTabViewItem?)
  optional func tabView(_ tabView: NSTabView, didSelect tabViewItem: NSTabViewItem?)
  optional func tabViewDidChangeNumber(ofTabViewItems tabView: NSTabView)
}

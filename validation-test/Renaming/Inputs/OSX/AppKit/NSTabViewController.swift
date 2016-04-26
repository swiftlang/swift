
@available(OSX 10.10, *)
enum NSTabViewControllerTabStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case segmentedControlOnTop
  case segmentedControlOnBottom
  case toolbar
  case unspecified
}
@available(OSX 10.10, *)
class NSTabViewController : NSViewController, NSTabViewDelegate, NSToolbarDelegate {
  var tabStyle: NSTabViewControllerTabStyle
  var tabView: NSTabView
  var transitionOptions: NSViewControllerTransitionOptions
  var canPropagateSelectedChildViewControllerTitle: Bool
  var tabViewItems: [NSTabViewItem]
  var selectedTabViewItemIndex: Int
  func addTabViewItem(_ tabViewItem: NSTabViewItem)
  func insertTabViewItem(_ tabViewItem: NSTabViewItem, at index: Int)
  func removeTabViewItem(_ tabViewItem: NSTabViewItem)
  @discardableResult
  func tabViewItem(for viewController: NSViewController) -> NSTabViewItem?
}


@available(iOS 8.0, *)
enum UISplitViewControllerDisplayMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case automatic
  case primaryHidden
  case allVisible
  case primaryOverlay
}
@available(iOS 8.0, *)
let UISplitViewControllerAutomaticDimension: CGFloat
@available(iOS 3.2, *)
class UISplitViewController : UIViewController {
  var viewControllers: [UIViewController]
  weak var delegate: @sil_weak UISplitViewControllerDelegate?
  @available(iOS 5.1, *)
  var presentsWithGesture: Bool
  @available(iOS 8.0, *)
  var isCollapsed: Bool { get }
  @available(iOS 8.0, *)
  var preferredDisplayMode: UISplitViewControllerDisplayMode
  @available(iOS 8.0, *)
  var displayMode: UISplitViewControllerDisplayMode { get }
  @available(iOS 8.0, *)
  @discardableResult
  func displayModeButtonItem() -> UIBarButtonItem
  @available(iOS 8.0, *)
  var preferredPrimaryColumnWidthFraction: CGFloat
  @available(iOS 8.0, *)
  var minimumPrimaryColumnWidth: CGFloat
  @available(iOS 8.0, *)
  var maximumPrimaryColumnWidth: CGFloat
  @available(iOS 8.0, *)
  var primaryColumnWidth: CGFloat { get }
}
protocol UISplitViewControllerDelegate {
  @available(iOS 8.0, *)
  optional func splitViewController(_ svc: UISplitViewController, willChangeTo displayMode: UISplitViewControllerDisplayMode)
  @available(iOS 8.0, *)
  @discardableResult
  optional func targetDisplayModeForAction(in svc: UISplitViewController) -> UISplitViewControllerDisplayMode
  @available(iOS 8.0, *)
  @discardableResult
  optional func splitViewController(_ splitViewController: UISplitViewController, show vc: UIViewController, sender sender: AnyObject?) -> Bool
  @available(iOS 8.0, *)
  @discardableResult
  optional func splitViewController(_ splitViewController: UISplitViewController, showDetailViewController vc: UIViewController, sender sender: AnyObject?) -> Bool
  @available(iOS 8.0, *)
  @discardableResult
  optional func primaryViewController(forCollapsing splitViewController: UISplitViewController) -> UIViewController?
  @available(iOS 8.0, *)
  @discardableResult
  optional func primaryViewController(forExpanding splitViewController: UISplitViewController) -> UIViewController?
  @available(iOS 8.0, *)
  @discardableResult
  optional func splitViewController(_ splitViewController: UISplitViewController, collapseSecondaryViewController secondaryViewController: UIViewController, ontoPrimaryViewController primaryViewController: UIViewController) -> Bool
  @available(iOS 8.0, *)
  @discardableResult
  optional func splitViewController(_ splitViewController: UISplitViewController, separateSecondaryViewControllerFromPrimaryViewController primaryViewController: UIViewController) -> UIViewController?
  @available(iOS 7.0, *)
  @discardableResult
  optional func splitViewControllerSupportedInterfaceOrientations(_ splitViewController: UISplitViewController) -> UIInterfaceOrientationMask
  @available(iOS 7.0, *)
  @discardableResult
  optional func splitViewControllerPreferredInterfaceOrientation(forPresentation splitViewController: UISplitViewController) -> UIInterfaceOrientation
  @available(iOS, introduced: 2.0, deprecated: 8.0, message: "Use splitViewController:willChangeToDisplayMode: and displayModeButtonItem instead")
  optional func splitViewController(_ svc: UISplitViewController, willHide aViewController: UIViewController, with barButtonItem: UIBarButtonItem, for pc: UIPopoverController)
  @available(iOS, introduced: 2.0, deprecated: 8.0, message: "Use splitViewController:willChangeToDisplayMode: and displayModeButtonItem instead")
  optional func splitViewController(_ svc: UISplitViewController, willShow aViewController: UIViewController, invalidatingBarButtonItem barButtonItem: UIBarButtonItem)
  @available(iOS, introduced: 2.0, deprecated: 8.0, message: "Use splitViewController:willChangeToDisplayMode: instead")
  optional func splitViewController(_ svc: UISplitViewController, popoverController pc: UIPopoverController, willPresent aViewController: UIViewController)
  @available(iOS, introduced: 5.0, deprecated: 8.0, message: "Use preferredDisplayMode instead")
  @discardableResult
  optional func splitViewController(_ svc: UISplitViewController, shouldHide vc: UIViewController, in orientation: UIInterfaceOrientation) -> Bool
}
extension UIViewController {
  var splitViewController: UISplitViewController? { get }
  @available(iOS 8.0, *)
  func collapseSecondaryViewController(_ secondaryViewController: UIViewController, for splitViewController: UISplitViewController)
  @available(iOS 8.0, *)
  @discardableResult
  func separateSecondaryViewController(for splitViewController: UISplitViewController) -> UIViewController?
}

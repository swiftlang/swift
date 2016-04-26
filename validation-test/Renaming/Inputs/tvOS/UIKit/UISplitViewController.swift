
@available(tvOS 8.0, *)
enum UISplitViewControllerDisplayMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case automatic
  case primaryHidden
  case allVisible
  case primaryOverlay
}
@available(tvOS 8.0, *)
let UISplitViewControllerAutomaticDimension: CGFloat
@available(tvOS 3.2, *)
class UISplitViewController : UIViewController {
  var viewControllers: [UIViewController]
  weak var delegate: @sil_weak UISplitViewControllerDelegate?
  @available(tvOS 5.1, *)
  var presentsWithGesture: Bool
  @available(tvOS 8.0, *)
  var isCollapsed: Bool { get }
  @available(tvOS 8.0, *)
  var preferredDisplayMode: UISplitViewControllerDisplayMode
  @available(tvOS 8.0, *)
  var displayMode: UISplitViewControllerDisplayMode { get }
  @available(tvOS 8.0, *)
  @discardableResult
  func displayModeButtonItem() -> UIBarButtonItem
  @available(tvOS 8.0, *)
  var preferredPrimaryColumnWidthFraction: CGFloat
  @available(tvOS 8.0, *)
  var minimumPrimaryColumnWidth: CGFloat
  @available(tvOS 8.0, *)
  var maximumPrimaryColumnWidth: CGFloat
  @available(tvOS 8.0, *)
  var primaryColumnWidth: CGFloat { get }
}
protocol UISplitViewControllerDelegate {
  @available(tvOS 8.0, *)
  optional func splitViewController(_ svc: UISplitViewController, willChangeTo displayMode: UISplitViewControllerDisplayMode)
  @available(tvOS 8.0, *)
  @discardableResult
  optional func targetDisplayModeForAction(in svc: UISplitViewController) -> UISplitViewControllerDisplayMode
  @available(tvOS 8.0, *)
  @discardableResult
  optional func splitViewController(_ splitViewController: UISplitViewController, show vc: UIViewController, sender sender: AnyObject?) -> Bool
  @available(tvOS 8.0, *)
  @discardableResult
  optional func splitViewController(_ splitViewController: UISplitViewController, showDetailViewController vc: UIViewController, sender sender: AnyObject?) -> Bool
  @available(tvOS 8.0, *)
  @discardableResult
  optional func primaryViewController(forCollapsing splitViewController: UISplitViewController) -> UIViewController?
  @available(tvOS 8.0, *)
  @discardableResult
  optional func primaryViewController(forExpanding splitViewController: UISplitViewController) -> UIViewController?
  @available(tvOS 8.0, *)
  @discardableResult
  optional func splitViewController(_ splitViewController: UISplitViewController, collapseSecondaryViewController secondaryViewController: UIViewController, ontoPrimaryViewController primaryViewController: UIViewController) -> Bool
  @available(tvOS 8.0, *)
  @discardableResult
  optional func splitViewController(_ splitViewController: UISplitViewController, separateSecondaryViewControllerFromPrimaryViewController primaryViewController: UIViewController) -> UIViewController?
}
extension UIViewController {
  var splitViewController: UISplitViewController? { get }
  @available(tvOS 8.0, *)
  func collapseSecondaryViewController(_ secondaryViewController: UIViewController, for splitViewController: UISplitViewController)
  @available(tvOS 8.0, *)
  @discardableResult
  func separateSecondaryViewController(for splitViewController: UISplitViewController) -> UIViewController?
}

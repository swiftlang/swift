
enum UINavigationControllerOperation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case push
  case pop
}
let UINavigationControllerHideShowBarDuration: CGFloat
@available(iOS 2.0, *)
class UINavigationController : UIViewController {
  @available(iOS 5.0, *)
  init(navigationBarClass navigationBarClass: AnyClass?, toolbarClass toolbarClass: AnyClass?)
  init(rootViewController rootViewController: UIViewController)
  func pushViewController(_ viewController: UIViewController, animated animated: Bool)
  @discardableResult
  func popViewController(animated animated: Bool) -> UIViewController?
  @discardableResult
  func pop(to viewController: UIViewController, animated animated: Bool) -> [UIViewController]?
  @discardableResult
  func popToRootViewController(animated animated: Bool) -> [UIViewController]?
  var topViewController: UIViewController? { get }
  var visibleViewController: UIViewController? { get }
  var viewControllers: [UIViewController]
  @available(iOS 3.0, *)
  func setViewControllers(_ viewControllers: [UIViewController], animated animated: Bool)
  var isNavigationBarHidden: Bool
  func setNavigationBarHidden(_ hidden: Bool, animated animated: Bool)
  var navigationBar: UINavigationBar { get }
  @available(iOS 3.0, *)
  var isToolbarHidden: Bool
  @available(iOS 3.0, *)
  func setToolbarHidden(_ hidden: Bool, animated animated: Bool)
  @available(iOS 3.0, *)
  var toolbar: UIToolbar! { get }
  weak var delegate: @sil_weak UINavigationControllerDelegate?
  @available(iOS 7.0, *)
  var interactivePopGestureRecognizer: UIGestureRecognizer? { get }
  @available(iOS 8.0, *)
  var hidesBarsWhenKeyboardAppears: Bool
  @available(iOS 8.0, *)
  var hidesBarsOnSwipe: Bool
  @available(iOS 8.0, *)
  var barHideOnSwipeGestureRecognizer: UIPanGestureRecognizer { get }
  @available(iOS 8.0, *)
  var hidesBarsWhenVerticallyCompact: Bool
  @available(iOS 8.0, *)
  var hidesBarsOnTap: Bool
  @available(iOS 8.0, *)
  unowned(unsafe) var barHideOnTapGestureRecognizer: @sil_unmanaged UITapGestureRecognizer { get }
}
protocol UINavigationControllerDelegate : NSObjectProtocol {
  @available(iOS 2.0, *)
  optional func navigationController(_ navigationController: UINavigationController, willShow viewController: UIViewController, animated animated: Bool)
  @available(iOS 2.0, *)
  optional func navigationController(_ navigationController: UINavigationController, didShow viewController: UIViewController, animated animated: Bool)
  @available(iOS 7.0, *)
  @discardableResult
  optional func navigationControllerSupportedInterfaceOrientations(_ navigationController: UINavigationController) -> UIInterfaceOrientationMask
  @available(iOS 7.0, *)
  @discardableResult
  optional func navigationControllerPreferredInterfaceOrientation(forPresentation navigationController: UINavigationController) -> UIInterfaceOrientation
  @available(iOS 7.0, *)
  @discardableResult
  optional func navigationController(_ navigationController: UINavigationController, interactionControllerForAnimationController animationController: UIViewControllerAnimatedTransitioning) -> UIViewControllerInteractiveTransitioning?
  @available(iOS 7.0, *)
  @discardableResult
  optional func navigationController(_ navigationController: UINavigationController, animationControllerFor operation: UINavigationControllerOperation, from fromVC: UIViewController, to toVC: UIViewController) -> UIViewControllerAnimatedTransitioning?
}
extension UIViewController {
  var navigationItem: UINavigationItem { get }
  var hidesBottomBarWhenPushed: Bool
  var navigationController: UINavigationController? { get }
}
extension UIViewController {
  @available(iOS 3.0, *)
  var toolbarItems: [UIBarButtonItem]?
  @available(iOS 3.0, *)
  func setToolbarItems(_ toolbarItems: [UIBarButtonItem]?, animated animated: Bool)
}

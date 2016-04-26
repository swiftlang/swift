
enum UINavigationControllerOperation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case push
  case pop
}
let UINavigationControllerHideShowBarDuration: CGFloat
@available(tvOS 2.0, *)
class UINavigationController : UIViewController {
  @available(tvOS 5.0, *)
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
  @available(tvOS 3.0, *)
  func setViewControllers(_ viewControllers: [UIViewController], animated animated: Bool)
  var isNavigationBarHidden: Bool
  func setNavigationBarHidden(_ hidden: Bool, animated animated: Bool)
  var navigationBar: UINavigationBar { get }
  weak var delegate: @sil_weak UINavigationControllerDelegate?
}
protocol UINavigationControllerDelegate : NSObjectProtocol {
  @available(tvOS 2.0, *)
  optional func navigationController(_ navigationController: UINavigationController, willShow viewController: UIViewController, animated animated: Bool)
  @available(tvOS 2.0, *)
  optional func navigationController(_ navigationController: UINavigationController, didShow viewController: UIViewController, animated animated: Bool)
  @available(tvOS 7.0, *)
  @discardableResult
  optional func navigationController(_ navigationController: UINavigationController, interactionControllerForAnimationController animationController: UIViewControllerAnimatedTransitioning) -> UIViewControllerInteractiveTransitioning?
  @available(tvOS 7.0, *)
  @discardableResult
  optional func navigationController(_ navigationController: UINavigationController, animationControllerFor operation: UINavigationControllerOperation, from fromVC: UIViewController, to toVC: UIViewController) -> UIViewControllerAnimatedTransitioning?
}
extension UIViewController {
  var navigationItem: UINavigationItem { get }
  var navigationController: UINavigationController? { get }
}
extension UIViewController {
}

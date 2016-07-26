
protocol UIAdaptivePresentationControllerDelegate : NSObjectProtocol {
  @available(iOS 8.0, *)
  @discardableResult
  optional func adaptivePresentationStyle(for controller: UIPresentationController) -> UIModalPresentationStyle
  @available(iOS 8.3, *)
  @discardableResult
  optional func adaptivePresentationStyle(for controller: UIPresentationController, traitCollection traitCollection: UITraitCollection) -> UIModalPresentationStyle
  @available(iOS 8.0, *)
  @discardableResult
  optional func presentationController(_ controller: UIPresentationController, viewControllerForAdaptivePresentationStyle style: UIModalPresentationStyle) -> UIViewController?
  @available(iOS 8.3, *)
  optional func presentationController(_ presentationController: UIPresentationController, willPresentWithAdaptiveStyle style: UIModalPresentationStyle, transitionCoordinator transitionCoordinator: UIViewControllerTransitionCoordinator?)
}
@available(iOS 8.0, *)
class UIPresentationController : NSObject, UIAppearanceContainer, UITraitEnvironment, UIContentContainer, UIFocusEnvironment {
  var presentingViewController: UIViewController { get }
  var presentedViewController: UIViewController { get }
  var presentationStyle: UIModalPresentationStyle { get }
  var containerView: UIView? { get }
  weak var delegate: @sil_weak UIAdaptivePresentationControllerDelegate?
  init(presentedViewController presentedViewController: UIViewController, presenting presentingViewController: UIViewController)
  @discardableResult
  func adaptivePresentationStyle() -> UIModalPresentationStyle
  @available(iOS 8.3, *)
  @discardableResult
  func adaptivePresentationStyle(for traitCollection: UITraitCollection) -> UIModalPresentationStyle
  func containerViewWillLayoutSubviews()
  func containerViewDidLayoutSubviews()
  @discardableResult
  func presentedView() -> UIView?
  @discardableResult
  func frameOfPresentedViewInContainerView() -> CGRect
  @discardableResult
  func shouldPresentInFullscreen() -> Bool
  @discardableResult
  func shouldRemovePresentersView() -> Bool
  func presentationTransitionWillBegin()
  func presentationTransitionDidEnd(_ completed: Bool)
  func dismissalTransitionWillBegin()
  func dismissalTransitionDidEnd(_ completed: Bool)
  @NSCopying var overrideTraitCollection: UITraitCollection?
}

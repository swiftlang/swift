
enum UIModalTransitionStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case coverVertical
  case crossDissolve
}
enum UIModalPresentationStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case fullScreen
  @available(tvOS 3.2, *)
  case currentContext
  @available(tvOS 7.0, *)
  case custom
  @available(tvOS 8.0, *)
  case overFullScreen
  @available(tvOS 8.0, *)
  case overCurrentContext
  @available(tvOS 7.0, *)
  case none
}
protocol UIContentContainer : NSObjectProtocol {
  @available(tvOS 8.0, *)
  var preferredContentSize: CGSize { get }
  @available(tvOS 8.0, *)
  func preferredContentSizeDidChange(forChildContentContainer container: UIContentContainer)
  @available(tvOS 8.0, *)
  func systemLayoutFittingSizeDidChange(forChildContentContainer container: UIContentContainer)
  @available(tvOS 8.0, *)
  @discardableResult
  func size(forChildContentContainer container: UIContentContainer, withParentContainerSize parentSize: CGSize) -> CGSize
  @available(tvOS 8.0, *)
  func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator)
  @available(tvOS 8.0, *)
  func willTransition(to newCollection: UITraitCollection, with coordinator: UIViewControllerTransitionCoordinator)
}
@available(tvOS 8.0, *)
let UIViewControllerShowDetailTargetDidChangeNotification: String
@available(tvOS 2.0, *)
class UIViewController : UIResponder, NSCoding, UIAppearanceContainer, UITraitEnvironment, UIContentContainer, UIFocusEnvironment {
  init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: NSBundle?)
  var view: UIView!
  func loadView()
  @available(tvOS 9.0, *)
  func loadViewIfNeeded()
  @available(tvOS 9.0, *)
  var viewIfLoaded: UIView? { get }
  func viewDidLoad()
  @available(tvOS 3.0, *)
  @discardableResult
  func isViewLoaded() -> Bool
  var nibName: String? { get }
  var nibBundle: NSBundle? { get }
  @available(tvOS 5.0, *)
  var storyboard: UIStoryboard? { get }
  @available(tvOS 5.0, *)
  func performSegue(withIdentifier identifier: String, sender sender: AnyObject?)
  @available(tvOS 6.0, *)
  @discardableResult
  func shouldPerformSegue(withIdentifier identifier: String, sender sender: AnyObject?) -> Bool
  @available(tvOS 5.0, *)
  func prepare(for segue: UIStoryboardSegue, sender sender: AnyObject?)
  @available(tvOS 6.0, *)
  @discardableResult
  func canPerformUnwindSegueAction(_ action: Selector, from fromViewController: UIViewController, withSender sender: AnyObject) -> Bool
  @available(tvOS 9.0, *)
  @discardableResult
  func allowedChildViewControllersForUnwinding(from source: UIStoryboardUnwindSegueSource) -> [UIViewController]
  @available(tvOS 9.0, *)
  @discardableResult
  func childViewControllerContaining(_ source: UIStoryboardUnwindSegueSource) -> UIViewController?
  @available(tvOS, introduced: 6.0, deprecated: 9.0)
  @discardableResult
  func forUnwindSegueAction(_ action: Selector, from fromViewController: UIViewController, withSender sender: AnyObject?) -> UIViewController?
  @available(tvOS 9.0, *)
  func unwind(for unwindSegue: UIStoryboardSegue, towardsViewController subsequentVC: UIViewController)
  @available(tvOS, introduced: 6.0, deprecated: 9.0)
  @discardableResult
  func segueForUnwinding(to toViewController: UIViewController, from fromViewController: UIViewController, identifier identifier: String?) -> UIStoryboardSegue?
  func viewWillAppear(_ animated: Bool)
  func viewDidAppear(_ animated: Bool)
  func viewWillDisappear(_ animated: Bool)
  func viewDidDisappear(_ animated: Bool)
  @available(tvOS 5.0, *)
  func viewWillLayoutSubviews()
  @available(tvOS 5.0, *)
  func viewDidLayoutSubviews()
  var title: String?
  func didReceiveMemoryWarning()
  weak var parent: @sil_weak UIViewController? { get }
  @available(tvOS 5.0, *)
  var presented: UIViewController? { get }
  @available(tvOS 5.0, *)
  var presenting: UIViewController? { get }
  @available(tvOS 5.0, *)
  var definesPresentationContext: Bool
  @available(tvOS 5.0, *)
  var providesPresentationContextTransitionStyle: Bool
  @available(tvOS 5.0, *)
  @discardableResult
  func isBeingPresented() -> Bool
  @available(tvOS 5.0, *)
  @discardableResult
  func isBeingDismissed() -> Bool
  @available(tvOS 5.0, *)
  @discardableResult
  func isMovingToParentViewController() -> Bool
  @available(tvOS 5.0, *)
  @discardableResult
  func isMovingFromParentViewController() -> Bool
  @available(tvOS 5.0, *)
  func present(_ viewControllerToPresent: UIViewController, animated flag: Bool, completion completion: (() -> Void)? = nil)
  @available(tvOS 5.0, *)
  func dismiss(animated flag: Bool, completion completion: (() -> Void)? = nil)
  @available(tvOS 3.0, *)
  var modalTransitionStyle: UIModalTransitionStyle
  @available(tvOS 3.2, *)
  var modalPresentationStyle: UIModalPresentationStyle
  @available(tvOS 4.3, *)
  @discardableResult
  func disablesAutomaticKeyboardDismissal() -> Bool
  @available(tvOS 7.0, *)
  var edgesForExtendedLayout: UIRectEdge
  @available(tvOS 7.0, *)
  var extendedLayoutIncludesOpaqueBars: Bool
  @available(tvOS 7.0, *)
  var automaticallyAdjustsScrollViewInsets: Bool
  @available(tvOS 8.0, *)
  @discardableResult
  func targetViewController(forAction action: Selector, sender sender: AnyObject?) -> UIViewController?
  @available(tvOS 8.0, *)
  func show(_ vc: UIViewController, sender sender: AnyObject?)
  @available(tvOS 8.0, *)
  func showDetailViewController(_ vc: UIViewController, sender sender: AnyObject?)
}
extension UIViewController {
}
extension UIViewController {
  var isEditing: Bool
  func setEditing(_ editing: Bool, animated animated: Bool)
  @discardableResult
  func editButtonItem() -> UIBarButtonItem
}
extension UIViewController {
}
@available(tvOS 5.0, *)
let UIViewControllerHierarchyInconsistencyException: String
extension UIViewController {
  @available(tvOS 5.0, *)
  var childViewControllers: [UIViewController] { get }
  @available(tvOS 5.0, *)
  func addChildViewController(_ childController: UIViewController)
  @available(tvOS 5.0, *)
  func removeFromParentViewController()
  @available(tvOS 5.0, *)
  func transition(from fromViewController: UIViewController, to toViewController: UIViewController, duration duration: NSTimeInterval, options options: UIViewAnimationOptions = [], animations animations: (() -> Void)?, completion completion: ((Bool) -> Void)? = nil)
  @available(tvOS 5.0, *)
  func beginAppearanceTransition(_ isAppearing: Bool, animated animated: Bool)
  @available(tvOS 5.0, *)
  func endAppearanceTransition()
  @available(tvOS 8.0, *)
  func setOverrideTraitCollection(_ collection: UITraitCollection?, forChildViewController childViewController: UIViewController)
  @available(tvOS 8.0, *)
  @discardableResult
  func overrideTraitCollection(forChildViewController childViewController: UIViewController) -> UITraitCollection?
}
extension UIViewController {
  @available(tvOS 6.0, *)
  @discardableResult
  func shouldAutomaticallyForwardAppearanceMethods() -> Bool
  @available(tvOS 5.0, *)
  func willMove(toParentViewController parent: UIViewController?)
  @available(tvOS 5.0, *)
  func didMove(toParentViewController parent: UIViewController?)
}
extension UIViewController : UIStateRestoring {
  @available(tvOS 6.0, *)
  var restorationIdentifier: String?
  @available(tvOS 6.0, *)
  var restorationClass: AnyObject.Type?
}
extension UIViewController {
  @available(tvOS 6.0, *)
  func updateViewConstraints()
}
extension UIViewController {
  @available(tvOS 7.0, *)
  weak var transitioningDelegate: @sil_weak UIViewControllerTransitioningDelegate?
}
extension UIViewController {
  @available(tvOS 7.0, *)
  var topLayoutGuide: UILayoutSupport { get }
  @available(tvOS 7.0, *)
  var bottomLayoutGuide: UILayoutSupport { get }
}
extension UIViewController {
  @available(tvOS 9.0, *)
  func addKeyCommand(_ keyCommand: UIKeyCommand)
  @available(tvOS 9.0, *)
  func removeKeyCommand(_ keyCommand: UIKeyCommand)
}
extension UIViewController : NSExtensionRequestHandling {
  @available(tvOS 8.0, *)
  var extensionContext: NSExtensionContext? { get }
}
extension UIViewController {
  @available(tvOS 8.0, *)
  var presentationController: UIPresentationController? { get }
  @available(tvOS 8.0, *)
  var popoverPresentationController: UIPopoverPresentationController? { get }
}
protocol UIViewControllerPreviewing : NSObjectProtocol {
  @available(tvOS 9.0, *)
  var previewingGestureRecognizerForFailureRelationship: UIGestureRecognizer { get }
  @available(tvOS 9.0, *)
  var delegate: UIViewControllerPreviewingDelegate { get }
  @available(tvOS 9.0, *)
  var sourceView: UIView { get }
  @available(tvOS 9.0, *)
  var sourceRect: CGRect { get set }
}
@available(tvOS 9.0, *)
protocol UIViewControllerPreviewingDelegate : NSObjectProtocol {
  @available(tvOS 9.0, *)
  @discardableResult
  func previewingContext(_ previewingContext: UIViewControllerPreviewing, viewControllerForLocation location: CGPoint) -> UIViewController?
  @available(tvOS 9.0, *)
  func previewingContext(_ previewingContext: UIViewControllerPreviewing, commit viewControllerToCommit: UIViewController)
}
extension UIViewController {
  @available(tvOS 9.0, *)
  @discardableResult
  func registerForPreviewing(with delegate: UIViewControllerPreviewingDelegate, sourceView sourceView: UIView) -> UIViewControllerPreviewing
  @available(tvOS 9.0, *)
  func unregisterForPreviewing(withContext previewing: UIViewControllerPreviewing)
}
extension UIViewController {
  @available(tvOS 9.0, *)
  @discardableResult
  func previewActionItems() -> [UIPreviewActionItem]
}
@available(tvOS 9.0, *)
protocol UIPreviewActionItem : NSObjectProtocol {
  var title: String { get }
}
@available(tvOS 9.0, *)
enum UIPreviewActionStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case selected
  case destructive
}
@available(tvOS 9.0, *)
class UIPreviewAction : NSObject, NSCopying, UIPreviewActionItem {
  var handler: (UIPreviewActionItem, UIViewController) -> Void { get }
  convenience init(title title: String, style style: UIPreviewActionStyle, handler handler: (UIPreviewAction, UIViewController) -> Void)
}
@available(tvOS 9.0, *)
class UIPreviewActionGroup : NSObject, NSCopying, UIPreviewActionItem {
  convenience init(title title: String, style style: UIPreviewActionStyle, actions actions: [UIPreviewAction])
}

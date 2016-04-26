
@available(OSX 10.10, *)
struct NSViewControllerTransitionOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var crossfade: NSViewControllerTransitionOptions { get }
  static var slideUp: NSViewControllerTransitionOptions { get }
  static var slideDown: NSViewControllerTransitionOptions { get }
  static var slideLeft: NSViewControllerTransitionOptions { get }
  static var slideRight: NSViewControllerTransitionOptions { get }
  static var slideForward: NSViewControllerTransitionOptions { get }
  static var slideBackward: NSViewControllerTransitionOptions { get }
  static var allowUserInteraction: NSViewControllerTransitionOptions { get }
}
@available(OSX 10.5, *)
class NSViewController : NSResponder, NSCoding, NSSeguePerforming, NSUserInterfaceItemIdentification {
  init?(nibName nibNameOrNil: String?, bundle nibBundleOrNil: NSBundle?)
  var nibName: String? { get }
  var nibBundle: NSBundle? { get }
  var representedObject: AnyObject?
  var title: String?
  var view: NSView
  func loadView()
  @available(OSX 10.10, *)
  func viewDidLoad()
  @available(OSX 10.10, *)
  var isViewLoaded: Bool { get }
  @available(OSX 10.10, *)
  func viewWillAppear()
  @available(OSX 10.10, *)
  func viewDidAppear()
  @available(OSX 10.10, *)
  func viewWillDisappear()
  @available(OSX 10.10, *)
  func viewDidDisappear()
  @available(OSX 10.10, *)
  var preferredContentSize: NSSize
  @available(OSX 10.10, *)
  func updateViewConstraints()
  @available(OSX 10.10, *)
  func viewWillLayout()
  @available(OSX 10.10, *)
  func viewDidLayout()
}
extension NSViewController {
  @available(OSX 10.10, *)
  func present(_ viewController: NSViewController, animator animator: NSViewControllerPresentationAnimator)
  @available(OSX 10.10, *)
  func dismiss(_ viewController: NSViewController)
  @available(OSX 10.10, *)
  @IBAction func dismiss(_ sender: AnyObject?)
  @available(OSX 10.10, *)
  var presentedViewControllers: [NSViewController]? { get }
  @available(OSX 10.10, *)
  unowned(unsafe) var presenting: @sil_unmanaged NSViewController? { get }
}
extension NSViewController {
  @available(OSX 10.10, *)
  func present(asSheet viewController: NSViewController)
  @available(OSX 10.10, *)
  func present(asModalWindow viewController: NSViewController)
  @available(OSX 10.10, *)
  func present(_ viewController: NSViewController, asPopoverRelativeTo positioningRect: NSRect, of positioningView: NSView, preferredEdge preferredEdge: NSRectEdge, behavior behavior: NSPopoverBehavior)
  @available(OSX 10.10, *)
  func transition(from fromViewController: NSViewController, to toViewController: NSViewController, options options: NSViewControllerTransitionOptions = [], completionHandler completion: (() -> Void)? = nil)
}
extension NSViewController {
  @available(OSX 10.10, *)
  var parent: NSViewController? { get }
  @available(OSX 10.10, *)
  var childViewControllers: [NSViewController]
  @available(OSX 10.10, *)
  func addChildViewController(_ childViewController: NSViewController)
  @available(OSX 10.10, *)
  func removeFromParentViewController()
  @available(OSX 10.10, *)
  func insertChildViewController(_ childViewController: NSViewController, at index: Int)
  @available(OSX 10.10, *)
  func removeChildViewController(at index: Int)
  @available(OSX 10.10, *)
  func preferredContentSizeDidChange(for viewController: NSViewController)
  @available(OSX 10.10, *)
  func viewWillTransition(to newSize: NSSize)
}
protocol NSViewControllerPresentationAnimator : NSObjectProtocol {
  @available(OSX 10.10, *)
  func animatePresentation(of viewController: NSViewController, from fromViewController: NSViewController)
  @available(OSX 10.10, *)
  func animateDismissal(of viewController: NSViewController, from fromViewController: NSViewController)
}
extension NSViewController {
  @available(OSX 10.10, *)
  var storyboard: NSStoryboard? { get }
}
extension NSViewController : NSExtensionRequestHandling {
  @available(OSX 10.10, *)
  var extensionContext: NSExtensionContext? { get }
  @available(OSX 10.10, *)
  @IBOutlet var sourceItemView: NSView?
  @available(OSX 10.10, *)
  var preferredScreenOrigin: NSPoint
  @available(OSX 10.10, *)
  var preferredMinimumSize: NSSize { get }
  @available(OSX 10.10, *)
  var preferredMaximumSize: NSSize { get }
}

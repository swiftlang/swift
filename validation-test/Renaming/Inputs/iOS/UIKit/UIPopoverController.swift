
@available(iOS, introduced: 3.2, deprecated: 9.0, message: "UIPopoverController is deprecated. Popovers are now implemented as UIViewController presentations. Use a modal presentation style of UIModalPresentationPopover and UIPopoverPresentationController.")
class UIPopoverController : NSObject, UIAppearanceContainer {
  init(contentViewController viewController: UIViewController)
  weak var delegate: @sil_weak UIPopoverControllerDelegate?
  var contentViewController: UIViewController
  func setContentViewController(_ viewController: UIViewController, animated animated: Bool)
  var popoverContentSize: CGSize
  func setPopoverContentSize(_ size: CGSize, animated animated: Bool)
  var isPopoverVisible: Bool { get }
  var popoverArrowDirection: UIPopoverArrowDirection { get }
  var passthroughViews: [UIView]?
  func presentPopover(from rect: CGRect, in view: UIView, permittedArrowDirections arrowDirections: UIPopoverArrowDirection, animated animated: Bool)
  func presentPopover(from item: UIBarButtonItem, permittedArrowDirections arrowDirections: UIPopoverArrowDirection, animated animated: Bool)
  func dismissPopover(animated animated: Bool)
  @available(iOS 7.0, *)
  @NSCopying var backgroundColor: UIColor?
  @available(iOS 5.0, *)
  var popoverLayoutMargins: UIEdgeInsets
  @available(iOS 5.0, *)
  var popoverBackgroundViewClass: AnyClass?
}
protocol UIPopoverControllerDelegate : NSObjectProtocol {
  @available(iOS, introduced: 3.2, deprecated: 9.0)
  @discardableResult
  optional func popoverControllerShouldDismissPopover(_ popoverController: UIPopoverController) -> Bool
  @available(iOS, introduced: 3.2, deprecated: 9.0)
  optional func popoverControllerDidDismissPopover(_ popoverController: UIPopoverController)
  @available(iOS, introduced: 7.0, deprecated: 9.0)
  optional func popoverController(_ popoverController: UIPopoverController, willRepositionPopoverTo rect: UnsafeMutablePointer<CGRect>, in view: AutoreleasingUnsafeMutablePointer<UIView>)
}


@available(iOS 3.2, *)
class UIDocumentInteractionController : NSObject, UIActionSheetDelegate {
  /*not inherited*/ init(url url: NSURL)
  weak var delegate: @sil_weak UIDocumentInteractionControllerDelegate?
  var url: NSURL?
  var uti: String?
  var name: String?
  var icons: [UIImage] { get }
  var annotation: AnyObject?
  @discardableResult
  func presentOptionsMenu(from rect: CGRect, in view: UIView, animated animated: Bool) -> Bool
  @discardableResult
  func presentOptionsMenu(from item: UIBarButtonItem, animated animated: Bool) -> Bool
  @discardableResult
  func presentPreview(animated animated: Bool) -> Bool
  @discardableResult
  func presentOpenInMenu(from rect: CGRect, in view: UIView, animated animated: Bool) -> Bool
  @discardableResult
  func presentOpenInMenu(from item: UIBarButtonItem, animated animated: Bool) -> Bool
  func dismissPreview(animated animated: Bool)
  func dismissMenu(animated animated: Bool)
  var gestureRecognizers: [UIGestureRecognizer] { get }
}
protocol UIDocumentInteractionControllerDelegate : NSObjectProtocol {
  @available(iOS 3.2, *)
  @discardableResult
  optional func documentInteractionControllerViewController(forPreview controller: UIDocumentInteractionController) -> UIViewController
  @available(iOS 3.2, *)
  @discardableResult
  optional func documentInteractionControllerRect(forPreview controller: UIDocumentInteractionController) -> CGRect
  @available(iOS 3.2, *)
  @discardableResult
  optional func documentInteractionControllerView(forPreview controller: UIDocumentInteractionController) -> UIView?
  @available(iOS 3.2, *)
  optional func documentInteractionControllerWillBeginPreview(_ controller: UIDocumentInteractionController)
  @available(iOS 3.2, *)
  optional func documentInteractionControllerDidEndPreview(_ controller: UIDocumentInteractionController)
  @available(iOS 3.2, *)
  optional func documentInteractionControllerWillPresentOptionsMenu(_ controller: UIDocumentInteractionController)
  @available(iOS 3.2, *)
  optional func documentInteractionControllerDidDismissOptionsMenu(_ controller: UIDocumentInteractionController)
  @available(iOS 3.2, *)
  optional func documentInteractionControllerWillPresentOpen(inMenu controller: UIDocumentInteractionController)
  @available(iOS 3.2, *)
  optional func documentInteractionControllerDidDismissOpen(inMenu controller: UIDocumentInteractionController)
  @available(iOS 3.2, *)
  optional func documentInteractionController(_ controller: UIDocumentInteractionController, willBeginSendingToApplication application: String?)
  @available(iOS 3.2, *)
  optional func documentInteractionController(_ controller: UIDocumentInteractionController, didEndSendingToApplication application: String?)
}

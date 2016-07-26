
@available(iOS 8.0, *)
class SLComposeServiceViewController : UIViewController, UITextViewDelegate {
  func presentationAnimationDidFinish()
  var textView: UITextView! { get }
  var contentText: String! { get }
  var placeholder: String!
  func didSelectPost()
  func didSelectCancel()
  func cancel()
  @discardableResult
  func isContentValid() -> Bool
  func validateContent()
  var charactersRemaining: NSNumber!
  @discardableResult
  func configurationItems() -> [AnyObject]!
  func reloadConfigurationItems()
  func pushConfigurationViewController(_ viewController: UIViewController!)
  func popConfigurationViewController()
  @discardableResult
  func loadPreviewView() -> UIView!
  var autoCompletionViewController: UIViewController!
}

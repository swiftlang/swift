
enum SLComposeViewControllerResult : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case cancelled
  case done
}
typealias SLComposeViewControllerCompletionHandler = (SLComposeViewControllerResult) -> Void
@available(iOS 6.0, *)
class SLComposeViewController : UIViewController {
  @discardableResult
  class func isAvailable(forServiceType serviceType: String!) -> Bool
  /*not inherited*/ init!(forServiceType serviceType: String!)
  var serviceType: String! { get }
  @discardableResult
  func setInitialText(_ text: String!) -> Bool
  @discardableResult
  func add(_ image: UIImage!) -> Bool
  @discardableResult
  func removeAllImages() -> Bool
  @discardableResult
  func add(_ url: NSURL!) -> Bool
  @discardableResult
  func removeAllURLs() -> Bool
  var completionHandler: SLComposeViewControllerCompletionHandler!
}

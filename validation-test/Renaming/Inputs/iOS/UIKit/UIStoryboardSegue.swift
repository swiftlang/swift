
@available(iOS 5.0, *)
class UIStoryboardSegue : NSObject {
  @available(iOS 6.0, *)
  convenience init(identifier identifier: String?, source source: UIViewController, destination destination: UIViewController, performHandler performHandler: () -> Void)
  init(identifier identifier: String?, source source: UIViewController, destination destination: UIViewController)
  var identifier: String? { get }
  var sourceViewController: UIViewController { get }
  var destinationViewController: UIViewController { get }
  func perform()
}
@available(iOS 9.0, *)
class UIStoryboardUnwindSegueSource : NSObject {
  var sourceViewController: UIViewController { get }
  var unwindAction: Selector { get }
  var sender: AnyObject? { get }
}


protocol TVInterfaceCreating : NSObjectProtocol {
  @available(tvOS 9.0, *)
  @discardableResult
  optional func makeView(element element: TVViewElement, existingView existingView: UIView?) -> UIView?
  @available(tvOS 9.0, *)
  @discardableResult
  optional func makeViewController(element element: TVViewElement, existingViewController existingViewController: UIViewController?) -> UIViewController?
  @available(tvOS 9.0, *)
  @discardableResult
  optional func resourceURL(name resourceName: String) -> NSURL?
  @available(tvOS 9.2, *)
  @discardableResult
  optional func image(forResource resourceName: String) -> UIImage?
}
@available(tvOS 9.0, *)
class TVInterfaceFactory : NSObject, TVInterfaceCreating {
  @discardableResult
  class func shared() -> Self
  var extendedInterfaceCreator: TVInterfaceCreating?
}


@available(tvOS 9.0, *)
class TVApplicationControllerContext : NSObject, NSCopying {
  @NSCopying var javaScriptApplicationURL: NSURL
  var storageIdentifier: String?
  var launchOptions: [String : AnyObject]
}
protocol TVApplicationControllerDelegate : NSObjectProtocol {
  @available(tvOS 9.0, *)
  optional func appController(_ appController: TVApplicationController, evaluateAppJavaScriptIn jsContext: JSContext)
  @available(tvOS 9.0, *)
  optional func appController(_ appController: TVApplicationController, didFinishLaunching options: [String : AnyObject]?)
  @available(tvOS 9.0, *)
  optional func appController(_ appController: TVApplicationController, didFail error: NSError)
  @available(tvOS 9.0, *)
  optional func appController(_ appController: TVApplicationController, didStop options: [String : AnyObject]?)
}
@available(tvOS 9.0, *)
class TVApplicationController : NSObject {
  init(context context: TVApplicationControllerContext, window window: UIWindow?, delegate delegate: TVApplicationControllerDelegate?)
  var window: UIWindow? { get }
  var context: TVApplicationControllerContext { get }
  weak var delegate: @sil_weak TVApplicationControllerDelegate? { get }
  var navigationController: UINavigationController { get }
  func evaluate(inJavaScriptContext evaluation: (JSContext) -> Void, completion completion: ((Bool) -> Void)? = nil)
  func stop()
}
